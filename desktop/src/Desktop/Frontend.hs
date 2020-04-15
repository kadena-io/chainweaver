{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Desktop.Frontend (desktopFrontend, bipWallet, bipCryptoGenPair, runFileStorageT) where

import Control.Exception (catch)
import Control.Lens ((?~))
import Control.Monad ((<=<), guard, void, when)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class
import Data.Bool (bool)
import Data.Bifunctor (first)
import Data.Dependent.Sum
import Data.Functor.Compose
import Data.Functor.Identity
import Data.GADT.Compare.TH
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time (NominalDiffTime, getCurrentTime, addUTCTime)
import Data.Traversable (for)
import Language.Javascript.JSaddle (liftJSM)
import Pact.Server.ApiClient (HasTransactionLogger, runTransactionLoggerT, logTransactionFile, askTransactionLogger)
import Reflex.Dom.Core
import qualified Cardano.Crypto.Wallet as Crypto
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as GlobalEventHandlers

import System.Directory (getCurrentDirectory, getTemporaryDirectory)
import System.FilePath ((</>))

import Common.Api (getConfigRoute)
import Common.Route
import Common.Wallet
import Frontend.AppCfg
import Desktop.Crypto.BIP
import Frontend.ModuleExplorer.Impl (loadEditorFromLocalStorage)
import Frontend.Log (defaultLogger)
import Frontend.Wallet (genZeroKeyPrefix, _unPublicKeyPrefix)
import Frontend.Storage
import Frontend.UI.Button
import Frontend.UI.Widgets
import Obelisk.Configs
import Obelisk.Generated.Static
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import qualified Frontend
import qualified Frontend.ReplGhcjs
import Frontend.VersionedStore (StoreFrontend(..))
import Frontend.Storage (runBrowserStorageT)

import Frontend.UI.Modal.Impl (showModalBrutal)
import Frontend.UI.Dialogs.LogoutConfirmation (uiIdeLogoutConfirmation)

import Desktop.Setup
import Desktop.ImportExport
import Desktop.Storage.File
import Desktop.WalletApi

import Pact.Server.ApiClient (WalletEvent (..), commandLogFilename, _transactionLogger_walletEvent, _transactionLogger_rotateLogFile)

-- | This is for development
-- > ob run --import desktop:Desktop
desktopFrontend :: Frontend (R FrontendRoute)
desktopFrontend = Frontend
  { _frontend_head = do
      let backendEncoder = either (error "frontend: Failed to check backendRouteEncoder") id $
            checkEncoder backendRouteEncoder
      base <- getConfigRoute
      void $ Frontend.newHead $ \r -> base <> renderBackendRoute backendEncoder r
  , _frontend_body = prerender_ blank $ do
    logDir <- (<> "/" <> commandLogFilename) <$> liftIO getTemporaryDirectory
    liftIO $ putStrLn $ "Logging to: " <> logDir
    (signingIOHandler, keysIOHandler, accountsIOHandler) <- walletServer
      (pure ()) -- Can't foreground or background things
      (pure ())
    mapRoutedT (flip runTransactionLoggerT (logTransactionFile logDir) . runBrowserStorageT) $ do
      (fileOpened, triggerOpen) <- Frontend.openFileDialog
      signingHandler <- mkFRPHandler signingIOHandler
      keysHandler <- mkFRPHandler keysIOHandler
      accountsHandler <- mkFRPHandler accountsIOHandler
      let fileFFI = FileFFI
            { _fileFFI_externalFileOpened = fileOpened
            , _fileFFI_openFileDialog = liftJSM . triggerOpen
            , _fileFFI_deliverFile = deliverFile
            }
      bipWallet fileFFI $ \enabledSettings -> AppCfg
        { _appCfg_gistEnabled = False
        , _appCfg_loadEditor = loadEditorFromLocalStorage
        , _appCfg_editorReadOnly = False
        , _appCfg_signingHandler = signingHandler
        , _appCfg_keysEndpointHandler = keysHandler
        , _appCfg_accountsEndpointHandler = accountsHandler
        , _appCfg_enabledSettings = enabledSettings
        , _appCfg_logMessage = defaultLogger
        }
  }

-- This is the deliver file that is only used for development (i.e jsaddle web version)
-- so it is hacky and just writes to the current directory. We could likely make a
-- thing with a data uri and then click on it with JS, but this is probably OK enough
-- for dev.
deliverFile
  :: (MonadIO (Performable m), PerformEvent t m)
  => Event t (FilePath, Text)
  -> m (Event t (Either Text FilePath))
deliverFile eInput = performEvent . ffor eInput $ \(fName, fContent) -> liftIO $ do
  dirName <- getCurrentDirectory
  catch
    ((T.writeFile (dirName </> fName) fContent) *> (pure . Right $ dirName </> fName))
    $ \(e :: IOError) ->
      pure . Left . T.pack $ "Error '" <> show e <> "' exporting file: " <> show fName <> " to  " <> dirName

data LockScreen a where
  LockScreen_Restore :: LockScreen Crypto.XPrv -- ^ Root key
  LockScreen_RunSetup :: LockScreen ()
  LockScreen_Locked :: LockScreen Crypto.XPrv -- ^ Root key
  LockScreen_Unlocked :: LockScreen (Crypto.XPrv, Text) -- ^ The root key and password

type MkAppCfg t m
  =  EnabledSettings Crypto.XPrv t (RoutedT t (R FrontendRoute) (BIPCryptoT t m))
  -- ^ Settings
  -> AppCfg Crypto.XPrv t (RoutedT t (R FrontendRoute) (BIPCryptoT t m))

bipWallet
  :: forall t m
  .  ( MonadWidget t m
     , RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m
     , HasConfigs m
     , HasStorage m, HasStorage (Performable m)
     , HasTransactionLogger m
     )
  => FileFFI t m
  -> MkAppCfg t m
  -> RoutedT t (R FrontendRoute) m ()
bipWallet fileFFI mkAppCfg = do
  txLogger <- askTransactionLogger

  let
    runSetup0
      :: Maybe (Behavior t Crypto.XPrv)
      -> WalletExists
      -> RoutedT t (R FrontendRoute) m (Event t (DSum LockScreen Identity))
    runSetup0 mPrv walletExists = do
      keyAndPass <- runSetup (liftFileFFI lift fileFFI) (isJust mPrv) walletExists
      performEvent $ flip push keyAndPass $ \case
        Right (x, Password p, newWallet) -> pure $ Just $ do
          setItemStorage localStorage BIPStorage_RootKey x
          when newWallet $ do
            liftIO $ _transactionLogger_rotateLogFile txLogger
            removeItemStorage localStorage StoreFrontend_Wallet_Keys
            removeItemStorage localStorage StoreFrontend_Wallet_Accounts
          pure $ LockScreen_Unlocked ==> (x, p)
        Left _ ->
          for mPrv $ fmap (pure . (LockScreen_Locked ==>)) . sample

  mRoot <- getItemStorage localStorage BIPStorage_RootKey
  let initScreen = case mRoot of
        Nothing -> LockScreen_RunSetup :=> Identity ()
        Just xprv -> LockScreen_Locked ==> xprv
  rec
    -- Which screen we are on, along with extra information
    whichScreen <- factorDyn =<< holdDyn initScreen updateScreen
    updateScreen <- switchHold never <=< dyn $ ffor whichScreen $ \case
      -- Run the restore process or return to the lock screen
      LockScreen_Restore :=> Compose root -> runSetup0 (Just $ fmap runIdentity $ current root) WalletExists_Yes
      -- We have no wallet so run the creation/setup process
      LockScreen_RunSetup :=> _ -> runSetup0 Nothing WalletExists_No
      -- Wallet exists but the lock screen is active
      LockScreen_Locked :=> Compose root -> do
        (restore, mLogin) <- lockScreen $ fmap runIdentity $ current root
        pure $ leftmost
          [ (LockScreen_Restore ==>) . runIdentity <$> current root <@ restore
          , (LockScreen_Unlocked ==>) <$> attach (runIdentity <$> current root) mLogin
          ]
      -- The user is logged in
      LockScreen_Unlocked :=> Compose details -> do
        mapRoutedT (runBIPCryptoT $ runIdentity <$> current details) $ do
          (onLogout, sidebarLogoutLink) <- mkSidebarLogoutLink

          onLogoutConfirm <- fmap switchDyn $ widgetHold (pure never)
            $ showModalBrutal "logout-confirm-modal" uiIdeLogoutConfirmation <$ onLogout

          (updates, trigger) <- newTriggerEvent

          let frontendFileFFI = liftFileFFI (lift . lift) fileFFI
          Frontend.ReplGhcjs.app sidebarLogoutLink frontendFileFFI $ mkAppCfg $ EnabledSettings
            { _enabledSettings_changePassword = Just $ ChangePassword
              { _changePassword_requestChange =
                let doChange (Identity (oldRoot, _)) (oldPass, newPass, repeatPass)
                      | passwordRoundTripTest oldRoot oldPass = case checkPassword newPass repeatPass of
                        Left e -> pure $ Left e
                        Right _ -> do
                          -- Change password for root key
                          let newRoot = Crypto.xPrvChangePass (T.encodeUtf8 oldPass) (T.encodeUtf8 newPass) oldRoot
                          setItemStorage localStorage BIPStorage_RootKey newRoot
                          liftIO $ trigger (newRoot, newPass)
                          pure $ Right ()
                      | otherwise = pure $ Left "Invalid password"
                in performEvent . attachWith doChange (current details)
              -- When updating the keys here, we just always regenerate the key from
              -- the new root
              , _changePassword_updateKeys = ffor updates $ \(newRoot, newPass) i _ ->
                let (newPrv, pub) = bipCryptoGenPair newRoot newPass i
                in Key $ KeyPair
                  { _keyPair_publicKey = pub
                  , _keyPair_privateKey = Just newPrv
                  }
              }
            , _enabledSettings_exportWallet = Just $ ExportWallet
              { _exportWallet_requestExport = \ePw -> do
                  let bOldPw = (\(Identity (_,oldPw)) -> oldPw) <$> current details
                      runExport oldPw newPw = do
                        pfx <- genZeroKeyPrefix
                        doExport txLogger pfx oldPw newPw

                      logExport = do
                        ts <- liftIO getCurrentTime
                        sender <- genZeroKeyPrefix
                        liftIO $ _transactionLogger_walletEvent txLogger
                          WalletEvent_Export
                          (_unPublicKeyPrefix sender)
                          ts

                  eExport <- performEvent $ runExport
                    <$> (Password <$> bOldPw)
                    <@> (Password <$> ePw)

                  let (eErrExport, eGoodExport) = fanEither eExport

                  eFileDone <- _fileFFI_deliverFile frontendFileFFI eGoodExport
                  eLogExportDone <- performEvent $ (\r -> r <$ logExport) <$> eFileDone

                  pure $ leftmost
                    [ Left <$> eErrExport
                    , first ExportWalletError_FileNotWritable <$> eLogExportDone
                    ]
              }
            , _enabledSettings_transactionLog = True
            }

          setRoute $ landingPageRoute <$ onLogoutConfirm
          pure $ leftmost
            [ (LockScreen_Unlocked ==>) <$> updates
            , (LockScreen_Locked ==>) . fst . runIdentity <$> current details <@ onLogoutConfirm
            ]
  pure ()


-- | Returns an event which fires at the given check interval when the user has
-- been inactive for at least the given timeout.
_watchInactivity :: MonadWidget t m => NominalDiffTime -> NominalDiffTime -> m (Event t ())
_watchInactivity checkInterval timeout = do
  t0 <- liftIO getCurrentTime
  (activity, act) <- newTriggerEvent
  liftJSM $ do
    win <- DOM.currentWindowUnchecked
    void $ EventM.on win GlobalEventHandlers.click $ liftIO $ act =<< getCurrentTime
    void $ EventM.on win GlobalEventHandlers.keyDown $ liftIO $ act =<< getCurrentTime
  lastActivity <- hold t0 activity
  check <- tickLossyFromPostBuildTime checkInterval
  let checkTime la ti = guard $ addUTCTime timeout la <= _tickInfo_lastUTC ti
  pure $ attachWithMaybe checkTime lastActivity check

mkSidebarLogoutLink :: (TriggerEvent t m, PerformEvent t n, PostBuild t n, DomBuilder t n, MonadIO (Performable n)) => m (Event t (), n ())
mkSidebarLogoutLink = do
  (logout, triggerLogout) <- newTriggerEvent
  pure $ (,) logout $ do
    clk <- uiSidebarIcon (pure False) (static @"img/menu/logout.svg") "Logout"
    performEvent_ $ liftIO . triggerLogout <$> clk

lockScreen :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m) => Behavior t Crypto.XPrv -> m (Event t (), Event t Text)
lockScreen xprv = setupDiv "fullscreen" $ divClass "wrapper" $ setupDiv "splash" $ do
  splashLogo

  el "div" $ mdo
    dValid <- holdDyn True $ leftmost
      [ isJust <$> isValid
      , True <$ _inputElement_input pass
      ]

    let unlock = void $ confirmButton (def & uiButtonCfg_type ?~ "submit") "Unlock"
        cfg = def & elementConfig_initialAttributes .~ ("class" =: setupClass "splash-terms-buttons")
    (eSubmit, pass) <- uiForm cfg unlock $ do
      elDynClass "div"
        (("lock-screen__invalid-password" <>) . bool " lock-screen__invalid-password--invalid" "" <$> dValid)
        (text "Invalid Password")
      uiPassword (setupClass "password-wrapper") (setupClass "password") "Password"

    restore <- setupDiv "button-horizontal-group" $ do
      elAttr "a" ( "class" =: "button button_type_secondary setup__help" <>
                   "href" =: "https://www.kadena.io/chainweaver" <>
                   "target" =: "_blank"
                 ) $ do
        elAttr "img" ("src" =: static @"img/launch_dark.svg" <> "class" =: "button__text-icon") blank
        text "Help"
      uiButton btnCfgSecondary $ text "Restore"

    let isValid = attachWith (\(p, x) _ -> p <$ guard (passwordRoundTripTest x p)) ((,) <$> current (value pass) <*> xprv) eSubmit
    pure (restore, fmapMaybe id isValid)

deriveGEq ''LockScreen
