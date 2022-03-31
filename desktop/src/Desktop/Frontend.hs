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

module Desktop.Frontend (bipWallet, bipCryptoGenPair, runFileStorageT) where

import Control.Concurrent (MVar)
import Control.Exception (catch)
import Control.Monad ((<=<), guard, void, when)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class
import Data.Bifunctor (second)
import Data.Dependent.Sum
import Data.Functor.Compose
import Data.Functor.Identity
import Data.GADT.Compare.TH
import Data.Maybe (isJust)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Time (NominalDiffTime, getCurrentTime, addUTCTime)
import Data.Traversable (for)
import Kadena.SigningApi (SigningRequest, QuickSignRequest)
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
import Desktop.Orphans ()
import Frontend.ModuleExplorer.Impl (loadEditorFromLocalStorage)
import Frontend.Log (defaultLogger)
import Frontend.Storage
import Frontend.UI.Modal.Impl (showModalBrutal)
import Frontend.UI.Dialogs.LogoutConfirmation (uiIdeLogoutConfirmation)
import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import qualified Frontend (newHead, openFileDialog)
import qualified Frontend.App
import Frontend.VersionedStore (StoreFrontend(..))
import Frontend.Storage (runBrowserStorageT)
import Frontend.Crypto.Password
import Frontend.Setup.Common
import Frontend.Setup.ImportExport
import Frontend.Setup.Password
import Frontend.Setup.Setup
import Frontend.Setup.Widgets
import Desktop.Storage.File
import Desktop.WalletApi
import Frontend.WalletConnect

import Pact.Server.ApiClient (WalletEvent (..), commandLogFilename, _transactionLogger_walletEvent, _transactionLogger_rotateLogFile)

--TODO: Can we remove this now that we no longer use a custom version of obelisk that supports
-- `ob run --import desktop:Desktop` workflow?

-- -- | This is for development
-- -- > ob run --import desktop:Desktop
-- desktopFrontend :: Frontend (R FrontendRoute)
-- desktopFrontend = Frontend
--   { _frontend_head = do
--       let backendEncoder = either (error "frontend: Failed to check backendRouteEncoder") id $
--             checkEncoder backendRouteEncoder
--       base <- getConfigRoute
--       void $ Frontend.newHead $ \r -> base <> renderBackendRoute backendEncoder r
--   , _frontend_body = prerender_ blank $ do
--       logDir <- (<> "/" <> commandLogFilename) <$> liftIO getTemporaryDirectory
--       liftIO $ putStrLn $ "Logging to: " <> logDir
--       signingHandler <- walletServer
--         (pure ()) -- Can't foreground or background things
--         (pure ())
--       mapRoutedT (flip runTransactionLoggerT (logTransactionFile logDir) . runBrowserStorageT) $ do
--         (fileOpened, triggerOpen) <- Frontend.openFileDialog
--         let fileFFI = FileFFI
--               { _fileFFI_externalFileOpened = fileOpened
--               , _fileFFI_openFileDialog = liftJSM . triggerOpen
--               , _fileFFI_deliverFile = deliverFile
--               }
--         bipWallet fileFFI (_mvarHandler_readRequest signingHandler) $ \enabledSettings -> AppCfg
--           { _appCfg_gistEnabled = False
--           , _appCfg_loadEditor = loadEditorFromLocalStorage
--           , _appCfg_editorReadOnly = False
--           , _appCfg_signingHandler = mkFRPHandler signingHandler
--           , _appCfg_enabledSettings = enabledSettings
--           , _appCfg_logMessage = defaultLogger
--           }
--   }

-- -- This is the deliver file that is only used for development (i.e jsaddle web version)
-- -- so it is hacky and just writes to the current directory. We could likely make a
-- -- thing with a data uri and then click on it with JS, but this is probably OK enough
-- -- for dev.
-- deliverFile
--   :: (MonadIO (Performable m), PerformEvent t m)
--   => Event t (FilePath, Text)
--   -> m (Event t (Either Text FilePath))
-- deliverFile eInput = performEvent . ffor eInput $ \(fName, fContent) -> liftIO $ do
--   dirName <- getCurrentDirectory
--   catch
--     ((T.writeFile (dirName </> fName) fContent) *> (pure . Right $ dirName </> fName))
--     $ \(e :: IOError) ->
--       pure . Left . T.pack $ "Error '" <> show e <> "' exporting file: " <> show fName <> " to  " <> dirName

data LockScreen a where
  LockScreen_Restore :: LockScreen Crypto.XPrv -- ^ Root key
  LockScreen_RunSetup :: LockScreen ()
  LockScreen_Locked :: LockScreen Crypto.XPrv -- ^ Root key
  LockScreen_Unlocked :: LockScreen (Crypto.XPrv, Password) -- ^ The root key and password

type MkAppCfg t m
  =  EnabledSettings Crypto.XPrv t (RoutedT t (R FrontendRoute) (BIPCryptoT t m))
  -- ^ Settings
  -> AppCfg Crypto.XPrv t (RoutedT t (R FrontendRoute) (BIPCryptoT t m))

bipWallet
  :: forall js t m
  .  ( MonadWidget t m
     , RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m
     , HasConfigs m
     , HasStorage m, HasStorage (Performable m)
     , HasTransactionLogger m
     , Prerender js t m
     )
  => FileFFI t m
  -> Event t ()
  -> MkAppCfg t m
  -> RoutedT t (R FrontendRoute) m ()
bipWallet fileFFI signingReqEv mkAppCfg = do
  txLogger <- askTransactionLogger

  let
    runSetup0
      :: Maybe (Behavior t Crypto.XPrv)
      -> WalletExists
      -> RoutedT t (R FrontendRoute) m (Event t (DSum LockScreen Identity))
    runSetup0 mPrv walletExists = do
      let pwCheck k p= pure $ passwordRoundTripTest k p
          runF k p = runBIPCryptoT (pure (k, p))
          importWidgetApis = ImportWidgetApis BIPStorage_RootKey pwCheck runF

      keyAndPass <- runSetup (liftFileFFI lift fileFFI) (isJust mPrv) walletExists importWidgetApis
      performEvent $ flip push keyAndPass $ \case
        Right (x, p, newWallet) -> pure $ Just $ do
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
        (restore, mLogin) <- lockScreenWidget signingReqEv (\k p -> pure $ passwordRoundTripTest k p) False $ fmap runIdentity $ current root
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

          let
            frontendFileFFI = liftFileFFI (lift . lift) fileFFI
            changePasswordDesktopAction i newRoot (Password newPass) = do
              let (newPrv, pub) = bipCryptoGenPair newRoot newPass i
              pure $ Key $ KeyPair
                { _keyPair_publicKey = pub
                , _keyPair_privateKey = Just newPrv
                }
          Frontend.App.app sidebarLogoutLink frontendFileFFI never $ mkAppCfg $ EnabledSettings
            { _enabledSettings_changePassword = Just $ ChangePassword
              { _changePassword_requestChange =
                let doChange (Identity (oldRoot, _)) (Password oldPass, Password newPass, Password repeatPass)
                      | passwordRoundTripTest oldRoot (Password oldPass) = case checkPassword (Password newPass) (Password repeatPass) of
                        Left e -> pure $ Left e
                        Right _ -> do
                          -- Change password for root key
                          let newRoot = Crypto.xPrvChangePass (T.encodeUtf8 oldPass) (T.encodeUtf8 newPass) oldRoot
                          setItemStorage localStorage BIPStorage_RootKey newRoot
                          liftIO $ trigger (newRoot, Password newPass)
                          pure $ Right ()
                      | otherwise = pure $ Left "Invalid password"
                in performEvent . attachWith doChange (current details)
              -- When updating the keys here, we just always regenerate the key from
              -- the new root
              , _changePassword_updateKeys = (updates, changePasswordDesktopAction)
              }
            , _enabledSettings_exportWallet =
              Just $ mkExportWallet txLogger frontendFileFFI details (Proxy :: Proxy (BIPStorage Crypto.XPrv))
            , _enabledSettings_transactionLog = True
            , _enabledSettings_walletConnect = Nothing
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

deriveGEq ''LockScreen
