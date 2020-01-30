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

module Desktop.Frontend (desktop, bipWallet, bipCryptoGenPair, runFileStorageT) where

import Control.Lens ((?~))
import Control.Monad ((<=<), guard, void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Aeson.GADT.TH
import Data.Bitraversable
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Constraint.Extras.TH
import Data.GADT.Show.TH
import Data.GADT.Compare.TH
import Data.Foldable (fold)
import Data.Maybe (isJust)
import Data.Monoid (Endo(..))
import Data.Text (Text)
import Data.Time (NominalDiffTime, getCurrentTime, addUTCTime)
import Data.Universe.Some.TH
import Language.Javascript.JSaddle (liftJSM)
import Pact.Server.ApiV1Client (HasTransactionLogger, runTransactionLoggerT, logTransactionStdout)
import Reflex.Dom.Core
import qualified Cardano.Crypto.Wallet as Crypto
import qualified Data.Text.Encoding as T
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as GlobalEventHandlers

import Common.Api (getConfigRoute)
import Common.Route
import Frontend.AppCfg
import Desktop.Crypto.BIP
import Frontend.ModuleExplorer.Impl (loadEditorFromLocalStorage)
import Frontend.Log (defaultLogger)
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
import Frontend.Store (StoreFrontend(..))
import Frontend.Storage (runBrowserStorageT)

import Frontend.UI.Modal.Impl (showModalBrutal)
import Frontend.UI.Dialogs.LogoutConfirmation (uiIdeLogoutConfirmation)

import Desktop.Orphans ()
import Desktop.Setup
import Desktop.SigningApi
import Desktop.Util
import Desktop.Storage.File

data BIPStorage a where
  BIPStorage_RootKey :: BIPStorage Crypto.XPrv
deriving instance Show (BIPStorage a)

concat <$> traverse ($ ''BIPStorage)
  [ deriveGShow
  , deriveGEq
  , deriveGCompare
  , deriveUniverseSome
  , deriveArgDict
  , deriveJSONGADT
  ]

-- | This is for development
-- > ob run --import desktop:Desktop.Frontend --frontend Desktop.Frontend.desktop
desktop :: Frontend (R FrontendRoute)
desktop = Frontend
  { _frontend_head = do
      let backendEncoder = either (error "frontend: Failed to check backendRouteEncoder") id $
            checkEncoder backendRouteEncoder
      base <- getConfigRoute
      void $ Frontend.newHead $ \r -> base <> renderBackendRoute backendEncoder r
  , _frontend_body = prerender_ blank $ do
    (signingRequestMVar, signingResponseMVar) <- signingServer
      (pure ()) -- Can't foreground or background things
      (pure ())
    mapRoutedT (flip runTransactionLoggerT logTransactionStdout . runBrowserStorageT) $ do
      (fileOpened, triggerOpen) <- Frontend.openFileDialog
      signingRequest <- mvarTriggerEvent signingRequestMVar
      bipWallet AppCfg
        { _appCfg_gistEnabled = False
        , _appCfg_externalFileOpened = fileOpened
        , _appCfg_openFileDialog = liftJSM triggerOpen
        , _appCfg_loadEditor = loadEditorFromLocalStorage
        , _appCfg_editorReadOnly = False
        , _appCfg_signingRequest = signingRequest
        , _appCfg_signingResponse = signingResponseHandler signingResponseMVar
        , _appCfg_enabledSettings = EnabledSettings
          {
          }
        , _appCfg_logMessage = defaultLogger
        }
  }

data LockScreen
  = LockScreen_Restore Crypto.XPrv
  | LockScreen_RunSetup
  | LockScreen_Locked (Maybe Text) Crypto.XPrv

bipWallet
  :: ( MonadWidget t m
     , RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m
     , HasConfigs m
     , HasStorage m, HasStorage (Performable m)
     , HasTransactionLogger m
     )
  => AppCfg Crypto.XPrv t (RoutedT t (R FrontendRoute) (BIPCryptoT m))
  -> RoutedT t (R FrontendRoute) m ()
bipWallet appCfg = do
  let
    runSetup0 r fromLockScreen = do
      keyAndPass <- runSetup fromLockScreen
      performEvent $ ffor keyAndPass $ \case
        Right (x, Password p) -> do
          setItemStorage localStorage BIPStorage_RootKey x
          removeItemStorage localStorage StoreFrontend_Wallet_Keys
          removeItemStorage localStorage StoreFrontend_Wallet_Accounts
          pure $ LockScreen_Locked (Just p) x
        Left _ ->
          pure r

  mRoot <- getItemStorage localStorage BIPStorage_RootKey
  rec
    root <- holdDyn (maybe LockScreen_RunSetup (LockScreen_Locked Nothing) mRoot) upd
    upd <- switchHold never <=< dyn $ ffor root $ \case
      -- Run the restore process or return to the lock screen
      LockScreen_Restore xprv -> runSetup0 (LockScreen_Locked Nothing xprv) True
      -- We have no wallet so run the creation/setup process
      LockScreen_RunSetup -> runSetup0 LockScreen_RunSetup False
      -- Wallet exists but the lock screen is active
      LockScreen_Locked mpass xprv -> mdo
        mPassword <- holdUniqDyn =<< holdDyn mpass userPassEvents
        (restore, userPassEvents) <- bitraverse (switchHold never) (switchHold never) $ splitE result
        result <- dyn $ ffor mPassword $ \case
          Nothing -> lockScreen xprv
          Just pass -> mapRoutedT (runBIPCryptoT xprv pass) $ do
            (onLogout, sidebarLogoutLink) <- mkSidebarLogoutLink

            onLogoutConfirm <- fmap switchDyn $ widgetHold (pure never)
              $ showModalBrutal "logout-confirm-modal" uiIdeLogoutConfirmation <$ onLogout

            Frontend.ReplGhcjs.app sidebarLogoutLink appCfg

            setRoute $ landingPageRoute <$ onLogoutConfirm
            pure (never, Nothing <$ onLogoutConfirm)

        pure $ LockScreen_Restore xprv <$ restore
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

lockScreen :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m) => Crypto.XPrv -> m (Event t (), Event t (Maybe Text))
lockScreen xprv = setupDiv "fullscreen" $ divClass "wrapper" $ setupDiv "splash" $ do
  splashLogo

  el "div" $ mdo
    dValid <- holdDyn True $ leftmost
      [ isJust <$> isValid
      , True <$ _inputElement_input pass
      ]

    let unlock = void $ confirmButton (def & uiButtonCfg_type ?~ "submit") "Unlock"
        cfg = def & elementConfig_initialAttributes .~ ("class" =: setupClass "splash-terms-buttons")
    (eSubmit, pass) <- form cfg unlock $ do
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

    let isValid = attachWith (\p _ -> p <$ guard (testKeyPassword xprv p)) (current $ value pass) eSubmit
    pure (restore, isValid)

-- | Check the validity of the password by signing and verifying a message
testKeyPassword :: Crypto.XPrv -> Text -> Bool
testKeyPassword xprv pass = Crypto.verify (Crypto.toXPub xprv) msg $ Crypto.sign (T.encodeUtf8 pass) xprv msg
  where msg = "test message" :: ByteString
