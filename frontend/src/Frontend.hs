{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend where

import Control.Monad (join, void)
import Control.Monad.IO.Class
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.FileReader as FileReader
import qualified GHCJS.DOM.HTMLElement as HTMLElement
import qualified GHCJS.DOM.HTMLInputElement as HTMLInput
import qualified GHCJS.DOM.Types as Types
import qualified GHCJS.DOM.File as JSFile
import Reflex.Dom
import Pact.Server.ApiClient (HasTransactionLogger, runTransactionLoggerT, logTransactionStdout, askTransactionLogger)
import Pact.Server.ApiClient (_transactionLogger_walletEvent, _transactionLogger_rotateLogFile)

import Obelisk.Frontend
import Obelisk.Route.Frontend
import Obelisk.Generated.Static

import Common.Api
import Common.Route
import Frontend.AppCfg
import Frontend.Log (defaultLogger)
import Frontend.Foundation
import Frontend.ModuleExplorer.Impl (loadEditorFromLocalStorage)
import Frontend.ReplGhcjs
import Frontend.Storage

import Frontend.GHCOnly.Setup
import Frontend.GHCOnly.Orphans ()
import Data.Dependent.Sum
import Data.Functor.Compose
import Data.Functor.Identity
import Data.GADT.Compare.TH
import qualified Data.Text.Encoding as T
import Common.Wallet
import Control.Monad.Trans (lift)
import Control.Monad
import Frontend.VersionedStore
import Data.Traversable (for)
import Frontend.Storage (runBrowserStorageT)
import Frontend.UI.Modal.Impl (showModalBrutal)
import Frontend.UI.Dialogs.LogoutConfirmation (uiIdeLogoutConfirmation)
import Frontend.UI.Button
import Frontend.UI.Widgets
import Control.Lens ((?~))
import Frontend.Crypto.Ed25519
import Frontend.Crypto.CommonBIP
import Frontend.Crypto.Browser

main :: IO ()
main = do
  let Right validFullEncoder = checkEncoder backendRouteEncoder
  run $ runFrontend validFullEncoder frontend

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      let backendEncoder = either (error "frontend: Failed to check backendRouteEncoder") id $
            checkEncoder backendRouteEncoder
      -- Global site tag (gtag.js) - Google Analytics
      gaTrackingId <- maybe "UA-127512784-1" T.strip <$> getTextCfg "frontend/tracking-id"
      let
        gtagSrc = "https://www.googletagmanager.com/gtag/js?id=" <> gaTrackingId
      elAttr "script" ("async" =: "" <> "src" =: gtagSrc) blank
      el "script" $ text $ T.unlines
        [ "window.dataLayer = window.dataLayer || [];"
        , "function gtag(){dataLayer.push(arguments);}"
        , "gtag('js', new Date());"
        , "gtag('config', '" <> gaTrackingId <> "');"
        ]

      base <- getConfigRoute
      _ <- newHead $ \r -> base <> renderBackendRoute backendEncoder r
      pure ()

  , _frontend_body = prerender_ loaderMarkup $ do
    (fileOpened, triggerOpen) <- openFileDialog
    mapRoutedT (flip runTransactionLoggerT logTransactionStdout . runBrowserStorageT) $ do
      let fileFFI = FileFFI
            { _fileFFI_externalFileOpened = fileOpened
            , _fileFFI_openFileDialog = liftJSM . triggerOpen
            , _fileFFI_deliverFile = \_ -> pure never
            }
          printResponsesHandler = pure $ FRPHandler never $ performEvent . fmap (liftIO . print)
      bipWallet fileFFI $ \enabledSettings -> AppCfg
        { _appCfg_gistEnabled = False
        , _appCfg_loadEditor = loadEditorFromLocalStorage
        , _appCfg_editorReadOnly = False
        , _appCfg_signingHandler = printResponsesHandler
        , _appCfg_keysEndpointHandler = printResponsesHandler
        , _appCfg_accountsEndpointHandler = printResponsesHandler
        , _appCfg_enabledSettings = enabledSettings
        , _appCfg_logMessage = defaultLogger
        }
  }

---------------------------------------------------------------------------------

data LockScreen a where
  LockScreen_Restore :: LockScreen PrivateKey -- ^ Root key
  LockScreen_RunSetup :: LockScreen ()
  LockScreen_Locked :: LockScreen PrivateKey -- ^ Root key
  LockScreen_Unlocked :: LockScreen (PrivateKey, Text) -- ^ The root key and password

type MkAppCfg t m
  =  EnabledSettings PrivateKey t (RoutedT t (R FrontendRoute) (BrowserCryptoT t m))
  -- ^ Settings
  -> AppCfg PrivateKey t (RoutedT t (R FrontendRoute) (BrowserCryptoT t m))

bipWallet
  :: forall js t m
  .  ( MonadWidget t m
     , RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m
     , HasConfigs m
     , HasStorage m, HasStorage (Performable m)
     , Prerender js t m
     , HasTransactionLogger m
     )
  => FileFFI t m
  -> MkAppCfg t m
  -> RoutedT t (R FrontendRoute) m ()
bipWallet fileFFI mkAppCfg = do
  txLogger <- askTransactionLogger
  let
    runSetup0
      :: Maybe (Behavior t PrivateKey)
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
        mapRoutedT (runBrowserCryptoT $ runIdentity <$> current details) $ do
          (onLogout, sidebarLogoutLink) <- mkSidebarLogoutLink

          onLogoutConfirm <- fmap switchDyn $ widgetHold (pure never)
            $ showModalBrutal "logout-confirm-modal" uiIdeLogoutConfirmation <$ onLogout

          (updates, trigger) <- newTriggerEvent

          let frontendFileFFI = liftFileFFI (lift . lift) fileFFI
          Frontend.ReplGhcjs.app sidebarLogoutLink frontendFileFFI $ mkAppCfg $ EnabledSettings
            { _enabledSettings_changePassword = Just $ ChangePassword
              { _changePassword_requestChange =
                let doChange (Identity (oldRoot, _)) (oldPass, newPass, repeatPass)
                      -- | passwordRoundTripTest oldRoot oldPass = case checkPassword newPass repeatPass of
                      | True = case checkPassword newPass repeatPass of
                        Left e -> pure $ Left e
                        Right _ -> do
                          -- Change password for root key
                          -- let newRoot = Crypto.xPrvChangePass (T.encodeUtf8 oldPass) (T.encodeUtf8 newPass) oldRoot
                          -- setItemStorage localStorage BIPStorage_RootKey newRoot
                          liftIO $ trigger (oldRoot, newPass)
                          pure $ Right ()
                      | otherwise = pure $ Left "Invalid password"
                in performEvent . attachWith doChange (current details)
              -- When updating the keys here, we just always regenerate the key from
              -- the new root

              -- TODO: We punted on handling this for now
              , _changePassword_updateKeys = never
              }
            , _enabledSettings_exportWallet = Nothing -- Just $ ExportWallet
              -- { _exportWallet_requestExport = \ePw -> do
              --     let bOldPw = (\(Identity (_,oldPw)) -> oldPw) <$> current details
              --         runExport oldPw newPw = do
              --           pfx <- genZeroKeyPrefix
              --           doExport txLogger pfx oldPw newPw

              --         logExport = do
              --           ts <- liftIO getCurrentTime
              --           sender <- genZeroKeyPrefix
              --           liftIO $ _transactionLogger_walletEvent txLogger
              --             WalletEvent_Export
              --             (_unPublicKeyPrefix sender)
              --             ts

              --     eExport <- performEvent $ runExport
              --       <$> (Password <$> bOldPw)
              --       <@> (Password <$> ePw)

              --     let (eErrExport, eGoodExport) = fanEither eExport

              --     eFileDone <- _fileFFI_deliverFile frontendFileFFI eGoodExport
              --     eLogExportDone <- performEvent $ (\r -> r <$ logExport) <$> eFileDone

              --     pure $ leftmost
              --       [ Left <$> eErrExport
              --       , first ExportWalletError_FileNotWritable <$> eLogExportDone
              --       ]
              -- }
            , _enabledSettings_transactionLog = False
            }

          setRoute $ landingPageRoute <$ onLogoutConfirm
          pure $ leftmost
            [ (LockScreen_Unlocked ==>) <$> updates
            , (LockScreen_Locked ==>) . fst . runIdentity <$> current details <@ onLogoutConfirm
            ]
  pure ()


mkSidebarLogoutLink :: (TriggerEvent t m, PerformEvent t n, PostBuild t n, DomBuilder t n, MonadIO (Performable n)) => m (Event t (), n ())
mkSidebarLogoutLink = do
  (logout, triggerLogout) <- newTriggerEvent
  pure $ (,) logout $ do
    clk <- uiSidebarIcon (pure False) (static @"img/menu/logout.svg") "Logout"
    performEvent_ $ liftIO . triggerLogout <$> clk

lockScreen
  :: (DomBuilder t m, PostBuild t m, TriggerEvent t m, PerformEvent t m, MonadIO m, MonadFix m, MonadHold t m)
  => Behavior t PrivateKey -> m (Event t (), Event t Text)
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

    -- req <- tryReadMVarTriggerEvent signingReq
    -- widgetHold_ blank $ ffor req $ \_ -> do
    --   let line = divClass (setupClass "signing-request") . text
    --   line "You have an incoming signing request."
    --   line "Unlock your wallet to view and sign the transaction."

    -- TODO: Fix
    -- let isValid = attachWith (\(p, x) _ -> p <$ guard (passwordRoundTripTest x p)) ((,) <$> current (value pass) <*> xprv) eSubmit
    let isValid = attachWith (\(p, x) _ -> Just p) ((,) <$> current (value pass) <*> xprv) eSubmit
    pure (restore, fmapMaybe id isValid)




---------------------------------------------------------------------------------
-- | The 'JSM' action *must* be run from a user initiated event in order for the
-- dialog to open
openFileDialog :: MonadWidget t m => m (Event t (FilePath, Text), FileType -> JSM ())
openFileDialog = do
  (pactE, triggerPact) <- fileDialog (fileTypeExtension FileType_Pact)
  (importE, triggerImport) <- fileDialog (fileTypeExtension FileType_Import)
  let trigger = \case
        FileType_Pact -> triggerPact
        FileType_Import -> triggerImport
  pure (pactE <> importE, trigger)
  where
    fileDialog accept = do
      let attrs = "type" =: "file" <> "accept" =: ("." <> accept) <> "style" =: "display: none"
      input <- inputElement $ def & initialAttributes .~ attrs
      let newFile = fmapMaybe listToMaybe $ updated $ _inputElement_files input
      mContents <- performEventAsync $ ffor newFile $ \file cb -> Types.liftJSM $ do
        fileReader <- FileReader.newFileReader
        FileReader.readAsText fileReader (Just file) (Nothing :: Maybe Text)
        _ <- EventM.on fileReader FileReader.loadEnd $ Types.liftJSM $ do
          mStringOrArrayBuffer <- FileReader.getResult fileReader
          mText <- traverse (Types.fromJSVal . Types.unStringOrArrayBuffer) mStringOrArrayBuffer
          name <- Types.fromJSString <$> JSFile.getName file
          liftIO $ cb $ ((name,) <$> join mText)
        pure ()
      let open = do
            -- This doesn't fix the bug where if you open the same file twice we don't get an event
            -- for the second select
            HTMLInput.setFiles (_inputElement_raw input) Nothing
            HTMLElement.click $ _inputElement_raw input
      pure (fmapMaybe id mContents, open)

loaderMarkup :: DomBuilder t m => m ()
loaderMarkup = divClass "spinner" $ do
  divClass "spinner__cubes" $ do
    divClass "cube1" blank
    divClass "cube2" blank
  divClass "spinner__msg" $ text "Loading"

newHead :: (Prerender js t m, DomBuilder t m) => (R BackendRoute -> Text) -> m (Event t ())
newHead routeText = do
  el "title" $ text "Kadena - Pact Testnet"
  elAttr "link" ("rel" =: "icon" <> "type" =: "image/png" <> "href" =: static @"img/favicon/favicon-96x96.png") blank
  meta ("name" =: "description" <> "content" =: "Write, test, and deploy safe smart contracts using Pact, Kadena's programming language")
  meta ("name" =: "keywords" <> "content" =: "kadena, pact, pact testnet, pact language, pact programming language, smart contracts, safe smart contracts, smart contract language, blockchain, learn blockchain programming, chainweb")
  meta ("charset" =: "utf-8")
  meta ("name" =: "google" <> "content" =: "notranslate")
  meta ("http-equiv" =: "Content-Language" <> "content" =: "en_US")
  elAttr "link" ("href" =: routeText (BackendRoute_Css :/ ()) <> "rel" =: "stylesheet") blank
  elAttr "style" ("type" =: "text/css") $ text haskellCss

  ss "https://fonts.googleapis.com/css?family=Roboto"
  ss "https://fonts.googleapis.com/css?family=Work+Sans"
  ss (static @"css/font-awesome.min.css")
  ss (static @"css/ace-theme-chainweaver.css")
  js "/static/js/ace/ace.js"
  prerender_ blank $ js "/static/js/ace/mode-pact.js"
  js (static @"js/nacl-fast.min-v1.0.0.js")
  js (static @"js/kadena-crypto.js")
  (bowser, _) <- js' (static @"js/bowser.min.js")
  pure $ domEvent Load bowser
  where
    js :: forall t n. DomBuilder t n => Text -> n ()
    js = void . js'
    js' :: forall t n. DomBuilder t n => Text -> n (Element EventResult (DomBuilderSpace n) t, ())
    js' url = elAttr' "script" ("type" =: "text/javascript" <> "src" =: url <> "charset" =: "utf-8") blank
    ss url = elAttr "link" ("href" =: url <> "rel" =: "stylesheet") blank
    meta attrs = elAttr "meta" attrs blank

    -- Allows the use of `static` in CSS and sharing parameters with desktop apps
    haskellCss = T.unlines
      [ "body { min-width: " <> tshow w <> "px; " <> "min-height: " <> tshow h <> "px; }"
      , alertImg ".icon_type_error"                $ static @"img/error.svg"
      , alertImg ".icon_type_warning"              $ static @"img/warning.svg"
      , alertImg "div.ace_gutter-cell.ace_error"   $ static @"img/error.svg"
      , alertImg "div.ace_gutter-cell.ace_warning" $ static @"img/warning.svg"
      ]
      where
        bgImg src = "background-image: url(" <> src <> "); background-position: left center"
        alertImg sel src = sel <> " { " <> bgImg src <> " } ";
        (w,h) = minWindowSize

minWindowSize :: (Int, Int)
minWindowSize = (800, 600)

defaultWindowSize :: (Int, Int)
defaultWindowSize = (1280, 800)

deriveGEq ''LockScreen
