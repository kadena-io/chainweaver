{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.Setup.Wallet where

import Control.Lens ((?~))
import Control.Monad.Trans (lift)
import Control.Monad
import Data.Dependent.Sum
import Data.Functor.Compose
import Data.Functor.Identity
import Data.GADT.Compare.TH
import Data.Traversable (for)
import Reflex.Dom hiding (Key)
import Pact.Server.ApiClient (HasTransactionLogger, askTransactionLogger, _transactionLogger_rotateLogFile)
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Common.Route
import Frontend.AppCfg
import Frontend.Foundation
import Frontend.Setup.Common
import Frontend.Storage
import qualified Frontend.ReplGhcjs as ReplGhcjs (app)
import Frontend.VersionedStore
import Frontend.UI.Modal.Impl (showModalBrutal)
import Frontend.UI.Dialogs.LogoutConfirmation (uiIdeLogoutConfirmation)
import Frontend.UI.Widgets
import Frontend.Crypto.Ed25519
import Frontend.Crypto.CommonBIP
import Frontend.Crypto.Browser
import Data.ByteString (ByteString)
import Frontend.Setup.Password
import Frontend.Crypto.Password
import Frontend.Setup.Browser
import Frontend.Setup.Widgets
import Common.Wallet

data LockScreen a where
  LockScreen_Restore :: LockScreen PrivateKey
  LockScreen_RunSetup :: LockScreen ()
  LockScreen_Locked :: LockScreen PrivateKey -- ^ Root key
  LockScreen_Unlocked :: LockScreen (PrivateKey, Password) -- ^ The root key and password

type MkAppCfg t m
  =  EnabledSettings PrivateKey t (RoutedT t (R FrontendRoute) (BrowserCryptoT t m))
  -- ^ Settings
  -> AppCfg PrivateKey t (RoutedT t (R FrontendRoute) (BrowserCryptoT t m))

bipWalletBrowser
  :: forall js t m
  .  ( MonadWidget t m
     , RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m
     , HasConfigs m
     , HasStorage m, HasStorage (Performable m)
     , Prerender js t m
     , HasTransactionLogger m
     , MonadJSM (Performable m)
     )
  => FileFFI t m
  -> MkAppCfg t m
  -> RoutedT t (R FrontendRoute) m ()
bipWalletBrowser fileFFI mkAppCfg = do
  txLogger <- askTransactionLogger
  let
    changePasswordBrowserAction i newRoot newPass = do
      keypairOrErr <- liftJSM $ generateKeypair newPass newRoot i
      case keypairOrErr of
        Left _ -> pure $ Key $ KeyPair (unsafePublicKey "") Nothing
        Right (newPrv, pub) -> pure $ Key $ KeyPair 
          { _keyPair_publicKey = pub
          , _keyPair_privateKey = Just newPrv
          }
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
          pure $ LockScreen_Unlocked ==> (x, Password p)
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
          ReplGhcjs.app sidebarLogoutLink frontendFileFFI $ mkAppCfg $ EnabledSettings
            { _enabledSettings_changePassword = Just $ ChangePassword
              { _changePassword_requestChange =
                let doChange (Identity (oldRoot, _)) (oldPass, newPass, repeatPass) = do
                      passesRoundTrip <- passwordRoundTripTest oldRoot oldPass
                      if passesRoundTrip then do
                        case checkPassword newPass repeatPass of
                          Left e -> pure $ Left e
                          Right _ -> do
                            -- Change password for root key
                            newRootOrErr <- liftJSM $ changePassword oldRoot oldPass newPass
                            case newRootOrErr of
                              Left e -> pure $ Left $ "Error changing password: " <> e
                              Right newRoot -> do
                                setItemStorage localStorage BIPStorage_RootKey newRoot
                                liftIO $ trigger (newRoot, newPass)
                                pure $ Right ()
                      else pure $ Left "Invalid password"
                in performEvent . attachWith doChange (current details)
              -- When updating the keys here, we just always regenerate the key from
              -- the new root
              , _changePassword_updateKeys = (updates, changePasswordBrowserAction)
              }
            , _enabledSettings_exportWallet = Nothing
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
  :: 
  (DomBuilder t m, PostBuild t m, TriggerEvent t m, PerformEvent t m,
   MonadIO m, MonadFix m, MonadHold t m, MonadJSM (Performable m)

  )
  => Behavior t PrivateKey -> m (Event t (), Event t Password)
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

    let prvAndPass = (,) <$> xprv <*> (fmap Password $ current $ value pass)

    isValid <- performEvent $ ffor (attach prvAndPass eSubmit) $ \((xprv', pass'), _) -> do
      isMatch <- passwordRoundTripTest xprv' pass'
      pure $ if isMatch then Just pass' else Nothing
    pure (restore, fmapMaybe id isValid)

-- | Check the validity of the password by signing and verifying a message
passwordRoundTripTest :: MonadJSM m => PrivateKey -> Password -> m Bool
passwordRoundTripTest xprv pass = liftJSM $ do
  --TODO: handle failures
  Right sig <- mkSignature pass msg xprv
  Right pub <- toPublic xprv
  verifySignature msg sig pub
  where
    msg :: ByteString
    msg = "the quick brown fox jumps over the lazy dog"

deriveGEq ''LockScreen
