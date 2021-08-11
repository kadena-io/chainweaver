{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Wallet setup screens
module Frontend.Setup.Browser (runSetup, bipWalletBrowser) where

import Control.Lens ((<>~), (^.), _1, _2, _3)
import Control.Monad (guard)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (lift)
import Control.Monad
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Aeson.GADT.TH
import Data.ByteString (ByteString)
import Data.Constraint.Extras.TH
import Data.Dependent.Sum
import Data.Functor.Compose
import Data.Functor.Identity
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Traversable (for)
import Data.Universe.Some.TH
import Language.Javascript.JSaddle (MonadJSM)
import Reflex.Dom.Core hiding (Key)
import Pact.Server.ApiClient (HasTransactionLogger, askTransactionLogger, _transactionLogger_rotateLogFile)
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Common.Wallet
import Common.Route
import qualified Frontend.App as App (app)
import Frontend.AppCfg
import Frontend.Crypto.Class
import Frontend.Crypto.Password
import Frontend.Crypto.Ed25519
import Frontend.Crypto.Browser
import Frontend.Foundation
import Frontend.Setup.Common
import Frontend.Setup.Password
import Frontend.Setup.Widgets
import Frontend.Storage
import Frontend.UI.Button
import Frontend.UI.Modal.Impl (showModalBrutal)
import Frontend.UI.Dialogs.LogoutConfirmation (uiIdeLogoutConfirmation)
import Frontend.UI.Widgets
import Frontend.VersionedStore


data BIPStorage a where
  BIPStorage_RootKey :: BIPStorage PrivateKey
deriving instance Show (BIPStorage a)

concat <$> traverse ($ ''BIPStorage)
  [ deriveGShow
  , deriveGEq
  , deriveGCompare
  , deriveUniverseSome
  , deriveArgDict
  , deriveJSONGADT
  ]

data LockScreen a where
  LockScreen_Restore :: LockScreen PrivateKey
  LockScreen_RunSetup :: LockScreen ()
  LockScreen_Locked :: LockScreen PrivateKey -- ^ Root key
  LockScreen_Unlocked :: LockScreen (PrivateKey, Password) -- ^ The root key and password

type MkAppCfg t m
  =  EnabledSettings PrivateKey t (RoutedT t (R FrontendRoute) (BrowserCryptoT t m))
  -- ^ Settings
  -> AppCfg PrivateKey t (RoutedT t (R FrontendRoute) (BrowserCryptoT t m))

runSetup
  :: ( DomBuilder t m
    , MonadFix m
    , MonadHold t m
    , PerformEvent t m
    , PostBuild t m
    , TriggerEvent t m
    , MonadJSM (Performable m)
    , HasStorage (Performable m)
    , MonadSample t (Performable m)
    , DerivableKey key mnemonic
    )
  => FileFFI t m
  -> Bool
  -> WalletExists
  -> m (Event t (Either () (key, Password, Bool)))
runSetup fileFFI showBackOverride walletExists = setupDiv "fullscreen" $ mdo
  let dCurrentScreen = (^._1) <$> dwf

  eBack <- fmap (domEvent Click . fst) $ elDynClass "div" ((setupClass "back " <>) . hideBack <$> dCurrentScreen) $
    el' "span" $ do
      elClass "i" "fa fa-fw fa-chevron-left" $ blank
      text "Back"

  _ <- dyn_ $ walletSetupRecoverHeader <$> dCurrentScreen

  dwf <- divClass "wrapper" $
    workflow (splashScreenBrowser eBack)

  pure $ leftmost
    [ fmap Right $ switchDyn $ (^. _2) <$> dwf
    , attachWithMaybe (\s () -> Left () <$ guard (s == WalletScreen_SplashScreen)) (current dCurrentScreen) eBack
    , fmap Left $ switchDyn $ (^. _3) <$> dwf
    ]
  where
    hideBack ws =
      if not showBackOverride && (ws `elem` [WalletScreen_SplashScreen, WalletScreen_Done]) then
        setupClass "hide"
      else
        setupScreenClass ws

splashScreenBrowser
  :: (DomBuilder t m, MonadFix m, MonadHold t m, PerformEvent t m
     , PostBuild t m, MonadJSM (Performable m), TriggerEvent t m, HasStorage (Performable m)
     , MonadSample t (Performable m), DerivableKey key mnemonic
     )
  => Event t ()
  -> SetupWF key t m
splashScreenBrowser eBack = selfWF
  where
    selfWF = Workflow $ setupDiv "splash" $ do
      agreed <- splashScreenAgreement
      let hasAgreed = gate (current agreed)
          disabledCfg = uiButtonCfg_disabled .~ fmap not agreed
          restoreCfg = uiButtonCfg_class <>~ "setup__restore-existing-button"

      create <- confirmButton (def & disabledCfg ) "Create a new wallet"

      restoreBipPhrase <- uiButtonDyn (btnCfgSecondary & disabledCfg & restoreCfg)
        $ text "Restore from recovery phrase"

      finishSetupWF WalletScreen_SplashScreen $ leftmost
        [ createNewWallet selfWF eBack <$ hasAgreed create
        , restoreBipWallet selfWF eBack <$ hasAgreed restoreBipPhrase
        ]

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
        (restore, mLogin) <- lockScreenWidget $ fmap runIdentity $ current root
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
          App.app sidebarLogoutLink frontendFileFFI $ mkAppCfg $
            appSettingsBrowser trigger details updates changePasswordBrowserAction

          setRoute $ landingPageRoute <$ onLogoutConfirm
          pure $ leftmost
            [ (LockScreen_Unlocked ==>) <$> updates
            , (LockScreen_Locked ==>) . fst . runIdentity <$> current details <@ onLogoutConfirm
            ]
  pure ()

appSettingsBrowser ::
   ( HasStorage m 
   , HasStorage (Performable m)
   , PerformEvent t m
   , MonadJSM (Performable m)
   ) 
  => ((PrivateKey, Password) -> IO ())
  -> Dynamic t (Identity (PrivateKey, Password))
  -> Event t (PrivateKey, Password) 
  -> (Int -> PrivateKey -> Password -> (Performable m) (Key PrivateKey))
  -> EnabledSettings PrivateKey t m
appSettingsBrowser newPwdTrigger details keyUpdates changePasswordBrowserAction = EnabledSettings
  { _enabledSettings_changePassword = Just $ ChangePassword
    { _changePassword_requestChange = performEvent . attachWith doChange (current details)
    -- When updating the keys here, we just always regenerate the key from
    -- the new root
    , _changePassword_updateKeys = (keyUpdates, changePasswordBrowserAction)
    }
  , _enabledSettings_exportWallet = Nothing
  , _enabledSettings_transactionLog = False
  }
  where
    doChange (Identity (oldRoot, _)) (oldPass, newPass, repeatPass) = do
      passesRoundTrip <- passwordRoundTripTest oldRoot oldPass
      case passesRoundTrip of
        False -> pure $ Left "Invalid password"
        True -> case checkPassword newPass repeatPass of
          Left e -> pure $ Left e
          Right _ -> do
            -- Change password for root key
            newRootOrErr <- liftJSM $ changePassword oldRoot oldPass newPass
            case newRootOrErr of
              Left e -> pure $ Left $ "Error changing password: " <> e
              Right newRoot -> do
                setItemStorage localStorage BIPStorage_RootKey newRoot
                liftIO $ newPwdTrigger (newRoot, newPass)
                pure $ Right ()

lockScreenWidget
  :: 
  (DomBuilder t m, PostBuild t m, TriggerEvent t m, PerformEvent t m,
   MonadIO m, MonadFix m, MonadHold t m, MonadJSM (Performable m)
  )
  => Behavior t PrivateKey -> m (Event t (), Event t Password)
lockScreenWidget xprv = setupDiv "fullscreen" $ divClass "wrapper" $ setupDiv "splash" $ mdo
  (restore, pass, eSubmit) <- lockScreen isValid
  let prvAndPass = (,) <$> xprv <*> (fmap Password $ current pass)
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
