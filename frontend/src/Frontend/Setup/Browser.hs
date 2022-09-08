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
import Data.Proxy (Proxy(..))
import Data.Traversable (for)
import Data.Universe.Some.TH
import Language.Javascript.JSaddle (MonadJSM)
import Reflex.Dom.Core hiding (Key)
import Pact.Server.ApiClient (HasTransactionLogger, TransactionLogger, askTransactionLogger, _transactionLogger_rotateLogFile)
import Obelisk.Route.Frontend
import WalletConnect.Wallet
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
import Frontend.Setup.ImportExport
import Frontend.Setup.Password
import Frontend.Setup.Setup
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
  -> WalletConnect t
  -> Event t (Maybe Metadata, String, WalletConnect.Wallet.Request)
  -> MkAppCfg t m
  -> RoutedT t (R FrontendRoute) m ()
bipWalletBrowser fileFFI walletConnect wcSignReqErrEv mkAppCfg = do
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
      let runF k p = runBrowserCryptoT (pure (k, p))
          importWidgetApis = ImportWidgetApis BIPStorage_RootKey passwordRoundTripTest runF
      keyAndPass <- runSetup (liftFileFFI lift fileFFI) (isJust mPrv) walletExists importWidgetApis
      performEvent $ flip push keyAndPass $ \case
        Right (x, Password p, newWallet) -> pure $ Just $ do
          when newWallet $ do
            liftIO $ _transactionLogger_rotateLogFile txLogger

            -- nuke it all before restart
            removeKeyUniverse storeProxy localStorage

          setItemStorage localStorage BIPStorage_RootKey x
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
        isWc <- do
          curRoute <- sample . current =<< askRoute
          return $ case curRoute of
            FrontendRoute_WalletConnect :/ _ -> True
            _ -> False
        let signingReqEv = () <$ _walletConnect_requests walletConnect
        (restore, mLogin) <- lockScreenWidget signingReqEv passwordRoundTripTest
          isWc $ fmap runIdentity $ current root
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
          App.app sidebarLogoutLink frontendFileFFI wcSignReqErrEv $ mkAppCfg $
            appSettingsBrowser txLogger frontendFileFFI trigger details updates changePasswordBrowserAction walletConnect

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
   , HasCrypto PrivateKey (Performable m)
   )
  => TransactionLogger
  -> FileFFI t m
  -> ((PrivateKey, Password) -> IO ())
  -> Dynamic t (Identity (PrivateKey, Password))
  -> Event t (PrivateKey, Password)
  -> (Int -> PrivateKey -> Password -> (Performable m) (Key PrivateKey))
  -> WalletConnect t
  -> EnabledSettings PrivateKey t m
appSettingsBrowser txLogger frontendFileFFI newPwdTrigger details keyUpdates changePasswordBrowserAction wc = EnabledSettings
  { _enabledSettings_changePassword = Just $ ChangePassword
    { _changePassword_requestChange = performEvent . attachWith doChange (current details)
    -- When updating the keys here, we just always regenerate the key from
    -- the new root
    , _changePassword_updateKeys = (keyUpdates, changePasswordBrowserAction)
    }
  , _enabledSettings_exportWallet = Just $ mkExportWallet txLogger frontendFileFFI details (Proxy :: Proxy (BIPStorage PrivateKey))
  , _enabledSettings_walletConnect = Just wc
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
