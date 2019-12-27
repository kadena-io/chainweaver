{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Desktop.Frontend (desktop, bipWallet, bipCryptoGenPair, fileStorage) where

import Control.Exception (try, catch)
import Control.Lens ((?~))
import Control.Monad (when, (<=<), guard, void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Free (iterM)
import Control.Monad.IO.Class
import Data.Bitraversable
import Data.Bits ((.|.))
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Maybe (isNothing, isJust)
import Data.Text (Text)
import Data.Time (NominalDiffTime, getCurrentTime, addUTCTime)
import Language.Javascript.JSaddle (JSM, liftJSM)
import Reflex.Dom.Core
import System.FilePath ((</>))
import qualified Cardano.Crypto.Wallet as Crypto
import qualified Control.Newtype.Generics as Newtype
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.IO as T
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as GlobalEventHandlers
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

import qualified Pact.Types.Crypto as PactCrypto
import Pact.Types.Util (parseB16TextOnly)
import qualified Pact.Types.Hash as Pact

import Common.Api (getConfigRoute)
import Common.Route
import Frontend.AppCfg
import Frontend.Crypto.Class
import Frontend.Crypto.Ed25519
import Frontend.ModuleExplorer.Impl (loadEditorFromLocalStorage)
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
import Frontend.Wallet (StoreWallet(..))

import Desktop.Orphans ()
import Desktop.Setup
import Desktop.SigningApi
import Desktop.Util

data BIPStorage a where
  BIPStorage_RootKey :: BIPStorage Crypto.XPrv
deriving instance Show (BIPStorage a)

-- | Store items as files in the given directory, using the key as the file name
fileStorage :: FilePath -> StorageInterpreter JSM
fileStorage dir = StorageInterpreter $ iterM go
  where
    go :: StorageF (JSM a) -> JSM a
    go = \case
      StorageF_Get _ k next -> do
        res <- liftIO $ do
          try (BS.readFile $ path k) >>= \case
            Left (e :: IOError) -> do
              putStrLn $ "Error reading storage: " <> show e <> " : " <> path k
              pure Nothing
            Right v -> do
              let result = Aeson.decodeStrict v
              when (isNothing result) $ do
                T.putStrLn $ "Error reading storage: can't decode contents: " <>
                  T.decodeUtf8With T.lenientDecode v
              pure result
        next res
      StorageF_Set _ k a next -> do
        liftIO $ catch (LBS.writeFile (path k) (Aeson.encode a)) $ \(e :: IOError) -> do
          putStrLn $ "Error writing storage: " <> show e <> " : " <> path k
        next
      StorageF_Remove _ k next -> do
        liftIO $ catch (Directory.removeFile (path k)) $ \(e :: IOError) -> do
          putStrLn $ "Error removing storage: " <> show e <> " : " <> path k
        next
    path :: Show a => a -> FilePath
    path k = dir </> FilePath.makeValid (show k)

bipCryptoGenPair :: Crypto.XPrv -> Text -> Int -> (Crypto.XPrv, PublicKey)
bipCryptoGenPair root pass i =
  let xprv = Crypto.deriveXPrv scheme (T.encodeUtf8 pass) root (mkHardened $ fromIntegral i)
  in (xprv, unsafePublicKey $ Crypto.xpubPublicKey $ Crypto.toXPub xprv)
  where
    scheme = Crypto.DerivationScheme2
    mkHardened = (0x80000000 .|.)

bipCrypto :: Crypto.XPrv -> Text -> Crypto Crypto.XPrv
bipCrypto root pass = Crypto
  { _crypto_sign = \bs xprv ->
      pure $ Newtype.pack $ Crypto.unXSignature $ Crypto.sign (T.encodeUtf8 pass) xprv bs

  , _crypto_genKey = \i -> do
      liftIO $ putStrLn $ "Deriving key at index: " <> show i
      pure $ bipCryptoGenPair root pass i

  -- This assumes that the secret is already base16 encoded (being pasted in, so makes sense)
  , _crypto_verifyPactKey = \pkScheme sec -> pure $ do
      secBytes <- parseB16TextOnly sec
      somePactKey <- importKey pkScheme Nothing secBytes
      pure $ PactKey pkScheme (unsafePublicKey $ PactCrypto.getPublic somePactKey) secBytes

  , _crypto_signWithPactKey = \bs pk -> do
      let someKpE = importKey
            (_pactKey_scheme pk)
            (Just $ Newtype.unpack $ _pactKey_publicKey pk)
            $ _pactKey_secret pk

      case someKpE of
        Right someKp -> liftIO $ Newtype.pack <$> PactCrypto.sign someKp (Pact.Hash bs)
        Left e -> error $ "Error importing pact key from account: " <> e
  }
  where
    importKey pkScheme mPubBytes secBytes = PactCrypto.importKeyPair
      (PactCrypto.toScheme pkScheme)
      (PactCrypto.PubBS <$> mPubBytes)
      (PactCrypto.PrivBS secBytes)

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
    mapRoutedT (flip runStorageT browserStorageIntepreter) $ do
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
        }
  }

bipWallet
  :: ( MonadWidget t m
     , RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m
     , HasConfigs m
     , HasStorage m, HasStorage (Performable m)
     , StorageM m ~ JSM, StorageM (Performable m) ~ JSM
     )
  => AppCfg Crypto.XPrv t (RoutedT t (R FrontendRoute) (CryptoT Crypto.XPrv m))
  -> RoutedT t (R FrontendRoute) m ()
bipWallet appCfg = do
  mRoot <- runStorageJSM $ getItemStorage localStorage BIPStorage_RootKey
  rec
    root <- holdDyn mRoot upd
    upd <- switchHold never <=< dyn $ ffor root $ \case
      Nothing -> do
        xprv <- runSetup
        saved <- performEvent $ ffor xprv $ \x -> do
          runStorageJSM $ do
            setItemStorage localStorage BIPStorage_RootKey x
            removeItemStorage localStorage StoreWallet_Keys
          pure x
        pure $ Just <$> saved
      Just xprv -> mdo
        mPassword <- holdUniqDyn =<< holdDyn Nothing userPassEvents
        (restore, userPassEvents) <- bitraverse (switchHold never) (switchHold never) $ splitE result
        result <- dyn $ ffor mPassword $ \case
          Nothing -> lockScreen xprv
          Just pass -> mapRoutedT (flip runCryptoT $ bipCrypto xprv pass) $ do
            (logout, sidebarLogoutLink) <- mkSidebarLogoutLink
            Frontend.ReplGhcjs.app sidebarLogoutLink appCfg
            setRoute $ landingPageRoute <$ logout
            pure (never, Nothing <$ logout)
        pure $ Nothing <$ restore
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
  elAttr "div"
    (  "style" =: ("background-image: url(" <> (static @"img/Wallet_Graphic_1.png") <> ");")
    <> "class" =: setupClass "splash-bg"
    ) kadenaWalletLogo

  setupDiv "splash-terms-buttons" $ mdo
    dValid <- holdDyn True . fmap isJust $ isValid

    let unlock = void $ confirmButton (def & uiButtonCfg_type ?~ "submit") "Unlock"
    (eSubmit, pass) <- form unlock $ do
      elDynClass "div"
        (("lock-screen__invalid-password" <>) . bool " lock-screen__invalid-password--invalid" "" <$> dValid)
        (text "Invalid Password")
      uiPassword (setupClass "password-wrapper") (setupClass "password") "Password"

    restore <- setupDiv "button-horizontal-group" $ do
      elAttr "a" ( "class" =: "button button_type_secondary" <>
                   "href" =: "https://www.kadena.io/chainweaver-support" <>
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
