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

module Desktop.Frontend (desktop, bipWallet, fileStorage) where

import Control.Exception (try, catch)
import Control.Lens ((?~))
import Control.Monad (when, (<=<), guard, void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Data.Bitraversable
import Data.Bits ((.|.))
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Maybe (isNothing, isJust)
import Data.Text (Text)
import Data.Time (NominalDiffTime, getCurrentTime, addUTCTime)
import Language.Javascript.JSaddle (liftJSM)
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

data BIPStorage a where
  BIPStorage_RootKey :: BIPStorage Crypto.XPrv
deriving instance Show (BIPStorage a)

-- | Store items as files in the given directory, using the key as the file name
fileStorage :: FilePath -> Storage
fileStorage dir = Storage
  { _storage_get = \_ k -> liftIO $ do
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
  , _storage_set = \_ k a -> liftIO $
    catch (LBS.writeFile (path k) (Aeson.encode a)) $ \(e :: IOError) -> do
      putStrLn $ "Error writing storage: " <> show e <> " : " <> path k
  , _storage_remove = \_ k -> liftIO $
    catch (Directory.removeFile (path k)) $ \(e :: IOError) -> do
      putStrLn $ "Error removing storage: " <> show e <> " : " <> path k
  }
    where path :: Show a => a -> FilePath
          path k = dir </> FilePath.makeValid (show k)

bipCrypto :: Crypto.XPrv -> Text -> Crypto Crypto.XPrv
bipCrypto root pass = Crypto
  { _crypto_sign = \bs k -> pure $ Newtype.pack $ Crypto.unXSignature $ Crypto.sign (T.encodeUtf8 pass) k bs
  , _crypto_genKey = \i -> do
    liftIO $ putStrLn $ "Deriving key at index: " <> show i
    let xprv = Crypto.deriveXPrv scheme (T.encodeUtf8 pass) root (mkHardened $ fromIntegral i)
    pure (xprv, unsafePublicKey $ Crypto.xpubPublicKey $ Crypto.toXPub xprv)
  }
  where
    scheme = Crypto.DerivationScheme2
    mkHardened = (0x80000000 .|.)

-- | This is for development
-- > ob run --import desktop:Desktop.Frontend --frontend Desktop.Frontend.desktop
desktop :: Frontend (R FrontendRoute)
desktop = Frontend
  { _frontend_head = do
      let backendEncoder = either (error "frontend: Failed to check backendRouteEncoder") id $
            checkEncoder backendRouteEncoder
      base <- getConfigRoute
      void $ Frontend.newHead $ \r -> base <> renderBackendRoute backendEncoder r
  , _frontend_body = prerender_ blank $ mapRoutedT (flip runStorageT browserStorage) $ do
    (fileOpened, triggerOpen) <- Frontend.openFileDialog
    bipWallet AppCfg
      { _appCfg_gistEnabled = False
      , _appCfg_externalFileOpened = fileOpened
      , _appCfg_openFileDialog = liftJSM triggerOpen
      , _appCfg_loadEditor = loadEditorFromLocalStorage
      , _appCfg_editorReadOnly = False
      , _appCfg_signingRequest = never
      , _appCfg_signingResponse = \_ -> pure ()
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
     )
  => AppCfg Crypto.XPrv t (RoutedT t (R FrontendRoute) (CryptoT Crypto.XPrv m))
  -> RoutedT t (R FrontendRoute) m ()
bipWallet appCfg = do
  mRoot <- getItemStorage localStorage BIPStorage_RootKey
  rec
    root <- holdDyn mRoot upd
    upd <- switchHold never <=< dyn $ ffor root $ \case
      Nothing -> do
        xprv <- runSetup
        saved <- performEvent $ ffor xprv $ \x -> do
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

mkSidebarLogoutLink :: (TriggerEvent t m, PerformEvent t n, DomBuilder t n, MonadIO (Performable n)) => m (Event t (), n ())
mkSidebarLogoutLink = do
  (logout, triggerLogout) <- newTriggerEvent
  pure $ (,) logout $ do
    (e, _) <- elAttr' "span" ("class" =: "sidebar__link") $ do
      elAttr "img" ("class" =: "normal" <> "src" =: static @"img/menu/logout.svg") blank
      elAttr "span" ("class" =: "sidebar__link-label") $ text "Logout"
    performEvent_ $ liftIO . triggerLogout <$> domEvent Click e

lockScreen :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m) => Crypto.XPrv -> m (Event t (), Event t (Maybe Text))
lockScreen xprv = setupDiv "fullscreen" $ divClass "wrapper" $ setupDiv "splash" $ mdo
  elAttr "div"
    (  "style" =: ("background-image: url(" <> (static @"img/Wallet_Graphic_1.png") <> ");")
    <> "class" =: setupClass "splash-bg"
    ) kadenaWalletLogo
  dValid <- holdDyn True . fmap isJust $ isValid
  (eSubmit, (_, restore, pass)) <- setupDiv "splash-terms-buttons" $ form "" $ do
    elDynClass "div"
      (("lock-screen__invalid-password" <>) . bool " lock-screen__invalid-password--invalid" "" <$> dValid)
      (text "Invalid Password")
    pass' <- uiPassword (setupClass "password-wrapper") (setupClass "password") "Password"

    -- Event handled by form onSubmit
    void $ confirmButton (def & uiButtonCfg_type ?~ "submit") "Unlock"
    setupDiv "button-horizontal-group" $ do
      help' <- uiButton btnCfgSecondary $ do
        elAttr "img" ("src" =: static @"img/launch_dark.svg" <> "class" =: "button__text-icon") blank
        text "Help" -- TODO where does this go?
      restore' <- uiButton btnCfgSecondary $ text "Restore"
      pure (help', restore', pass')

  let isValid = attachWith (\p _ -> p <$ guard (testKeyPassword xprv p)) (current $ value pass) eSubmit
  pure (restore, isValid)

-- | Check the validity of the password by signing and verifying a message
testKeyPassword :: Crypto.XPrv -> Text -> Bool
testKeyPassword xprv pass = Crypto.verify (Crypto.toXPub xprv) msg $ Crypto.sign (T.encodeUtf8 pass) xprv msg
  where msg = "test message" :: ByteString
