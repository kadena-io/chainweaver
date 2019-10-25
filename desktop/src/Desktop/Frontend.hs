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

module Desktop.Frontend (desktop, desktopCss, fileStorage) where

import Control.Exception (try, catch)
import Control.Lens ((?~))
import Control.Monad (when, (<=<), guard, void)
import Control.Monad.IO.Class
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.Maybe (isNothing)
import Data.Text (Text)
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
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Text.RawString.QQ as QQ

import Common.Api (getConfigRoute)
import Common.Route
import Frontend.AppCfg
import Frontend.Crypto.Class
import Frontend.Crypto.Ed25519
import Frontend.ModuleExplorer.Impl (loadEditorFromLocalStorage)
import Frontend.Storage
import Frontend.UI.Button
import Frontend.UI.Widgets
import Obelisk.Generated.Static
import Obelisk.Frontend
import Obelisk.Route
import qualified Frontend
import qualified Frontend.ReplGhcjs

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
      el "style" $ text desktopCss
      void $ Frontend.newHead $ \r -> base <> renderBackendRoute backendEncoder r
  , _frontend_body = prerender_ blank $ flip runStorageT browserStorage $ do
    mRoot <- getItemStorage localStorage BIPStorage_RootKey
    rec
      root <- holdDyn mRoot upd
      upd <- switchHold never <=< dyn $ ffor root $ \case
        Nothing -> do
          xprv <- runSetup
          saved <- performEvent $ ffor xprv $ \x -> setItemStorage localStorage BIPStorage_RootKey x >> pure x
          pure $ Just <$> saved
        Just xprv -> mdo
          mPassword <- holdUniqDyn =<< holdDyn Nothing passEvts
          -- TODO expire password
          passEvts <- switchHold never passEvts'
          let (restore', passEvts') = splitE result
          result <- dyn $ ffor mPassword $ \case
            Nothing -> do
              el "h1" $ text "Wallet locked"
              kadenaWalletLogo
              form "" $ do
                pass <- uiInputElement $ def & initialAttributes .~ "type" =: "password" -- TODO padlock icon
                e <- confirmButton (def & uiButtonCfg_type ?~ "submit") "Unlock"
                help <- uiButton def $ text "Help" -- TODO where does this go?
                restore <- uiButton def $ text "Restore" -- TODO
                let isValid = attachWith (\p _ -> p <$ guard (testKeyPassword xprv p)) (current $ value pass) e
                pure (restore, isValid)
            Just pass -> flip runCryptoT (bipCrypto xprv pass) $ do
              (fileOpened, triggerOpen) <- Frontend.openFileDialog
              (logout, triggerLogout) <- newTriggerEvent
              let appCfg = AppCfg
                    { _appCfg_gistEnabled = False
                    , _appCfg_externalFileOpened = fileOpened
                    , _appCfg_openFileDialog = liftJSM triggerOpen
                    , _appCfg_loadEditor = loadEditorFromLocalStorage
                    , _appCfg_editorReadOnly = False
                    , _appCfg_signingRequest = never
                    , _appCfg_signingResponse = \_ -> pure ()
                    , _appCfg_sidebarExtra = do
                      (e, _) <- elAttr' "span" ("class" =: "sidebar__link") $ do
                        elAttr "img" ("class" =: "normal" <> "src" =: static @"img/menu/logout.png") blank
                      performEvent_ $ liftIO . triggerLogout <$> domEvent Click e
                    }
              Frontend.ReplGhcjs.app appCfg
              pure (never, Nothing <$ logout)
          restore <- switchHold never restore'
          pure $ Nothing <$ restore
    pure ()
  }

desktopCss :: Text
desktopCss = [QQ.r|
.fullscreen { width: 100vw; height: 100vh; display: flex; justify-content: center; align-items: center; }
.fullscreen p { margin: 2rem auto; }
.fullscreen .checkbox-wrapper { margin: 2rem auto; }
.fullscreen .checkbox { font-size: 20px; color: #fff; text-align: left; display: inline-block; padding-left: 40px; }
.fullscreen .checkbox .checkbox__checkmark { top: 2px; height: 20px; width: 20px; }
.fullscreen .checkbox .checkbox__checkmark_type_secondary:after { top: 2px; left: 6px; width: 3px; height: 10px; }
.fullscreen .group { color: #222; margin: 2rem 0; }
.fullscreen .group.dark { background-color: rgba(0,0,0,0.3); }
.fullscreen textarea.wallet-recovery-phrase { display: block; width: 30rem; height: 6rem; font-size: 18px; margin: 2rem auto; }
.button_hidden { display: none; }
.group.group_buttons { text-align: center; }
.group button { margin: 0.2rem; }
.page:not(.contracts) button.button_type_confirm { border: none; background-color: rgb(30,40,50); font-weight: normal; }
.page:not(.contracts) button.button_type_confirm { background: linear-gradient(180deg, rgb(40,50,60) 0%, rgb(20,30,40) 100%); }
.page:not(.contracts) button.button_type_confirm:hover:not([disabled]) { background: linear-gradient(180deg, rgb(60,70,80) 0%, rgb(40,50,60) 100%); }
form.inline { margin: 1rem 0; border-radius: 4px; display: inline-block; }
form.inline > input { width: 12rem; margin: 0; margin-right: 1rem; }
form.inline > button { margin: 0; }
form.inline > input { width: 12rem; }
form .messages { background-color: rgba(30,40,50,0.2); border-radius: 4px; padding: 0.5rem; list-style-type: none; }
form .messages > li { padding: 0.5rem; }
form .header { color: rgb(30,40,50); font-weight: bold; font-size: 18px; margin: 1rem 0; }
form .header .detail { color: #666; font-weight: normal; font-size: 14px; }
|]

-- | Check the validity of the password by signing and verifying a message
testKeyPassword :: Crypto.XPrv -> Text -> Bool
testKeyPassword xprv pass = Crypto.verify (Crypto.toXPub xprv) msg $ Crypto.sign (T.encodeUtf8 pass) xprv msg
  where msg = "test message" :: ByteString
