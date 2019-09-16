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

module Desktop (desktop, desktopCss, runWallet, fileStorage) where

import Control.Applicative (liftA2)
import Control.Exception (try, catch)
import Control.Lens ((?~))
import Control.Monad (when, (<=<), guard, void)
import Control.Monad.IO.Class
import Data.Bimap (Bimap)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Maybe (isNothing, fromMaybe, catMaybes)
import Data.Set (Set)
import Data.Text (Text)
import Language.Javascript.JSaddle (liftJSM)
import Reflex.Dom.Core
import System.FilePath ((</>))
import qualified Cardano.Crypto.Wallet as Crypto
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.Aeson as Aeson
import qualified Data.Bimap as Bimap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.IO as T
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Text.RawString.QQ as QQ

import Common.Api (getConfigRoute)
import Common.Route
import Frontend.AppCfg
import Frontend.ModuleExplorer.Impl (loadEditorFromLocalStorage)
import Frontend.Storage
import Frontend.UI.Button
import Frontend.UI.Icon
import Frontend.UI.Widgets
import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import qualified Frontend
import qualified Frontend.ReplGhcjs

import Desktop.Orphans ()
import Desktop.Setup

data Wallet a where
  Wallet_RootKey :: Wallet Crypto.XPrv
  Wallet_ChildKeys :: Wallet (Map Crypto.DerivationIndex Crypto.XPrv)
  Wallet_NamedKeys :: Wallet (Bimap Text Ed25519.PublicKey)
deriving instance Show (Wallet a)

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
          T.putStrLn $ "Error reading storape: can't decode contents: " <>
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

-- | This is for development
-- > ob run --import desktop:Desktop --frontend Desktop.desktop
desktop :: Frontend (R FrontendRoute)
desktop = Frontend
  { _frontend_head = do
      let backendEncoder = either (error "frontend: Failed to check backendRouteEncoder") id $
            checkEncoder backendRouteEncoder
      base <- getConfigRoute
      _ <- Frontend.newHead $ \r -> base <> renderBackendRoute backendEncoder r
      el "style" $ text desktopCss
      pure ()
  , _frontend_body = prerender_ blank $ flip runStorageT browserStorage $ do
    (fileOpened, triggerOpen) <- Frontend.openFileDialog
    let appCfg = AppCfg
          { _appCfg_gistEnabled = False
          , _appCfg_externalFileOpened = fileOpened
          , _appCfg_openFileDialog = liftJSM triggerOpen
          , _appCfg_loadEditor = loadEditorFromLocalStorage
          , _appCfg_editorReadOnly = False
          , _appCfg_signingRequest = never
          , _appCfg_signingResponse = \_ -> pure ()
          , _appCfg_forceResize = never
          }
    runWallet appCfg
  }

desktopCss :: Text
desktopCss = [QQ.r|
.fullscreen { width: 100vw; height: 100vh; display: flex; justify-content: center; align-items: center; color: #fff; }
.fullscreen { background: rgb(30,40,50); background: radial-gradient(circle, rgba(40,50,60,1) 0%, rgba(27,30,46,1) 100%); }
.fullscreen p { margin: 2rem auto; }
.fullscreen .checkbox-wrapper { margin: 2rem auto; }
.fullscreen .checkbox { font-size: 20px; color: #fff; text-align: left; display: inline-block; padding-left: 40px; }
.fullscreen .checkbox .checkbox__checkmark { top: 2px; height: 20px; width: 20px; }
.fullscreen .checkbox input:checked ~ .checkbox__checkmark { background-color: #ed098f; border-color: #ed098f }
.fullscreen .checkbox .checkbox__checkmark_type_secondary:after { top: 2px; left: 6px; width: 3px; height: 10px; }
.fullscreen button.button { background: #ddd; color: #333; }
.fullscreen .group { color: #222; margin: 2rem 0; }
.fullscreen .group.dark { background-color: rgba(0,0,0,0.3); }
.fullscreen button.button_type_confirm:not([disabled]) { border: none; background: #ed098f; }
.fullscreen button.button_type_confirm:hover:not([disabled]) { background: #fd199f; }
.fullscreen .wrapper { max-width: 40rem; text-align: center; }
.fullscreen .wrapper .logo { width: 20rem; margin: 0 auto; font-size: 30px; }
.fullscreen textarea.wallet-recovery-phrase { display: block; width: 30rem; height: 6rem; font-size: 18px; margin: 2rem auto; }
.fullscreen .passphrase { display: block; margin: 1rem auto; width: 20rem; }
.fullscreen .passphrase.hidden { display: none; }
.fullscreen .message-wrapper { margin: 2rem auto; }
.fullscreen .message-wrapper > .message { background-color: rgba(0,0,0,0.4); border-radius: 0.3rem; padding: 0.5rem; display: inline-block; }
.logo { position: relative; font-size: 20px; font-weight: bold; color: white; }
.logo > img { width: 100%; }
.logo > span { position: absolute; bottom: 0; right: 0; }
body { display: flex; flex-direction: row; }
.sidebar.closed { width: 0; }
.sidebar { display: flex; flex-direction: column; width: 15rem; overflow: hidden; transition: width 0.2s; z-index: 1; }
.sidebar { background: rgb(30,40,50); background: linear-gradient(90deg, rgb(40,50,60) 0%, rgb(34,40,53) 100%); }
.sidebar .logo { width: 13rem; margin: 1rem; }
.sidebar-control { font-size: 2rem; cursor: pointer; }
.sidebar-control.opener { color: rgb(30,40,50); position: absolute; top: 4.8rem; left: 0.5rem; z-index: 1; }
.sidebar-control.closer { display: block; color: white; margin: 1rem; margin-top: -1rem; text-align: right; }
.sidebar > .sidebar-item { padding: 1rem; color: rgb(160,180,200); text-decoration: none; }
.sidebar > .sidebar-item:hover, .sidebar > .sidebar-item:focus { background-color: rgba(0,0,0,0.1); }
.sidebar > .sidebar-item.selected { font-weight: bold; background-color: rgba(0,0,0,0.2); }
.sidebar > button { border-radius: 4px; border: 0px; padding: 1rem; margin: 1rem; font-size: 16px; cursor: pointer; }
.sidebar > button { background-color: rgba(0,0,0,0.2); color: rgb(160,180,200); }
.sidebar > button:hover, .sidebar > button:focus { background-color: rgba(0,0,0,0.1) }
.page { display: none; flex-grow: 1; margin: 2rem; }
.page h1 { font-size: 1.5rem; margin-top: 2rem; margin-bottom: 1rem; color: rgb(30,40,50); }
.page h1:first-child { margin-top: 0; }
.page.visible { display: block; }
.page.contracts { margin: 0; flex-direction: column; }
.page.contracts.visible { display: flex; }
.page.wallet .key { font-family: monospace; color: #444; font-size: 16px; }
.page.wallet .key.root { padding: 1rem; background-color: white; border-radius: 4px; display: inline-block; }
.page.wallet table { margin: 1rem 0; border-spacing: 1rem; }
.page.wallet table th { text-align: left; }
.page.wallet table .numeric { text-align: right; }
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

runWallet :: AppConstraints t m => AppCfg t m -> m ()
runWallet appCfg = do
  mRoot <- getItemStorage localStorage Wallet_RootKey
  _ <- workflow $ maybe (runSetupWF appCfg) (walletMain appCfg) mRoot
  pure ()

type AppWF t m = Workflow t m ()

finishAppWF :: Applicative m => a -> m ((), a)
finishAppWF = pure . (,) ()

type AppConstraints t m =
  ( HasConfigs m
  , HasStorage m
  , HasStorage (Performable m)
  , MonadWidget t m
  , RouteToUrl (R FrontendRoute) m
  , Routed t (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  )

walletMain
  :: forall t m. AppConstraints t m
  => AppCfg t m
  -> Crypto.XPrv
  -- ^ Root key
  -> Workflow t m ()
walletMain appCfg root = Workflow $ do
  pb <- getPostBuild
  page <- walletSidebar
  let appCfg' = appCfg {  _appCfg_forceResize = void (updated page) <> pb }
  let pageDemux = demux page
      mkPage :: Page -> Text -> m a -> m a
      mkPage p c = elDynAttr "div" (ffor (demuxed pageDemux p) $ \s -> "class" =: (c <> if s then " page visible" else " page"))
  (_childKeys, _namedKeys, removed) <- mkPage Page_Wallet "wallet" $ walletPage root
  mkPage Page_Contracts "contracts" $ do
    -- TODO appCfg should probably be partially defined here instead of
    -- completely in the executable.
    _ <- Frontend.ReplGhcjs.app appCfg'
    pure ()
  finishAppWF $ runSetupWF appCfg' <$ removed

data AddKeyError
  = AddKeyError_InvalidPassword
  | AddKeyError_NameTaken
  | AddKeyError_NameRequired
  deriving (Eq, Ord)

addKeyErrorText :: AddKeyError -> Text
addKeyErrorText = \case
  AddKeyError_InvalidPassword -> "Invalid password"
  AddKeyError_NameTaken -> "Key name is already in use"
  AddKeyError_NameRequired -> "Enter a key name"

walletPage
  :: AppConstraints t m
  => Crypto.XPrv
  -> m (Dynamic t (Map Crypto.DerivationIndex Crypto.XPrv), Dynamic t (Bimap Text Ed25519.PublicKey), Event t ())
walletPage root = do
  el "h1" $ text "Root key"
  divClass "root key" $ text $ T.decodeUtf8 $ B16.encode $ Crypto.xpubPublicKey $ Crypto.toXPub root
  el "h1" $ text "Derived keys"
  initChildKeys <- fromMaybe mempty <$> getItemStorage localStorage Wallet_ChildKeys
  initNamedKeys <- fromMaybe Bimap.empty <$> getItemStorage localStorage Wallet_NamedKeys
  rec
    (keyStore, derivationErrors) <- mapAccumMaybeDyn (&) (initChildKeys, initNamedKeys) newDerived
    let (childKeys, namedKeys) = splitDynPure keyStore
    mKeyStore <- maybeDyn $ ffor keyStore $ \(c,n) -> if M.null c then Nothing else Just (c,n)
    dyn_ $ ffor mKeyStore $ \case
      Nothing -> el "p" $ text "You haven't generated any derived keys yet."
      Just keyStore' -> el "table" $ do
        el "thead" $ el "tr" $ do
          elClass "th" "numeric" $ text "Index"
          el "th" $ text "Name"
          el "th" $ text "Public Key"
        el "tbody" $ dyn_ $ ffor keyStore' $ \(im, names) -> flip M.traverseWithKey im $ \i xprv -> el "tr" $ do
          elClass "td" "numeric" $ text $ T.pack $ show i
          let name = Bimap.lookupR (Crypto.xPubGetPublicKey $ Crypto.toXPub xprv) names
          el "td" $ text $ fromMaybe "No name" name
          let pk = T.decodeUtf8 $ B16.encode $ Crypto.xpubPublicKey $ Crypto.toXPub xprv
          elClass "td" "key" $ text pk
          copyButton def (pure pk)
    newDerived <- form "inline" $ do
      divClass "header" $ do
        text "Generate a new key"
        divClass "detail" $ text "The key will be derived from the wallet root key"
      name <- fmap (current . value) $ uiInputElement $ def
        & initialAttributes .~ "placeholder" =: "Key name"
        & inputElementConfig_setValue .~ ("" <$ updated childKeys)
      pass <- fmap (current . value) $ uiInputElement $ def
        & initialAttributes .~ "type" =: "password" <> "placeholder" =: "Enter password"
        & inputElementConfig_setValue .~ ("" <$ updated childKeys)
      add <- confirmButton (def & uiButtonCfg_type ?~ "submit")  "Generate key"
      lastErrors <- holdDyn Nothing $ leftmost [Just <$> derivationErrors, Nothing <$ updated childKeys]
      dyn_ $ ffor lastErrors $ \case
        Nothing -> blank
        Just es -> elClass "ul" "messages" $ for_ es $ el "li" . text . addKeyErrorText
      pure $ attachWith (const . derive) (liftA2 (,) name pass) add
  performEvent_ $ ffor (updated childKeys) $ setItemStorage localStorage Wallet_ChildKeys
  performEvent_ $ ffor (updated namedKeys) $ setItemStorage localStorage Wallet_NamedKeys
  delete <- el "div" $ uiButton def $ text "Delete wallet"
  removed <- performEvent $ ffor delete $ \_ -> do
    removeItemStorage localStorage Wallet_RootKey
    removeItemStorage localStorage Wallet_ChildKeys
  pure (childKeys, namedKeys, removed)
  where
    scheme = Crypto.DerivationScheme2
    mkHardened = (0x80000000 .|.)
    derive
      :: (Text, Text)
      -> (Map Crypto.DerivationIndex Crypto.XPrv, Bimap Text Ed25519.PublicKey)
      -> (Maybe (Map Crypto.DerivationIndex Crypto.XPrv, Bimap Text Ed25519.PublicKey), Maybe (Set AddKeyError))
    derive (name, pass) (xprvs, names) =
      let errs = S.fromList $ catMaybes
            [ AddKeyError_NameTaken <$ guard (Bimap.member name names)
            , AddKeyError_NameRequired <$ guard (T.null name)
            , AddKeyError_InvalidPassword <$ guard (not $ testKeyPassword root pass)
            ]
          n = maybe 0 (succ . fst) (M.lookupMax xprvs)
          xprv = Crypto.deriveXPrv scheme (T.encodeUtf8 pass) root (mkHardened $ fromIntegral n)
       in if S.null errs
          then (Just (M.insert n xprv xprvs, Bimap.insert name (Crypto.xPubGetPublicKey $ Crypto.toXPub xprv) names), Nothing)
          else (Nothing, Just errs)

data Page
  = Page_Wallet
  | Page_Contracts
  deriving (Eq, Ord, Enum, Bounded)

pageText :: DomBuilder t m => Page -> m ()
pageText = \case
  Page_Wallet -> text "Wallet"
  Page_Contracts -> text "Contracts"

walletSidebar :: AppConstraints t m => m (Dynamic t Page)
walletSidebar = do
  rec
    open <- holdUniqDyn <=< holdDyn True $ leftmost [False <$ closed, True <$ opened]
    opened <- uiIcon "fa-caret-right sidebar-control opener" def
    (closed, page) <- elDynAttr "div" (ffor open $ \o -> "class" =: ("sidebar" <> if o then "" else " closed")) $ do
      kadenaWalletLogo
      closed' <- uiIcon "fa-caret-left sidebar-control closer" def
      rec
        let sidebarLink p = do
              let mkAttrs sel = "href" =: "#" <> "class" =: ("sidebar-item" <> if sel then " selected" else "")
              (e, ()) <- elDynAttr' "a" (mkAttrs <$> demuxed pageDemux p) $ pageText p
              pure $ p <$ domEvent Click e
        page <- holdUniqDyn =<< holdDyn Page_Wallet . leftmost =<< traverse sidebarLink [minBound..maxBound]
        let pageDemux = demux page
      elAttr "div" ("style" =: "flex-grow: 1") blank
      pure (closed', page)
  pure page

runSetupWF :: AppConstraints t m => AppCfg t m -> AppWF t m
runSetupWF appCfg = Workflow $ do
  xprv <- runSetup
  saved <- performEvent $ ffor xprv $ \x -> setItemStorage localStorage Wallet_RootKey x >> pure x
  finishAppWF $ walletMain appCfg <$> saved

-- | Check the validity of the password by signing and verifying a message
testKeyPassword :: Crypto.XPrv -> Text -> Bool
testKeyPassword xprv pass = Crypto.verify (Crypto.toXPub xprv) msg $ Crypto.sign (T.encodeUtf8 pass) xprv msg
  where msg = "test message" :: ByteString
