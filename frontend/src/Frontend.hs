{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PackageImports #-}
module Frontend where

import Control.Lens ((^.))
import Control.Monad (join, void)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Morph (lift)
import Data.Coerce (coerce)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Blob as Blob
import qualified "ghcjs-dom" GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.FileReader as FileReader
import qualified GHCJS.DOM.HTMLAnchorElement as HTMLAnchorElement
import qualified GHCJS.DOM.HTMLBaseElement as HTMLBaseElement
import qualified GHCJS.DOM.HTMLElement as HTMLElement
import qualified GHCJS.DOM.HTMLInputElement as HTMLInput
import qualified GHCJS.DOM.Types as Types
import qualified GHCJS.DOM.File as JSFile
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.URL as URL
import Foreign.JavaScript.Utils (bsToArrayBuffer)
import Language.Javascript.JSaddle (JSException(..), js0, js1, (<#), (!), valToText)
import Kadena.SigningApi
import Reflex.Dom
import Pact.Server.ApiClient (runTransactionLoggerT, noLogger)
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import System.IO

import Common.Api
import Common.Route
import Frontend.AppCfg
import Frontend.Log (errorLevelLogger)
import Frontend.Foundation
import Frontend.ModuleExplorer.Impl (loadEditorFromLocalStorage)
import Frontend.Storage
import Frontend.Setup.Browser (bipWalletBrowser)
import Frontend.WalletConnect

main :: IO ()
main = do
  let Right validFullEncoder = checkEncoder backendRouteEncoder
  run $ runFrontend validFullEncoder frontend

-- This frontend is only used for the web/browser app
-- See Backend.App.main' for native's equivalent
-- See Desktop.App.main' for the development (ob run) equivalent
-- See Frontend.App for the code shared across both browser and native
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
    liftIO $ hSetBuffering stderr LineBuffering
    liftIO $ hSetBuffering stdout LineBuffering
    (fileOpened, triggerOpen) <- openFileDialog
    mapRoutedT (flip runTransactionLoggerT noLogger . runBrowserStorageT) $ do
      let fileFFI = FileFFI
            { _fileFFI_externalFileOpened = fileOpened
            , _fileFFI_openFileDialog = liftJSM . triggerOpen
            , _fileFFI_deliverFile = triggerFileDownload
            }

      (walletConnect, signingHandler, wcSignReqErrEv) <-
        setupWalletConnect
      bipWalletBrowser fileFFI walletConnect wcSignReqErrEv $ \enabledSettings -> AppCfg
        { _appCfg_gistEnabled = False
        , _appCfg_loadEditor = loadEditorFromLocalStorage
        , _appCfg_editorReadOnly = False
        , _appCfg_signingHandler = mapRoutedT lift signingHandler
        , _appCfg_enabledSettings = enabledSettings
        , _appCfg_logMessage = errorLevelLogger
        }
  }

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

triggerFileDownload :: (MonadJSM (Performable m), PerformEvent t m)
  => Event t (FilePath, Text) -> m (Event t (Either Text FilePath))
triggerFileDownload ev = performEvent $ ffor ev $ \(fileName, c) -> liftJSM $ catch (do
  doc   <- DOM.currentDocumentUnchecked
  a :: HTMLAnchorElement.HTMLAnchorElement <- coerce <$> Document.createElement doc ("a" :: Text)
  array <- bsToArrayBuffer (T.encodeUtf8 c)
  blob <- Blob.newBlob [array] (Nothing :: Maybe DOM.BlobPropertyBag)
  (url :: DOM.JSString) <- URL.createObjectURL blob
  HTMLBaseElement.setHref (coerce a) url
  HTMLAnchorElement.setDownload a fileName
  body <- Document.getBodyUnchecked doc
  void $ Node.appendChild body a
  HTMLElement.click a
  void $ Node.removeChild body a
  URL.revokeObjectURL url
  pure (Right fileName))
  (\(JSException e) -> valToText e >>= return . Left)

loaderMarkup :: DomBuilder t m => m ()
loaderMarkup = divClass "spinner" $ do
  divClass "spinner__cubes" $ do
    divClass "cube1" blank
    divClass "cube2" blank
  divClass "spinner__msg" $ text "Loading"

newHead :: (Prerender js t m, DomBuilder t m) => (R BackendRoute -> Text) -> m (Event t ())
newHead routeText = do
  el "title" $ text "(BETA) Kadena Chainweaver: Wallet & IDE"
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
  -- Allows importing private keys
  js (static @"js/nacl-fast.min-v1.0.0.js")
  -- Allows for BIP39-based key generation and encrypted storage of private keys
  js (static @"js/kadena-crypto.js")
  js (static @"js/wallet-connect/umd/index.min.js")
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
