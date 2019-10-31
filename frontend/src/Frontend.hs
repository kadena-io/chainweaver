{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TemplateHaskell  #-}
module Frontend where

import           Control.Monad            (join, void)
import           Control.Monad.IO.Class
import           Data.Maybe               (listToMaybe)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified GHCJS.DOM.EventM         as EventM
import qualified GHCJS.DOM.FileReader     as FileReader
import qualified GHCJS.DOM.HTMLElement    as HTMLElement
import qualified GHCJS.DOM.Types          as Types
import           Reflex.Dom.Core

import           Obelisk.Frontend
import           Obelisk.Route.Frontend
import           Obelisk.Generated.Static

import           Common.Api
import           Common.Route
import           Frontend.AppCfg
import           Frontend.Foundation
import           Frontend.ModuleExplorer.Impl (loadEditorFromLocalStorage)
import           Frontend.ReplGhcjs
import           Frontend.Storage

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
    let store = browserStorage
    flip runStorageT store $ app $ AppCfg
      { _appCfg_gistEnabled = True
      , _appCfg_externalFileOpened = fileOpened
      , _appCfg_openFileDialog = liftJSM triggerOpen
      , _appCfg_loadEditor = loadEditorFromLocalStorage
      , _appCfg_editorReadOnly = False
      , _appCfg_signingRequest = never
      , _appCfg_signingResponse = liftIO . print
      , _appCfg_forceResize = never
      }
  }

-- | The 'JSM' action *must* be run from a user initiated event in order for the
-- dialog to open
openFileDialog :: MonadWidget t m => m (Event t Text, JSM ())
openFileDialog = do
  let attrs = "type" =: "file" <> "accept" =: ".pact" <> "style" =: "display: none"
  input <- inputElement $ def & initialAttributes .~ attrs
  let newFile = fmapMaybe listToMaybe $ updated $ _inputElement_files input
  mContents <- performEventAsync $ ffor newFile $ \file cb -> Types.liftJSM $ do
    fileReader <- FileReader.newFileReader
    FileReader.readAsText fileReader (Just file) (Nothing :: Maybe Text)
    _ <- EventM.on fileReader FileReader.loadEnd $ Types.liftJSM $ do
      mStringOrArrayBuffer <- FileReader.getResult fileReader
      mText <- traverse (Types.fromJSVal . Types.unStringOrArrayBuffer) mStringOrArrayBuffer
      liftIO $ cb $ join mText
    pure ()
  let open = HTMLElement.click $ _inputElement_raw input
  pure (fmapMaybe id mContents, open)

loaderMarkup :: DomBuilder t m => m ()
loaderMarkup = do
  divClass "spinner" $ do
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
  ss "https://fonts.googleapis.com/css?family=Roboto"
  ss "https://fonts.googleapis.com/css?family=Work+Sans"
  ss (static @"css/font-awesome.min.css")
  ss (static @"css/ace-theme-chainweaver.css")
  js "/static/js/ace/ace.js"
  prerender_ blank $ js "/static/js/ace/mode-pact.js"
  js (static @"js/nacl-fast.min-v1.0.0.js")
  (bowser, _) <- js' (static @"js/bowser.min.js")
  pure $ domEvent Load bowser
  where
    js :: forall t n. DomBuilder t n => Text -> n ()
    js = void . js'
    js' :: forall t n. DomBuilder t n => Text -> n (Element EventResult (DomBuilderSpace n) t, ())
    js' url = elAttr' "script" ("type" =: "text/javascript" <> "src" =: url <> "charset" =: "utf-8") blank
    ss url = elAttr "link" ("href" =: url <> "rel" =: "stylesheet") blank
    meta attrs = elAttr "meta" attrs blank
