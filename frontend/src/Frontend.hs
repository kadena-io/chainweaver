{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Frontend where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T
import           Reflex.Dom.Core

import           Obelisk.ExecutableConfig (get)
import           Obelisk.Frontend
import           Obelisk.Generated.Static
import           Obelisk.Route


import           Common.Route
import           Frontend.ReplGhcjs

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head= do
      -- Global site tag (gtag.js) - Google Analytics
      gaTrackingId <- fmap T.strip $ liftIO $ fromMaybe "UA-127512784-1"
        <$> get "config/frontend/tracking-id"
      let
        gtagSrc = "https://www.googletagmanager.com/gtag/js?id=" <> gaTrackingId
      elAttr "script" ("async" =: "" <> "src" =: gtagSrc) blank
      el "script" $ text $ T.unlines
        [ "window.dataLayer = window.dataLayer || [];"
        , "function gtag(){dataLayer.push(arguments);}"
        , "gtag('js', new Date());"
        , "gtag('config', '" <> gaTrackingId <> "');"
        ]

      newHead

  , _frontend_body = prerender loaderMarkup app
  , _frontend_headRender = HeadRender_Static
  }

loaderMarkup :: DomBuilder t m => m ()
loaderMarkup = do
  divClass "spinner" $ do
    divClass "cube1" blank
    divClass "cube2" blank
  divClass "spinner-msg" $ text "Loading"

newHead :: DomBuilder t m => m ()
newHead = do
    el "title" $ text "Pact Test Net"
    meta ("name" =: "description" <> "content" =: "Write, test, and deploy Pact smart contracts in your browser")
    meta ("name" =: "keywords" <> "content" =: "pact, pact language, pact programming language, smart contracts, smart contract language, blockchain, cryptocurrency, learn blockchain programming, kadena, chainweb")
    meta ("charset" =: "utf-8")
    meta ("name" =: "google" <> "content" =: "notranslate")
    meta ("http-equiv" =: "Content-Language" <> "content" =: "en_US")
    ss "https://fonts.googleapis.com/css?family=Roboto"
    ss "https://fonts.googleapis.com/css?family=Work+Sans"
    ss (static @"css/font-awesome.min.css")
    ss (static @"css/styles.css")
    ss (static @"css/extra.css")
    ss (static @"css/ace-theme-pact-web.css")
    js "https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.1/ace.js"
    js (static @"js/ace-mode-pact.js")
    js (static @"js/nacl-fast.min-v1.0.0.js")
  where
    js url = elAttr "script" ("type" =: "text/javascript" <> "src" =: url <> "charset" =: "utf-8") blank
    ss url = elAttr "link" ("href" =: url <> "rel" =: "stylesheet") blank
    meta attrs = elAttr "meta" attrs blank
