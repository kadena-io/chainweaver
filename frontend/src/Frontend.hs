{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Frontend where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Maybe               (fromMaybe)
import           Data.Monoid
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
      gaTrackingId <- liftIO $ fromMaybe "UA-127512784-1"
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

      el "title" $ text "Pact IDE"
      newHead

  , _frontend_body = prerender (text "Loading, stand by...") app
  , _frontend_headRender = HeadRender_Static
  }

--oldHead :: MonadWidget t m => m ()
--oldHead = do
--    elAttr "meta" ("content" =: "#ffffff" <> "name" =: "msapplication-TileColor") blank
--    elAttr "meta" ("content" =: "ms-icon-144x144.png" <> "name" =: "msapplication-TileImage") blank
--    elAttr "meta" ("content" =: "#ffffff" <> "name" =: "theme-color") blank
--    elAttr "link" ("href" =: "https://fonts.googleapis.com/css?family=Roboto:300,600" <> "rel" =: "stylesheet" <> "type" =: "text/css") blank
--    -- elAttr "link" ("href" =: "https://code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css" <> "rel" =: "stylesheet") blank
--    elAttr "link" ("href" =: static @"semantic-reflex/semantic.min.css" <> "rel" =: "stylesheet") blank
--    elAttr "link" ("href" =: static @"css/semantic.superhero.min.css" <> "rel" =: "stylesheet") blank
--    elAttr "link" ("href" =: static @"css/font-awesome.min.css" <> "rel" =: "stylesheet") blank
--    -- elAttr "script" ("type" =: "text/javascript" <> "src" =: "https://cdnjs.cloudflare.com/ajax/libs/underscore.js/1.8.3/underscore-min.js") blank
--    -- elAttr "script" ("type" =: "text/javascript" <> "src" =: "https://cdnjs.cloudflare.com/ajax/libs/jquery/1.10.2/jquery.js") blank
--    -- elAttr "script" ("src" =: "https://code.jquery.com/ui/1.12.1/jquery-ui.js") blank
--    elAttr "script" ("type" =: "text/javascript" <> "src" =: "https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.1/ace.js" <> "charset" =: "utf-8") blank
--    elAttr "script" ("type" =: "text/javascript" <> "src" =: static @"js/ace-mode-pact.js") blank
--    elAttr "script" ("type" =: "text/javascript" <> "src" =: static @"js/nacl-fast.min-v1.0.0.js") blank
--    elAttr "script" ("type" =: "text/javascript" <> "src" =: "https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.1/theme-solarized_dark.js") blank
--    elAttr "link" ("href" =: static @"css/index.css" <> "rel" =: "stylesheet" <> "type" =: "text/css") blank

newHead :: DomBuilder t m => m ()
newHead = do
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
