{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Frontend where

import Data.Monoid
import Reflex.Dom.Core

import Frontend.ReplGhcjs
import Static

frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body)
 where
  head' = do
    elAttr
      "meta"
      ("content" =: "#ffffff" <> "name" =: "msapplication-TileColor")
      blank
    elAttr
      "meta"
      ("content" =: "ms-icon-144x144.png" <> "name" =: "msapplication-TileImage"
      )
      blank
    elAttr "meta" ("content" =: "#ffffff" <> "name" =: "theme-color") blank

    css "https://fonts.googleapis.com/css?family=Roboto:300,600"
    -- css "https://code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"
    css $ static @"semantic-reflex/semantic.min.css"
    css $ static @"css/semantic.superhero.min.css"
    css $ static @"css/font-awesome.min.css"
    -- js "https://cdnjs.cloudflare.com/ajax/libs/underscore.js/1.8.3/underscore-min.js"
    -- js "https://cdnjs.cloudflare.com/ajax/libs/jquery/1.10.2/jquery.js"
    -- js "https://code.jquery.com/ui/1.12.1/jquery-ui.js"
    js "https://cdnjs.cloudflare.com/ajax/libs/ace/1.2.5/ace.js"
    js $ static @"js/ace-mode-pact.js"
    js
      "https://cdnjs.cloudflare.com/ajax/libs/ace/1.2.5/theme-solarized_dark.js"
    css $ static @"css/index.css"
  body = app
  js loc = elAttr "script" ("type" =: "text/javascript" <> "src" =: loc) blank
  css loc = elAttr
    "link"
    ("href" =: loc <> "rel" =: "stylesheet" <> "type" =: "text/css")
    blank
------------------------------------------------------------------------------

