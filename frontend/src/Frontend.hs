{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Frontend where

import           Data.Monoid
import           Reflex.Dom.Core

import           Frontend.ReplGhcjs
import           Static

frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body)
  where
    head' = do
      el "title" $ text "Pact IDE"
      elAttr "meta" ("content" =: "#ffffff" <> "name" =: "msapplication-TileColor") blank
      elAttr "meta" ("content" =: "ms-icon-144x144.png" <> "name" =: "msapplication-TileImage") blank
      elAttr "meta" ("content" =: "#ffffff" <> "name" =: "theme-color") blank
      elAttr "link" ("href" =: "https://fonts.googleapis.com/css?family=Roboto:300,600" <> "rel" =: "stylesheet" <> "type" =: "text/css") blank
      -- elAttr "link" ("href" =: "https://code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: static @"semantic-reflex/semantic.min.css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: static @"css/semantic.superhero.min.css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: static @"css/font-awesome.min.css" <> "rel" =: "stylesheet") blank
      -- elAttr "script" ("type" =: "text/javascript" <> "src" =: "https://cdnjs.cloudflare.com/ajax/libs/underscore.js/1.8.3/underscore-min.js") blank
      -- elAttr "script" ("type" =: "text/javascript" <> "src" =: "https://cdnjs.cloudflare.com/ajax/libs/jquery/1.10.2/jquery.js") blank
      -- elAttr "script" ("src" =: "https://code.jquery.com/ui/1.12.1/jquery-ui.js") blank
      elAttr "script" ("type" =: "text/javascript" <> "src" =: "https://cdnjs.cloudflare.com/ajax/libs/ace/1.2.5/ace.js" <> "charset" =: "utf-8") blank
      elAttr "script" ("type" =: "text/javascript" <> "src" =: static @"js/ace-mode-pact.js") blank
      elAttr "script" ("type" =: "text/javascript" <> "src" =: "https://cdnjs.cloudflare.com/ajax/libs/ace/1.2.5/theme-solarized_dark.js") blank
      elAttr "link" ("href" =: static @"css/index.css" <> "rel" =: "stylesheet" <> "type" =: "text/css") blank
    body = app
