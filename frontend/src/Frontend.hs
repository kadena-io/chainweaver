{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Text as T
import Reflex.Dom.Core
import           Control.Lens
import           Control.Monad.State.Strict
import qualified Data.List.Zipper as Z
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.String.QQ
import           Data.Text (Text)
import qualified Data.Text as T
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom hiding (Element, fromJSString)
import           Reflex.Dom.ACE.Extended

import           Pact.Repl
import           Pact.Repl.Types
import           Pact.Types.Lang
import           ReplGhcjs

import Common.Api
import Static

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
      elAttr "link" ("href" =: static @"css/font-awesome.min.css" <> "rel" =: "stylesheet") blank
      -- elAttr "script" ("type" =: "text/javascript" <> "src" =: "https://cdnjs.cloudflare.com/ajax/libs/underscore.js/1.8.3/underscore-min.js") blank
      -- elAttr "script" ("type" =: "text/javascript" <> "src" =: "https://cdnjs.cloudflare.com/ajax/libs/jquery/1.10.2/jquery.js") blank
      -- elAttr "script" ("src" =: "https://code.jquery.com/ui/1.12.1/jquery-ui.js") blank
      elAttr "script" ("type" =: "text/javascript" <> "src" =: "https://cdnjs.cloudflare.com/ajax/libs/ace/1.2.5/ace.js" <> "charset" =: "utf-8") blank
      elAttr "script" ("type" =: "text/javascript" <> "src" =: static @"js/ace-mode-pact.js") blank
      elAttr "script" ("type" =: "text/javascript" <> "src" =: "https://cdnjs.cloudflare.com/ajax/libs/ace/1.2.5/theme-solarized_light.js") blank
      elAttr "link" ("href" =: static @"css/index.css" <> "rel" =: "stylesheet" <> "type" =: "text/css") blank
    body = app
