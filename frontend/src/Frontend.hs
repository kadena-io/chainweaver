{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TemplateHaskell  #-}
module Frontend where

import           Control.Monad            (void)
import           Data.Functor             (($>))
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Reflex.Dom.Core

import           Obelisk.Frontend
import           Obelisk.Route.Frontend
import           Obelisk.Generated.Static

import           Common.Api
import           Common.Route
import           Frontend.ReplGhcjs

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

  , _frontend_body = prerender_ loaderMarkup app
  }

loaderMarkup :: DomBuilder t m => m ()
loaderMarkup = do
  divClass "spinner" $ do
    divClass "cube1" blank
    divClass "cube2" blank
  divClass "spinner__msg" $ text "Loading"

newHead :: DomBuilder t m => (R BackendRoute -> Text) -> m (Event t ())
newHead routeText = do
  el "title" $ text "Kadena - Pact Testnet"
  meta ("name" =: "description" <> "content" =: "Write, test, and deploy safe smart contracts using Pact, Kadena's programming language")
  meta ("name" =: "keywords" <> "content" =: "kadena, pact, pact testnet, pact language, pact programming language, smart contracts, safe smart contracts, smart contract language, blockchain, learn blockchain programming, chainweb")
  meta ("charset" =: "utf-8")
  meta ("name" =: "google" <> "content" =: "notranslate")
  meta ("http-equiv" =: "Content-Language" <> "content" =: "en_US")
  elAttr "link" ("href" =: routeText (BackendRoute_Css :/ ()) <> "rel" =: "stylesheet") blank
  ss "https://fonts.googleapis.com/css?family=Roboto"
  ss "https://fonts.googleapis.com/css?family=Work+Sans"
  ss (static @"css/font-awesome.min.css")
  ss (static @"css/ace-theme-pact-web.css")
  -- "https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.1/ace.js"
  (ace, _) <- js' (static @"js/ace/ace.js")
  el "script" $ text $ "ace.config.set('basePath', 'static/js/ace')"
  _ <- runWithReplace (pure ()) $ domEvent Load ace $> do
    js (static @"js/ace-mode-pact.js")
  js (static @"js/nacl-fast.min-v1.0.0.js")
  (bowser, _) <- js' (static @"js/bowser.min.js")
  pure $ domEvent Load bowser
  where
    js = void . js'
    js' url = elAttr' "script" ("type" =: "text/javascript" <> "src" =: url <> "charset" =: "utf-8") blank
    ss url = elAttr "link" ("href" =: url <> "rel" =: "stylesheet") blank
    meta attrs = elAttr "meta" attrs blank
