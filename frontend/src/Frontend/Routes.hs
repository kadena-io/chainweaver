{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE ExtendedDefaultRules   #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Handling of route updates for pact-web.
--
--   This module takes care of triggering actions on route updates and keeping
--   routes and internal state in sync.
--
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.Routes where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Dependent.Sum              (DSum ((:=>)))
import           Reflex
------------------------------------------------------------------------------
import           Obelisk.Route                   (R)
import           Obelisk.Route.Frontend
------------------------------------------------------------------------------
import           Common.Route
import           Frontend.Foundation
import           Frontend.ModuleExplorer
import           Frontend.ModuleExplorer.RefPath
import           Frontend.OAuth                  (HasOAuth (..))
------------------------------------------------------------------------------


handleRoutes
  :: forall m t mConf model.
  ( Monad m, PostBuild t m
  , Routed t (R FrontendRoute) m, SetRoute t (R FrontendRoute) m
  , Monoid mConf, HasModuleExplorer model t , HasModuleExplorerCfg mConf t
  , HasOAuth model t
  )
  => model
  -> m mConf
handleRoutes m = do
    route <- askRoute
    let loaded = m ^. moduleExplorer_loaded
    onRoute <- tagOnPostBuild route
    let
      onNewLoaded = fmapMaybe id
        . attachWith getLoaded (current loaded)
        $ onRoute

      onOAuthRouteReset = (FrontendRoute_Main :/ ()) <$ m ^. oAuth_error

      onNewRoute = fmapMaybe id
        . attachWith buildRoute (current route)
        $ updated loaded

    setRoute $ leftmost [onNewRoute, onOAuthRouteReset]

    pure $ mempty
      & moduleExplorerCfg_loadFile .~ fmapMaybe (^? _LoadedRef_File) onNewLoaded
      & moduleExplorerCfg_loadModule .~ fmapMaybe (^? _LoadedRef_Module) onNewLoaded

  where

    getLoaded :: Maybe LoadedRef -> R FrontendRoute -> Maybe LoadedRef
    getLoaded cLoaded routeL = do
      let rp = parseRoute routeL
      loaded <- runParseRef rp
      guard $ Just loaded /= cLoaded
      pure loaded

    buildRoute :: R FrontendRoute -> Maybe LoadedRef -> Maybe (R FrontendRoute)
    buildRoute cRoute ref =
      let
        newRoute = maybe (FrontendRoute_New :=> Identity ()) (renderRoute . renderRef) ref
      in
        if newRoute == cRoute
           then Nothing
           else Just newRoute

    parseRoute :: R FrontendRoute -> RefPath
    parseRoute = \case
       FrontendRoute_Main     :/ () -> RefPath []
       FrontendRoute_Example  :/ xs -> RefPath ("example":xs)
       FrontendRoute_Stored   :/ xs -> RefPath ("stored":xs)
       FrontendRoute_Gist     :/ xs -> RefPath ("gist":xs)
       FrontendRoute_Deployed :/ xs -> RefPath ("deployed":xs)
       FrontendRoute_New      :/ () -> RefPath []
       FrontendRoute_OAuth    :/ _  -> RefPath []

    renderRoute :: RefPath -> R FrontendRoute
    renderRoute (RefPath (n:ns)) = case n of
      "example"  -> FrontendRoute_Example  :/ ns
      "stored"   -> FrontendRoute_Stored   :/ ns
      "gist"     -> FrontendRoute_Gist     :/ ns
      "deployed" -> FrontendRoute_Deployed :/ ns
      _          -> FrontendRoute_Main     :/ ()
    renderRoute _ = FrontendRoute_Main     :/ ()
