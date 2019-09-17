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
import qualified Text.Megaparsec as MP
import           Data.Void (Void)
------------------------------------------------------------------------------
import           Obelisk.Route                   (R)
import           Obelisk.Route.Frontend
------------------------------------------------------------------------------
import           Common.Route
import           Frontend.Foundation
import           Frontend.ModuleExplorer
import           Frontend.Messages
import           Common.RefPath
import           Frontend.OAuth                  (HasOAuth (..))
------------------------------------------------------------------------------


handleRoutes
  :: forall m t mConf model.
  ( Monad m, PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m)
  , Routed t (R FrontendRoute) m, SetRoute t (R FrontendRoute) m
  , Monoid mConf, HasModuleExplorer model t , HasModuleExplorerCfg mConf t
  , HasMessagesCfg mConf t
  , HasOAuth model t
  )
  => model
  -> m mConf
handleRoutes m = do
    route <- askRoute
    let loaded = m ^. moduleExplorer_loaded
    onRoute <- tagOnPostBuild route
    let
      onParsedRoute = parseRoute <$> onRoute
      onNewLoaded = fmapMaybe id
        . attachWith getLoadedUniq (current loaded)
        . fmap (^? _Just . _Right)
        $ onParsedRoute

      onInvalidRoute = () <$ fmapMaybe (^? _Just . _Left) onParsedRoute

      onOAuthRouteReset = (FrontendRoute_Main :/ ()) <$ m ^. oAuth_error

      onNewRoute = fmapMaybe id
        . attachWith buildRoute (current route)
        $ updated loaded

    -- Delay here break FRP loops which appeared after an obelisk bump
    setRoute <=< delay 0 $ leftmost [onNewRoute, onOAuthRouteReset]

    pure $ mempty
      & moduleExplorerCfg_loadFile .~ fmapMaybe (^? _LoadedRef_File) onNewLoaded
      & moduleExplorerCfg_loadModule .~ fmapMaybe (^? _LoadedRef_Module) onNewLoaded
      & moduleExplorerCfg_clearLoaded .~ onInvalidRoute
      & messagesCfg_send .~ ([ "Given URI seemed to be invalid and could not be loaded." ] <$ onInvalidRoute)

  where


    getLoadedUniq :: Maybe LoadedRef -> Maybe LoadedRef -> Maybe LoadedRef
    getLoadedUniq cLoaded newLoaded = do
      guard $ newLoaded /= cLoaded
      newLoaded

    buildRoute :: R FrontendRoute -> Maybe LoadedRef -> Maybe (R FrontendRoute)
    buildRoute cRoute ref =
      let
        newRoute = maybe (FrontendRoute_New :=> Identity ()) (renderRoute . renderRef) ref
      in
        if newRoute == cRoute
           then Nothing
           else Just newRoute

    parseRoute :: R FrontendRoute -> Maybe (Either (MP.ParseErrorBundle RefPath Void) LoadedRef)
    parseRoute = fmap runRefParser . \case
       FrontendRoute_Main     :/ () -> Nothing
       FrontendRoute_Example  :/ xs -> Just $ "example":xs
       FrontendRoute_Stored   :/ xs -> Just $ "stored":xs
       FrontendRoute_Gist     :/ xs -> Just $ "gist":xs
       FrontendRoute_Deployed :/ xs -> Just $ "deployed":xs
       FrontendRoute_New      :/ () -> Nothing
       FrontendRoute_OAuth    :/ _  -> Nothing

    runRefParser = MP.parse parseRef "URL" . RefPath

    renderRoute :: RefPath -> R FrontendRoute
    renderRoute (RefPath (n:ns)) = case n of
      "example"  -> FrontendRoute_Example  :/ ns
      "stored"   -> FrontendRoute_Stored   :/ ns
      "gist"     -> FrontendRoute_Gist     :/ ns
      "deployed" -> FrontendRoute_Deployed :/ ns
      _          -> FrontendRoute_Main     :/ ()
    renderRoute _ = FrontendRoute_Main     :/ ()
