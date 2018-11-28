{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Backend where

import           Control.Monad.Identity   (Identity (..))
import           Control.Monad.IO.Class   (liftIO)
import           Data.Dependent.Sum       (DSum ((:=>)))
import           Data.List                (foldl')
import qualified Data.List                as L
import           Data.Maybe               (isJust)
import qualified Data.Text                as T
import qualified Obelisk.Backend          as Ob
import           Obelisk.Route            (R)
import           Snap                     (Snap, pass)
import           Snap.Util.FileServe      (serveFile)
import           System.FilePath          ((</>))
import           System.Directory         (canonicalizePath, doesFileExist)

import           Common.Api
import           Common.Route
import qualified Backend.Devel as Devel

backend :: Ob.Backend BackendRoute FrontendRoute
backend = Ob.Backend
    { Ob._backend_run = \serve -> do
        hasServerList <- isJust <$> getPactServerList
        if hasServerList
           -- Production mode:
           then serve $ serveBackendRoute "/var/lib/pact-web/dyn-configs"
           --  Devel mode:
           else Devel.withPactInstances serve

    , Ob._backend_routeEncoder = backendRouteEncoder
    }

-- | Serve our dynconfigs file.
serveBackendRoute :: FilePath -> R BackendRoute -> Snap ()
serveBackendRoute dynConfigs = \case
  BackendRoute_DynConfigs :=> Identity ps
    -> do
      let
        strSegs = map T.unpack ps
        p = foldl' (</>) dynConfigs strSegs
      pNorm <- liftIO $ canonicalizePath p
      baseNorm <- liftIO $ canonicalizePath dynConfigs
      -- Sanity check: Make sure we are serving a file in the target directory.
      exists <- liftIO $ doesFileExist pNorm
      if L.isPrefixOf baseNorm pNorm && exists
         then serveFile pNorm
         else pass
  _ -> pure ()
