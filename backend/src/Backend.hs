{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend where

import           Control.Monad.Identity (Identity (..))
import           Control.Monad.IO.Class (liftIO)
import           Data.Dependent.Sum     (DSum ((:=>)))
import           Data.List              (foldl')
import qualified Data.List              as L
import           Data.Maybe             (isJust)
import qualified Data.Text              as T
import qualified Obelisk.Backend        as Ob
import           Obelisk.Route          (pattern (:/), R)
import           Snap                   (Snap, pass, writeBS)
import           Snap.Util.FileServe    (serveFile)
import           System.Directory       (canonicalizePath, doesFileExist)
import           System.FilePath        ((</>))

import qualified Backend.Devel          as Devel
import           Common.Api
import           Common.Route

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
  BackendRoute_DynConfigs :/ ps
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
  BackendRoute_Robots :=> _
    -> writeBS "User-agent: *\nDisallow: \n"
  -- TODO: Those params are actually mandatory here, but I'd like to re-use the
  -- existing encoder:
  {- BackendRoute_OAuthGetToken :/ OAuthRoute_Redirect :/ (provId, mParams) -}
  {-   -> -}
  _ -> pure ()
