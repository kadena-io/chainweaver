{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Backend where

import           Control.Concurrent.Async        (withAsync, mapConcurrently_)
import           Data.Foldable                   (traverse_)
import           Data.Monoid                     ((<>))
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import qualified Obelisk.ExecutableConfig        as Conf
import qualified Pact.Server.Server              as Pact
import           System.Directory                (createDirectoryIfMissing)
import           System.FilePath                 ((</>))

import           Common.Api
import           Common.Route
import qualified Obelisk.Backend                 as Ob


-- | Configuration for pact instances.
data PactInstanceConfig = PactInstanceConfig
  { _pic_conf :: FilePath -- ^ Config file path
  , _pic_log  :: FilePath -- ^ Persist and logging directory
  , _pic_num  :: Int      -- ^ What instance number do we have?
  }

-- | Directory for storing the configuration for our development pact instances.
pactConfigDir :: FilePath
pactConfigDir = "/var/tmp/pact-conf"

-- | Root directory for pact logging directories.
pactLogBaseDir :: FilePath
pactLogBaseDir = "/var/tmp/pact-log"

-- | Pact instance configurations
pactConfigs :: [PactInstanceConfig]
pactConfigs = map mkConfig [1 .. numPactInstances]
  where
    mkConfig num = PactInstanceConfig
      { _pic_conf = pactConfigDir  </> mkFileName num <> ".yaml"
      , _pic_log  = pactLogBaseDir </> mkFileName num
      , _pic_num  = num
      }
    mkFileName num = "pact-" <> show num

backend :: Ob.Backend BackendRoute FrontendRoute
backend = Ob.Backend
    { Ob._backend_run = \serve -> do

        wantsBackends <- getHasDevPactBackends
        if wantsBackends
           then withPactInstances serve
           else serve $ const $ pure ()

    , Ob._backend_routeEncoder = backendRouteEncoder
    }
  where
    getHasDevPactBackends :: IO Bool
    getHasDevPactBackends = do
      mR <- Conf.get "config/backend/pact-backends-file"
      pure $ case mR of
        Nothing -> True
        Just _  -> False

    withPactInstances serve = do
      traverse_ (createDirectoryIfMissing True . _pic_log) pactConfigs
      createDirectoryIfMissing False $ pactConfigDir
      traverse_ writePactConfig pactConfigs

      let servePact = Pact.serve . _pic_conf

      withAsync (mapConcurrently_ servePact pactConfigs) $ \_ ->
          serve (const $ pure ())

writePactConfig :: PactInstanceConfig -> IO ()
writePactConfig cfg =
 T.writeFile (_pic_conf cfg) $ T.unlines
   [ "port: "       <> getPactInstancePort (_pic_num cfg)
   , "logDir: "     <> (T.pack $ _pic_log cfg)
   , "persistDir: " <> (T.pack $ _pic_log cfg)
   , "pragmas: []"
   , "verbose: True"
   ]

