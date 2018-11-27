{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

-- | Provide a working test environment in "devel mode", e.g. on `ob run`.
--
--   This module is meant to be imported qualified as "Devel".
module Backend.Devel (withPactInstances) where

import           Control.Concurrent.Async (mapConcurrently_, withAsync)
import           Data.Foldable            (traverse_)
import           Data.Monoid              ((<>))
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Obelisk.Route            (R)
import qualified Pact.Server.Server       as Pact
import           Snap                     (Snap)
import           System.Directory         (createDirectoryIfMissing)
import           System.FilePath          ((</>))

import           Common.Api


-- | Configuration for pact instances.
data PactInstanceConfig = PactInstanceConfig
  { _pic_conf :: FilePath -- ^ Config file path
  , _pic_log  :: FilePath -- ^ Persist and logging directory
  , _pic_num  :: Int      -- ^ What instance number do we have?
  }

-- | Directory for storing the configuration for our development pact instances.
pactConfigDir :: FilePath
pactConfigDir = "/var/tmp/pact-conf"

-- | Root directory for development pact logging directories.
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

-- | Server starting development Pact instances.
withPactInstances :: ((R backendRoute -> Snap ()) -> IO ()) -> IO ()
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
   , "gasLimit: 50"
   , "gasRate: 1"
   ]

