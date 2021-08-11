-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Provide a working test environment in "devel mode", e.g. on `ob run`.
--
--   This module is meant to be imported qualified as "Devel".
module Backend.Devel (frontend, withPactInstances) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Control.Concurrent.Async.Lifted (mapConcurrently_, withAsync)
import           Data.Foldable            (traverse_)
import           Data.Monoid              ((<>))
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Pact.Server.Server       as Pact
import qualified Pact.Types.SPV           as Pact
import           System.Directory         (createDirectoryIfMissing)
import           System.FilePath          ((</>))

import           Obelisk.Frontend         (Frontend)
import           Obelisk.Route            (R)

import           Common.Network
import           Common.Route             (FrontendRoute)
--import           Desktop.Frontend         (desktopFrontend)
import qualified Frontend                 as F

-- required by 'ob run'
frontend :: Frontend (R FrontendRoute)
frontend = devFrontend

devFrontend :: Frontend (R FrontendRoute)
devFrontend = F.frontend --if True then frontend else desktopFrontend

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
withPactInstances :: (MonadIO m, MonadBaseControl IO m) => m () -> m ()
withPactInstances serveIt = do
  liftIO $ do
    traverse_ (createDirectoryIfMissing True . _pic_log) pactConfigs
    createDirectoryIfMissing False $ pactConfigDir
    traverse_ writePactConfig pactConfigs

  let servePact pic = liftIO $ Pact.serve (_pic_conf pic) Pact.noSPVSupport

  withAsync (mapConcurrently_ servePact pactConfigs) $ \_ ->
      serveIt


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
