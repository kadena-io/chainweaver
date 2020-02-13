{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend.CommandLogSpec where

import Data.ByteString.Lazy (ByteString)
import Data.Dependent.Map (DSum((:=>)))
import qualified Data.Dependent.Map as DMap
import Data.Functor (void)
import Data.Functor.Identity (Identity(Identity))
import Data.Maybe (isJust)
import Data.Proxy (Proxy(Proxy))
import Data.Some (Some)
import Data.Text (Text)
import Data.Universe (Finite)
import Control.Monad.IO.Class (liftIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Frontend.Storage
import Frontend.Storage.InMemoryStorage
import qualified Frontend.Storage.InMemoryStorageSpec as InMemoryStorageSpec
import Frontend.VersionedStoreTestKey
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Aeson
import qualified Data.Text as T

import Pact.Server.ApiV1Client
import System.FilePath ((</>))
import Data.Time
import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Util as Pact

test_commandLog :: Int -> TestTree
test_commandLog v = testCase ("CommandLog v" <> show v) $ do
  cmdLog <- test_commandLogs v
  -- Golden test
  bs <- LBS.readFile $ path v
  Aeson.decode bs @?= Just cmdLog
  -- Roundtrip
  Aeson.decode (Aeson.encode cmdLog) @?= Just cmdLog
  where path v = "tests" </> "Frontend" </> "CommandLog.files" </> "v" <> show v

test_commandLogs :: Monad m => Int -> m CommandLog
test_commandLogs = \case
  0 -> pure $ CommandLog
    { _commandLog_command = Pact.Command
      { Pact._cmdPayload = "payload"
      , Pact._cmdSigs = [Pact.UserSig "5fd6b3e2836d6dd84114b5948898e6ed63d832c618d01c21e32335e75d4623e5352a63daad37bd1e978ee453255469937ec607e46b97e0364c40a30e0c3dd90a"]
      , Pact._cmdHash = either error id $ Pact.fromText' "n1bWvpxcplbqw3IsaL34bmqvFO1F0iy0FcEcjj8adhA"
      }
    , _commandLog_timestamp = UTCTime (fromGregorian 2020 01 24) 47976.199626747
    , _commandLog_url = "https://eu1.testnet.chainweb.com/chainweb/0.0/testnet04/chain/0/pact"
    }
  n -> fail $ "Missing test case for command log version: " <> show n

tests :: TestTree
tests = do
  testGroup "CommandLog" $ test_commandLog <$> [0..commandLogCurrentVersion]
