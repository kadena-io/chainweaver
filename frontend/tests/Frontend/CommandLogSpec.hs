{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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
import Data.Dependent.Sum (DSum((:=>)))
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

import GHC.TypeLits
import qualified GHC.TypeLits as Lits

import Frontend.Storage
import Frontend.Storage.InMemoryStorage
import qualified Frontend.Storage.InMemoryStorageSpec as InMemoryStorageSpec
import Frontend.VersionedStoreTestKey
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Aeson
import qualified Data.Text as T

import qualified Pact.Server.ApiClient.V0 as V0
import qualified Pact.Server.ApiClient.V1 as V1

import Pact.Server.ApiClient (commandLogCurrentVersion)
import qualified Pact.Server.ApiClient as Latest

import System.FilePath ((</>))
import Data.Time
import qualified Pact.Types.Command as Pact
import qualified Pact.Types.ChainId as Pact
import qualified Pact.Types.Util as Pact

test_commandLog :: (Aeson.FromJSON a, Aeson.ToJSON a, Eq a, Show a) => Int -> a -> TestTree
test_commandLog v cmdLog = testCase ("CommandLog v" <> show v) $ do
  -- Golden test
  bs <- LBS.readFile $ path v
  Aeson.decode bs @?= Just cmdLog
  -- Roundtrip
  Aeson.decode (Aeson.encode cmdLog) @?= Just cmdLog
  where path v = "tests" </> "Frontend" </> "CommandLog.files" </> "v" <> show v

test_commandLogs :: Int -> TestTree
test_commandLogs v = case v of
  0 -> test_commandLog v v0CommandLogDecoded
  1 -> test_commandLog v v1CommandLogDecoded
  n -> error $ "Missing test case for command log version: " <> show n

tests :: TestTree
tests = testGroup "CommandLog" $
  test_commandLogs <$> [0..commandLogCurrentVersion]

v0CommandLogDecoded :: V0.CommandLog
v0CommandLogDecoded = V0.CommandLog
  { V0._commandLog_command = Pact.Command
    { Pact._cmdPayload = "payload"
    , Pact._cmdSigs = [Pact.UserSig "5fd6b3e2836d6dd84114b5948898e6ed63d832c618d01c21e32335e75d4623e5352a63daad37bd1e978ee453255469937ec607e46b97e0364c40a30e0c3dd90a"]
    , Pact._cmdHash = either error id $ Pact.fromText' "n1bWvpxcplbqw3IsaL34bmqvFO1F0iy0FcEcjj8adhA"
    }
  , V0._commandLog_timestamp = UTCTime (fromGregorian 2020 01 24) 47976.199626747
  , V0._commandLog_url = "https://eu1.testnet.chainweb.com/chainweb/0.0/testnet04/chain/0/pact"
  }

v1CommandLogDecoded :: V1.CommandLog
v1CommandLogDecoded = V1.CommandLog
  { V1._commandLog_timestamp = UTCTime (fromGregorian 2020 02 14) 44727.58977078
  , V1._commandLog_sender = "dib"
  , V1._commandLog_chain = Pact.ChainId "0"
  , V1._commandLog_requestKey = either error id $ Pact.fromText' "B8H6ZFcoCLAMHTeoKMC5oOi32MG2abRkZfz3EaKuJTA"
  , V1._commandLog_url = "https://eu1.testnet.chainweb.com/chainweb/0.0/testnet04/chain/0/pact"
  , V1._commandLog_command = Pact.Command
    { Pact._cmdPayload = "payload"
    , Pact._cmdSigs = [Pact.UserSig "6b5eef89c3c930f2e555aa468e7fea423ca42a29dc7e30dad11c42911ddab24bda4b13b03b7c6342efa2a0c91de46d8be016785b410a2688e17430a045855a0b"]
    , Pact._cmdHash = either error id $ Pact.fromText' "B8H6ZFcoCLAMHTeoKMC5oOi32MG2abRkZfz3EaKuJTA"
    }
  }
