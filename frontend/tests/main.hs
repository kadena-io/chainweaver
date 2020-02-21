module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Frontend.CommandLogSpec as CommandLog
import qualified Frontend.StorageSpec as Storage
import qualified Frontend.VersionedStoreSpec as Store
import qualified Frontend.Network.NodeInfoSpec as NodeInfoSpec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Frontend Tests"
  [ Storage.tests
  , Store.tests
  , NodeInfoSpec.tests
  , CommandLog.tests
  ]
