module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Frontend.StorageSpec as Storage
import qualified Frontend.StoreSpec as Store
import qualified KadenaAddressSpec as KadenaAddress

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Frontend Tests"
  [ Storage.tests
  , Store.tests
  , KadenaAddress.tests
  ]
