import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Frontend.StorageSpec as Storage
import qualified Frontend.Storage.InMemoryStorageSpec as InMemoryStorage
import qualified KadenaAddressSpec as KadenaAddress

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Frontend Tests"
  [ InMemoryStorage.tests
  , Storage.tests
  , KadenaAddress.tests
  ]
