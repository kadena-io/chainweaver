import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified StorageSpec as Storage
import qualified KadenaAddressSpec as KadenaAddress

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Frontend Tests"
  [ Storage.tests
  , KadenaAddress.tests
  ]
