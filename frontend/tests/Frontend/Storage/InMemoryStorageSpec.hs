module Frontend.Storage.InMemoryStorageSpec where

import Control.Monad.IO.Class (liftIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Frontend.Storage
import Frontend.StoreTestKey
import Frontend.Storage.InMemoryStorage

test_inMemoryStorage :: TestTree
test_inMemoryStorage = testCase "In Memory Storage" $ do
  (s,_,_) <- inMemoryStorage
  flip runStorageT s $ do
    mInt1 <- runStorageIO $ getItemStorage localStorage StoreInt
    liftIO $ mInt1 @?= Nothing
    runStorageIO $ setItemStorage localStorage StoreInt 2
    mInt2 <- runStorageIO $ getItemStorage localStorage StoreInt
    liftIO $ mInt2 @?= (Just 2)
    runStorageIO $ setItemStorage localStorage StoreString "tester"
    mStr1 <- runStorageIO $ getItemStorage localStorage StoreString
    liftIO $ mStr1 @?= (Just "tester")
    runStorageIO $ removeItemStorage localStorage StoreInt
    mInt3 <- runStorageIO $ getItemStorage localStorage StoreInt
    liftIO $ mInt3 @?= Nothing
    mStr2 <- runStorageIO $ getItemStorage localStorage StoreString
    liftIO $ mStr2 @?= (Just "tester")


tests :: TestTree
tests = testGroup "InMemoryStorageSpec"
  [ test_inMemoryStorage
  ]
