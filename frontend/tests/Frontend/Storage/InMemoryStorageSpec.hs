{-# LANGUAGE TypeApplications #-}
module Frontend.Storage.InMemoryStorageSpec where

import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy(Proxy))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import System.FilePath ((</>))

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

test_inMemoryStorageFromTestData :: TestTree
test_inMemoryStorageFromTestDataEmptyDir = testCase "In Memory storage from /var/empty" $ do
  (s,_,_) <- inMemoryStorageFromTestData storeTestKeyMetaPrefix (Proxy @StoreTestKey) 0 "/var/empty/"
  (mInt, mStr) <- flip runStorageT s $ runStorageIO $ do
    mInt <- getItemStorage localStorage StoreInt
    mStr <- getItemStorage localStorage StoreString
    pure (mInt, mStr)

  mStr @?= Nothing
  mInt @?= Nothing

test_inMemoryStorageFromTestData = testCase ("In Memory Storage from " <> testDataPath) $ do
  (s,_,_) <- inMemoryStorageFromTestData storeTestKeyMetaPrefix (Proxy @StoreTestKey) 0 testDataPath
  (mInt, mStr) <- flip runStorageT s $ runStorageIO $ do
    mInt <- getItemStorage localStorage StoreInt
    mStr <- getItemStorage localStorage StoreString
    pure (mInt, mStr)

  mStr @?= (Just "from a file")
  mInt @?= (Just 21)
  where
    testDataPath = "tests" </> "Frontend" </> "Storage" </> "InMemoryStorage.files"

tests :: TestTree
tests = testGroup "InMemoryStorageSpec"
  [ test_inMemoryStorage
  , testGroup "From Files" $
    [ test_inMemoryStorageFromTestDataEmptyDir
    , test_inMemoryStorageFromTestData
    ]
  ]
