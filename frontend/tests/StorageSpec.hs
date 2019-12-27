{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module StorageSpec where

import Control.Monad.Free (iterM)
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Dependent.Map as DMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy (Proxy(Proxy))
import Data.IORef (newIORef, readIORef, modifyIORef)
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Frontend.Storage

inMemoryStorage :: IO (StorageInterpreter IO)
inMemoryStorage = do
  localRef <- newIORef (Map.empty :: Map Text Text)
  sessionRef <- newIORef (Map.empty :: Map Text Text)
  let
    chooseRef StoreType_Local = localRef
    chooseRef StoreType_Session = sessionRef

    storage_get :: StoreType -> Text -> IO (Maybe Text)
    storage_get st k = do
      let ref = chooseRef st
      mp <- readIORef ref
      pure $ Map.lookup k mp

    storage_set :: StoreType -> Text -> Text -> IO ()
    storage_set st k v = do
      let ref = chooseRef st
      _ <- modifyIORef ref (Map.insert k v)
      pure ()

    storage_remove :: StoreType -> Text -> IO ()
    storage_remove st k = do
      let ref = chooseRef st
      _ <- liftIO $ modifyIORef ref (Map.delete k)
      pure ()

    interpreter = iterM $ \case
      StorageF_Get storeType key next -> do
        res <- storage_get storeType key
        next res
      StorageF_Set storeType key data' next -> do
        storage_set storeType key data'
        next
      StorageF_Remove storeType key next -> do
        storage_remove storeType key
        next

  pure $ StorageInterpreter interpreter

data StorageTestKey a where
  StorageString :: StorageTestKey String
  StorageInt :: StorageTestKey Int

deriving instance Show (StorageTestKey a)

test_inMemoryStorage :: TestTree
test_inMemoryStorage = testCase "In Memory Storage" $ do
  s <- inMemoryStorage
  flip runStorageT s $ do
    mInt1 <- runStorageIO $ getItemStorage localStorage StorageInt
    liftIO $ mInt1 @?= Nothing
    runStorageIO $ setItemStorage localStorage StorageInt 2
    mInt2 <- runStorageIO $ getItemStorage localStorage StorageInt
    liftIO $ mInt2 @?= (Just 2)
    runStorageIO $ setItemStorage localStorage StorageString "tester"
    mStr1 <- runStorageIO $ getItemStorage localStorage StorageString
    liftIO $ mStr1 @?= (Just "tester")
    runStorageIO $ removeItemStorage localStorage StorageInt
    mInt3 <- runStorageIO $ getItemStorage localStorage StorageInt
    liftIO $ mInt3 @?= Nothing
    mStr2 <- runStorageIO $ getItemStorage localStorage StorageString
    liftIO $ mStr2 @?= (Just "tester")

test_allEntries :: TestTree
test_allEntries = testCase "All Entries" $ do
  s <- inMemoryStorage
  (localRes, sessionRes) <- flip runStorageT s $ runStorageIO $ do
    setItemStorage localStorage StorageInt 42
    setItemStorage localStorage StorageString "This is a string"
    --local <- allEntries localStorage (Proxy @StorageTestKey)
    --session <- allEntries sessionStorage (Proxy @StorageTestKey)
    --pure (local, session)
    pure ((), ())
  --localRes @?= DMap.empty
  --sessionRes @?= DMap.empty
  pure ()


tests :: TestTree
tests = testGroup "StorageSpec"
  [ test_inMemoryStorage
  ]
