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
module StorageSpec where

import Control.Monad.Free (iterM)
import Data.Aeson (ToJSON(..), FromJSON(..), encode, decode)
import Data.Aeson.GADT.TH
import Data.ByteString.Lazy (ByteString)
import Data.Constraint.Extras.TH
import Data.Dependent.Map (DSum((:=>)))
import qualified Data.Dependent.Map as DMap
import Data.Functor (void)
import Data.Functor.Identity (Identity(Identity))
import Data.Map (Map)
import Data.GADT.Show.TH
import Data.GADT.Compare.TH
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import qualified Data.Map as Map
import Data.Proxy (Proxy(Proxy))
import Data.Some (Some)
import Data.Text (Text)
import Data.Universe.Some.TH
import Data.Universe (Finite)
import Control.Monad.IO.Class (liftIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Frontend.Storage

lookupRef :: IORef (Map Text Text) -> Text -> IO (Maybe Text)
lookupRef ref k = Map.lookup k <$> readIORef ref

insertRef :: IORef (Map Text Text) -> Text -> Text -> IO ()
insertRef ref k v = void $ modifyIORef ref (Map.insert k v)

removeRef :: IORef (Map Text Text) -> Text -> IO ()
removeRef ref k = void $ modifyIORef ref (Map.delete k)

inMemoryStorage :: IO (StorageInterpreter IO, IORef (Map Text Text), IORef (Map Text Text))
inMemoryStorage = do
  localRef <- newIORef (Map.empty :: Map Text Text)
  sessionRef <- newIORef (Map.empty :: Map Text Text)
  let
    chooseRef StoreType_Local = localRef
    chooseRef StoreType_Session = sessionRef

    storage_get :: StoreType -> Text -> IO (Maybe Text)
    storage_get st k = do
      let ref = chooseRef st
      lookupRef ref k

    storage_set :: StoreType -> Text -> Text -> IO ()
    storage_set st k v = do
      let ref = chooseRef st
      insertRef ref k v

    storage_remove :: StoreType -> Text -> IO ()
    storage_remove st k = do
      let ref = chooseRef st
      removeRef ref k

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

  pure $ (StorageInterpreter interpreter, localRef, sessionRef)

data StorageTestKey a where
  StorageString :: StorageTestKey String
  StorageInt :: StorageTestKey Int

deriving instance Show (StorageTestKey a)

concat <$> traverse ($ ''StorageTestKey)
  [ deriveGShow
  , deriveGEq
  , deriveGCompare
  , deriveUniverseSome
  , deriveArgDict
  , deriveJSONGADT
  ]

test_inMemoryStorage :: TestTree
test_inMemoryStorage = testCase "In Memory Storage" $ do
  (s,_,_) <- inMemoryStorage
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

test_dumpStorage :: TestTree
test_dumpStorage = testCase "Dump Storage" $ do
  (s,_,_) <- inMemoryStorage
  localRes <- flip runStorageT s $ runStorageIO $ do
    setItemStorage localStorage StorageInt 42
    setItemStorage localStorage StorageString "This is a string"
    dumpLocalStorage @StorageTestKey
  localRes @?= DMap.fromList [ StorageInt :=> Identity 42, StorageString :=> Identity "This is a string" ]
  pure ()

storeKeyPrefixText :: Text
storeKeyPrefixText = "StorageTestMeta"

storeKeyPrefix :: StoreKeyMetaPrefix
storeKeyPrefix = StoreKeyMetaPrefix storeKeyPrefixText


test_backupLocalStorage :: TestTree
test_backupLocalStorage = testCase "Backup Storage" $ do
  (s,localRef,_) <- inMemoryStorage
  r1 <- flip runStorageT s $ runStorageIO $ do
    setItemStorage localStorage StorageInt 42
    setItemStorage localStorage StorageString "This is a string"
    backupLocalStorage storeKeyPrefix (Proxy @StorageTestKey) 0
  mSeq1 <- lookupRef localRef (storeKeyPrefixText <> "_backups_v0_latest")
  r1 @?= True
  mSeq1 @?= Just "0"
  r2 <- flip runStorageT s $ runStorageIO $ do
    setItemStorage localStorage StorageString "Less String"
    backupLocalStorage storeKeyPrefix (Proxy @StorageTestKey) 0
  mSeq2 <- lookupRef localRef (storeKeyPrefixText <> "_backups_v0_latest")
  r2 @?= True
  mSeq2 @?= Just "1"
  mBackupText1 <- lookupRef localRef (storeKeyPrefixText <> "_backups_v0_0")
  mBackupText2 <- lookupRef localRef (storeKeyPrefixText <> "_backups_v0_1")
  mBackupText1 @?= Just "[[[\"StorageString\",[]],\"This is a string\"],[[\"StorageInt\",[]],42]]"
  mBackupText2 @?= Just "[[[\"StorageString\",[]],\"Less String\"],[[\"StorageInt\",[]],42]]"
  pure ()

test_restoreLocalStorage :: TestTree
test_restoreLocalStorage = testCase "Restore Storage" $ do
  let backupText = "[[[\"StorageString\",[]],\"Restored\"],[[\"StorageInt\",[]],1337]]"
  (s,localRef,_) <- inMemoryStorage
  insertRef localRef (storeKeyPrefixText <> "_backups_v0_0") backupText
  (s,i) <- flip runStorageT s $ runStorageIO $ do
    restoreLocalStorage storeKeyPrefix (Proxy @StorageTestKey) 0 0
    s <- getItemStorage localStorage StorageString
    i <- getItemStorage localStorage StorageInt
    pure (s,i)
  s @?= Just "Restored"
  i @?= Just 1337

test_getVersion :: TestTree
test_getVersion = testGroup "Get Version"
  [ testCase "No Version" $ do
    (s,_,_) <- inMemoryStorage
    v <- flip runStorageT s $ runStorageIO $ do
      getCurrentVersion storeKeyPrefix
    v @?= 0
  , testCase "Existing Version" $ do
    (s,localRef,_) <- inMemoryStorage
    insertRef localRef (storeKeyPrefixText <> "_version") "1"
    v <- flip runStorageT s $ runStorageIO $ do
      getCurrentVersion storeKeyPrefix
    v @?= 1
  ]

tests :: TestTree
tests = testGroup "StorageSpec"
  [ test_inMemoryStorage
  , test_dumpStorage
  , test_backupLocalStorage
  , test_restoreLocalStorage
  , test_getVersion
  ]
