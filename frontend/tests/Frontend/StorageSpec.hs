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
module Frontend.StorageSpec where

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
import Frontend.StoreTestKey

test_dumpStorage :: TestTree
test_dumpStorage = testCase "Dump Storage" $ do
  (s,_,_) <- inMemoryStorage
  localRes <- flip runStorageT s $ runStorageIO $ do
    setItemStorage localStorage StoreInt 42
    setItemStorage localStorage StoreString "This is a string"
    dumpLocalStorage @StoreTestKey
  localRes @?= DMap.fromList [ StoreInt :=> Identity 42, StoreString :=> Identity "This is a string" ]
  pure ()

storeKeyPrefixText :: Text
storeKeyPrefixText = "StoreTestMeta"

storeKeyPrefix :: StoreKeyMetaPrefix
storeKeyPrefix = StoreKeyMetaPrefix storeKeyPrefixText

test_backupLocalStorage :: TestTree
test_backupLocalStorage = testCase "Backup Storage" $ do
  (s,localRef,_) <- inMemoryStorage
  r1 <- flip runStorageT s $ runStorageIO $ do
    setItemStorage localStorage StoreInt 42
    setItemStorage localStorage StoreString "This is a string"
    backupLocalStorage storeKeyPrefix (Proxy @StoreTestKey) 0
  mSeq1 <- lookupRef localRef (storeKeyPrefixText <> "_backups_v0_latest")
  isJust r1 @?= True
  mSeq1 @?= Just "0"
  r2 <- flip runStorageT s $ runStorageIO $ do
    setItemStorage localStorage StoreString "Less String"
    backupLocalStorage storeKeyPrefix (Proxy @StoreTestKey) 0
  mSeq2 <- lookupRef localRef (storeKeyPrefixText <> "_backups_v0_latest")
  mSeq2 @?= Just "1"
  mBackupText1 <- lookupRef localRef (storeKeyPrefixText <> "_backups_v0_0")
  mBackupText2 <- lookupRef localRef (storeKeyPrefixText <> "_backups_v0_1")
  mBackupText1 @?= Just "[[[\"StoreString\",[]],\"This is a string\"],[[\"StoreInt\",[]],42]]"
  mBackupText2 @?= Just "[[[\"StoreString\",[]],\"Less String\"],[[\"StoreInt\",[]],42]]"
  pure ()

test_restoreLocalStorage :: TestTree
test_restoreLocalStorage = testCase "Restore Storage" $ do
  let backupText = "[[[\"StoreString\",[]],\"Restored\"],[[\"StoreInt\",[]],1337]]"
  (s,localRef,_) <- inMemoryStorage
  insertRef localRef (storeKeyPrefixText <> "_backups_v0_0") backupText
  (s,i) <- flip runStorageT s $ runStorageIO $ do
    restoreLocalStorage storeKeyPrefix (Proxy @StoreTestKey) 0 0
    s <- getItemStorage localStorage StoreString
    i <- getItemStorage localStorage StoreInt
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
  [ test_dumpStorage
  , test_backupLocalStorage
  , test_restoreLocalStorage
  , test_getVersion
  ]
