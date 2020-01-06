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
import qualified Frontend.Storage.InMemoryStorageSpec as InMemoryStorageSpec
import Frontend.StoreTestKey

test_dumpStorage :: TestTree
test_dumpStorage = testCase "Dump Storage" $ do
  ims <- newInMemoryStorage
  localRes <- flip runInMemoryStorage ims $ do
    setItemStorage localStorage StoreInt 42
    setItemStorage localStorage StoreString "This is a string"
    dumpLocalStorage @StoreTestKey
  localRes @?= DMap.fromList [ StoreInt :=> Identity 42, StoreString :=> Identity "This is a string" ]
  pure ()

test_backupLocalStorage :: TestTree
test_backupLocalStorage = testCase "Backup Storage" $ do
  ims@(localRef,_) <- newInMemoryStorage
  r1 <- flip runInMemoryStorage ims $ do
    setItemStorage localStorage StoreInt 42
    setItemStorage localStorage StoreString "This is a string"
    backupLocalStorage storeTestKeyMetaPrefix (Proxy @StoreTestKey) 0
  mSeq1 <- lookupRef localRef (storeTestKeyMetaPrefixText <> "_Backups_V0_Latest")
  isJust r1 @?= True
  mSeq1 @?= Just "0"
  r2 <- flip runInMemoryStorage ims $ do
    setItemStorage localStorage StoreString "Less String"
    backupLocalStorage storeTestKeyMetaPrefix (Proxy @StoreTestKey) 0
  mSeq2 <- lookupRef localRef (storeTestKeyMetaPrefixText <> "_Backups_V0_Latest")
  mSeq2 @?= Just "1"
  mBackupText1 <- lookupRef localRef (storeTestKeyMetaPrefixText <> "_Backups_V0_0")
  mBackupText2 <- lookupRef localRef (storeTestKeyMetaPrefixText <> "_Backups_V0_1")
  mBackupText1 @?= Just "[[[\"StoreString\",[]],\"This is a string\"],[[\"StoreInt\",[]],42]]"
  mBackupText2 @?= Just "[[[\"StoreString\",[]],\"Less String\"],[[\"StoreInt\",[]],42]]"
  pure ()

test_restoreLocalStorage :: TestTree
test_restoreLocalStorage = testCase "Restore Storage" $ do
  let backupText = "[[[\"StoreString\",[]],\"Restored\"],[[\"StoreInt\",[]],1337]]"
  ims@(localRef,_) <- newInMemoryStorage
  insertRef localRef (storeTestKeyMetaPrefixText <> "_Backups_V0_0") backupText
  (s,i) <- flip runInMemoryStorage ims $ do
    restoreLocalStorage storeTestKeyMetaPrefix (Proxy @StoreTestKey) 0 0
    s <- getItemStorage localStorage StoreString
    i <- getItemStorage localStorage StoreInt
    pure (s,i)
  s @?= Just "Restored"
  i @?= Just 1337

test_getVersion :: TestTree
test_getVersion = testGroup "Get Version"
  [ testCase "No Version" $ do
    ims <- newInMemoryStorage
    v <- flip runInMemoryStorage ims $ do
      getCurrentVersion storeTestKeyMetaPrefix
    v @?= 0
  , testCase "Existing Version" $ do
    ims@(localRef,_) <- newInMemoryStorage
    insertRef localRef (storeTestKeyMetaPrefixText <> "_Version") "1"
    v <- flip runInMemoryStorage ims $ do
      getCurrentVersion storeTestKeyMetaPrefix
    v @?= 1
  ]

tests :: TestTree
tests = testGroup "StorageSpec"
  [ InMemoryStorageSpec.tests
  , test_dumpStorage
  , test_backupLocalStorage
  , test_restoreLocalStorage
  , test_getVersion
  ]
