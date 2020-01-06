{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Storage.Class
  ( localStorage
  , sessionStorage
  , getItemStorage
  , setItemStorage
  , removeItemStorage
  , removeKeyUniverse
  , dumpLocalStorage
  , backupLocalStorage
  , restoreLocalStorage
  , restoreLocalStorageDump
  , getCurrentVersion
  , StoreType (..)
  , HasStorage(..)
  -- Versioning Stuff
  , StorageVersioner(..)
  , StoreKeyMetaPrefix(..)
  , VersioningError(..)
  , StorageVersion
  ) where

import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON)
import Data.Constraint.Extras (Has, Has', has)
import Data.Constraint.Forall (ForallF)
import Data.Dependent.Map (DMap, DSum((:=>)))
import Data.Dependent.Sum.Orphans ()
import Data.Functor.Identity (Identity(Identity))
import Data.GADT.Compare (GCompare)
import Data.GADT.Show (GShow,gshow)
import Data.Proxy (Proxy)
import Data.Some (Some(Some))
import Data.Text (Text)
import Data.Universe.Some (UniverseSome, universeSome)
import Frontend.Foundation
import Numeric.Natural (Natural)
import Obelisk.Route.Frontend

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Dependent.Map as DMap
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

{-- Notes for migration
 * We can't close off the keys to a single GADT because that would mean that
   desktop and web need to share the same storage keys, which will not work.
   Currently Desktop has the root BIP key on top of the rest of the storage.
 * The backup/restore process for the user is going to be interesting. Because we don't
   want to export the whole store because I don't think that we want to export the
   encrypted keys. It's not quite what we want for the versioning process so we are not
   going to get it "for free" sadly.
 * The current storage type locks things up in JSM which makes it really tricky to test.
   We could run the frontend storage tests in ghcjs but then we'd be stuck not being able to
   test the desktop migrations. We could parameterise Storage with a type parameter for the
   inner monad, but then HasStorage needs to become a MTPC and stuff gets weird for the rest
   of the app. Really, we just want to be able to test the derived functions from the algebra
   and we don't want "testability" stuff bleeding out into the app.
   For this reason, I'm going to change Storage to a Free Monad and HasStorage / StorageT just
   to something that holds an interpreter that can run the free in a given m. Way cleaner.
--}

data StoreType
  = StoreType_Local
  | StoreType_Session
  deriving (Eq, Show)

newtype StoreKeyMetaPrefix = StoreKeyMetaPrefix Text

encodeText :: Aeson.ToJSON a => a -> Text
encodeText = T.decodeUtf8 . BL.toStrict . Aeson.encode

decodeText :: Aeson.FromJSON a => Text -> Maybe a
decodeText = Aeson.decodeStrict . T.encodeUtf8

getItemStorage
  :: (HasStorage m, GShow k, Aeson.FromJSON a, Functor m)
  => StoreType -> k a -> m (Maybe a)
getItemStorage st k = (decodeText =<<) <$> getItemStorage' st (keyToText k)

setItemStorage
  :: (HasStorage m, GShow k, Aeson.ToJSON a)
  => StoreType -> k a -> a -> m ()
setItemStorage st k = setItemStorage' st (keyToText k) . encodeText

removeItemStorage
  :: (HasStorage m, GShow k)
  => StoreType -> k a -> m ()
removeItemStorage s k = removeItemStorage' s (keyToText k)

currentVersionKeyText :: StoreKeyMetaPrefix -> Text
currentVersionKeyText (StoreKeyMetaPrefix p) = (p <> "_Version")

backupKeyPrefixText :: StoreKeyMetaPrefix -> Natural -> Text
backupKeyPrefixText (StoreKeyMetaPrefix p) ver = (p <> "_Backups_V" <> tshow ver)

latestBackupSequenceKeyText :: StoreKeyMetaPrefix -> Natural -> Text
latestBackupSequenceKeyText p ver = (backupKeyPrefixText p ver) <> "_Latest"

backupKeyText :: StoreKeyMetaPrefix -> Natural -> Natural -> Text
backupKeyText p ver seqNo = (backupKeyPrefixText p ver) <> "_" <> tshow seqNo

getCurrentVersion
  :: (HasStorage m, Functor m)
  => StoreKeyMetaPrefix
  -> m Natural
getCurrentVersion p = fromMaybe 0 . (decodeText =<<) <$> getItemStorage' localStorage (currentVersionKeyText p)

setCurrentVersion
  :: HasStorage m
  => StoreKeyMetaPrefix
  -> Natural
  -> m ()
setCurrentVersion p = setItemStorage' localStorage (currentVersionKeyText p) . encodeText

getLatestBackupSequence
  :: (HasStorage m, Functor m)
  => StoreKeyMetaPrefix
  -> Natural
  -> m (Maybe Natural)
getLatestBackupSequence p ver = (decodeText =<<) <$> getItemStorage' localStorage (latestBackupSequenceKeyText p ver)

setLatestBackupSequence
  :: HasStorage m
  => StoreKeyMetaPrefix
  -> Natural
  -> Natural
  -> m ()
setLatestBackupSequence p ver = setItemStorage' localStorage (latestBackupSequenceKeyText p ver) . encodeText

getBackup
  :: forall storeKeys m
  .  ( HasStorage m
     , Functor m
     , FromJSON (Some storeKeys)
     , Has' FromJSON storeKeys Identity
     , GCompare storeKeys
     )
  => StoreKeyMetaPrefix
  -> Natural
  -> Natural
  -> m (Maybe (DMap storeKeys Identity))
getBackup p ver seqNo = (decodeText =<<) <$> getItemStorage' localStorage (backupKeyText p ver seqNo)

setBackup
  :: ( HasStorage m
     , ForallF ToJSON storeKeys
     , Has' ToJSON storeKeys f
     )
  => StoreKeyMetaPrefix
  -> Natural
  -> Natural
  -> DMap storeKeys f
  -> m ()
setBackup p ver seqNo dump = setItemStorage' localStorage (backupKeyText p ver seqNo) $ encodeText dump

backupLocalStorage
  :: forall storeKeys m
  . ( HasStorage m
    , Monad m
    , GCompare storeKeys
    , UniverseSome storeKeys
    , ForallF ToJSON storeKeys
    , Has' ToJSON storeKeys Identity
    , Has FromJSON storeKeys
    , GShow storeKeys
    )
  => StoreKeyMetaPrefix
  -> Proxy storeKeys
  -> Natural  -- This version is the expectation set by the caller who has already chosen the key type
  -> m (Maybe (DMap storeKeys Identity))
backupLocalStorage p _ expectedVer = do
  actualVer <- getCurrentVersion p
  if actualVer /= expectedVer
    then pure Nothing
    else do
      dump <- dumpLocalStorage @storeKeys
      thisSeqNo <- maybe 0 (+1) <$> getLatestBackupSequence p expectedVer
      setBackup p expectedVer thisSeqNo dump
      setLatestBackupSequence p expectedVer thisSeqNo
      pure (Just dump)

restoreLocalStorage
  :: forall storeKeys m
  . ( HasStorage m
    , Monad m
    , GCompare storeKeys
    , Has' FromJSON storeKeys Identity
    , Has ToJSON storeKeys
    , FromJSON (Some storeKeys)
    , GShow storeKeys
    )
  => StoreKeyMetaPrefix
  -> Proxy storeKeys
  -> Natural
  -> Natural
  -> m Bool
restoreLocalStorage p _ ver seqNo = do
  mDump <- getBackup @storeKeys p ver seqNo
  case mDump of
    Nothing -> pure False
    Just dump -> do
      restoreLocalStorageDump p dump ver
      pure True

dumpLocalStorage
  :: forall storeKeys m
  . ( HasStorage m
    , Monad m
    , GCompare storeKeys
    , UniverseSome storeKeys
    , Has FromJSON storeKeys
    , GShow storeKeys
    )
  => m (DMap storeKeys Identity)
dumpLocalStorage = fmap (DMap.fromList . catMaybes)
  . traverse (\(Some k) -> has @FromJSON k $
    (fmap (\v -> k :=> Identity v)) <$> getItemStorage StoreType_Local k
  )
  $ universeSome @storeKeys

restoreLocalStorageDump
  :: forall storeKeys m
  . ( HasStorage m
    , Monad m
    , Has ToJSON storeKeys
    , GShow storeKeys
    )
  => StoreKeyMetaPrefix
  -> DMap storeKeys Identity
  -> Natural
  -> m ()
restoreLocalStorageDump p dump ver = do
  for_ (DMap.toList dump) setSum
  setCurrentVersion p ver
  where
    setSum :: DSum storeKeys Identity -> m ()
    setSum (k :=> ( Identity v )) =
      has @ToJSON k $ setItemStorage localStorage k v

removeKeyUniverse
  :: forall storeKeys m
   . ( HasStorage m
     , Monad m
     , GShow storeKeys
     , UniverseSome storeKeys
     )
  => Proxy storeKeys
  -> StoreType
  -> m ()
removeKeyUniverse _ st =
  traverse_ (\(Some k) -> removeItemStorage st k)
  $ universeSome @storeKeys

keyToText :: (GShow k) => k a -> Text
keyToText = T.pack . gshow

type StorageVersion = Natural
data VersioningError
  = VersioningError_UnknownVersion StorageVersion


data StorageVersioner m ( k :: * -> * ) = StorageVersioner
  { _storageVersioner_metaPrefix :: StoreKeyMetaPrefix
  , _storageVersioner_upgrade :: m (Maybe VersioningError)
  -- It's entirely possible that a simpler just "copy the directory" or copy "all the storage keys" is
  -- a better way here, but lets explore this route and see what falls out for export / import
  , _storageVersioner_backupVersion :: m (Maybe VersioningError)
  }

-- | Get access to browser's local storage.
localStorage :: StoreType
localStorage = StoreType_Local

-- | Get access to browser's session storage.
sessionStorage :: StoreType
sessionStorage = StoreType_Session

class HasStorage m where
  getItemStorage' :: StoreType -> Text -> m (Maybe Text)
  setItemStorage' :: StoreType -> Text -> Text -> m ()
  removeItemStorage' :: StoreType -> Text -> m ()

  default getItemStorage' :: (m ~ t n, MonadTrans t, Monad n, HasStorage n) => StoreType -> Text -> m (Maybe Text)
  getItemStorage' t = lift . getItemStorage' t
  default setItemStorage' :: (m ~ t n, MonadTrans t, Monad n, HasStorage n) => StoreType -> Text -> Text -> m ()
  setItemStorage' t k = lift . setItemStorage' t k
  default removeItemStorage' :: (m ~ t n, MonadTrans t, Monad n, HasStorage n) => StoreType -> Text -> m ()
  removeItemStorage' t = lift . removeItemStorage' t

instance (HasStorage m, Monad m) => HasStorage (RoutedT t r m)
instance (HasStorage m, Monad m) => HasStorage (ReaderT r m)
