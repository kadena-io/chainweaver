{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Storage
  ( localStorage
  , sessionStorage
  , getItemStorage
  , setItemStorage
  , removeItemStorage
  , dumpLocalStorage
  , backupLocalStorage
  , restoreLocalStorage
  , restoreLocalStorageDump
  , getCurrentVersion
  , StoreType (..)
  , HasStorage(..)
  , StorageT(..)
  , runStorageT
  , runStorage
  , runStorageJSM
  , runStorageIO
  , browserStorageIntepreter
  -- Versioning Stuff
  , StorageVersioner(..)
  , StoreKeyMetaPrefix(..)
  , VersioningError(..)
  , StorageVersion
  , StorageUniverse(..)
  -- Consider moving to a .Internal
  , Storage
  , StorageF(..)
  , StorageInterpreter(StorageInterpreter)
  ) where

import Control.Monad.Free (MonadFree, Free, iterM, liftF)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad (PrimState, primitive))
import Control.Monad.Reader
import Control.Monad.Ref (MonadRef, MonadAtomicRef)
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Constraint.Forall (ForallF)
import Data.Constraint.Extras (Has, Has', has)
import Data.Dependent.Map (DMap, DSum((:=>)))
import Data.Dependent.Sum.Orphans ()
import Data.Functor.Identity (Identity(Identity))
import Data.GADT.Compare (GCompare)
import Data.GADT.Show (GShow,gshow)
import qualified Data.Dependent.Map as DMap
import Data.Proxy (Proxy)
import Data.Some (Some(Some))
import Data.Universe.Some (UniverseSome, universeSome)
import Numeric.Natural (Natural)
import Frontend.Foundation
import GHCJS.DOM.Types (fromJSString, toJSString)
import Language.Javascript.JSaddle (JSM, MonadJSM, liftJSM)
import Obelisk.Route.Frontend
import Reflex.Dom hiding (fromJSString)
import Reflex.Host.Class (MonadReflexCreateTrigger)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Storage as GHCJS
import qualified GHCJS.DOM.Window as Window

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

data StorageF next
  = StorageF_Get StoreType Text (Maybe Text -> next)
  | StorageF_Set StoreType Text Text next
  | StorageF_Remove StoreType Text next
  deriving Functor

type Storage = Free StorageF

getItemStorage
  :: (MonadFree StorageF m, GShow k, Aeson.FromJSON a)
  => StoreType -> k a -> m (Maybe a)
getItemStorage st k = getItemStorage' st (keyToText k)

getItemStorage' :: (MonadFree StorageF m, Aeson.FromJSON a) => StoreType -> Text -> m (Maybe a)
getItemStorage' st kt = liftF (StorageF_Get st kt (Aeson.decodeStrict . T.encodeUtf8 =<<))

setItemStorage
  :: (MonadFree StorageF m, GShow k, Aeson.ToJSON a)
  => StoreType -> k a -> a -> m ()
setItemStorage st k = setItemStorage' st (keyToText k)

setItemStorage'
  :: (MonadFree StorageF m, Aeson.ToJSON a)
  => StoreType -> Text -> a -> m ()
setItemStorage' st kt a = liftF (StorageF_Set st kt (T.decodeUtf8 . BL.toStrict . Aeson.encode $ a) ())

removeItemStorage
  :: (MonadFree StorageF m, GShow k)
  => StoreType -> k a -> m ()
removeItemStorage s k = liftF (StorageF_Remove s (keyToText k) ())

currentVersionKeyText :: StoreKeyMetaPrefix -> Text
currentVersionKeyText (StoreKeyMetaPrefix p) = (p <> "_version")

backupKeyPrefixText :: StoreKeyMetaPrefix -> Natural -> Text
backupKeyPrefixText (StoreKeyMetaPrefix p) ver = (p <> "_backups_v" <> tshow ver)

latestBackupSequenceKeyText :: StoreKeyMetaPrefix -> Natural -> Text
latestBackupSequenceKeyText p ver = (backupKeyPrefixText p ver) <> "_latest"

backupKeyText :: StoreKeyMetaPrefix -> Natural -> Natural -> Text
backupKeyText p ver seqNo = (backupKeyPrefixText p ver) <> "_" <> tshow seqNo

getCurrentVersion
  :: (MonadFree StorageF m)
  => StoreKeyMetaPrefix
  -> m Natural
getCurrentVersion p = fromMaybe 0 <$> getItemStorage' localStorage (currentVersionKeyText p)

getLatestBackupSequence
  :: (MonadFree StorageF m)
  => StoreKeyMetaPrefix
  -> Natural
  -> m (Maybe Natural)
getLatestBackupSequence p ver = getItemStorage' localStorage (latestBackupSequenceKeyText p ver)

setLatestBackupSequence
  :: (MonadFree StorageF m)
  => StoreKeyMetaPrefix
  -> Natural
  -> Natural
  -> m ()
setLatestBackupSequence p ver = setItemStorage' localStorage (latestBackupSequenceKeyText p ver)

getBackup
  :: forall storeKeys m
  .  ( MonadFree StorageF m
     , FromJSON (Some storeKeys)
     , Has' FromJSON storeKeys Identity
     , GCompare storeKeys
     )
  => StoreKeyMetaPrefix
  -> Natural
  -> Natural
  -> m (Maybe (DMap storeKeys Identity))
getBackup p ver seqNo = getItemStorage' localStorage (backupKeyText p ver seqNo)

setBackup
  :: ( MonadFree StorageF m
     , ForallF ToJSON storeKeys
     , Has' ToJSON storeKeys f
     )
  => StoreKeyMetaPrefix
  -> Natural
  -> Natural
  -> DMap storeKeys f
  -> m ()
setBackup p ver seqNo dump = setItemStorage' localStorage (backupKeyText p ver seqNo) dump

backupLocalStorage
  :: forall storeKeys m
  . ( MonadFree StorageF m
    , GCompare storeKeys
    , UniverseSome storeKeys
    , ForallF ToJSON storeKeys
    , Has' ToJSON storeKeys Identity
    , Has FromJSON storeKeys
    , Has' FromJSON storeKeys Identity
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
  . ( MonadFree StorageF m
    , GCompare storeKeys
    , UniverseSome storeKeys
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
    Just dump -> restoreLocalStorageDump dump *> pure True

dumpLocalStorage
  :: forall storeKeys m
  . ( MonadFree StorageF m
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
  . ( MonadFree StorageF m
    , GCompare storeKeys
    , Has ToJSON storeKeys
    , GShow storeKeys
    )
  => DMap storeKeys Identity
  -> m ()
restoreLocalStorageDump dump = for_ (DMap.toList dump) setSum
  where
    setSum :: DSum storeKeys Identity -> m ()
    setSum (k :=> ( Identity v )) =
      has @ToJSON k $ setItemStorage localStorage k v

keyToText :: (GShow k) => k a -> Text
keyToText = T.pack . gshow

type StorageVersion = Natural
data VersioningError
  = VersioningError_UnknownVersion StorageVersion


data StorageVersioner ( k :: * -> * ) = StorageVersioner
  { storageVersioner_upgrade :: Storage (Maybe VersioningError)
  -- It's entirely possible that a simpler just "copy the directory" or copy "all the storage keys" is
  -- a better way here, but lets explore this route and see what falls out for export / import
  , storageVersioner_backupVersion :: Storage (Maybe VersioningError)
  }

class StorageUniverse k where
  loadEntireDatabase :: Storage (DMap k Identity)

-- | Uses the browser's local/session storage, as appropriate
browserStorageIntepreter :: StorageInterpreter JSM
browserStorageIntepreter = StorageInterpreter $ iterM $ \case
  StorageF_Get storeType key next -> do
    storage <- getStorage storeType
    res <- fmap fromJSString <$> GHCJS.getItem storage (toJSString key)
    next res
  StorageF_Set storeType key data' next -> do
    storage <- getStorage storeType
    GHCJS.setItem storage (toJSString key) (toJSString data')
    next
  StorageF_Remove storeType key next -> do
    storage <- getStorage storeType
    GHCJS.removeItem storage (toJSString key)
    next
  where
    getStorage = \case
      StoreType_Local -> Window.getLocalStorage =<< DOM.currentWindowUnchecked
      StoreType_Session -> Window.getSessionStorage =<< DOM.currentWindowUnchecked

-- | Get access to browser's local storage.
localStorage :: StoreType
localStorage = StoreType_Local

-- | Get access to browser's session storage.
sessionStorage :: StoreType
sessionStorage = StoreType_Session

newtype StorageInterpreter m = StorageInterpreter { unStorageInterpreter :: forall a. Storage a -> m a }

class HasStorage m where
  type StorageM m :: * -> *
  askStorageInterpreter  :: m (StorageInterpreter (StorageM m))

runStorage
  :: (Monad m, HasStorage m)
  => (forall b. StorageM m b -> m b)
  -> Storage a
  -> m a
runStorage mmorph prog = askStorageInterpreter >>= mmorph . ($ prog) . unStorageInterpreter

runStorageJSM :: (MonadJSM m, HasStorage m, StorageM m ~ JSM) => Storage a -> m a
runStorageJSM = runStorage liftJSM

runStorageIO :: (MonadIO m, HasStorage m, StorageM m ~ IO) => Storage a -> m a
runStorageIO = runStorage liftIO

instance Monad m => HasStorage (StorageT n m) where
  type StorageM (StorageT n m) = n
  askStorageInterpreter = StorageT ask

instance (HasStorage m, Monad m) => HasStorage (RoutedT t r m) where
  type StorageM (RoutedT t r m) = StorageM m
  askStorageInterpreter = lift askStorageInterpreter


newtype StorageT n m a = StorageT
  { unStorageT :: ReaderT (StorageInterpreter n) m a
  } deriving
    ( Functor, Applicative, Monad
    , MonadFix, MonadIO, MonadRef, MonadAtomicRef
    , DomBuilder t, NotReady t, MonadHold t, MonadSample t
    , TriggerEvent t, PostBuild t, HasJS x
    , MonadReflexCreateTrigger t, MonadQuery t q, Requester t
    )

instance PerformEvent t m => PerformEvent t (StorageT n m) where
  type Performable (StorageT n m) = StorageT n (Performable m)
  performEvent_ e = do
    s <- askStorageInterpreter
    lift $ performEvent_ $ flip runStorageT s <$> e
  performEvent e = do
    s <- askStorageInterpreter
    lift $ performEvent $ flip runStorageT s <$> e

instance PrimMonad m => PrimMonad (StorageT n m) where
  type PrimState (StorageT n m) = PrimState m
  primitive = lift . primitive

instance MonadReader r m => MonadReader r (StorageT n m) where
  ask = lift ask
  local f m = askStorageInterpreter >>= lift . local f . runStorageT m

instance HasDocument m => HasDocument (StorageT n m)
instance HasJSContext m => HasJSContext (StorageT n m) where
  type JSContextPhantom (StorageT n m) = JSContextPhantom m
  askJSContext = StorageT askJSContext
#if !defined(ghcjs_HOST_OS)
instance MonadJSM m => MonadJSM (StorageT n m)
#endif

instance (Monad m, Routed t r m) => Routed t r (StorageT n m) where
  askRoute = lift askRoute

instance (Monad m, RouteToUrl r m) => RouteToUrl r (StorageT n m) where
  askRouteToUrl = lift askRouteToUrl

instance (Reflex t, Monad m, SetRoute t r m) => SetRoute t r (StorageT n m) where
  modifyRoute = lift . modifyRoute

instance EventWriter t w m => EventWriter t w (StorageT n m) where
  tellEvent = lift . tellEvent

instance MonadTrans (StorageT n) where
  lift = StorageT . lift

instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (StorageT n m) where
  runWithReplace a0 a' = StorageT $ runWithReplace (unStorageT a0) (fmapCheap unStorageT a')
  traverseDMapWithKeyWithAdjust f dm0 dm' = StorageT $ traverseDMapWithKeyWithAdjust (coerce . f) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = StorageT $ traverseDMapWithKeyWithAdjustWithMove (coerce . f) dm0 dm'
  traverseIntMapWithKeyWithAdjust f im0 im' = StorageT $ traverseIntMapWithKeyWithAdjust (coerce f) im0 im'

deriving instance DomRenderHook t m => DomRenderHook t (StorageT n m)

instance (Prerender js t m, Monad m, Reflex t) => Prerender js t (StorageT n m) where
  type Client (StorageT n m) = StorageT n (Client m)
  prerender a b = StorageT $ prerender (unStorageT a) (unStorageT b)

instance HasConfigs m => HasConfigs (StorageT n m) where
  getConfigs = lift getConfigs

runStorageT :: StorageT n m a -> StorageInterpreter n -> m a
runStorageT = runReaderT . unStorageT
