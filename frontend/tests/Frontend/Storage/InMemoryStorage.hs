{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Storage.InMemoryStorage where

import Control.Monad.Free (iterM)
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Data.Bool (bool)
import qualified Data.ByteString.Lazy as LBS
import Data.Constraint.Extras (Has, Has', has)
import Data.Dependent.Map (DMap, DSum(..), Some(Some))
import qualified Data.Dependent.Map as DMap
import Data.Functor.Identity (Identity(..))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Data.Functor (void)
import Data.GADT.Show (GShow, gshow)
import Data.GADT.Compare (GCompare)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Universe.Some (UniverseSome, universeSome)
import Numeric.Natural (Natural)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Frontend.Storage

lookupRef :: IORef (Map Text Text) -> Text -> IO (Maybe Text)
lookupRef ref k = Map.lookup k <$> readIORef ref

insertRef :: IORef (Map Text Text) -> Text -> Text -> IO ()
insertRef ref k v = void $ modifyIORef ref (Map.insert k v)

removeRef :: IORef (Map Text Text) -> Text -> IO ()
removeRef ref k = void $ modifyIORef ref (Map.delete k)

keysRef :: IORef (Map Text Text) -> IO [Text]
keysRef ref = Map.keys <$> readIORef ref

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

-- This function *should* be cool because it ought to allow us to drop in a desktop storage directory
-- into the folder for a given version and then source those files into the inmem store for the tests
--
-- We could use the desktop interpreter, but that's probably a bit weird given it is operating in JSM (even
-- though it doesn't need to for its own needs).
inMemoryStorageFromTestData
  :: forall k
  . ( GShow k
    , GCompare k
    , Has FromJSON k
    , Has' FromJSON k Identity
    , Has ToJSON k
    , FromJSON (Some k)
    , UniverseSome k
    )
  => StoreKeyMetaPrefix
  -> Proxy k
  -> Natural
  -> FilePath
  -> IO (StorageInterpreter IO, IORef (Map Text Text), IORef (Map Text Text))
inMemoryStorageFromTestData p _ ver dirPath = do
  (interpreter, localRef, sessionRef) <- inMemoryStorage
  dmap <- keyUniverseToFilesDMap
  flip runStorageT interpreter $ runStorageIO $ restoreLocalStorageDump p dmap ver
  pure (interpreter, localRef, sessionRef)
  where
    keyToPath :: k a -> FilePath
    keyToPath k = dirPath </> gshow k

    keyToByteString :: k a -> IO (Maybe LBS.ByteString)
    keyToByteString k = do
      let kp = keyToPath k
      exists <- doesFileExist kp
      bool (pure Nothing) (fmap Just (LBS.readFile $ keyToPath k)) exists

    keyToFileDSum :: forall a. k a -> IO (Maybe (DSum k Identity))
    keyToFileDSum k = do
      mbs <- keyToByteString k
      pure $ has @FromJSON k $ do
        let decRes = traverse (eitherDecode @a) mbs
        either error (fmap (\v -> (k :=> Identity v))) decRes

    keyUniverseToFilesDMap :: IO (DMap k Identity)
    keyUniverseToFilesDMap = fmap (DMap.fromList . catMaybes)
      . traverse (\(Some k) -> keyToFileDSum k)
      $ universeSome @k

