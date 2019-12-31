{-# LANGUAGE LambdaCase #-}
module Frontend.Storage.InMemoryStorage where

import Control.Monad.Free (iterM)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import Frontend.Storage (StorageInterpreter(..), StorageF(..), StoreType(..))

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

