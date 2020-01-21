{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Storage.InMemoryStorage where

import Debug.Trace
import Control.Monad.Free (iterM)
import Control.Monad.Reader
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

type IMS = (IORef (Map Text Text), IORef (Map Text Text))
newtype InMemoryStorage a = InMemoryStorage
  { unInMemoryStorage :: ReaderT IMS IO a
  } deriving (Functor, Applicative, Monad, MonadIO)

chooseRef :: StoreType -> InMemoryStorage (IORef (Map Text Text))
chooseRef st = do
  (localRef, sessionRef) <- InMemoryStorage ask
  pure $ case st of
    StoreType_Local -> localRef
    StoreType_Session -> sessionRef

instance HasStorage InMemoryStorage where
  getItemStorage' st k = do
    ref <- chooseRef st
    InMemoryStorage $ lift $ lookupRef ref k

  setItemStorage' st k v = do
    ref <- chooseRef st
    InMemoryStorage $ lift $ insertRef ref k v

  removeItemStorage' st k = do
    ref <- chooseRef st
    InMemoryStorage $ lift $ removeRef ref k

runInMemoryStorage :: InMemoryStorage a -> IMS -> IO a
runInMemoryStorage (InMemoryStorage r) = runReaderT r

newInMemoryStorage :: IO IMS
newInMemoryStorage = do
  localRef <- newIORef (Map.empty :: Map Text Text)
  sessionRef <- newIORef (Map.empty :: Map Text Text)
  pure (localRef, sessionRef)

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
  -> IO IMS
inMemoryStorageFromTestData p _ ver dirPath = do
  dmap <- keyUniverseToFilesDMap
  ims <- newInMemoryStorage
  _ <- runInMemoryStorage (restoreLocalStorageDump p dmap ver) ims
  pure ims
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
      mbs <- traceShowId <$> keyToByteString k
      pure $ has @FromJSON k $ do
        let decRes = traverse (eitherDecode @a) mbs
        either error (fmap (\v -> (k :=> Identity v))) decRes

    keyUniverseToFilesDMap :: IO (DMap k Identity)
    keyUniverseToFilesDMap = fmap (DMap.fromList . catMaybes)
      . traverse (\(Some k) -> keyToFileDSum k)
      $ universeSome @k

