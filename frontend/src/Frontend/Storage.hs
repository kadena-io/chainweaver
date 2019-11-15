{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Storage
  ( localStorage
  , sessionStorage
  , getItemStorage
  , setItemStorage
  , removeItemStorage
  , Storage (..)
  , StoreType (..)
  , HasStorage(..)
  , StorageT(..)
  , runStorageT
  , browserStorage
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad (PrimState, primitive))
import Control.Monad.Reader
import Control.Monad.Ref (MonadRef, MonadAtomicRef)
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Frontend.Foundation
import GHCJS.DOM.Types (JSString, fromJSString, toJSString)
import Language.Javascript.JSaddle (JSM, MonadJSM, liftJSM)
import Obelisk.Route.Frontend
import Reflex.Dom hiding (fromJSString)
import Reflex.Host.Class (MonadReflexCreateTrigger)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Storage as GHCJS
import qualified GHCJS.DOM.Window as Window

data StoreType
  = StoreType_Local
  | StoreType_Session
  deriving (Eq, Show)

-- | Local/session storage manager. On mac, this should store the data _outside_
-- the browser storage, such that data is persisted.
data Storage = Storage
  { _storage_get
    :: forall k a. (Show (k a), FromJSON a)
    => StoreType -> k a -> JSM (Maybe a)
  -- ^ Get an item using the key `k a`
  , _storage_set
    :: forall k a. (Show (k a), ToJSON a)
    => StoreType -> k a -> a -> JSM ()
  -- ^ Set the item at key `k a` to the value `a`
  , _storage_remove
    :: forall k a. Show (k a)
    => StoreType -> k a -> JSM ()
  -- ^ Remove an item using the key `k a`
  }

getItemStorage
  :: (MonadJSM m, HasStorage m, Show (k a), Aeson.FromJSON a)
  => StoreType -> k a -> m (Maybe a)
getItemStorage s k = liftJSM . (\c -> _storage_get c s k) =<< askStorage

setItemStorage
  :: (MonadJSM m, HasStorage m, Show (k a), Aeson.ToJSON a)
  => StoreType -> k a -> a -> m ()
setItemStorage s k a = liftJSM . (\c -> _storage_set c s k a) =<< askStorage

removeItemStorage
  :: (MonadJSM m, HasStorage m, Show (k a))
  => StoreType -> k a -> m ()
removeItemStorage s k = liftJSM . (\c -> _storage_remove c s k) =<< askStorage

-- | Uses the browser's local/session storage, as appropriate
browserStorage :: Storage
browserStorage = Storage
  { _storage_get = \storeType key -> do
    storage <- getStorage storeType
    (fromJsonString =<< ) <$> GHCJS.getItem storage (keyToString key)
  , _storage_set = \storeType key data' -> do
    storage <- getStorage storeType
    GHCJS.setItem storage (keyToString key) (toJsonString data')
  , _storage_remove = \storeType key -> do
    storage <- getStorage storeType
    GHCJS.removeItem storage (keyToString key)
  }
  where
    getStorage = \case
      StoreType_Local -> Window.getLocalStorage =<< DOM.currentWindowUnchecked
      StoreType_Session -> Window.getSessionStorage =<< DOM.currentWindowUnchecked
    toJsonString :: ToJSON a => a -> JSString
    toJsonString = toJSString . safeDecodeUtf8 . BL.toStrict . Aeson.encode
    fromJsonString :: FromJSON a => JSString -> Maybe a
    fromJsonString = Aeson.decodeStrict . T.encodeUtf8 . fromJSString
    keyToString :: Show a => a -> JSString
    keyToString = toJSString . T.pack . show

-- | Get access to browser's local storage.
localStorage :: StoreType
localStorage = StoreType_Local

-- | Get access to browser's session storage.
sessionStorage :: StoreType
sessionStorage = StoreType_Session

class HasStorage m where
  askStorage :: m Storage
  default askStorage :: (MonadTrans t, Monad n, HasStorage n, m ~ t n) => m Storage
  askStorage = lift askStorage

instance Monad m => HasStorage (StorageT m) where
  askStorage = StorageT ask

instance (HasStorage m, Monad m) => HasStorage (RoutedT t r m)

newtype StorageT m a = StorageT
  { unStorageT :: ReaderT Storage m a
  } deriving
    ( Functor, Applicative, Monad
    , MonadFix, MonadIO, MonadRef, MonadAtomicRef
    , DomBuilder t, NotReady t, MonadHold t, MonadSample t
    , TriggerEvent t, PostBuild t, HasJS x
    , MonadReflexCreateTrigger t, MonadQuery t q, Requester t
    )

instance PerformEvent t m => PerformEvent t (StorageT m) where
  type Performable (StorageT m) = StorageT (Performable m)
  performEvent_ e = do
    s <- askStorage
    lift $ performEvent_ $ flip runStorageT s <$> e
  performEvent e = do
    s <- askStorage
    lift $ performEvent $ flip runStorageT s <$> e

instance PrimMonad m => PrimMonad (StorageT m) where
  type PrimState (StorageT m) = PrimState m
  primitive = lift . primitive

instance MonadReader r m => MonadReader r (StorageT m) where
  ask = lift ask
  local f m = askStorage >>= lift . local f . runStorageT m

instance HasDocument m => HasDocument (StorageT m)
instance HasJSContext m => HasJSContext (StorageT m) where
  type JSContextPhantom (StorageT m) = JSContextPhantom m
  askJSContext = StorageT askJSContext
#if !defined(ghcjs_HOST_OS)
instance MonadJSM m => MonadJSM (StorageT m)
#endif

instance (Monad m, Routed t r m) => Routed t r (StorageT m) where
  askRoute = lift askRoute

instance (Monad m, RouteToUrl r m) => RouteToUrl r (StorageT m) where
  askRouteToUrl = lift askRouteToUrl

instance (Reflex t, Monad m, SetRoute t r m) => SetRoute t r (StorageT m) where
  modifyRoute = lift . modifyRoute

instance EventWriter t w m => EventWriter t w (StorageT m) where
  tellEvent = lift . tellEvent

instance MonadTrans (StorageT) where
  lift = StorageT . lift

instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (StorageT m) where
  runWithReplace a0 a' = StorageT $ runWithReplace (unStorageT a0) (fmapCheap unStorageT a')
  traverseDMapWithKeyWithAdjust f dm0 dm' = StorageT $ traverseDMapWithKeyWithAdjust (coerce . f) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = StorageT $ traverseDMapWithKeyWithAdjustWithMove (coerce . f) dm0 dm'
  traverseIntMapWithKeyWithAdjust f im0 im' = StorageT $ traverseIntMapWithKeyWithAdjust (coerce f) im0 im'

deriving instance DomRenderHook t m => DomRenderHook t (StorageT m)

instance (Prerender js t m, Monad m, Reflex t) => Prerender js t (StorageT m) where
  type Client (StorageT m) = StorageT (Client m)
  prerender a b = StorageT $ prerender (unStorageT a) (unStorageT b)

instance HasConfigs m => HasConfigs (StorageT m) where
  getConfigs = lift getConfigs

runStorageT :: StorageT m a -> Storage -> m a
runStorageT = runReaderT . unStorageT
