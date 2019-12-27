{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Storage
  ( localStorage
  , sessionStorage
  , getItemStorage
  , setItemStorage
  , removeItemStorage
  , StoreType (..)
  , HasStorage(..)
  , StorageT(..)
  , runStorageT
  , runStorage
  , runStorageJSM
  , runStorageIO
  , browserStorageIntepreter
  , allEntries
  -- Consider moving to a .Internal
  , Storage
  , StorageF(..)
  , StorageInterpreter(StorageInterpreter)
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad (PrimState, primitive))
import Control.Monad.Reader
import Control.Monad.Ref (MonadRef, MonadAtomicRef)
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity)
import Data.Proxy (Proxy(Proxy))
import Control.Monad.Free (MonadFree, Free, iterM, liftF)
import Data.Dependent.Map (DMap)
import Data.Universe (Finite(..))
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

data StorageF next
  = StorageF_Get StoreType Text (Maybe Text -> next)
  | StorageF_Set StoreType Text Text next
  | StorageF_Remove StoreType Text next
  deriving Functor

type Storage = Free StorageF

getItemStorage
  :: (MonadFree StorageF m, Show (k a), Aeson.FromJSON a)
  => StoreType -> k a -> m (Maybe a)
getItemStorage s k = liftF (StorageF_Get s (keyToString k) (Aeson.decodeStrict . T.encodeUtf8 =<<))

setItemStorage
  :: (MonadFree StorageF m, Show (k a), Aeson.ToJSON a)
  => StoreType -> k a -> a -> m ()
setItemStorage s k a = liftF (StorageF_Set s (keyToString k) (T.decodeUtf8 . BL.toStrict . Aeson.encode $ a) ())

removeItemStorage
  :: (MonadFree StorageF m, Show (k a))
  => StoreType -> k a -> m ()
removeItemStorage s k = liftF (StorageF_Remove s (keyToString k) ())

allEntries
  :: (MonadFree StorageF m, Show (k a), Finite (k a))
  => StoreType
  -> Proxy k
  -> m (DMap k Identity)
allEntries = undefined

keyToString :: Show a => a -> Text
keyToString = T.pack . show

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
