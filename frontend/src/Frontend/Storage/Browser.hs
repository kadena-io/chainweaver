{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Storage.Browser
  ( BrowserStorageT(..)
  , runBrowserStorageT
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad (PrimState, primitive))
import Control.Monad.Reader
import Control.Monad.Ref (MonadRef, MonadAtomicRef)
import Data.Coerce (coerce)
import Frontend.Foundation
import Frontend.Storage.Class
import GHCJS.DOM.Types (fromJSString, toJSString)
import Language.Javascript.JSaddle (MonadJSM)
import Obelisk.Route.Frontend
import Pact.Server.ApiV1Client (HasTransactionLogger)
import Reflex.Dom hiding (fromJSString)
import Reflex.Host.Class (MonadReflexCreateTrigger)

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Storage as GHCJS
import qualified GHCJS.DOM.Window as Window

-- | Uses the browser's local/session storage, as appropriate
instance MonadJSM m => HasStorage (BrowserStorageT m) where
  getItemStorage' storeType key = liftJSM $ do
    storage <- getStorageJSM storeType
    fmap fromJSString <$> GHCJS.getItem storage (toJSString key)
  setItemStorage' storeType key data' = liftJSM $ do
    storage <- getStorageJSM storeType
    GHCJS.setItem storage (toJSString key) (toJSString data')
  removeItemStorage' storeType key = liftJSM $ do
    storage <- getStorageJSM storeType
    GHCJS.removeItem storage (toJSString key)

getStorageJSM :: StoreType -> JSM GHCJS.Storage
getStorageJSM = \case
  StoreType_Local -> Window.getLocalStorage =<< DOM.currentWindowUnchecked
  StoreType_Session -> Window.getSessionStorage =<< DOM.currentWindowUnchecked

newtype BrowserStorageT m a = BrowserStorageT
  { unBrowserStorageT :: m a
  } deriving
    ( Functor, Applicative, Monad
    , MonadFix, MonadIO, MonadRef, MonadAtomicRef
    , DomBuilder t, NotReady t, MonadHold t, MonadSample t
    , TriggerEvent t, PostBuild t, HasJS x
    , MonadReflexCreateTrigger t, MonadQuery t q, Requester t
    , HasTransactionLogger
    )

instance PerformEvent t m => PerformEvent t (BrowserStorageT m) where
  type Performable (BrowserStorageT m) = BrowserStorageT (Performable m)
  performEvent_ = lift . performEvent_ . fmap runBrowserStorageT
  performEvent = lift . performEvent . fmap runBrowserStorageT

instance PrimMonad m => PrimMonad (BrowserStorageT m) where
  type PrimState (BrowserStorageT m) = PrimState m
  primitive = lift . primitive

instance MonadReader r m => MonadReader r (BrowserStorageT m) where
  ask = lift ask
  local f = lift . local f . runBrowserStorageT

instance HasDocument m => HasDocument (BrowserStorageT m)
instance HasJSContext m => HasJSContext (BrowserStorageT m) where
  type JSContextPhantom (BrowserStorageT m) = JSContextPhantom m
  askJSContext = BrowserStorageT askJSContext
#if !defined(ghcjs_HOST_OS)
instance MonadJSM m => MonadJSM (BrowserStorageT m)
#endif

instance (Monad m, Routed t r m) => Routed t r (BrowserStorageT m) where
  askRoute = lift askRoute

instance (Monad m, RouteToUrl r m) => RouteToUrl r (BrowserStorageT m) where
  askRouteToUrl = lift askRouteToUrl

instance (Reflex t, Monad m, SetRoute t r m) => SetRoute t r (BrowserStorageT m) where
  modifyRoute = lift . modifyRoute

instance EventWriter t w m => EventWriter t w (BrowserStorageT m) where
  tellEvent = lift . tellEvent

instance MonadTrans (BrowserStorageT) where
  lift = BrowserStorageT

instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (BrowserStorageT m) where
  runWithReplace a0 a' = BrowserStorageT $ runWithReplace (unBrowserStorageT a0) (fmapCheap unBrowserStorageT a')
  traverseDMapWithKeyWithAdjust f dm0 dm' = BrowserStorageT $ traverseDMapWithKeyWithAdjust (coerce . f) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = BrowserStorageT $ traverseDMapWithKeyWithAdjustWithMove (coerce . f) dm0 dm'
  traverseIntMapWithKeyWithAdjust f im0 im' = BrowserStorageT $ traverseIntMapWithKeyWithAdjust (coerce f) im0 im'

deriving instance DomRenderHook t m => DomRenderHook t (BrowserStorageT m)

instance (Prerender js t m, Reflex t) => Prerender js t (BrowserStorageT m) where
  type Client (BrowserStorageT m) = BrowserStorageT (Client m)
  prerender a b = BrowserStorageT $ prerender (unBrowserStorageT a) (unBrowserStorageT b)

instance HasConfigs m => HasConfigs (BrowserStorageT m) where
  getConfigs = lift getConfigs

runBrowserStorageT :: BrowserStorageT m a -> m a
runBrowserStorageT = unBrowserStorageT
