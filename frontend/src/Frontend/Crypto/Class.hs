{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Crypto.Class where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad (PrimState, primitive))
import Control.Monad.Reader
import Control.Monad.Ref (MonadRef, MonadAtomicRef)
import Data.Coerce (coerce)
import Language.Javascript.JSaddle (JSM, MonadJSM, liftJSM)
import Obelisk.Route.Frontend
import Reflex.Dom hiding (fromJSString)
import Reflex.Host.Class (MonadReflexCreateTrigger)
import Data.ByteString (ByteString)
import Data.Text (Text)

import Pact.Types.Scheme (PPKScheme)

import Frontend.Crypto.Ed25519
import Frontend.Foundation
import Frontend.Storage

instance (HasStorage m, Monad m) => HasStorage (CryptoT key m)
-- TODO : Hide the pact key constructor so the caller is forced to verify it
data PactKey = PactKey
  { _pactKey_scheme :: PPKScheme
  , _pactKey_publicKey :: PublicKey
  , _pactKey_secret :: ByteString
  } deriving Show

data GenKeyArg
  = GenWalletIndex Int
  | GenFromPactKey PactKey

data Crypto key = Crypto
  { _crypto_sign :: ByteString -> key -> JSM Signature
  , _crypto_genKey :: GenKeyArg -> JSM (key, PublicKey)
  , _crypto_verifyPactKey :: PPKScheme -> Text -> JSM (Either String PactKey)
  }

cryptoGenPubKeyFromPrivate
  :: ( MonadJSM m
     , HasCrypto key m
     )
  => PPKScheme
  -> Text
  -> m (Either String PactKey)
cryptoGenPubKeyFromPrivate scheme k = do
  crypto <- askCrypto
  liftJSM $ _crypto_verifyPactKey crypto scheme k

cryptoGenKey :: (MonadJSM m, HasCrypto key m) => GenKeyArg -> m (key, PublicKey)
cryptoGenKey i = do
  crypto <- askCrypto
  liftJSM $ _crypto_genKey crypto i

cryptoSign :: (MonadJSM m, HasCrypto key m) => ByteString -> key -> m Signature
cryptoSign bs key = do
  crypto <- askCrypto
  liftJSM $ _crypto_sign crypto bs key

class HasCrypto key m | m -> key where
  askCrypto :: m (Crypto key)
  default askCrypto :: (MonadTrans t, Monad n, HasCrypto key n, m ~ t n) => m (Crypto key)
  askCrypto = lift askCrypto

instance (HasCrypto key m, Monad m) => HasCrypto key (RoutedT t r m)

instance Monad m => HasCrypto key (CryptoT key m) where
  askCrypto = CryptoT ask

newtype CryptoT key m a = CryptoT
  { unCryptoT :: ReaderT (Crypto key) m a
  } deriving
    ( Functor, Applicative, Monad
    , MonadFix, MonadIO, MonadRef, MonadAtomicRef
    , DomBuilder t, NotReady t, MonadHold t, MonadSample t
    , TriggerEvent t, PostBuild t, HasJS x
    , MonadReflexCreateTrigger t, MonadQuery t q, Requester t
    )

instance PerformEvent t m => PerformEvent t (CryptoT key m) where
  type Performable (CryptoT key m) = CryptoT key (Performable m)
  performEvent_ e = do
    c <- askCrypto
    lift $ performEvent_ $ flip runCryptoT c <$> e
  performEvent e = do
    c <- askCrypto
    lift $ performEvent $ flip runCryptoT c <$> e

instance PrimMonad m => PrimMonad (CryptoT key m) where
  type PrimState (CryptoT key m) = PrimState m
  primitive = lift . primitive

instance MonadReader r m => MonadReader r (CryptoT key m) where
  ask = lift ask
  local f m = askCrypto >>= lift . local f . runCryptoT m

instance HasDocument m => HasDocument (CryptoT key m)
instance HasJSContext m => HasJSContext (CryptoT key m) where
  type JSContextPhantom (CryptoT key m) = JSContextPhantom m
  askJSContext = CryptoT askJSContext
#if !defined(ghcjs_HOST_OS)
instance MonadJSM m => MonadJSM (CryptoT key m)
#endif

instance (Monad m, Routed t r m) => Routed t r (CryptoT key m) where
  askRoute = lift askRoute

instance (Monad m, RouteToUrl r m) => RouteToUrl r (CryptoT key m) where
  askRouteToUrl = lift askRouteToUrl

instance (Reflex t, Monad m, SetRoute t r m) => SetRoute t r (CryptoT key m) where
  modifyRoute = lift . modifyRoute

instance EventWriter t w m => EventWriter t w (CryptoT key m) where
  tellEvent = lift . tellEvent

instance MonadTrans (CryptoT key) where
  lift = CryptoT . lift

instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (CryptoT key m) where
  runWithReplace a0 a' = CryptoT $ runWithReplace (unCryptoT a0) (fmapCheap unCryptoT a')
  traverseDMapWithKeyWithAdjust f dm0 dm' = CryptoT $ traverseDMapWithKeyWithAdjust (coerce . f) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = CryptoT $ traverseDMapWithKeyWithAdjustWithMove (coerce . f) dm0 dm'
  traverseIntMapWithKeyWithAdjust f im0 im' = CryptoT $ traverseIntMapWithKeyWithAdjust (coerce f) im0 im'

deriving instance DomRenderHook t m => DomRenderHook t (CryptoT key m)

instance (Prerender js t m, Monad m, Reflex t) => Prerender js t (CryptoT key m) where
  type Client (CryptoT key m) = CryptoT key (Client m)
  prerender a b = CryptoT $ prerender (unCryptoT a) (unCryptoT b)

instance HasConfigs m => HasConfigs (CryptoT key m) where
  getConfigs = lift getConfigs

runCryptoT :: CryptoT key m a -> Crypto key -> m a
runCryptoT = runReaderT . unCryptoT
