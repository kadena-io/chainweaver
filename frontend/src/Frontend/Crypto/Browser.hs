{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Crypto.Browser where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad (PrimState, primitive))
import Control.Monad.Reader
import Control.Monad.Ref (MonadRef, MonadAtomicRef)
import Data.Coerce (coerce)
import qualified Data.ByteString as B
import Data.Text.Encoding
import Language.Javascript.JSaddle (MonadJSM)
import Obelisk.Route.Frontend
import Reflex.Dom hiding (fromJSString)
import Reflex.Host.Class (MonadReflexCreateTrigger)

import Frontend.Crypto.Ed25519
import Frontend.Crypto.Class
import Frontend.Foundation
import Frontend.Storage
import Pact.Server.ApiClient (HasTransactionLogger)

newtype BrowserCryptoT m a = BrowserCryptoT
  { unBrowserCryptoT :: m a
  } deriving
    ( Functor, Applicative, Monad
    , MonadFix, MonadIO, MonadRef, MonadAtomicRef
    , DomBuilder t, NotReady t, MonadHold t, MonadSample t
    , TriggerEvent t, PostBuild t, HasJS x
    , MonadReflexCreateTrigger t, MonadQuery t q, Requester t
    , HasStorage, MonadReader r, HasDocument
    , Routed t r, RouteToUrl r, SetRoute t r, EventWriter t w
    , DomRenderHook t
    , HasConfigs
    , HasTransactionLogger
    )


instance MonadJSM m => HasCrypto PrivateKey (BrowserCryptoT m) where
  cryptoSign = mkSignature
  cryptoVerify = verifySignature
  cryptoGenKey = const genKeyPair
  cryptoGenPubKeyFromPrivate pkScheme t = do
    case textToKey t of
      Nothing -> pure $ Left $ "cryptoGenPubKeyFromPrivate: not a valid private key"
      Just (PrivateKey k) -> do
        (priv,pub) <- deriveKeyPairFromPrivateKey k
        pure $ Right $ PactKey pkScheme pub (unPrivateKey priv)
  cryptoSignWithPactKey m (PactKey _ (PublicKey pub) sec) = mkSignature m priv
    where
     priv = PrivateKey $ sec <> pub
  cryptoSignWithPactKeyEither m pk = Right <$> cryptoSignWithPactKey m pk

instance PerformEvent t m => PerformEvent t (BrowserCryptoT m) where
  type Performable (BrowserCryptoT m) = BrowserCryptoT (Performable m)
  performEvent_ = lift . performEvent_ . fmap runBrowserCryptoT
  performEvent = lift . performEvent . fmap runBrowserCryptoT

instance PrimMonad m => PrimMonad (BrowserCryptoT m) where
  type PrimState (BrowserCryptoT m) = PrimState m
  primitive = lift . primitive

instance HasJSContext m => HasJSContext (BrowserCryptoT m) where
  type JSContextPhantom (BrowserCryptoT m) = JSContextPhantom m
  askJSContext = BrowserCryptoT askJSContext
#if !defined(ghcjs_HOST_OS)
instance MonadJSM m => MonadJSM (BrowserCryptoT m)
#endif

instance MonadTrans BrowserCryptoT where
  lift = BrowserCryptoT

instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (BrowserCryptoT m) where
  runWithReplace a0 a' = BrowserCryptoT $ runWithReplace (unBrowserCryptoT a0) (fmapCheap unBrowserCryptoT a')
  traverseDMapWithKeyWithAdjust f dm0 dm' = BrowserCryptoT $ traverseDMapWithKeyWithAdjust (coerce . f) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = BrowserCryptoT $ traverseDMapWithKeyWithAdjustWithMove (coerce . f) dm0 dm'
  traverseIntMapWithKeyWithAdjust f im0 im' = BrowserCryptoT $ traverseIntMapWithKeyWithAdjust (coerce f) im0 im'

instance (Prerender js t m, Reflex t) => Prerender js t (BrowserCryptoT m) where
  type Client (BrowserCryptoT m) = BrowserCryptoT (Client m)
  prerender a b = BrowserCryptoT $ prerender (unBrowserCryptoT a) (unBrowserCryptoT b)

runBrowserCryptoT :: BrowserCryptoT m a -> m a
runBrowserCryptoT = unBrowserCryptoT
