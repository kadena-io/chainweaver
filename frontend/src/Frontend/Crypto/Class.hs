{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Crypto.Class where

import Control.Monad.Reader
import Obelisk.Route.Frontend
import Data.ByteString (ByteString)
import Data.Text (Text)

import Pact.Types.Scheme (PPKScheme)
import Reflex.Dom hiding (fromJSString)
import Reflex.Host.Class (MonadReflexCreateTrigger)

import Frontend.Crypto.Ed25519
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad (PrimState, primitive))
import Control.Monad.Reader
import Control.Monad.Ref (MonadRef, MonadAtomicRef)
import Frontend.Foundation
import Frontend.Storage
import Pact.Server.ApiClient (HasTransactionLogger)

-- TODO : Hide the pact key constructor so the caller is forced to verify it
data PactKey = PactKey
  { _pactKey_scheme :: PPKScheme
  , _pactKey_publicKey :: PublicKey
  , _pactKey_secret :: ByteString
  } deriving Show

class HasCrypto key m | m -> key where
  cryptoSign :: ByteString -> key -> m Signature
  cryptoVerify :: ByteString -> Signature -> PublicKey -> m Bool
  cryptoGenKey :: Int -> m (key, PublicKey)
  cryptoSignWithPactKey :: ByteString -> PactKey -> m Signature
  cryptoSignWithPactKeyEither :: ByteString -> PactKey -> m (Either Text Signature)
  cryptoGenPubKeyFromPrivate :: PPKScheme -> Text -> m (Either String PactKey)

  default cryptoSign :: (MonadTrans t, Monad n, HasCrypto key n, m ~ t n) => ByteString -> key -> m Signature
  cryptoSign bs = lift . cryptoSign bs

  default cryptoVerify :: (MonadTrans t, Monad n, HasCrypto key n, m ~ t n) => ByteString -> Signature -> PublicKey -> m Bool
  cryptoVerify bs sig = lift . cryptoVerify bs sig

  default cryptoGenKey :: (MonadTrans t, Monad n, HasCrypto key n, m ~ t n) => Int -> m (key, PublicKey)
  cryptoGenKey = lift . cryptoGenKey

  default cryptoSignWithPactKey :: (MonadTrans t, Monad n, HasCrypto key n, m ~ t n) => ByteString -> PactKey -> m Signature
  cryptoSignWithPactKey bs = lift . cryptoSignWithPactKey bs

  default cryptoSignWithPactKeyEither :: (MonadTrans t, Monad n, HasCrypto key n, m ~ t n) => ByteString -> PactKey -> m (Either Text Signature)
  cryptoSignWithPactKeyEither bs = lift . cryptoSignWithPactKeyEither bs

  default cryptoGenPubKeyFromPrivate :: (MonadTrans t, Monad n, HasCrypto key n, m ~ t n) => PPKScheme -> Text -> m (Either String PactKey)
  cryptoGenPubKeyFromPrivate scheme = lift . cryptoGenPubKeyFromPrivate scheme

instance (HasCrypto key m, Monad m) => HasCrypto key (RoutedT t r m)

-- newtype CryptoT key t m a = CryptoT
--   { unCryptoT :: ReaderT (Behavior t (key, Text)) m a
--   } deriving
--     ( Functor, Applicative, Monad
--     , MonadFix, MonadIO, MonadRef, MonadAtomicRef
--     , DomBuilder t, NotReady t, MonadHold t, MonadSample t
--     , TriggerEvent t, PostBuild t, HasJS x
--     , MonadReflexCreateTrigger t, MonadQuery t q, Requester t
--     , HasStorage, HasDocument
--     , Routed t r, RouteToUrl r, SetRoute t r, EventWriter t w
--     , DomRenderHook t
--     , HasConfigs, HasTransactionLogger
--     )

-- instance PerformEvent t m => PerformEvent t (CryptoT key t m) where
--   type Performable (CryptoT key t m) = CryptoT key t (Performable m)
--   performEvent_ = lift . performEvent_ . fmap runBrowserCryptoT
--   performEvent = lift . performEvent . fmap runBrowserCryptoT

-- instance PrimMonad m => PrimMonad (CryptoT key t m) where
--   type PrimState (CryptoT key t m) = PrimState m
--   primitive = lift . primitive

-- instance HasJSContext m => HasJSContext (CryptoT key t m) where
--   type JSContextPhantom (CryptoT key t m) = JSContextPhantom m
--   askJSContext = BrowserCryptoT askJSContext
-- #if !defined(ghcjs_HOST_OS)
-- instance MonadJSM m => MonadJSM (CryptoT key t m)
-- #endif

-- -- instance MonadTrans BrowserCryptoT where
-- --   lift = BrowserCryptoT

-- instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (BrowserCryptoT m) where
--   runWithReplace a0 a' = BrowserCryptoT $ runWithReplace (unBrowserCryptoT a0) (fmapCheap unBrowserCryptoT a')
--   traverseDMapWithKeyWithAdjust f dm0 dm' = BrowserCryptoT $ traverseDMapWithKeyWithAdjust (coerce . f) dm0 dm'
--   traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = BrowserCryptoT $ traverseDMapWithKeyWithAdjustWithMove (coerce . f) dm0 dm'
--   traverseIntMapWithKeyWithAdjust f im0 im' = BrowserCryptoT $ traverseIntMapWithKeyWithAdjust (coerce f) im0 im'

-- instance (Prerender js t m, Reflex t) => Prerender js t (BrowserCryptoT m) where
--   type Client (BrowserCryptoT m) = BrowserCryptoT (Client m)
--   prerender a b = BrowserCryptoT $ prerender (unBrowserCryptoT a) (unBrowserCryptoT b)
