{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Crypto.Browser where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad (PrimState, primitive))
import Control.Monad.Reader
import Control.Monad.Ref (MonadRef, MonadAtomicRef)
import Data.Coerce (coerce)
import Language.Javascript.JSaddle (MonadJSM)
import Obelisk.Route.Frontend
import Reflex.Dom hiding (fromJSString)
import Reflex.Host.Class (MonadReflexCreateTrigger)

import qualified Control.Newtype.Generics as Newtype
import Frontend.Crypto.Ed25519
import Frontend.Crypto.Class
import Frontend.Crypto.Password
import Frontend.Crypto.Signature
import Frontend.Foundation
import Frontend.Storage
import Pact.Server.ApiClient (HasTransactionLogger)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Either (fromRight)

newtype BrowserCryptoT t m a = BrowserCryptoT
  { unBrowserCryptoT :: ReaderT (Behavior t (PrivateKey, Password)) m a
  } deriving
    ( Functor, Applicative, Monad
    , MonadFix, MonadIO, MonadRef, MonadAtomicRef
    , DomBuilder t, NotReady t, MonadHold t, MonadSample t
    , TriggerEvent t, PostBuild t
    , MonadReflexCreateTrigger t, MonadQuery t q, Requester t
    , HasStorage, HasDocument
    , Routed t r, RouteToUrl r, SetRoute t r, EventWriter t w
    , DomRenderHook t
    , HasConfigs
    , HasTransactionLogger
    , MonadReader (Behavior t (PrivateKey, Password))
    )

instance (MonadJSM m, MonadSample t m) => HasCrypto PrivateKey (BrowserCryptoT t m) where
  cryptoSign msg key = do
    (_, p) <- sample =<< ask
    sigOrErr <- mkSignature p msg key
    pure $ fromRight (Signature "") sigOrErr
  cryptoVerify = verifySignature
  cryptoGenKey i = BrowserCryptoT $ do
    (root, p) <- sample =<< ask
    keysOrErr <- liftJSM $ generateKeypair p root i
    pure $ fromRight (PrivateKey "", PublicKey "") keysOrErr

  --TODO: Is this func used anywhere?
  cryptoGenPubKeyFromPrivate _ _ = pure $ Left $ "cryptoGenPubKeyFromPrivate: not supported on browser"

  -- We use the same js package as pact here (nacl) and not the BIP-browser one that
  -- we use for everything else because the bip-browser one only works with
  -- encrypted private keys, and not raw one
  cryptoSignWithPactKey m (PactKey _ (PublicKey pub) sec) = mkSignatureLegacyJS m priv
    where
     priv = PrivateKey $ sec <> pub

  cryptoSignWithPactKeyEither m pk = Right <$> cryptoSignWithPactKey m pk


instance PerformEvent t m => PerformEvent t (BrowserCryptoT t m) where
  type Performable (BrowserCryptoT t m) = BrowserCryptoT t (Performable m)
  performEvent_ = BrowserCryptoT . performEvent_ . fmap unBrowserCryptoT
  performEvent = BrowserCryptoT . performEvent . fmap unBrowserCryptoT

instance PrimMonad m => PrimMonad (BrowserCryptoT t m) where
  type PrimState (BrowserCryptoT t m) = PrimState m
  primitive = lift . primitive

#if !defined(ghcjs_HOST_OS)
instance MonadJSM m => MonadJSM (BrowserCryptoT t m)
#endif

instance MonadTrans (BrowserCryptoT t) where
  lift = BrowserCryptoT . lift

instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (BrowserCryptoT t m) where
  runWithReplace a0 a' = BrowserCryptoT $ runWithReplace (unBrowserCryptoT a0) (fmapCheap unBrowserCryptoT a')
  traverseDMapWithKeyWithAdjust f dm0 dm' = BrowserCryptoT $ traverseDMapWithKeyWithAdjust (coerce . f) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = BrowserCryptoT $ traverseDMapWithKeyWithAdjustWithMove (coerce . f) dm0 dm'
  traverseIntMapWithKeyWithAdjust f im0 im' = BrowserCryptoT $ traverseIntMapWithKeyWithAdjust (coerce f) im0 im'

instance (Prerender t m, Monad m, Reflex t) => Prerender t (BrowserCryptoT t m) where
  type Client (BrowserCryptoT t m) = BrowserCryptoT t (Client m)
  prerender a b = BrowserCryptoT $ prerender (unBrowserCryptoT a) (unBrowserCryptoT b)

runBrowserCryptoT :: Behavior t (PrivateKey, Password) -> BrowserCryptoT t m a -> m a
runBrowserCryptoT b (BrowserCryptoT m) = runReaderT m b
