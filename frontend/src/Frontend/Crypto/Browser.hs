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
import Language.Javascript.JSaddle (MonadJSM)
import Obelisk.Route.Frontend
import Reflex.Dom hiding (fromJSString)
import Reflex.Host.Class (MonadReflexCreateTrigger)

import qualified Control.Newtype.Generics as Newtype
import Frontend.Crypto.Ed25519
import Frontend.Crypto.Class
import Frontend.Foundation
import Frontend.Storage
import Pact.Server.ApiClient (HasTransactionLogger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Pact.Types.Crypto as PactCrypto
import qualified Pact.Types.Hash as Pact
import Pact.Types.Util (parseB16TextOnly)
import Data.ByteString (ByteString)

newtype BrowserCryptoT t m a = BrowserCryptoT
  { unBrowserCryptoT :: ReaderT (Behavior t (PrivateKey, Text)) m a
  } deriving
    ( Functor, Applicative, Monad
    , MonadFix, MonadIO, MonadRef, MonadAtomicRef
    , DomBuilder t, NotReady t, MonadHold t, MonadSample t
    , TriggerEvent t, PostBuild t, HasJS x
    , MonadReflexCreateTrigger t, MonadQuery t q, Requester t
    , HasStorage, HasDocument
    , Routed t r, RouteToUrl r, SetRoute t r, EventWriter t w
    , DomRenderHook t
    , HasConfigs
    , HasTransactionLogger
    , MonadReader (Behavior t (PrivateKey, Text))
    )

instance (MonadJSM m, MonadSample t m) => HasCrypto PrivateKey (BrowserCryptoT t m) where
  cryptoSign msg key = do
    (_, p) <- sample =<< ask
    mkSignature p msg key
  cryptoVerify = verifySignature
  cryptoGenKey i = BrowserCryptoT $ do
    (root, p) <- sample =<< ask
    --TODO handle maybe
    mKeys <- liftJSM $ generateKeypair p root i
    case mKeys of
      --TODO FIX THIS
      Nothing -> undefined
      Just pair -> pure pair
  -- cryptoGenPubKeyFromPrivate pkScheme t = do
  --   case textToKey t of
  --     Nothing -> pure $ Left $ "cryptoGenPubKeyFromPrivate: not a valid private key"
  --     Just (PrivateKey k) -> do
  --       (priv,pub) <- deriveKeyPairFromPrivateKey k
  --       pure $ Right $ PactKey pkScheme pub (unPrivateKey priv)
  -- cryptoSignWithPactKey m (PactKey _ (PublicKey pub) sec) = mkSignature m priv
  --   where
  --    priv = PrivateKey $ sec <> pub
  -- cryptoSignWithPactKeyEither m pk = Right <$> cryptoSignWithPactKey m pk
-- TODO: Copy this into common
-- COPIED FROM BIP
  -- This assumes that the secret is already base16 encoded (being pasted in, so makes sense)
  cryptoGenPubKeyFromPrivate pkScheme sec = pure $ do
    secBytes <- parseB16TextOnly sec
    somePactKey <- importKey pkScheme Nothing secBytes
    pure $ PactKey pkScheme (unsafePublicKey $ PactCrypto.getPublic somePactKey) secBytes
  cryptoSignWithPactKey bs pk = do
    let someKpE = importKey
          (_pactKey_scheme pk)
          (Just $ Newtype.unpack $ _pactKey_publicKey pk)
          $ _pactKey_secret pk

    case someKpE of
      Right someKp -> liftIO $ Newtype.pack <$> PactCrypto.sign someKp (Pact.Hash bs)
      Left e -> error $ "Error importing pact key from account: " <> e
  cryptoSignWithPactKeyEither bs pk = do
    let someKpE = importKey
          (_pactKey_scheme pk)
          (Just $ Newtype.unpack $ _pactKey_publicKey pk)
          $ _pactKey_secret pk

    case someKpE of
      Right someKp -> liftIO $ (Right . Newtype.pack) <$> PactCrypto.sign someKp (Pact.Hash bs)
      Left e -> pure $ Left $ "Error importing pact key from account: " <> T.pack e

importKey :: PactCrypto.PPKScheme -> Maybe ByteString -> ByteString -> Either String PactCrypto.SomeKeyPair
importKey pkScheme mPubBytes secBytes = PactCrypto.importKeyPair
  (PactCrypto.toScheme pkScheme)
  (PactCrypto.PubBS <$> mPubBytes)
  (PactCrypto.PrivBS secBytes)

instance PerformEvent t m => PerformEvent t (BrowserCryptoT t m) where
  type Performable (BrowserCryptoT t m) = BrowserCryptoT t (Performable m)
  performEvent_ = BrowserCryptoT . performEvent_ . fmap unBrowserCryptoT
  performEvent = BrowserCryptoT . performEvent . fmap unBrowserCryptoT

instance PrimMonad m => PrimMonad (BrowserCryptoT t m) where
  type PrimState (BrowserCryptoT t m) = PrimState m
  primitive = lift . primitive

instance HasJSContext m => HasJSContext (BrowserCryptoT t m) where
  type JSContextPhantom (BrowserCryptoT t m) = JSContextPhantom m
  askJSContext = BrowserCryptoT askJSContext
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

instance (Prerender js t m, Monad m, Reflex t) => Prerender js t (BrowserCryptoT t m) where
  type Client (BrowserCryptoT t m) = BrowserCryptoT t (Client m)
  prerender a b = BrowserCryptoT $ prerender (unBrowserCryptoT a) (unBrowserCryptoT b)

runBrowserCryptoT :: Behavior t (PrivateKey, Text) -> BrowserCryptoT t m a -> m a
runBrowserCryptoT b (BrowserCryptoT m) = runReaderT m b
