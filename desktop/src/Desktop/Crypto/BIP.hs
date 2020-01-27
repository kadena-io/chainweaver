{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Desktop.Crypto.BIP where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad (PrimState, primitive))
import Control.Monad.Reader
import Control.Monad.Ref (MonadRef, MonadAtomicRef)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Text (Text)
import Language.Javascript.JSaddle (MonadJSM)
import Obelisk.Route.Frontend
import Pact.Server.ApiV1Client (HasTransactionLogger)
import Pact.Types.Util (parseB16TextOnly)
import Reflex.Dom hiding (fromJSString)
import Reflex.Host.Class (MonadReflexCreateTrigger)

import qualified Cardano.Crypto.Wallet as Crypto
import qualified Control.Newtype.Generics as Newtype
import qualified Data.Text.Encoding as T
import qualified Pact.Types.Crypto as PactCrypto
import qualified Pact.Types.Hash as Pact

import Frontend.Crypto.Ed25519
import Frontend.Crypto.Class
import Frontend.Foundation
import Frontend.Storage

newtype BIPCryptoT m a = BIPCryptoT
  { unBIPCryptoT :: ReaderT (Crypto.XPrv, Text) m a
  } deriving
    ( Functor, Applicative, Monad
    , MonadFix, MonadIO, MonadRef, MonadAtomicRef
    , DomBuilder t, NotReady t, MonadHold t, MonadSample t
    , TriggerEvent t, PostBuild t, HasJS x
    , MonadReflexCreateTrigger t, MonadQuery t q, Requester t
    , HasStorage, HasDocument
    , Routed t r, RouteToUrl r, SetRoute t r, EventWriter t w
    , DomRenderHook t
    , HasConfigs, HasTransactionLogger
    )

bipCryptoGenPair :: Crypto.XPrv -> Text -> Int -> (Crypto.XPrv, PublicKey)
bipCryptoGenPair root pass i =
  let xprv = Crypto.deriveXPrv scheme (T.encodeUtf8 pass) root (mkHardened $ fromIntegral i)
  in (xprv, unsafePublicKey $ Crypto.xpubPublicKey $ Crypto.toXPub xprv)
  where
    scheme = Crypto.DerivationScheme2
    mkHardened = (0x80000000 .|.)

instance MonadJSM m => HasCrypto Crypto.XPrv (BIPCryptoT m) where
  cryptoSign bs xprv = BIPCryptoT $ do
    (_root, pass) <- ask
    pure $ Newtype.pack $ Crypto.unXSignature $ Crypto.sign (T.encodeUtf8 pass) xprv bs
  cryptoGenKey i = BIPCryptoT $ do
    (root, pass) <- ask
    liftIO $ putStrLn $ "Deriving key at index: " <> show i
    pure $ bipCryptoGenPair root pass i
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

importKey :: PactCrypto.PPKScheme -> Maybe ByteString -> ByteString -> Either String PactCrypto.SomeKeyPair
importKey pkScheme mPubBytes secBytes = PactCrypto.importKeyPair
  (PactCrypto.toScheme pkScheme)
  (PactCrypto.PubBS <$> mPubBytes)
  (PactCrypto.PrivBS secBytes)


instance PerformEvent t m => PerformEvent t (BIPCryptoT m) where
  type Performable (BIPCryptoT m) = BIPCryptoT (Performable m)
  performEvent_ e = BIPCryptoT $ do
    (x, p) <- ask
    lift $ performEvent_ $ runBIPCryptoT x p <$> e
  performEvent e = BIPCryptoT $ do
    (x, p) <- ask
    lift $ performEvent $ runBIPCryptoT x p <$> e

instance PrimMonad m => PrimMonad (BIPCryptoT m) where
  type PrimState (BIPCryptoT m) = PrimState m
  primitive = lift . primitive

instance HasJSContext m => HasJSContext (BIPCryptoT m) where
  type JSContextPhantom (BIPCryptoT m) = JSContextPhantom m
  askJSContext = BIPCryptoT askJSContext
#if !defined(ghcjs_HOST_OS)
instance MonadJSM m => MonadJSM (BIPCryptoT m)
#endif

instance MonadTrans BIPCryptoT where
  lift = BIPCryptoT . lift

instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (BIPCryptoT m) where
  runWithReplace a0 a' = BIPCryptoT $ runWithReplace (unBIPCryptoT a0) (fmapCheap unBIPCryptoT a')
  traverseDMapWithKeyWithAdjust f dm0 dm' = BIPCryptoT $ traverseDMapWithKeyWithAdjust (coerce . f) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = BIPCryptoT $ traverseDMapWithKeyWithAdjustWithMove (coerce . f) dm0 dm'
  traverseIntMapWithKeyWithAdjust f im0 im' = BIPCryptoT $ traverseIntMapWithKeyWithAdjust (coerce f) im0 im'

instance (Prerender js t m, Monad m, Reflex t) => Prerender js t (BIPCryptoT m) where
  type Client (BIPCryptoT m) = BIPCryptoT (Client m)
  prerender a b = BIPCryptoT $ prerender (unBIPCryptoT a) (unBIPCryptoT b)

runBIPCryptoT :: Crypto.XPrv -> Text -> BIPCryptoT m a -> m a
runBIPCryptoT xprv pass (BIPCryptoT m) = runReaderT m (xprv, pass)
