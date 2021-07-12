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
import Frontend.Crypto.Signature
import Frontend.Crypto.Password

import Pact.Server.ApiClient (HasTransactionLogger)

-- TODO : Hide the pact key constructor so the caller is forced to verify it
data PactKey = PactKey
  { _pactKey_scheme :: PPKScheme
  , _pactKey_publicKey :: PublicKey
  , _pactKey_secret :: ByteString
  } deriving Show

-- Derive a root key from mnemonic; mostly used for setup workflow
class BIP39Root key where
  type Sentence key 
  deriveRoot :: MonadJSM m => Password -> Sentence key -> m (Maybe key)

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
