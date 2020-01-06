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

import Frontend.Crypto.Ed25519

-- TODO : Hide the pact key constructor so the caller is forced to verify it
data PactKey = PactKey
  { _pactKey_scheme :: PPKScheme
  , _pactKey_publicKey :: PublicKey
  , _pactKey_secret :: ByteString
  } deriving Show

class HasCrypto key m | m -> key where
  cryptoSign :: ByteString -> key -> m Signature
  cryptoGenKey :: Int -> m (key, PublicKey)
  cryptoSignWithPactKey :: ByteString -> PactKey -> m Signature
  cryptoGenPubKeyFromPrivate :: PPKScheme -> Text -> m (Either String PactKey)

  default cryptoSign :: (MonadTrans t, Monad n, HasCrypto key n, m ~ t n) => ByteString -> key -> m Signature
  cryptoSign bs = lift . cryptoSign bs

  default cryptoGenKey :: (MonadTrans t, Monad n, HasCrypto key n, m ~ t n) => Int -> m (key, PublicKey)
  cryptoGenKey = lift . cryptoGenKey

  default cryptoSignWithPactKey :: (MonadTrans t, Monad n, HasCrypto key n, m ~ t n) => ByteString -> PactKey -> m Signature
  cryptoSignWithPactKey bs = lift . cryptoSignWithPactKey bs

  default cryptoGenPubKeyFromPrivate :: (MonadTrans t, Monad n, HasCrypto key n, m ~ t n) => PPKScheme -> Text -> m (Either String PactKey)
  cryptoGenPubKeyFromPrivate scheme = lift . cryptoGenPubKeyFromPrivate scheme

instance (HasCrypto key m, Monad m) => HasCrypto key (RoutedT t r m)
