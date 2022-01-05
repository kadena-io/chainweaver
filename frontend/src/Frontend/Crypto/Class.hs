{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Crypto.Class where

import Control.Monad.Reader
import Control.Error.Util
import Obelisk.Route.Frontend
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T

import Pact.Types.Scheme (PPKScheme)
import Frontend.Crypto.Ed25519
import Frontend.Foundation
import Frontend.Crypto.Signature
import Frontend.Crypto.Password

-- TODO : Hide the pact key constructor so the caller is forced to verify it
data PactKey = PactKey
  { _pactKey_scheme :: PPKScheme
  , _pactKey_publicKey :: PublicKey
  , _pactKey_secret :: ByteString
  } deriving Show

data MnemonicError =
    MnemonicError_InvalidPhrase
  | MnemonicError_NotEnoughWords
  deriving (Show)

class DisplayError e where
  displayError :: e -> Text

instance DisplayError MnemonicError where
  displayError MnemonicError_InvalidPhrase = "Invalid Mnemonic Phrase"
  displayError MnemonicError_NotEnoughWords = ""


class (DisplayError (BIP39MnemonicError mnem)) => BIP39Mnemonic mnem where
  type BIP39MnemonicError mnem
  generateMnemonic :: MonadJSM m => m (Either Text mnem)
  toMnemonic :: MonadJSM m => [Text] -> m (Either (BIP39MnemonicError mnem) mnem)
  mnemonicToText :: mnem -> [Text]

instance BIP39Mnemonic [Text] where
  type BIP39MnemonicError [Text] = MnemonicError

  generateMnemonic = fmap Right genMnemonic

  -- Basic case where input type ([Text]) is the same as output type and we are just validating
  toMnemonic sentence = case (length sentence == 12) of
    False -> pure $ Left MnemonicError_NotEnoughWords
    True -> do
      ffor (liftJSM $ validateMnemonic $ T.unwords sentence) $ \case
        False -> Left MnemonicError_InvalidPhrase
        True -> Right sentence
  mnemonicToText = id

-- Derive a root key from mnemonic; mostly used for setup workflow
class BIP39Root key where
  type Sentence key
  deriveRoot :: MonadJSM m => Password -> Sentence key -> m (Maybe key)

instance BIP39Root PrivateKey where
  type Sentence PrivateKey = [Text]
  deriveRoot pwd sentence = liftJSM $
    fmap hush $ generateRoot pwd $ T.unwords sentence

-- |Main constraint for frontend use
type DerivableKey key mnem = (BIP39Mnemonic mnem, BIP39Root key, Sentence key ~ mnem)

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
