{-# OPTIONS_GHC -Wno-orphans -Werror #-}

module Desktop.Orphans where

import Control.Monad ((<=<))
import Data.Bimap (Bimap)
import Data.Ord (comparing)
import qualified Cardano.Crypto.Wallet as Crypto
import qualified Crypto.Error as Ed25519
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.Aeson as Aeson
import qualified Data.Bimap as Bimap
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text.Encoding as T

-- Encode as a list of tuples
instance (Ord a, Aeson.FromJSON a, Ord b, Aeson.FromJSON b) => Aeson.FromJSON (Bimap a b) where
  parseJSON = fmap Bimap.fromList . Aeson.parseJSON

instance (Aeson.ToJSON a, Aeson.ToJSON b) => Aeson.ToJSON (Bimap a b) where
  toJSON = Aeson.toJSON . Bimap.toList

-- Order by the base-16 encoding
instance Ord Ed25519.PublicKey where
  compare = comparing $ B16.encode . BA.pack . BA.unpack

-- Encode as base-16
instance Aeson.ToJSON Ed25519.PublicKey where
  toJSON = Aeson.toJSON . T.decodeUtf8 . B16.encode . BA.pack . BA.unpack

instance Aeson.FromJSON Ed25519.PublicKey where
  parseJSON = Ed25519.onCryptoFailure (fail . show) pure . Ed25519.publicKey . fst . B16.decode . T.encodeUtf8 <=< Aeson.parseJSON

-- Encode as base-16
instance Aeson.ToJSON Crypto.XPrv where
  toJSON = Aeson.toJSON . T.decodeUtf8 . B16.encode . Crypto.unXPrv

instance Aeson.FromJSON Crypto.XPrv where
  parseJSON = either fail pure . Crypto.xprv . fst . B16.decode . T.encodeUtf8 <=< Aeson.parseJSON
