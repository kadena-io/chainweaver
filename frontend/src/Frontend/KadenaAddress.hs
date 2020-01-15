{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
module Frontend.KadenaAddress where

import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Pact.Types.ChainId
import Kadena.SigningApi (AccountName(..))

newtype AddressPubKey = AddressPubKey { _addressPubKey_bytes :: ByteString }
  deriving (Eq,Ord,Show)

-- Encode as base-16
instance ToJSON AddressPubKey where
  toJSON = toJSON . TE.decodeUtf8 . B16.encode . _addressPubKey_bytes

instance FromJSON AddressPubKey where
  parseJSON = return . AddressPubKey . fst . B16.decode . TE.encodeUtf8 <=< parseJSON

data AddressKeyset = AddressKeyset
  { _addressKeyset_keys :: Set AddressPubKey
  , _addressKeyset_pred :: Text
  } deriving (Eq,Ord,Show)

instance ToJSON AddressKeyset where
  toJSON o = object
      [ "keys" .= _addressKeyset_keys o
      , "pred" .= _addressKeyset_pred o
      ]

instance FromJSON AddressKeyset where
  parseJSON = withObject "AddressKeyset" $ \o -> AddressKeyset
    <$> o .: "keys"
    <*> o .: "pred"

data KadenaAddress = KadenaAddress
  { _kadenaAddress_accountName :: AccountName
    -- ^ The account name associated with this address
  , _kadenaAddress_chainId :: ChainId
    -- ^ The chain where this account resides
  , _kadenaAddress_keyset :: Maybe AddressKeyset
    -- ^ Presence or absence of a keyset may be used to determine transfer vs
    -- transfer-create. If the keyset is present and the account already exists
    -- you could choose to do either a transfer or a transfer-create.
  }
  deriving (Show, Eq)

instance ToJSON KadenaAddress where
  toJSON o = object $ catMaybes
      [ Just $ "account" .= _kadenaAddress_accountName o
      , Just $ "chain" .= _kadenaAddress_chainId o
      , ("keyset" .=) <$> _kadenaAddress_keyset o
      ]

instance FromJSON KadenaAddress where
  parseJSON = withObject "KadenaAddress" $ \o -> KadenaAddress
    <$> o .: "account"
    <*> o .: "chain"
    <*> o .:? "keyset"
