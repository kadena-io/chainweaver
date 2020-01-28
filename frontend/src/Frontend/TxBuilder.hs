{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
module Frontend.TxBuilder where

import Data.Aeson
import Data.Maybe
import Pact.Types.ChainId
import Kadena.SigningApi (AccountName(..))

import Common.Wallet

data TxBuilder = TxBuilder
  { _txBuilder_accountName :: AccountName
    -- ^ The account name associated with this address
  , _txBuilder_chainId :: ChainId
    -- ^ The chain where this account resides
  , _txBuilder_keyset :: Maybe AddressKeyset
    -- ^ Presence or absence of a keyset may be used to determine transfer vs
    -- transfer-create. If the keyset is present and the account already exists
    -- you could choose to do either a transfer or a transfer-create.
  }
  deriving (Show, Eq)

instance ToJSON TxBuilder where
  toJSON o = object $ catMaybes
      [ Just $ "account" .= _txBuilder_accountName o
      , Just $ "chain" .= _txBuilder_chainId o
      , ("keyset" .=) <$> _txBuilder_keyset o
      ]

instance FromJSON TxBuilder where
  parseJSON = withObject "TxBuilder" $ \o -> TxBuilder
    <$> o .: "account"
    <*> o .: "chain"
    <*> o .:? "keyset"
