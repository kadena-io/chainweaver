{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
module Frontend.TxBuilder where

import           Control.Applicative ((<|>))
import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import           Data.Maybe
import           Data.Functor ((<&>))
import           Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import           Pact.Types.ChainId
import           Kadena.SigningApi (AccountName(..))
import           Pact.Types.Term (KeySet (..), KeySetName(..))

data TxBuilder = TxBuilder
  { _txBuilder_accountName :: AccountName
    -- ^ The account name associated with this address
  , _txBuilder_chainId :: ChainId
    -- ^ The chain where this account resides
  , _txBuilder_keyset :: Maybe (Either KeySetName KeySet)
    -- ^ Presence or absence of a keyset may be used to determine transfer vs
    -- transfer-create. If the keyset is present and the account already exists
    -- you could choose to do either a transfer or a transfer-create.
  }
  deriving (Show, Eq)

instance ToJSON TxBuilder where
  toJSON o = object $ catMaybes
      [ Just $ "account" .= _txBuilder_accountName o
      , Just $ "chain" .= _txBuilder_chainId o
      -- , ("keyset" .=) <$> _txBuilder_keyset o
      , _txBuilder_keyset o <&> \case
          Left ksName -> "keyset-ref" .= ksName
          Right ks -> "keyset" .= ks
      ]

instance FromJSON TxBuilder where
  parseJSON = withObject "TxBuilder" $ \o -> TxBuilder
    <$> o .: "account"
    <*> o .: "chain"
    <*> (fmap (fmap Right) (o .:? "keyset") <|> fmap (fmap Left) (o .:? "keyset-ref"))
    -- <*> o .:? "keyset"

prettyTxBuilder :: TxBuilder -> Text
prettyTxBuilder = LT.toStrict . LTB.toLazyText . AesonPretty.encodePrettyToTextBuilder
