module Frontend.Store.V0.Wallet where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(Null), object, (.=), (.:), withObject, (.:?), withText)
import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import Data.Maybe (catMaybes)
import Data.IntMap (IntMap)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)

-- WARNING: Be careful about changing stuff in here. Tests will catch snafus here and upstream though
import Common.Wallet (UnfinishedCrossChainTransfer, AccountBalance)
import Common.Network (NetworkName)

-- This is lifted from the wallet code prior to V1

newtype AccountName = AccountName { unAccountName :: Text } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)
newtype ChainId = ChainId { unChainId :: Text } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)
newtype AccountNotes = AccountNotes { unAccountNotes :: Text } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

newtype PublicKey = PublicKey { unPublicKey :: ByteString } deriving (Eq, Ord, Show)

instance ToJSON PublicKey where
  toJSON = toJSON . T.decodeUtf8 . Base16.encode . unPublicKey

-- Checks that the publickey is indeed base 16
instance FromJSON PublicKey where
  parseJSON = withText "PublicKey" $ \t ->
    case (Base16.decode . T.encodeUtf8 $ t) of
      (bs, "") -> pure (PublicKey bs)
      _ -> fail "Was not a base16 string"

type Accounts key = IntMap (SomeAccount key)

data Account key = Account
  { _account_name :: AccountName
  , _account_key :: KeyPair key
  , _account_chainId :: ChainId
  , _account_network :: NetworkName
  , _account_notes :: AccountNotes
  , _account_balance :: Maybe AccountBalance
  -- ^ We also treat this as proof of the account's existence.
  , _account_unfinishedCrossChainTransfer :: Maybe UnfinishedCrossChainTransfer
  }

instance ToJSON key => ToJSON (Account key) where
  toJSON a = object $ catMaybes
    [ Just $ "name" .= _account_name a
    , Just $ "key" .= _account_key a
    , Just $ "chain" .= _account_chainId a
    , Just $ "network" .= _account_network a
    , Just $ "notes" .= _account_notes a
    , ("balance" .=) <$> _account_balance a
    , ("unfinishedCrossChainTransfer" .=) <$> _account_unfinishedCrossChainTransfer a
    ]

instance FromJSON key => FromJSON (Account key) where
  parseJSON = withObject "Account" $ \o -> do
    name <- o .: "name"
    key <- o .: "key"
    chain <- o .: "chain"
    network <- o .: "network"
    notes <- o .: "notes"
    balance <- o .:? "balance"
    unfinishedCrossChainTransfer <- o .:? "unfinishedCrossChainTransfer"
    pure $ Account
      { _account_name = name
      , _account_key = key
      , _account_chainId = chain
      , _account_network = network
      , _account_notes = notes
      , _account_balance = balance
      , _account_unfinishedCrossChainTransfer = unfinishedCrossChainTransfer
      }

data KeyPair key = KeyPair
  { _keyPair_publicKey  :: PublicKey
  , _keyPair_privateKey :: Maybe key
  } deriving Generic

instance ToJSON key => ToJSON (KeyPair key) where
  toJSON p = object
    [ "public" .= _keyPair_publicKey p
    , "private" .= _keyPair_privateKey p
    ]

instance FromJSON key => FromJSON (KeyPair key) where
  parseJSON = withObject "KeyPair" $ \o -> do
    public <- o .: "public"
    private <- o .: "private"
    pure $ KeyPair
      { _keyPair_publicKey = public
      , _keyPair_privateKey = private
      }

-- | We keep track of deletions at a given index so that we don't regenerate
-- keys with BIP32.
data SomeAccount key
  = SomeAccount_Deleted
  | SomeAccount_Inflight (Account key)
  | SomeAccount_Account (Account key)

someAccount :: a -> (Account key -> a) -> SomeAccount key -> a
someAccount a _ SomeAccount_Deleted = a
someAccount a _ (SomeAccount_Inflight _) = a
someAccount _ f (SomeAccount_Account a) = f a

someAccountWithInflight :: a -> (Account key -> a) -> SomeAccount key -> a
someAccountWithInflight _ f (SomeAccount_Inflight a) = f a
someAccountWithInflight a f sa = someAccount a f sa

instance ToJSON key => ToJSON (SomeAccount key) where
  toJSON = \case
    SomeAccount_Deleted -> Null
    SomeAccount_Inflight a -> object ["inflight" .= toJSON a]
    SomeAccount_Account a -> toJSON a

instance FromJSON key => FromJSON (SomeAccount key) where
  parseJSON Null = pure SomeAccount_Deleted
  parseJSON x = (SomeAccount_Inflight <$> (withObject "Inflight Acc" (.: "inflight") x))
    <|> SomeAccount_Account <$> parseJSON x

