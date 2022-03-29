{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module WalletConnect.Common where


import qualified Data.Aeson as A
import qualified Data.List.Split                       as L
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)
import           Language.Javascript.JSaddle

type Account = Text
type Chain = Text
type Method = Text
type Topic = Text
type PublicKey = Text

data Metadata = Metadata
  { _metadata_name :: Text
  , _metadata_url :: Text
  , _metadata_icons :: [Text]
  , _metadata_description :: Text
  }
  deriving (Show, Generic)

data Permissions = Permissions
  { _permissions_chains :: [Text]
  , _permissions_methods :: [Text]
  }
  deriving (Show, Generic)

data Request = Request
  { _request_chainId :: Chain
  , _request_method :: Method
  , _request_params :: A.Value
  }

data Pairing = Pairing
  { _pairing_topic :: Topic
  , _pairing_peer :: (PublicKey, Metadata)
  , _pairing_permissions :: Permissions
  , _pairing_connect :: (Permissions, Metadata) -> JSM ()
  , _pairing_delete :: JSM ()
  }

type PairingURI = Text

instance A.ToJSON Metadata where
  toJSON = A.genericToJSON compactEncoding
  toEncoding = A.genericToEncoding compactEncoding

instance A.FromJSON Metadata where
  parseJSON = A.genericParseJSON compactEncoding

instance A.ToJSON Permissions where
  toJSON (Permissions chains methods) =
    A.object [ "blockchain" A..= (A.object ["chains" A..= chains])
             , "jsonrpc" A..= A.object ["methods" A..= methods]]

instance A.FromJSON Permissions where
  parseJSON = A.withObject "Permissions" $ \v -> Permissions
        <$> (v A..: "blockchain" >>= A.withObject "blockchain" (\c -> c A..: "chains"))
        <*> (v A..: "jsonrpc" >>= A.withObject "jsonrpc" (\c -> c A..: "methods"))

compactEncoding :: A.Options
compactEncoding = A.defaultOptions
    { A.fieldLabelModifier = shortener
    , A.allNullaryToStringTag = True
    , A.constructorTagModifier = shortener
    , A.omitNothingFields = True
    , A.sumEncoding = A.ObjectWithSingleField
    , A.unwrapUnaryRecords = True
    , A.tagSingleConstructors = False
    }
  where
    -- As long as names are not empty or just underscores this head should be fine:
    shortener = head . reverse . filter (/= "") . L.splitOn "_"
