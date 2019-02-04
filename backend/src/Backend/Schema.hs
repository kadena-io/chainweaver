{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Database schema for persisting user data.
module Backend.Schema
  ( Secret
  , generateSecret
  , Blob (..)
  ) where

import           Control.Error.Safe         (rightZ)
import           Data.Aeson                 (FromJSON, ToJSON (..),
                                             Value (String), parseJSON, toJSON)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Base64     as Base64
import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Database.Groundhog.Core
import           Database.Groundhog.Generic
import           Database.Groundhog.TH      (groundhog)
import           GHC.Generics               (Generic)

import           Rhyolite.Backend.Schema.TH (makeDefaultKeyIdInt64,
                                             mkRhyolitePersist)
import           Rhyolite.Schema            (HasId (..), Id, Json (..),
                                             SchemaName (..))

import Backend.Env

-- | Some large random value that can be used for securely referencing some
-- information.
newtype Secret = Secret ByteString
  deriving (Generic, Show, Read, Ord, Eq)

-- | Length of a generated `Secret` in bytes.
secretLength :: Int
secretLength = 16

instance FromJSON Secret where
  parseJSON (String t) = Secret <$> (rightZ . Base64.decode . encodeUtf8 $ t)
  parseJSON _          = fail "Expecting a string when parsing a secret."

instance ToJSON Secret where
  toJSON (Secret bs) = String . decodeUtf8 . Base64.encode $ bs
  toEncoding (Secret bs) = toEncoding $ (decodeUtf8 . Base64.encode) bs


instance PrimitivePersistField Secret where
  toPrimitivePersistValue p (Secret x) = toPrimitivePersistValue p x
  fromPrimitivePersistValue p v = Secret $ fromPrimitivePersistValue p v

instance PersistField Secret


generateSecret :: MonadServer m => m Secret
generateSecret = Secret <$> genRandomBytes secretLength

-- | A blob in spirit of a git blob: Just the file contents,
--
--   without its name. What we do add though is a secret identifier that is
--   hard to guess. This way we can provide a link to a blob that can be
--   shared. We don't use the hashed content or something for the reference as
--   blobs are mutable and the reference should stay the same on mutations.
data Blob = Blob
  { _blob_reference :: !Secret
    -- ^ Some random hard to guess number that can be used for referencing a
    -- blob in a secure way.
  , _blob_content   :: !Text
    -- ^ The actual contents of the blob.
  }
  deriving (Eq, Show, Generic)

instance HasId Blob



mkRhyolitePersist (Just "migrateSchema") [groundhog|
  - entity: Blob
    constructors:
      - name: Blob
        uniques:
          - name: _blob_reference
            type: constraint
            fields: [_blob_reference]
  |]
