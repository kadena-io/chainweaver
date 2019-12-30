{-# LANGUAGE DeriveGeneric #-}
module Common.GistStore where

import Generics.Deriving.Monoid    (mappenddefault)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)

-- | Meta data about a gist. (Like name and description)
data GistMeta = GistMeta
  { _gistMeta_fileName    :: Text
    -- ^ What filename to use in the gist.
  , _gistMeta_description :: Text
    -- ^ The description of the gist to create.
  }
  deriving (Show, Generic)

-- TODO Remove these derived instances
instance ToJSON GistMeta
instance FromJSON GistMeta
instance Semigroup GistMeta where
  (<>) = mappenddefault
