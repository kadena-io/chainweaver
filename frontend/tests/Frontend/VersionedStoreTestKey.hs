{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend.VersionedStoreTestKey where

import Data.Aeson (ToJSON(..), FromJSON(..), encode, decode)
import Data.Aeson.GADT.TH
import Data.GADT.Show.TH
import Data.GADT.Compare.TH
import Data.Constraint.Extras.TH
import Data.Text (Text)
import Data.Universe.Some.TH

import Frontend.Storage (StoreKeyMetaPrefix(..))

data StoreTestKey a where
  StoreString :: StoreTestKey String
  StoreInt :: StoreTestKey Int

deriving instance Show (StoreTestKey a)

storeTestKeyMetaPrefixText :: Text
storeTestKeyMetaPrefixText = "StoreTestKey_Meta"

storeTestKeyMetaPrefix :: StoreKeyMetaPrefix
storeTestKeyMetaPrefix = StoreKeyMetaPrefix "StoreTestKey_Meta"

concat <$> traverse ($ ''StoreTestKey)
  [ deriveGShow
  , deriveGEq
  , deriveGCompare
  , deriveUniverseSome
  , deriveArgDict
  , deriveJSONGADT
  ]
