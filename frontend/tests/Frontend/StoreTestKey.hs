{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend.StoreTestKey where

import Data.Aeson (ToJSON(..), FromJSON(..), encode, decode)
import Data.Aeson.GADT.TH
import Data.GADT.Show.TH
import Data.GADT.Compare.TH
import Data.Constraint.Extras.TH
import Data.Universe.Some.TH

data StoreTestKey a where
  StoreString :: StoreTestKey String
  StoreInt :: StoreTestKey Int

deriving instance Show (StoreTestKey a)

concat <$> traverse ($ ''StoreTestKey)
  [ deriveGShow
  , deriveGEq
  , deriveGCompare
  , deriveUniverseSome
  , deriveArgDict
  , deriveJSONGADT
  ]
