{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Common.Orphans where

import Data.Aeson (FromJSONKey, ToJSONKey)
import qualified Pact.Types.ChainId as Pact

deriving instance Ord Pact.ChainId
deriving instance ToJSONKey Pact.ChainId
deriving instance FromJSONKey Pact.ChainId
