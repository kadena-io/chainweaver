{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Common.Orphans where

import qualified Pact.Types.ChainId as Pact

deriving instance Ord Pact.ChainId
