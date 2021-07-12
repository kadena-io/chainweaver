{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Crypto.CommonBIP where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Aeson.GADT.TH
import Data.ByteString (ByteString)
import Data.Constraint.Extras.TH
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Universe.Some.TH

import Frontend.Crypto.Ed25519

-- Right now we have a restriction where this BIPStorage
-- can never actually change.
--
-- This feels like a good thing, because our frontend schema is pretty
-- locked into the Crypto key type not being upgradeable, and we really
-- want to store all of the frontend state in frontend. This spot for
-- the root key should be a special case and stay static, I hope.
--
-- I think that we should wait till we figure out what we want from web
-- and whether we can simplify the split before we do anything too crazy here.
-- Hopefully that happens before we have to modify this storage!
data BIPStorage a where
  BIPStorage_RootKey :: BIPStorage PrivateKey
deriving instance Show (BIPStorage a)

concat <$> traverse ($ ''BIPStorage)
  [ deriveGShow
  , deriveGEq
  , deriveGCompare
  , deriveUniverseSome
  , deriveArgDict
  , deriveJSONGADT
  ]
