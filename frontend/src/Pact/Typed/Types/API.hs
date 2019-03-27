{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- Workaround for untyped and unparsable Pact API. It should get properly fixed in Pact at some point, see:
-- https://github.com/kadena-io/pact/pull/375
-- Note: As those types are not shared with pact, we don't really gain much*)
-- safety/guarantees by those types - it just makes handling of the API easier. At
-- some point the above mentioned PR or something similar should really get merged.

--  *) not much, but not nothing, as some types are shared.

-- |
-- Module      :  Pact.Types.API
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Pact REST API types.
--

module Pact.Typed.Types.API
  ( RequestKeys(..), rkRequestKeys
  , SubmitBatch(..), sbCmds
  , Poll(..)
  , PollResponses(..)
  , ListenerRequest(..)
  , ApiResult(..), arMetaData, arResult, arTxId
  ) where

import Data.Aeson hiding (Success)
import Control.Lens hiding ((.=))
import GHC.Generics
import qualified Data.HashMap.Strict as HM
import Control.Arrow
import Control.Monad

import Pact.Typed.Types.Command
import Pact.Types.Util
import Pact.Types.Runtime

import Pact.Types.API hiding (ApiResult (..), arMetaData, arResult, arTxId, PollResponses (..))

data ApiResult = ApiResult {
  _arResult :: !CommandValue,
  _arTxId :: !(Maybe TxId),
  _arMetaData :: !(Maybe Value)
  } deriving (Eq,Show,Generic)
makeLenses ''ApiResult
instance FromJSON ApiResult where parseJSON = lensyParseJSON 3
instance ToJSON ApiResult where toJSON = lensyToJSON 3

-- | What you get back from a Poll
newtype PollResponses = PollResponses (HM.HashMap RequestKey ApiResult)
  deriving (Eq, Show)

instance ToJSON PollResponses where
  toJSON (PollResponses m) = object $ map (requestKeyToB16Text *** toJSON) $ HM.toList m

instance FromJSON PollResponses where
  parseJSON = withObject "PollResponses" $ \o ->
    (PollResponses . HM.fromList <$> forM (HM.toList o)
      (\(k,v) -> (,) <$> parseJSON (String k) <*> parseJSON v))

