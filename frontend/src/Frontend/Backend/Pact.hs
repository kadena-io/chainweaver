{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}

-- | Pact types adjusted to work with ghcjs.
--
--   This module is used for interfacing with the Pact server. On ghc it just
--   re-exports Pact modules, on ghcjs if provides compatible definitions for
--   used types.
module Frontend.Backend.Pact
  ( RequestKey
  , ApiResponse (..)
  , RequestKeys (..)
  , Hash (..)
  , hash
  ) where


import           Pact.Types.Hash            (hash)
import           Pact.Types.Util



#if !defined(ghcjs_HOST_OS)
{- #if 0 -}

import           Pact.Types.API             (ApiResponse (..), RequestKeys (..))
import           Pact.Types.Command         (RequestKey)

#else

import           Control.Concurrent
import           Control.Lens               hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Char
import           Data.Text                  (Text, pack)
import           Data.Text.Encoding
import           GHC.Generics

import           Pact.Types.Util            (ParseText (..), lensyParseJSON,
                                             lensyToJSON)

data ApiResponse a =
  ApiSuccess
    { _apiResponse :: !a} |
  ApiFailure
    { _apiError :: !String}
  deriving (Show, Eq, Generic)
makeLenses ''ApiResponse

instance ToJSON a => ToJSON (ApiResponse a) where
  toJSON (ApiSuccess a)= object [ "status" .= String "success", "response" .= a]
  toJSON (ApiFailure a)= object [ "status" .= String "failure", "error" .= a]

instance FromJSON a => FromJSON (ApiResponse a) where
  parseJSON (Object o) = do
    st <- o .: "status"
    if st == String "success"
      then ApiSuccess <$> o .: "response"
      else ApiFailure <$> o .: "error"
  parseJSON _ = mempty

newtype RequestKey = RequestKey { unRequestKey :: Hash}
  deriving (Eq, Ord, Generic, FromJSON, ToJSON, Show, ParseText)

data RequestKeys = RequestKeys
  { _rkRequestKeys :: ![RequestKey]
  } deriving (Show, Eq, Ord, Generic)
makeLenses ''RequestKeys

instance ToJSON RequestKeys where
  toJSON = lensyToJSON 3
instance FromJSON RequestKeys where
  parseJSON = lensyParseJSON 3



#endif


