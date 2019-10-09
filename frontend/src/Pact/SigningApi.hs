{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Pact.SigningApi
  ( SigningApi
  , SigningRequest(..)
  , SigningResponse(..)
  , swaggerDocs
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.String.Conv (toS)
import Data.Proxy
import Data.Swagger
import Data.Text (Text)
import GHC.Generics
import Servant.API
import Servant.Swagger
import System.Directory
import System.IO.Extra

import Pact.Parse (ParsedInteger)
import Pact.Types.ChainMeta (TTLSeconds(..))
import Pact.Types.Command (Command)
import qualified Pact.Types.Command as P
import qualified Pact.Types.Hash as P
import Pact.Types.Runtime (GasLimit(..))

import Common.Foundation (compactEncoding)

type SigningApi = "v1" :> V1SigningApi
type V1SigningApi = "sign" :> ReqBody '[JSON] SigningRequest :> Post '[JSON] SigningResponse

swaggerPath, swaggerFile :: String
swaggerPath = "backend/swagger/"
swaggerFile = "swagger.txt"

data SigningRequest = SigningRequest
  { _signingRequest_code :: Text
  , _signingRequest_data :: Maybe Aeson.Object
  , _signingRequest_nonce :: Maybe Text
  , _signingRequest_chainId :: Maybe Text
  , _signingRequest_gasLimit :: Maybe GasLimit
  , _signingRequest_ttl :: Maybe TTLSeconds
  , _signingRequest_sender :: Maybe Text
  } deriving Generic

instance ToSchema SigningRequest

instance Aeson.ToJSON SigningRequest where
  toJSON = Aeson.genericToJSON compactEncoding
  toEncoding = Aeson.genericToEncoding compactEncoding

instance Aeson.FromJSON SigningRequest where
  parseJSON = Aeson.genericParseJSON compactEncoding

data SigningResponse = SigningResponse
  { _signingResponse_body :: Command Text
  , _signingResponse_chainId :: Text
  } deriving (Eq, Show, Generic)

instance ToSchema SigningResponse

instance Aeson.ToJSON SigningResponse where
  toJSON = Aeson.genericToJSON compactEncoding
  toEncoding = Aeson.genericToEncoding compactEncoding

instance Aeson.FromJSON SigningResponse where
  parseJSON = Aeson.genericParseJSON compactEncoding

swaggerDocs :: IO ()
swaggerDocs = do
  let bsl = BSL.toStrict $ Aeson.encode $ toSwagger (Proxy :: Proxy SigningApi)
  createDirectoryIfMissing True swaggerPath
  writeFileUTF8 (swaggerPath ++ swaggerFile) $ toS bsl

-- Move these to Pact...
instance ToSchema (P.TypedHash 'P.Blake2b_256)  where
  declareNamedSchema _ = return $ NamedSchema (Just "TypedHash") $ byteSchema

instance ToSchema P.UserSig
instance ToSchema a => ToSchema (Command a)
instance ToSchema GasLimit
instance ToSchema ParsedInteger
instance ToSchema TTLSeconds
