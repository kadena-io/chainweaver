{-# OPTIONS_GHC -fno-warn-orphans#-}
module Pact.Server.Orphans where

import Data.Text (Text)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Text as Aeson

import qualified Pact.Types.Hash as Pact
import qualified Pact.Types.Util as Pact

import Pact.Types.Command (Command,RequestKey)
import qualified Pact.Types.Command as Pact
import Pact.Types.ChainId (ChainId)
import qualified Pact.Types.ChainId as Pact

import Database.SQLite.Simple.FromField (FromField (..), returnError)
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple (FromRow (..), ToRow (..))
import qualified Database.SQLite.Simple as Sql

-- TODO : Find a better place for this to live
instance FromField (Command Text) where
  fromField f = fromField f >>= \bs -> case Aeson.eitherDecode' bs of
    Left err -> returnError Sql.ConversionFailed f err
    Right cmdtext -> Ok cmdtext

instance FromField RequestKey where
  fromField = fmap (Pact.RequestKey . Pact.pactHash) . fromField

instance FromField ChainId where
  fromField = fmap Pact.ChainId . fromField
