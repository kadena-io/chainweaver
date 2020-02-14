module Pact.Server.ApiClient.V1
  ( CommandLog (..)
  , parseCommandLog
  , encodeCommandLog
  , versionNumber
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import Pact.Types.Command
import Pact.Types.ChainId (ChainId)
import Pact.Server.ApiClient.Helpers (decodeForVersion)

data CommandLog = CommandLog
  { _commandLog_timestamp :: UTCTime
  , _commandLog_sender :: Text
  , _commandLog_chain :: ChainId
  , _commandLog_requestKey :: RequestKey
  , _commandLog_url :: Text
  , _commandLog_command :: Command Text
  } deriving (Eq, Show)

instance ToJSON CommandLog where
  toJSON = encodeCommandLog

instance FromJSON CommandLog where
  parseJSON = decodeForVersion "CommandLog" versionNumber parseCommandLog

versionNumber :: Int
versionNumber = 1

encodeCommandLog :: CommandLog -> Aeson.Value
encodeCommandLog cl = Aeson.object
  [ "version" Aeson..= versionNumber
  , "sender" Aeson..= _commandLog_sender cl
  , "chain" Aeson..= _commandLog_chain cl
  , "request_key" Aeson..= _commandLog_requestKey cl
  , "timestamp" Aeson..= _commandLog_timestamp cl
  , "url" Aeson..= _commandLog_url cl
  , "command" Aeson..= _commandLog_command cl
  ]

parseCommandLog :: Aeson.Object -> Aeson.Parser CommandLog
parseCommandLog o = CommandLog
  <$> o Aeson..: "timestamp"
  <*> o Aeson..: "sender"
  <*> o Aeson..: "chain"
  <*> o Aeson..: "request_key"
  <*> o Aeson..: "url"
  <*> o Aeson..: "command"
