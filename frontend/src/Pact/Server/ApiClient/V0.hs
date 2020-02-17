module Pact.Server.ApiClient.V0
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
  , _commandLog_url :: Text
  , _commandLog_command :: Command Text
  } deriving (Eq, Show)

instance ToJSON CommandLog where
  toJSON = encodeCommandLog

instance FromJSON CommandLog where
  parseJSON = decodeForVersion "CommandLog" versionNumber parseCommandLog

versionNumber :: Int
versionNumber = 0

parseCommandLog :: Aeson.Object -> Aeson.Parser CommandLog
parseCommandLog o = do
  timestamp <- o Aeson..: "timestamp"
  url <- o Aeson..: "url"
  command <- o Aeson..: "command"
  pure $ CommandLog
    { _commandLog_timestamp = timestamp
    , _commandLog_url = url
    , _commandLog_command = command
    }

encodeCommandLog :: CommandLog -> Aeson.Value
encodeCommandLog cl = Aeson.object
  [ "version" Aeson..= versionNumber
  , "timestamp" Aeson..= _commandLog_timestamp cl
  , "url" Aeson..= _commandLog_url cl
  , "command" Aeson..= _commandLog_command cl
  ]

