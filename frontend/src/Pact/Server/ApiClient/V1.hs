module Pact.Server.ApiClient.V1
  ( CommandLog (..)
  , parseCommandLog
  , encodeCommandLog
  , versionNumber
  , exportHeader
  , exportCommandLog
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Map as Map

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import qualified Data.Csv as Csv
import qualified Data.Csv.Builder as Csv

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSL

import Pact.Types.ChainId (ChainId)
import Pact.Types.Command (Command (..), RequestKey (..))
import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Util as Pact
import qualified Pact.Types.Hash as Pact
import qualified Pact.Types.ChainId as Pact
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

exportHeader :: Csv.Header
exportHeader = Csv.header
  [ "index"
  , "timestamp"
  , "sender"
  , "chain"
  , "requestKey"
  , "url"
  , "command_payload"
  , "command_sigs"
  , "command_hash"
  ]

exportCommandLog :: Int -> CommandLog -> BSL.Builder
exportCommandLog i cl = Csv.encodeNamedRecord exportHeader $ Map.fromList
  [ ("index" :: BSL.ByteString, Pact.tShow i)
  , ("timestamp", Pact.tShow $ _commandLog_timestamp cl)
  , ("sender", _commandLog_sender cl)
  , ("chain", Pact._chainId $ _commandLog_chain cl)
  , ("requestKey", Pact.hashToText $ Pact.unRequestKey $ _commandLog_requestKey cl)
  , ("url", _commandLog_url cl)
  , ("command_payload", Pact._cmdPayload $ _commandLog_command cl)
  , ("command_sigs", Pact.tShow $ fmap Pact._usSig $ Pact._cmdSigs $ _commandLog_command cl)
  , ("command_hash", Pact.tShow $ Pact._cmdHash $ _commandLog_command cl)
  ]
