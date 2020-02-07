{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

-- Limit API to the parts that are common to chainweb and `pact -s`.
module Pact.Server.ApiClient.V1
  ( ApiV1API
  , ApiV1Client(..)
  , apiV1Client
  , CommandLog(..)
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Reader hiding (local)
import Control.Monad.Ref
import Data.Aeson (ToJSON, FromJSON)
import Data.Coerce
import Data.Foldable (for_)
import Data.Proxy
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Language.Javascript.JSaddle (MonadJSM)
import Obelisk.Configs
import Obelisk.Route.Frontend
import Pact.Server.API
import Pact.Types.API
import Pact.Types.Command
import Pact.Types.Hash (Hash)
import Reflex.Dom.Core
import Reflex.Host.Class (MonadReflexCreateTrigger)
import Servant.API
import Servant.Client.Core hiding (Client)
import Servant.Client.JSaddle hiding (Client)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy    as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.List.NonEmpty as NE
import qualified Control.Monad.Reader as Reader

import Database.SQLite.Simple (FromRow (..), ToRow (..))
import qualified Database.SQLite.Simple as Sql
import Database.SQLite.SimpleErrors (runDBAction)
import Database.SQLite.SimpleErrors.Types (SQLiteResponse)
import Database.SQLite.Simple.QQ (sql)

import Data.Pool (Pool)
import qualified Data.Pool as Pool

import qualified Pact.Types.Hash as Pact
import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Util as Pact
import Pact.Types.ChainId (ChainId)
import qualified Pact.Types.ChainId as Pact

import Pact.Server.Orphans ()

data ApiV1Client m = ApiV1Client
  { send :: TransactionLogger -> Text -> ChainId -> SubmitBatch -> m RequestKeys
  , poll :: Poll -> m PollResponses
  , listen :: ListenerRequest -> m ListenResponse
  , local :: Command Text -> m (CommandResult Hash)
  }

{- apiV1API :: Proxy ApiV1API -}
{- apiV1API = Proxy -}
apiV1Client :: forall m. (MonadIO m, MonadReader ClientEnv m, RunClient m) => ApiV1Client m
apiV1Client = ApiV1Client
  { send = \txnLogger sender chain batch@(SubmitBatch commands) -> do
      url <- asks baseUrl
      timestamp <- liftIO getCurrentTime
      rqkeys <- sendF batch

      for_ commands $ \command -> liftIO $ _transactionLogger_appendLog txnLogger $ CommandLog
        { _commandLog_command = command
        , _commandLog_sender = sender
        , _commandLog_chain = chain
        , _commandLog_request_key = NE.head $ _rkRequestKeys rqkeys
        , _commandLog_timestamp = timestamp
        , _commandLog_url = T.pack $ showBaseUrl url
        }

      pure rqkeys
  , poll = pollF
  , listen = listenF
  , local = localF
  }
  where
    sendF :<|> pollF :<|> listenF :<|> localF = clientIn apiV1API (Proxy :: Proxy m)

-- | Commands are logged with the time they were sent and the node URL they were
-- sent to. We only define 'toJSON' for the current version, thus only write
-- logs in the latest version, but we allow parsing older versions automatically
-- with 'commandLogParsers'.
data CommandLog = CommandLog
  { _commandLog_timestamp :: UTCTime
  , _commandLog_url :: Text
  , _commandLog_command :: Command Text
  } deriving (Eq, Show)

instance ToJSON CommandLog where
  toJSON cl = Aeson.object
    [ "version" Aeson..= commandLogCurrentVersion
    , "timestamp" Aeson..= _commandLog_timestamp cl
    , "url" Aeson..= _commandLog_url cl
    , "command" Aeson..= _commandLog_command cl
    ]

instance FromJSON CommandLog where
  parseJSON = Aeson.withObject "CommandLog" $ \o -> do
    version <- o Aeson..: "version"
    case commandLogParsers ^? ix version of
      Nothing -> fail $ "Unexpected version: " <> show version
      Just parse -> parse o

commandLogCurrentVersion :: Int
commandLogCurrentVersion = pred $ length commandLogParsers

-- | Add newer versions to the end of this list
commandLogParsers :: [Aeson.Object -> Aeson.Parser CommandLog]
commandLogParsers =
  [ parseCommandLogV0
  ]

parseCommandLogV0 :: Aeson.Object -> Aeson.Parser CommandLog
parseCommandLogV0 o = do
  timestamp <- o Aeson..: "timestamp"
  url <- o Aeson..: "url"
  command <- o Aeson..: "command"
  pure $ CommandLog
    { _commandLog_timestamp = timestamp
    , _commandLog_url = url
    , _commandLog_command = command
    }

