{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- Limit API to the parts that are common to chainweb and `pact -s`.
module Pact.Server.ApiV1Client
  ( ApiV1API
  , ApiV1Client(..)
  , apiV1Client
  , CommandLog(..)
  , commandLogCurrentVersion
  , HasTransactionLogger(..)
  , TransactionLoggerT(..)
  , runTransactionLoggerT
  , logTransactionStdout
  , logTransactionFile
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
import qualified Data.Text.Lazy.IO as LT
import qualified Control.Monad.Reader as Reader

type TransactionLogger = CommandLog -> IO ()

data ApiV1Client m = ApiV1Client
  { send :: TransactionLogger -> SubmitBatch -> m RequestKeys
  , poll :: Poll -> m PollResponses
  , listen :: ListenerRequest -> m ListenResponse
  , local :: Command Text -> m (CommandResult Hash)
  }

{- apiV1API :: Proxy ApiV1API -}
{- apiV1API = Proxy -}
apiV1Client :: forall m. (MonadIO m, MonadReader ClientEnv m, RunClient m) => ApiV1Client m
apiV1Client = ApiV1Client
  { send = \appendLog batch@(SubmitBatch commands) -> do
    url <- asks baseUrl
    timestamp <- liftIO getCurrentTime
    for_ commands $ \command -> liftIO $ appendLog $ CommandLog
      { _commandLog_command = command
      , _commandLog_timestamp = timestamp
      , _commandLog_url = T.pack $ showBaseUrl url
      }
    sendF batch
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

class HasTransactionLogger m where
  askTransactionLogger :: m TransactionLogger

instance (HasTransactionLogger m, Monad m) => HasTransactionLogger (RoutedT t r m) where
  askTransactionLogger = lift askTransactionLogger

instance (HasTransactionLogger m, Monad m) => HasTransactionLogger (ReaderT r m) where
  askTransactionLogger = lift askTransactionLogger

newtype TransactionLoggerT m a = TransactionLoggerT
  { unTransactionLoggerT :: ReaderT TransactionLogger m a
  } deriving
    ( Functor, Applicative, Monad, MonadTrans
    , MonadFix, MonadIO, MonadRef, MonadAtomicRef
    , DomBuilder t, NotReady t, MonadHold t, MonadSample t
    , TriggerEvent t, PostBuild t, HasJS x
    , MonadReflexCreateTrigger t, MonadQuery t q, Requester t
    , HasDocument
    , Routed t r, RouteToUrl r, SetRoute t r, EventWriter t w
    , DomRenderHook t
    , HasConfigs
    )

instance PerformEvent t m => PerformEvent t (TransactionLoggerT m) where
  type Performable (TransactionLoggerT m) = TransactionLoggerT (Performable m)
  performEvent_ e = do
    logger <- askTransactionLogger
    lift . performEvent_ $ flip runTransactionLoggerT logger <$> e
  performEvent e = do
    logger <- askTransactionLogger
    lift . performEvent $ flip runTransactionLoggerT logger <$> e

instance PrimMonad m => PrimMonad (TransactionLoggerT m) where
  type PrimState (TransactionLoggerT m) = PrimState m
  primitive = lift . primitive

instance HasJSContext m => HasJSContext (TransactionLoggerT m) where
  type JSContextPhantom (TransactionLoggerT m) = JSContextPhantom m
  askJSContext = TransactionLoggerT askJSContext
#if !defined(ghcjs_HOST_OS)
instance MonadJSM m => MonadJSM (TransactionLoggerT m)
#endif

instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (TransactionLoggerT m) where
  runWithReplace a0 a' = TransactionLoggerT $ runWithReplace (unTransactionLoggerT a0) (fmapCheap unTransactionLoggerT a')
  traverseDMapWithKeyWithAdjust f dm0 dm' = TransactionLoggerT $ traverseDMapWithKeyWithAdjust (coerce . f) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = TransactionLoggerT $ traverseDMapWithKeyWithAdjustWithMove (coerce . f) dm0 dm'
  traverseIntMapWithKeyWithAdjust f im0 im' = TransactionLoggerT $ traverseIntMapWithKeyWithAdjust (coerce f) im0 im'

instance (Prerender js t m, Reflex t, Monad m) => Prerender js t (TransactionLoggerT m) where
  type Client (TransactionLoggerT m) = TransactionLoggerT (Client m)
  prerender a b = do
    l <- askTransactionLogger
    lift $ prerender (runTransactionLoggerT a l) (runTransactionLoggerT b l)

instance (Monad m, RunClient m) => RunClient (TransactionLoggerT m) where
  runRequest = lift . runRequest
  throwClientError = lift . throwClientError

instance MonadReader r m => MonadReader r (TransactionLoggerT m) where
  ask = lift ask
  local g t = do
    l <- askTransactionLogger
    lift $ Reader.local g (runTransactionLoggerT t l)

instance Monad m => HasTransactionLogger (TransactionLoggerT m) where
  askTransactionLogger = TransactionLoggerT ask

runTransactionLoggerT :: TransactionLoggerT m a -> TransactionLogger -> m a
runTransactionLoggerT = runReaderT . unTransactionLoggerT

logTransactionStdout :: TransactionLogger
logTransactionStdout = LT.putStrLn . Aeson.encodeToLazyText

logTransactionFile :: FilePath -> CommandLog -> IO ()
logTransactionFile f = LT.appendFile f . (<> "\n") . Aeson.encodeToLazyText
