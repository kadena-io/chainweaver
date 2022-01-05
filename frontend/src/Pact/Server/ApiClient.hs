{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

-- Limit API to the parts that are common to chainweb and `pact -s`.
module Pact.Server.ApiClient
  ( -- * Named V1 from Pact
    ApiV1API
  , ApiClient(..)
  , apiV1Client

    -- * Parent structure for log entries
  , LogEntry (..)
  , WalletEvent (..)

    -- * Command log structures, versioned in submodules
  , CommandLog(..)
  , commandLogCurrentVersion
  , commandLogFilename

    -- * Transaction specific logging impl
  , HasTransactionLogger(..)
  , TransactionLoggerT(..)
  , TransactionLogger (..)
  , runTransactionLoggerT
  , logTransactionStdout
  , logTransactionFile
  , noLogger
  ) where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Reader hiding (local)
import Control.Monad.Except
import Control.Exception (try, displayException, catch)
import Control.Monad.Ref
import Data.Coerce
import Data.Foldable (for_)
import Data.Proxy
import Data.Text (Text)
import Data.Time (UTCTime, defaultTimeLocale, getCurrentTime)
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

import Data.Aeson (ToJSON (..), FromJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.List.NonEmpty as NE
import qualified Control.Monad.Reader as Reader

import qualified Data.Map as Map
import qualified Data.Csv.Builder as Csv

import Text.Printf (printf)

import Data.Time (TimeLocale, formatTime, iso8601DateFormat)
import qualified System.Directory as Dir
import qualified System.FilePath as File

import qualified Pact.Types.Util as Pact
import Pact.Types.ChainId (ChainId)

import Pact.Server.ApiClient.V1 (CommandLog (..))
import qualified Pact.Server.ApiClient.V1 as Latest

data WalletEvent
  = WalletEvent_Import
  | WalletEvent_Export
  deriving (Eq, Show)

instance FromJSON WalletEvent where
  parseJSON = Aeson.withText "WalletEvent" $ \case
    "import" -> pure WalletEvent_Import
    "export" -> pure WalletEvent_Export
    x -> fail $ "Unknown WalletEvent value: " <> T.unpack x

instance ToJSON WalletEvent where
  toJSON WalletEvent_Import = Aeson.String "import"
  toJSON WalletEvent_Export = Aeson.String "export"

data LogEntry
  = LogEntry_Cmd Latest.CommandLog
  | LogEntry_Event
    { _logEntry_eventType :: WalletEvent
    , _logEntry_zeroKey :: Text
    , _logEntry_timestamp :: UTCTime
    }

encodeLogEntryEvent :: WalletEvent -> Text -> UTCTime -> Aeson.Value
encodeLogEntryEvent etype sender timestamp = Aeson.object
  [ "event_type" Aeson..= etype
  , "sender" Aeson..= sender
  , "timestamp" Aeson..= timestamp
  ]

instance FromJSON LogEntry where
  parseJSON v = (LogEntry_Cmd <$> parseJSON v) <|> parseLogEvent v
    where
      parseLogEvent = Aeson.withObject "LogEntry_Event" $ \o -> LogEntry_Event
        <$> (o Aeson..: "event_type")
        <*> (o Aeson..: "sender")
        <*> (o Aeson..: "timestamp")

instance ToJSON LogEntry where
  toJSON (LogEntry_Cmd clog) = toJSON clog
  toJSON (LogEntry_Event e s t) = encodeLogEntryEvent e s t

data TransactionLogger = TransactionLogger
  { _transactionLogger_appendLog :: Latest.CommandLog -> IO ()
  , _transactionLogger_walletEvent :: WalletEvent -> Text -> UTCTime -> IO ()
  , _transactionLogger_destination :: Maybe FilePath
  , _transactionLogger_loadLastNLogs :: Int -> IO (Either String (TimeLocale, [LogEntry]))
  , _transactionLogger_exportFile :: Text -> IO (Either String (FilePath, Text))
  , _transactionLogger_rotateLogFile :: IO ()
  }

data ApiClient m = ApiClient
  { send :: TransactionLogger -> Text -> ChainId -> SubmitBatch -> m RequestKeys
  , poll :: Poll -> m PollResponses
  , listen :: ListenerRequest -> m ListenResponse
  , local :: Command Text -> m (CommandResult Hash)
  }

{- apiV1API :: Proxy ApiV1API -}
{- apiV1API = Proxy -}

-- | Commands are logged with the time they were sent and the node URL they were sent
-- to. We only define 'toJSON' for the current version, thus only write logs in the latest
-- version.
apiV1Client :: forall m. (MonadIO m, MonadReader ClientEnv m, RunClient m) => ApiClient m
apiV1Client = ApiClient
  { send = \txnLogger sender chain batch@(SubmitBatch commands) -> do
      url <- asks baseUrl
      timestamp <- liftIO getCurrentTime
      rqkeys <- sendF batch
      for_ commands $ \command -> liftIO $ _transactionLogger_appendLog txnLogger $ CommandLog
        { _commandLog_command = command
        , _commandLog_sender = sender
        , _commandLog_chain = chain
        , _commandLog_requestKey = NE.head $ _rkRequestKeys rqkeys
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

commandLogCurrentVersion :: Int
commandLogCurrentVersion = Latest.versionNumber

commandLogFilename :: FilePath
commandLogFilename = "chainweaver_transaction_log"

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

noLogger :: TransactionLogger
noLogger = TransactionLogger
  { _transactionLogger_appendLog = const $ pure ()
  , _transactionLogger_walletEvent = \_ _ _ -> pure ()
  , _transactionLogger_destination = Nothing
  , _transactionLogger_loadLastNLogs = logsdisabled
  , _transactionLogger_exportFile = logsdisabled
  , _transactionLogger_rotateLogFile = pure ()
  }
  where
    logsdisabled = const $ pure $ Left "Logs Disabled"

logTransactionStdout :: TransactionLogger
logTransactionStdout = TransactionLogger
  { _transactionLogger_appendLog = LT.putStrLn . Aeson.encodeToLazyText
  , _transactionLogger_walletEvent = \wE pk t ->
      T.putStrLn $ T.unwords [T.pack $ show wE, "[", pk, "]", T.pack $ show t]
  , _transactionLogger_destination = Nothing
  , _transactionLogger_loadLastNLogs = logsdisabled
  , _transactionLogger_exportFile = logsdisabled
  , _transactionLogger_rotateLogFile = pure ()
  }
  where
    logsdisabled = const $ pure $ Left "Logs Disabled"

logTransactionFile :: FilePath -> TransactionLogger
logTransactionFile f = TransactionLogger
  { _transactionLogger_appendLog =
      LT.appendFile f . (<> "\n") . Aeson.encodeToLazyText

  , _transactionLogger_walletEvent = \etype pk t ->
      LT.appendFile f $ (<> "\n") $ Aeson.encodeToLazyText $ encodeLogEntryEvent etype pk t

  , _transactionLogger_destination =
      Just f

  , _transactionLogger_loadLastNLogs = \n -> runExceptT $ do
      logmsg $ printf "Loading logs from: %s" f
      nLogs <- liftIO (Dir.doesFileExist f) >>= \case
        True -> do
          logmsg "Log file exists"
          ExceptT $ over (mapped . _Left) ppIOException $ try $ lastN n . BS8.lines <$> BS.readFile f
        False -> do
          logmsg $ printf "No log file found at %s" f
          throwError "Chainweaver transaction log is currently empty"
      xs <- liftEither $ traverse Aeson.eitherDecodeStrict nLogs
      pure (defaultTimeLocale, xs)

  , _transactionLogger_exportFile = \pk ->
      createCommandLogExportFileV1 pk f

  , _transactionLogger_rotateLogFile = Dir.doesFileExist f >>= \exists -> when exists $ do
      putStrLn "Moving existing log file."
      nowish <- getCurrentTime
      let timestamp = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H-%M-%S")) nowish
      catch (Dir.renameFile f $ printf "%s_v%i_%s" f commandLogCurrentVersion timestamp) $ \(e :: IOError) ->
        putStrLn $ "Unable to move existing log file. Reason: " <> displayException e
  }
  where
    logmsg = liftIO . putStrLn

    lastN n as = drop (length as - n) as

    ppIOException :: IOError -> String
    ppIOException = displayException

createCommandLogExportFileV1 :: Text -> FilePath -> IO (Either String (FilePath, Text))
createCommandLogExportFileV1 filePfx fp = runExceptT $ do
  let
    -- Use system-filepath to avoid being caught out by platform differences
    filename = snd $ File.splitFileName fp
    exportFilePath = printf "%s_%s_v%i" filePfx filename commandLogCurrentVersion

  logContents <- ExceptT $ catch (Right . BS8.lines <$> BS.readFile fp)
    $ \(e :: IOError) -> pure $ Left $ displayException e

  xs <- liftEither $ traverse Aeson.eitherDecodeStrict logContents

  let
    aesonTextEncode :: ToJSON a => a -> Text
    aesonTextEncode = LT.toStrict . LT.decodeUtf8With T.lenientDecode . Aeson.encode

    toCsv i (LogEntry_Cmd cmdLog) = Latest.exportCommandLog i cmdLog
    toCsv i (LogEntry_Event eType sender timestamp) = Csv.encodeNamedRecord Latest.exportHeader $ Map.fromList
      [ ("index" :: BSL.ByteString, Pact.tShow i)
      , ("timestamp", aesonTextEncode timestamp)
      , ("sender", sender)
      , ("chain", mempty)
      , ("requestKey", aesonTextEncode eType)
      , ("url", mempty)
      , ("command_payload", mempty)
      , ("command_sigs", mempty)
      , ("command_hash", mempty)
      ]

  let
    csvContents = BSL.toStrict $ BSL.toLazyByteString $ ifoldMap toCsv xs
  pure (exportFilePath, T.decodeUtf8With T.lenientDecode csvContents)
