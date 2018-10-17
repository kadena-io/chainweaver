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
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies            #-}

-- | Interface for accessing Pact backends.
--
--   The module offers the possibility of selecting a particular backend and
--   sending commands to it, by making use of the Pact REST API.
module Frontend.Backend
  ( -- * Types & Classes
    BackendUri
  , BackendName (..)
  , BackendRequest (..)
  , BackendError (..)
  , BackendErrorResult
  , BackendCfg (..)
  , HasBackendCfg (..)
  , Backend (..)
  , HasBackend (..)
    -- * Creation
  , makeBackend
    -- * Perform requests
  , backendRequest
    -- * Utilities
  , prettyPrintBackendErrorResult
  , prettyPrintBackendError
  ) where

import           Control.Arrow            (first, left)
import           Control.Concurrent       (forkIO)
import           Control.Lens             hiding ((.=))
import           Control.Monad.Except
import           Data.Aeson               (FromJSON (..), Object, Value,
                                           Value (..), decode, eitherDecode,
                                           encode, object, toJSON, withObject,
                                           (.:), (.=))
import           Data.Aeson.Types         (typeMismatch, parseMaybe)
import qualified Data.ByteString.Lazy     as BSL
import           Data.Default             (def)
import qualified Data.HashMap.Strict      as H
import qualified Data.Map                 as Map
import           Data.Map.Strict          (Map)
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.IO             as T
import           Data.Traversable         (for)
import           Data.Time.Clock          (getCurrentTime)
import           Generics.Deriving.Monoid (mappenddefault, memptydefault)
import           Obelisk.ExecutableConfig (get)
import           Reflex.NotReady.Class
import           Reflex.Dom.Class
import           Reflex.Dom.Xhr
import           Safe

import Language.Javascript.JSaddle.Monad (JSM, askJSM, JSContextRef)

import           Frontend.Backend.Pact
import           Frontend.Crypto.Ed25519
import           Frontend.Foundation
import           Frontend.Wallet

-- | URI for accessing a backend.
type BackendUri = Text

newtype PactError = PactError Text deriving (Show, FromJSON)
newtype PactDetail = PactDetail Text deriving (Show, FromJSON)

-- | Name that uniquely describes a valid backend.
newtype BackendName = BackendName
  { unBackendName :: Text
  }
  deriving (Generic, Eq, Ord, Show, Semigroup, Monoid)

-- | Request data to be sent to the backend.
data BackendRequest = BackendRequest
  { _backendRequest_code :: Text
    -- ^ Pact code to be deployed, the `code` field of the <https://pact-language.readthedocs.io/en/latest/pact-reference.html#cmd-field-and-payload exec> payload.
  , _backendRequest_data :: Object
    -- ^ The data to be deployed (referenced by deployed code). This is the
    -- `data` field of the `exec` payload.
  , _backendRequest_backend :: BackendUri
    -- ^ Which backend to use
  }


data BackendError
  = BackendError_Failure Text
  -- ^ The backend responded with status "failure"
  -- The contained `Text` will hold the full message, which might be useful for
  -- debugging.
  | BackendError_XhrException XhrException
  -- ^ There was some problem with the connection, as described by the
  -- contained `XhrException`
  | BackendError_ParseError Text
  -- ^ Parsing the JSON response failed.
  | BackendError_NoResponse
  -- ^ Server response was empty.
  | BackendError_ResultFailure PactError PactDetail
  -- ^ The status in the /listen result object was `failure`.
  | BackendError_ResultFailureText Text
-- ^ The the /listen result object was actually just an error message string
  | BackendError_Other Text
  -- ^ Other errors that should really never happen.
  deriving Show

-- | We either have a `BackendError` or some `Value`.
type BackendErrorResult = Either BackendError Value

-- | Config for creating a `Backend`.
data BackendCfg t = BackendCfg
  -- , _backendCfg_send       :: Event t BackendRequest
    -- ^ Send a request to the currently selected backend.
  { _backendCfg_refreshModule :: Event t ()
    -- ^ We are unfortunately not notified by the pact backend when new
    -- contracts appear on the blockchain, so UI code needs to request a
    -- refresh at appropriate times.
  }
  deriving Generic

makePactLenses ''BackendCfg

data Backend t = Backend
  { _backend_backends :: Dynamic t (Maybe (Map BackendName BackendUri))
    -- ^ All available backends that can be selected.
  , _backend_modules  :: Dynamic t (Map BackendName (Maybe [Text]))
   -- ^ Available modules on all backends. `Nothing` if not loaded yet.
  }

makePactLenses ''Backend

-- Helper types for interfacing with Pact endpoints:

-- | Status as parsed from pact server response.
--
--   See
--   <https://pact-language.readthedocs.io/en/latest/pact-reference.html#send
--   here> for more information.
data PactStatus
  = PactStatus_Success
  | PactStatus_Failure
  deriving Show

instance FromJSON PactStatus where
  parseJSON = \case
    String "success" -> pure PactStatus_Success
    String "failure" -> pure PactStatus_Failure
      -- case str of
      --   "success" -> PactStatus_Success
      --   "failure" -> PactStatus_Failure
      --   invalid   ->
    invalid -> typeMismatch "Status" invalid


-- | Result as received from the backend.
data PactResult
  = PactResult_Success Value
  | PactResult_Failure PactError PactDetail
  | PactResult_FailureText Text -- when decimal fields are empty, for example

instance FromJSON PactResult where
  parseJSON (String t) = pure $ PactResult_FailureText t
  parseJSON (Object o) = do
    status <- o .: "status"
    case status of
      PactStatus_Success -> PactResult_Success <$> o .: "data"
      PactStatus_Failure -> PactResult_Failure <$> o .: "error" <*> o .: "detail"
  parseJSON v = typeMismatch "PactResult" v

-- | Response object contained in a /listen response.
data ListenResponse = ListenResponse
 { _lr_result :: PactResult
 , _lr_txId   :: Integer
 }

instance FromJSON ListenResponse where
  parseJSON = withObject "Response" $ \o ->
    ListenResponse <$> o .: "result" <*> o .: "txId"


-- | Available backends:
getBackends :: IO (Map BackendName BackendUri)
getBackends = do
  serverUrl <- T.strip . fromMaybe "http://localhost:7010" <$> get "common/server-url"
  serverUrl2 <- T.strip . fromMaybe "http://localhost:7020" <$> get "common/server-url"
  pure $ Map.fromList . map (first BackendName) $
    [ ("dev-backend", serverUrl)
    , ("2-dev-backend", serverUrl2)
    ]


makeBackend
  :: forall t m
  . (MonadHold t m, PerformEvent t m, MonadFix m, NotReady t m, Adjustable t m
    , MonadJSM (Performable m), HasJSContext (Performable m)
    , TriggerEvent t m, MonadSample t (Performable m)
    , PostBuild t m
    , MonadIO m
    )
  => Wallet t
  -> BackendCfg t
  -> m (Backend t)
makeBackend w cfg = mfix $ \b -> do
  pb <- getPostBuild
  bs <- holdDyn Nothing . fmap Just <=< performEvent $ liftIO getBackends <$ pb

  modules <- loadModules w bs cfg

  pure $ Backend
    { _backend_backends = bs
    , _backend_modules = modules
    }


loadModules
  :: forall t m
  . (MonadHold t m, PerformEvent t m, MonadFix m, MonadIO m, NotReady t m, Adjustable t m
    , MonadJSM (Performable m), HasJSContext (Performable m)
    , TriggerEvent t m, MonadSample t (Performable m), PostBuild t m
    )
 => Wallet t -> Dynamic t (Maybe (Map BackendName BackendUri)) -> BackendCfg t
  -> m (Dynamic t (Map BackendName (Maybe [Text])))
loadModules w bs cfg = do
  let req = BackendRequest "(list-modules)" H.empty
  backendMap <- networkView $ ffor bs $ \case
    Nothing -> pure mempty
    Just bs' -> do
      onPostBuild <- getPostBuild
      bm <- flip Map.traverseWithKey bs' $ \backendName uri -> do
        onErrResp <- backendRequest w $
          leftmost [ req uri <$ cfg ^. backendCfg_refreshModule
                   , req uri <$ onPostBuild
                   ]
        let
          (onErr, onResp) = fanEither $ fmap snd onErrResp

          onModules :: Event t (Maybe [Text])
          onModules = parseMaybe parseJSON <$> onResp
        performEvent_ $ (liftIO . putStrLn . ("ERROR: " <>) . show) <$> onErr

        holdUniqDyn =<< holdDyn Nothing onModules
      pure $ sequence bm

  join <$> holdDyn mempty backendMap



-- | Transform an endpoint like /send to a valid URL.
url :: BackendUri -> Text -> Text
url b endpoint = b <> "/api/v1" <> endpoint

-- | Send a transaction via the /send endpoint.
--
--   And wait for its result via /listen.
backendRequest
  :: forall t m
  . ( MonadHold t m, PerformEvent t m, MonadFix m
    , MonadJSM (Performable m), HasJSContext (Performable m)
    , TriggerEvent t m, MonadSample t (Performable m)
    )
  => Wallet t -> Event t BackendRequest -> m (Event t (BackendUri, BackendErrorResult))
backendRequest w onReq = performEventAsync $ ffor onReq $ \req cb -> do
  let uri = _backendRequest_backend req
      callback = liftIO . void . forkIO . cb . (,) uri
  sendReq <- buildSendXhrRequest w req
  void $ newXMLHttpRequestWithError sendReq $ \r -> case getResPayload r of
    Left e -> callback $ Left e
    Right send -> case _rkRequestKeys send of
      [] -> callback $ Left $ BackendError_Other "Response did not contain any RequestKeys"
      [key] -> do
        let listenReq = buildListenXhrRequest uri key
        void $ newXMLHttpRequestWithError listenReq $ \r -> case getResPayload r of
          Left e -> callback $ Left e
          Right listen -> case _lr_result listen of
            PactResult_Failure err detail -> callback $ Left $ BackendError_ResultFailure err detail
            PactResult_FailureText err -> callback $ Left $ BackendError_ResultFailureText err
            PactResult_Success result -> callback $ Right result
      _ -> callback $ Left $ BackendError_Other "Response contained more than one RequestKey"

-- TODO: upstream this?
instance HasJSContext JSM where
  type JSContextPhantom JSM = JSContextRef
  askJSContext = JSContextSingleton <$> askJSM

-- Request building ....

-- | Build Xhr request for the /send endpoint using the given URI.
buildSendXhrRequest
  :: (Reflex t, MonadIO m, MonadJSM m, MonadSample t m)
  => Wallet t -> BackendRequest -> m (XhrRequest Text)
buildSendXhrRequest w req = do
  fmap (xhrRequest "POST" (url (_backendRequest_backend req) "/send")) $ do
    kps <- sample . current . joinKeyPairs $ _wallet_keys w
    sendData <- encodeAsText . encode <$> buildSendPayload kps req
    pure $ def & xhrRequestConfig_sendData .~ sendData

-- | Build Xhr request for the /listen endpoint using the given URI and request
-- key.
buildListenXhrRequest :: BackendUri -> RequestKey -> XhrRequest Text
buildListenXhrRequest uri key = do
  xhrRequest "POST" (url uri "/listen") $ def
    & xhrRequestConfig_sendData .~ encodeAsText (encode $ object [ "listen" .= key ])

-- | Build payload as expected by /send endpoint.
buildSendPayload :: (MonadIO m, MonadJSM m) => KeyPairs -> BackendRequest -> m Value
buildSendPayload keys req = do
  cmd <- buildCmd keys req
  pure $ object
    [ "cmds" .= [ cmd ]
    ]

-- | Build a single cmd as expected in the `cmds` array of the /send payload.
--
-- As specified <https://pact-language.readthedocs.io/en/latest/pact-reference.html#send here>.
buildCmd :: (MonadIO m, MonadJSM m) => KeyPairs -> BackendRequest -> m Value
buildCmd keys req = do
  cmd <- encodeAsText . encode <$> buildExecPayload req
  let
    cmdHash = hash (T.encodeUtf8 cmd)
  sigs <- buildSigs cmdHash keys
  pure $ object
    [ "hash" .= cmdHash
    , "sigs" .= sigs
    , "cmd"  .= cmd
    ]

-- | Build signatures for a single `cmd`.
buildSigs :: MonadJSM m => Hash -> KeyPairs -> m Value
buildSigs cmdHash keys = do
  let
    -- isJust filter is necessary so indices are guaranteed stable even after
    -- the following `mapMaybe`:
    isForSigning (KeyPair _ priv forSign) = forSign && isJust priv

    signingPairs = filter isForSigning . Map.elems $ keys
    signingKeys = mapMaybe _keyPair_privateKey signingPairs

  sigs <- traverse (mkSignature (unHash cmdHash)) signingKeys

  let
    mkSigPubKey :: KeyPair -> Signature -> Value
    mkSigPubKey kp sig = object
      [ "sig" .= sig
      , "pubKey" .= _keyPair_publicKey kp
      ]
  pure . toJSON $ zipWith mkSigPubKey signingPairs sigs


-- | Build exec `cmd` payload.
--
--   As specified <https://pact-language.readthedocs.io/en/latest/pact-reference.html#cmd-field-and-payloads here>.
--   Use `encodedAsText` for passing it as the `cmd` payload.
buildExecPayload :: MonadIO m => BackendRequest -> m Value
buildExecPayload req = do
  nonce <- getNonce
  let
    payload = object
      [ "code" .= _backendRequest_code req
      , "data" .= _backendRequest_data req
      ]
  pure $ object
    [ "nonce" .= nonce
    , "payload" .= object [ "exec" .= payload ]
    ]

-- Response handling ...

-- | Extract the response body from a Pact server response:
getResPayload
  :: FromJSON resp
  => Either XhrException XhrResponse
  -> Either BackendError resp
getResPayload xhrRes = do
  r <- left BackendError_XhrException xhrRes
  let
    mRespT = BSL.fromStrict . T.encodeUtf8 <$> _xhrResponse_responseText r
  respT <- maybe (throwError BackendError_NoResponse) pure mRespT
  pactRes <- case eitherDecode respT of
    Left e -> Left $ BackendError_ParseError $ T.pack e <> fromMaybe "" (_xhrResponse_responseText r)
    Right v -> Right v
  case pactRes of
    ApiFailure str ->
      throwError $ BackendError_Failure . T.pack $ str
    ApiSuccess a -> pure a

-- Other utilities:

prettyPrintBackendError :: BackendError -> Text
prettyPrintBackendError = ("ERROR: " <>) . \case
  BackendError_Failure msg -> "Backend failure: " <> msg
  BackendError_XhrException e -> "Connection failed: " <> (T.pack . show) e
  BackendError_ParseError m -> "Server response could not be parsed: " <> m
  BackendError_NoResponse -> "Server response was empty."
  BackendError_ResultFailure (PactError e) (PactDetail d) -> e <> ": " <> d
  BackendError_ResultFailureText e -> e
  BackendError_Other m -> "Some unknown problem: " <> m

prettyPrintBackendErrorResult :: BackendErrorResult -> Text
prettyPrintBackendErrorResult = \case
  Left e -> prettyPrintBackendError e
  Right r -> "Server result: " <> (T.pack . show) r



-- | Get unique nonce, based on current time.
getNonce :: MonadIO m => m Text
getNonce = do
  t <- liftIO getCurrentTime
  pure $ T.pack . show $ t

-- | Treat encoded JSON as a Text value which can be encoded again.
--
--   This way you get stringified JSON.
encodeAsText :: BSL.ByteString -> Text
encodeAsText = T.decodeUtf8 . BSL.toStrict

instance Reflex t => Semigroup (BackendCfg t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (BackendCfg t) where
  mempty = memptydefault
  mappend = (<>)
