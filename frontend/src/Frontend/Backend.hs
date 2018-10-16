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

-- | Interface for accessing Pact backends.
--
--   The module offers the possibility of selecting a particular backend and
--   sending commands to it, by making use of the Pact REST API.
module Frontend.Backend
  ( -- * Types & Classes
    BackendUri
  , BackendName
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
  , backendPerformSend
    -- * Utilities
  , prettyPrintBackendErrorResult
  , prettyPrintBackendError
  ) where

import           Control.Arrow            (first, left)
import           Control.Lens             hiding ((.=))
import           Control.Monad.Except
import           Data.Aeson               (FromJSON (..), Object, Value,
                                           Value (..), decode, eitherDecode,
                                           encode, object, toJSON, withObject,
                                           (.:), (.=))
import           Data.Aeson.Types         (typeMismatch)
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
import           Data.Time.Clock          (getCurrentTime)
import           Generics.Deriving.Monoid (mappenddefault, memptydefault)
import           Obelisk.ExecutableConfig (get)
import           Reflex.Dom.Class
import           Reflex.Dom.Xhr
import           Safe


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
  { _backendCfg_selBackend    :: Event t BackendName
    -- ^ Select the backend to operate with.
  -- , _backendCfg_send       :: Event t BackendRequest
    -- ^ Send a request to the currently selected backend.
  , _backendCfg_refreshModule :: Event t ()
    -- ^ We are unfortunately not notified by the pact backend when new
    -- contracts appear on the blockchain, so UI code needs to request a
    -- refresh at appropriate times.
  }
  deriving Generic

makePactLenses ''BackendCfg

data Backend t = Backend
  { _backend_backends :: Map BackendName BackendUri
    -- ^ All available backends that can be selected.
  , _backend_current  :: Dynamic t BackendName
   --  ^ Currently selected `Backend`
  , _backend_modules  :: Dynamic t (Maybe [Text])
   -- ^ Available modules on the backend. `Nothing` if not loaded yet.
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
  pure $ Map.fromList . map (first BackendName) $
    [ ("dev-backend", serverUrl) ]


makeBackend
  :: forall t m
  . (MonadHold t m, PerformEvent t m, MonadFix m
    , MonadJSM (Performable m), HasJSContext (Performable m)
    , TriggerEvent t m, MonadSample t (Performable m)
    , PostBuild t m
    , MonadIO m
    )
  => Wallet t
  -> BackendCfg t
  -> m (Backend t)
makeBackend w cfg = mfix $ \b -> do
  backends <- liftIO getBackends
  let names = Map.keysSet backends
  cName <-
    holdDyn (Set.findMin $ names) $ cfg ^. backendCfg_selBackend

  modules <- loadModules w b cfg

  pure $ Backend
    { _backend_backends = backends
    , _backend_current = cName
    , _backend_modules = modules
    }


loadModules
  :: forall t m
  . (MonadHold t m, PerformEvent t m, MonadFix m
    , MonadJSM (Performable m), HasJSContext (Performable m)
    , TriggerEvent t m, MonadSample t (Performable m), PostBuild t m
    )
  => Wallet t -> Backend t -> BackendCfg t
  -> m (Dynamic t (Maybe [Text]))
loadModules w b cfg = do
  onPostBuild <- getPostBuild
  let
    req = BackendRequest "(list-modules)" H.empty
  onErrResp <- backendPerformSend w b $
    leftmost [ req <$ cfg ^. backendCfg_refreshModule
             , req <$ updated (_backend_current b)
             , req <$ onPostBuild
             ]
  let
    (onErr, onResp) = fanEither onErrResp

    onModules :: Event t (Maybe [Text])
    onModules = decode . encode <$> onResp
  performEvent_ $ (liftIO . putStrLn . ("ERROR: " <>) . show) <$> onErr

  holdUniqDyn <=< holdDyn Nothing $ leftmost
    [ onModules
    , Nothing <$ cfg ^. backendCfg_selBackend
    ]



-- | Transform an endpoint like /send to a valid URL.
url :: BackendUri -> Text -> Text
url b endpoint = b <> "/api/v1" <> endpoint

-- | Send a transaction via the /send endpoint.
--
--   And wait for its result via /listen.
backendPerformSend
  :: forall t m
  . (MonadHold t m, PerformEvent t m, MonadFix m
    , MonadJSM (Performable m), HasJSContext (Performable m)
    , TriggerEvent t m, MonadSample t (Performable m)
    )
  => Wallet t -> Backend t -> Event t BackendRequest -> m (Event t BackendErrorResult)
backendPerformSend w b onReq = do
    onXhr <- performEvent $
      attachPromptlyDynWith (buildXhrRequest w) (_backend_current b) onReq

    onErrRespJson <- performRequestAsyncWithError onXhr
    performEvent_ $ printResp <$> onErrRespJson
    let
      onReqKey :: Event t (Either BackendError RequestKey)
      onReqKey = getRequestKey <$> onErrRespJson
    onR <- backendPerformListen b onReqKey
    performEvent_ $ (liftIO . putStrLn . ("Stuff: " <>) . show) <$> onR
    pure onR
  where
    printResp = \case
      Left _ -> liftIO $ putStrLn "Some Xhr error"
      Right r -> liftIO $ print $ _xhrResponse_responseText r

    getRequestKey
      :: Either XhrException XhrResponse
      -> Either BackendError RequestKey
    getRequestKey i = do
      keys <- getResPayload i
      case _rkRequestKeys keys of
        [] -> throwError $
          BackendError_Other "Response did not contain any RequestKeys"
        [k] -> pure k
        _   -> throwError $
          BackendError_Other "Response contained more than one RequestKey"


-- | Helper function for performing a call to /listen based on the response
-- from /send.
backendPerformListen
  :: forall t m
  . (MonadHold t m, PerformEvent t m, MonadFix m
    , MonadJSM (Performable m), HasJSContext (Performable m)
    , TriggerEvent t m, MonadSample t (Performable m)
    , MonadIO (Performable m)
    )
  => Backend t
  -> Event t (Either BackendError RequestKey)
  -> m (Event t BackendErrorResult)
backendPerformListen b onKey = do
  let
    getBackend n = fromJustNote "Invalid backend name!" . Map.lookup n $ _backend_backends b
  let
    buildPayload k = encodeAsText . encode $ object [ "listen" .= k ]
    buildReq k = def & xhrRequestConfig_sendData .~ buildPayload k

    mkXhr
      :: BackendUri
      -> Either BackendError RequestKey
      -> Either BackendError (XhrRequest Text)
    mkXhr cb r =
      fmap (xhrRequest "POST" (url cb "/listen") . buildReq) r

    (onErr, onReq) = fanEither $
      attachPromptlyDynWith mkXhr (getBackend <$> _backend_current b) onKey

  performEvent_ $ liftIO . T.putStrLn . (("Request: ") <>) . _xhrRequestConfig_sendData . _xhrRequest_config <$> onReq

  onErrRespJson <- performRequestAsyncWithError onReq
  let
    getValue :: ListenResponse -> BackendErrorResult
    getValue lr = do
      let result = _lr_result lr
      case result of
        PactResult_Failure err detail -> throwError $ BackendError_ResultFailure err detail
        PactResult_FailureText err -> throwError $ BackendError_ResultFailureText err
        PactResult_Success pr -> pure pr

  pure $ leftmost $
    [ fmap (getValue <=< getResPayload) onErrRespJson
    , Left <$> onErr
    ]


-- Request building ....


-- | Build Xhr request for the /send endpoint.
buildXhrRequest
  :: (Reflex t, MonadIO m, MonadJSM m, MonadSample t m)
  => Wallet t -> BackendName -> BackendRequest -> m (XhrRequest Text)
buildXhrRequest w cbName req = do
  let
    -- TODO: This should not call getBackends but use backends in `Backend`.
    getBackend n = fromJustNote "Invalid backend name!" . Map.lookup n <$> getBackends
  cb <- liftIO $ getBackend cbName
  fmap (xhrRequest "POST" (url cb "/send")) $ do
    kps <- sample . current . joinKeyPairs $ _wallet_keys w
    sendData <- encodeAsText . encode <$> buildSendPayload kps req
    pure $ def & xhrRequestConfig_sendData .~ sendData

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
