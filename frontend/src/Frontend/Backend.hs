{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

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
  , Backend (..)
    -- * Creation
  , makeBackend
    -- * Perform requests
  , backendPerformSend
  ) where

import           Control.Arrow           (first, left)
import           Control.Lens            hiding ((.=))
import           Control.Monad.Except
import           Data.Aeson              (FromJSON (..), Object, Value,
                                          Value (..), eitherDecode, encode,
                                          object, toJSON, withObject, (.:),
                                          (.=))
import           Data.Aeson.Types        (typeMismatch)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Lazy    as BSL
import           Data.Default            (def)
import qualified Data.Map                as Map
import           Data.Map.Strict         (Map)
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.IO            as T
import           Data.Time.Clock         (getCurrentTime)
import           Reflex.Dom.Class
import           Reflex.Dom.Xhr
import           Safe


import           Frontend.Backend.Pact
import           Frontend.Crypto.Ed25519
import           Frontend.Foundation
import           Frontend.Wallet

-- | URI for accessing a backend.
type BackendUri = Text

-- | Name that uniquely describes a valid backend.
newtype BackendName = BackendName
  { unBackendName :: Text
  }
  deriving (Generic, Eq, Ord, Show)

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
  | BackendError_ResultFailure Value
  -- ^ The status in the /listen result object was `failure`.
  | BackendError_Other Text
  -- ^ Other errors that should really never happen.
  deriving Show

-- | We either have a `BackendError` or some `Value`.
type BackendErrorResult = Either BackendError Value

-- | Select a backend, send messages to it.
data BackendCfg t = BackendCfg
  { _backendCfg_selBackend :: Event t BackendName
    -- ^ Select the backend to operate with.
  -- , _backendCfg_send       :: Event t BackendRequest
    -- ^ Send a request to the currently selected backend.

  }

makePactLenses ''BackendCfg

data Backend t = Backend
  { _backend_backends :: Set BackendName
    -- ^ All available backends that can be selected.
  , _backend_current  :: Dynamic t BackendName
   --  ^ Currently selected `Backend`
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
data PactResult = PactResult
  { _pr_status :: PactStatus
  , _pr_data   :: Value
  }

instance FromJSON PactResult where
  parseJSON = withObject "PactResult" $ \o ->
    PactResult <$> o .: "status" <*> o .: "data"

-- | Response object contained in a /listen response.
data ListenResponse = ListenResponse
 { _lr_result :: PactResult
 , _lr_txId   :: Integer
 }

instance FromJSON ListenResponse where
  parseJSON = withObject "Response" $ \o ->
    ListenResponse <$> o .: "result" <*> o .: "txId"


-- | Available backends:
backends :: Map BackendName BackendUri
backends = Map.fromList . map (first BackendName) $
  [ ("dev-backend", "http://localhost:7010") -- TODO: should read from config ...
  ]


makeBackend :: MonadHold t m => BackendCfg t -> m (Backend t)
makeBackend cfg = do
  let
    names = Map.keysSet backends
  cName <-
    holdDyn (Set.findMin $ names) $ cfg ^. backendCfg_selBackend

  pure $ Backend
    { _backend_backends = Map.keysSet backends
    , _backend_current = cName
    }

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
    onXhr <- performEvent $ buildXhrRequest w b <$> onReq

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


backendPerformListen
  :: forall t m
  . (MonadHold t m, PerformEvent t m, MonadFix m
    , MonadJSM (Performable m), HasJSContext (Performable m)
    , TriggerEvent t m, MonadSample t (Performable m)
    )
  => Backend t
  -> Event t (Either BackendError RequestKey)
  -> m (Event t BackendErrorResult)
backendPerformListen b onKey = do
  let
    getBackend n = fromJustNote "Invalid backend name!" $ Map.lookup n backends
  let
    buildPayload k = encodeAsText . encode $ object [ "listen" .= k ]
    buildReq k = def & xhrRequestConfig_sendData .~ buildPayload k

    mkXhr
      :: Either BackendError RequestKey
      ->  PushM t (Either BackendError (XhrRequest Text))
    mkXhr r = do
      cb <- fmap getBackend . sample . current $ _backend_current b
      pure $ fmap (xhrRequest "GET" (url cb "/listen") . buildReq) r

    (onErr, onReq) = fanEither $ pushAlways mkXhr onKey

  performEvent_ $ liftIO . T.putStrLn . (("Request: ") <>) . _xhrRequestConfig_sendData . _xhrRequest_config <$> onReq

  onErrRespJson <- performRequestAsyncWithError onReq
  let
    getValue :: ListenResponse -> BackendErrorResult
    getValue lr = do
      let result = _lr_result lr
      case _pr_status result of
        PactStatus_Failure ->
          throwError $ BackendError_ResultFailure (_pr_data result)
        PactStatus_Success ->
          pure $ _pr_data result

  pure $ leftmost $
    [ fmap (getValue <=< getResPayload) onErrRespJson
    , Left <$> onErr
    ]


-- Request building ....


-- | Build Xhr request for the /send endpoint.
buildXhrRequest
  :: (Reflex t, MonadSample t m, MonadIO m, MonadJSM m)
  => Wallet t -> Backend t -> BackendRequest -> m (XhrRequest Text)
buildXhrRequest w b req = do
  let
    getBackend n = fromJustNote "Invalid backend name!" $ Map.lookup n backends
  cb <- fmap getBackend . sample . current $ _backend_current b
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
    isForSigning (KeyPair pub priv forSign) = forSign && isJust priv

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
  pactRes <- left (BackendError_ParseError . T.pack) . eitherDecode $ respT
  case pactRes of
    ApiFailure str ->
      throwError $ BackendError_Failure . T.pack $ str
    ApiSuccess a -> pure a

-- Other utilities:


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
