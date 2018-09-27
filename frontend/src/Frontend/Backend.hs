{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleInstances      #-}

-- | Interface for accessing Pact backends.
--
--   The module offers the possibility of selecting a particular backend and
--   sending commands to it, by making use of the Pact REST API.
module Frontend.Backend
  ( -- * Types & Classes
    BackendUri
  , BackendName
  , BackendRequest (..)
  , BackendStatus (..)
  , BackendResult (..)
  , BackendError (..)
  , BackendErrorResult
  , BackendCfg (..)
  , Backend (..)
    -- * Creation
  , makeBackend
    -- * Perform requests
  , backendPerformSend
  ) where

import           Control.Arrow           (first)
import           Control.Lens            hiding ((.=))
import           Data.Aeson              (Object, Value, encode, object, toJSON,
                                          (.=))
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
import           Data.Time.Clock         (getCurrentTime)
import           Pact.Types.Hash         (hash)
import           Pact.Types.Util         (Hash (..))
import           Reflex.Dom.Class
import           Reflex.Dom.Xhr


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

-- | Status as parsed from pact server response.
--
--   See
--   <https://pact-language.readthedocs.io/en/latest/pact-reference.html#send
--   here> for more information.
data BackendStatus
  = BackendStatus_Success
  | BackenStatus_Failure

-- | Result as received from the backend.
data BackendResult = BackendResult
  { _backendResult_status :: BackendStatus
  , _backendResult_data   :: Value
  }

data BackendError
  = BackendError_Failure Text
  -- ^ The backend responded with status "failure"
  -- The contained `Text` will hold the full message, which might be useful for
  -- debugging.
  | BackendError_XhrException XhrException
  -- ^ There was some problem with the connection, as described by the
  -- contained `XhrException`

-- | Combined `BackendError` and `BackendResult`.
type BackendErrorResult = Either BackendError BackendResult

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

    onErrResp <- performRequestAsyncWithError onXhr
    performEvent_ $ printResp <$> onErrResp
    pure never
  where
    printResp = \case
      Left _ -> liftIO $ putStrLn "Some Xhr error"
      Right r -> liftIO $ print $ _xhrResponse_responseText r
    -- toErrorResult = \case
    --   Left e -> BackendError_XhrException e
    --   Right r ->


buildXhrRequest
  :: (Reflex t, MonadSample t m, MonadIO m, MonadJSM m)
  => Wallet t -> Backend t -> BackendRequest -> m (XhrRequest Text)
buildXhrRequest w b req = do
  let
    getBackend n = fromJust $ Map.lookup n backends
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
