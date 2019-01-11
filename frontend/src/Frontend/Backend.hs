{-# LANGUAGE ConstraintKinds            #-}
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
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Interface for accessing Pact backends.
--
--   The module offers the possibility of selecting a particular backend and
--   sending commands to it, by making use of the Pact REST API.
module Frontend.Backend
  ( -- * Types & Classes
    BackendName
  , textBackendName
  , BackendRef
  , backendRefUri
  , backendRefName
  , BackendRequest (..)
  , BackendError (..)
  , BackendErrorResult
  , BackendCfg (..)
  , HasBackendCfg (..)
  , IsBackendCfg
  , Backend (..)
  , HasBackend (..)
    -- * Creation
  , makeBackend
    -- * Perform requests
  , performBackendRequest
  , performBackendRequestCustom
  , backendRequest
    -- * Utilities
  , prettyPrintBackendErrorResult
  , prettyPrintBackendError
  ) where

import           Control.Arrow                     (left, (&&&), (***))
import           Control.Lens                      hiding ((.=))
import           Control.Monad.Except
import           Data.Aeson                        (FromJSON (..), Object,
                                                    Value (..), eitherDecode,
                                                    encode, object, toJSON,
                                                    withObject, (.:), (.=))
import           Data.Aeson.Types                  (parseMaybe, typeMismatch)
import qualified Data.ByteString.Lazy              as BSL
import           Data.Default                      (def)
import qualified Data.HashMap.Strict               as H
import qualified Data.Map                          as Map
import           Data.Map.Strict                   (Map)
import           Data.Set                          (Set)
import qualified Data.Set                          as Set
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import           Data.Time.Clock                   (getCurrentTime)
import           Generics.Deriving.Monoid          (mappenddefault,
                                                    memptydefault)
import           Reflex.Dom.Class
import           Reflex.Dom.Xhr
import           Reflex.NotReady.Class
import Data.Coerce (coerce)

import           Language.Javascript.JSaddle.Monad (JSContextRef, JSM, askJSM)

import           Common.Api
import           Common.Route                      (pactServerListPath)
import           Frontend.Backend.Pact
import           Frontend.Crypto.Ed25519
import           Frontend.Foundation
import           Frontend.Wallet
import           Frontend.Messages

-- | URI for accessing a backend.
type BackendUri = Text

newtype PactError = PactError Text deriving (Show, FromJSON)
newtype PactDetail = PactDetail Text deriving (Show, FromJSON)

-- | Name that uniquely describes a valid backend.
newtype BackendName = BackendName
  { unBackendName :: Text
  }
  deriving (Generic, Eq, Ord, Show, Semigroup, Monoid)

-- | Render a backend name as `Text`.
textBackendName :: BackendName -> Text
textBackendName = coerce

-- | Backend reference.
data BackendRef = BackendRef
  { brName :: BackendName
  , brUri  :: BackendUri
  }
  deriving (Show, Generic, Eq, Ord)

-- | Extract the actual URI.
backendRefUri :: BackendRef -> BackendUri
backendRefUri = brUri

-- | Extract the name of the backend.
backendRefName :: BackendRef -> BackendName
backendRefName = brName

-- | Request data to be sent to the backend.
data BackendRequest = BackendRequest
  { _backendRequest_code    :: Text
    -- ^ Pact code to be deployed, the `code` field of the
    -- <https://pact-language.readthedocs.io/en/latest/pact-reference.html#cmd-field-and-payload
    -- exec> payload.
  , _backendRequest_data    :: Object
    -- ^ The data to be deployed (referenced by deployed code). This is the
    -- `data` field of the `exec` payload.
  , _backendRequest_backend :: BackendRef
    -- ^ What backend to use.
  , _backendRequest_signing :: Set KeyName
  } deriving (Show, Generic)


data BackendError
  = BackendError_BackendError Text
  -- ^ Server responded with a non 200 status code.
  | BackendError_ReqTooLarge
  -- ^ Request size exceeded the allowed limit.
  | BackendError_Failure Text
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
    -- TODO: This should go to `ModuleExplorer`
  , _backendCfg_deployCode :: Event t BackendRequest
    -- ^ Deploy some code to the backend. Response will be logged to `Messages`.
  }
  deriving Generic

makePactLenses ''BackendCfg

-- | HasBackendCfg with additional constraints to make it behave like a proper
-- "Config".
type IsBackendCfg cfg t = (HasBackendCfg cfg t, Monoid cfg, Flattenable cfg t)

data Backend t = Backend
  { _backend_backends :: Dynamic t (Maybe (Map BackendName BackendRef))
    -- ^ All available backends that can be selected.
  , _backend_modules  :: Dynamic t (Map BackendName (Maybe [Text]))
   -- ^ Available modules on all backends. `Nothing` if not loaded yet.
   -- TODO: This should really go to the `ModuleExplorer`.
  , _backend_deployed :: Event t ()
   -- ^ Event gets triggered whenever some code got deployed sucessfully.
  }

makePactLenses ''Backend

-- | Model/dependencies of Backend.
type HasBackendModel model t = HasWallet model t

-- | Model config needed by Backend.
type HasBackendModelCfg mConf t = (Monoid mConf, HasMessagesCfg mConf t)

makeBackend
  :: forall t m model mConf
  . ( MonadHold t m, PerformEvent t m, MonadFix m, NotReady t m, Adjustable t m
    , MonadJSM (Performable m), HasJSContext (Performable m)
    , TriggerEvent t m, PostBuild t m, MonadIO m
    , HasBackendModel model t
    , HasBackendModelCfg mConf t
    )
  => model
  -> BackendCfg t
  -> m (mConf, Backend t)
makeBackend w cfg = do
    bs <- getBackends

    (mConf, onDeployed) <- deployCode w $ cfg ^. backendCfg_deployCode

    modules <- loadModules bs $ leftmost [ onDeployed, cfg ^. backendCfg_refreshModule ]

    pure
      ( mConf
      , Backend
          { _backend_backends = bs
          , _backend_modules = modules
          , _backend_deployed = onDeployed
          }
      )

deployCode
  :: forall t m model mConf
  . (MonadHold t m, PerformEvent t m, MonadFix m, NotReady t m, Adjustable t m
    , MonadJSM (Performable m), HasJSContext (Performable m)
    , TriggerEvent t m, PostBuild t m, MonadIO m
    , HasBackendModel model t
    , HasBackendModelCfg mConf t
    )
  => model
  -> Event t BackendRequest
  -> m (mConf, Event t ())
deployCode w onReq = do
    reqRes <- performBackendRequest (w ^. wallet) onReq
    pure $ ( mempty & messagesCfg_send .~ fmap renderReqRes reqRes
           , () <$ ffilter (either (const False) (const True) . snd) reqRes
           )
  where
    renderReqRes :: (BackendRequest, BackendErrorResult) -> Text
    renderReqRes (req, res) =
      T.unlines [renderReq req, prettyPrintBackendErrorResult res]

    renderReq :: BackendRequest -> Text
    renderReq req =
      let
        backendName = brName $ _backendRequest_backend req
        code = _backendRequest_code req
        -- Not really helpful to display deployed code if it is large:
        mkMsg msg = if T.length code < 100 then msg else ""
      in
        mkMsg $ T.unlines
          [ "Sent code to backend '" <> coerce backendName <> "':"
          , ""
          , code
          ]

-- | Available backends:
getBackends
  :: ( MonadJSM (Performable m), HasJSContext (Performable m)
     , PerformEvent t m, TriggerEvent t m, PostBuild t m, MonadIO m
     , MonadHold t m
     )
  => m (Dynamic t (Maybe (Map BackendName BackendRef)))
getBackends = do
  let
    buildUrl = ("http://localhost:" <>) . getPactInstancePort
    buildName = BackendName . ("dev-" <>) . T.pack . show
    buildRef x = BackendRef (buildName x) (buildUrl x)
    buildServer =  buildName &&& buildRef
    devServers = Map.fromList $ map buildServer [1 .. numPactInstances]
  onPostBuild <- getPostBuild

  prodStaticServerList <- liftIO getPactServerList
  case prodStaticServerList of
    Nothing -> -- Development mode
      holdDyn Nothing $ Just devServers <$ onPostBuild
    Just c -> do -- Production mode
      let
        staticList = parsePactServerList c
      onResError <-
        performRequestAsyncWithError $ ffor onPostBuild $ \_ ->
          xhrRequest "GET" pactServerListPath def

      let
        getListFromResp r =
          if _xhrResponse_status r == 200
             then fmap parsePactServerList . _xhrResponse_responseText $ r
             else Just staticList
        onList = either (const (Just staticList)) getListFromResp <$> onResError
      holdDyn Nothing onList

-- | Parse server list.
--
--   Format:
--
--   ```
--   serverName: serveruri
--   serverName2: serveruri2
--   ...
--   ```
parsePactServerList :: Text -> Map BackendName BackendRef
parsePactServerList raw =
  let
    rawEntries = map (fmap (T.dropWhile (== ':')) . T.breakOn ":") . T.lines $ raw
    strippedUris = map (BackendName . T.strip *** T.strip) rawEntries
    refs = map (fst &&& uncurry BackendRef) strippedUris
  in
    Map.fromList refs

-- | Load modules on startup and on every occurrence of the given event.
loadModules
  :: forall t m
  . (MonadHold t m, PerformEvent t m, MonadFix m, NotReady t m, Adjustable t m
    , MonadJSM (Performable m), HasJSContext (Performable m)
    , TriggerEvent t m, PostBuild t m
    )
  => Dynamic t (Maybe (Map BackendName BackendRef))
  -> Event t ()
  -> m (Dynamic t (Map BackendName (Maybe [Text])))
loadModules bs onRefresh = do
  let req r = (BackendRequest "(list-modules)" H.empty r Set.empty)
  backendMap <- networkView $ ffor bs $ \case
    Nothing -> pure mempty
    Just bs' -> do
      onPostBuild <- getPostBuild
      bm <- flip Map.traverseWithKey bs' $ \_ r -> do
        onErrResp <- performBackendRequest emptyWallet $
          leftmost [ req r <$ onRefresh
                   , req r <$ onPostBuild
                   ]
        let
          (onErr, onResp) = fanEither $ snd <$> onErrResp

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
--   This is a convenience wrapper around `backendRequest`, use that if you
--   need some richer request information attached to your response or if this is really all you need `performBackendRequestCustom`.
performBackendRequest
  :: forall t m
  . ( PerformEvent t m, MonadJSM (Performable m)
    , HasJSContext (Performable m), TriggerEvent t m
    )
  => Wallet t
  -> Event t BackendRequest
  -> m (Event t (BackendRequest, BackendErrorResult))
performBackendRequest w onReq = performBackendRequestCustom w id onReq

-- | Send a transaction via the /send endpoint.
--
--   This is a convenience wrapper around `backendRequest`, attaching a custom
--   request type to the response.
performBackendRequestCustom
  :: forall t m req
  . ( PerformEvent t m, MonadJSM (Performable m)
    , HasJSContext (Performable m), TriggerEvent t m
    )
  => Wallet t
  -> (req -> BackendRequest)
  -> Event t req
  -> m (Event t (req, BackendErrorResult))
performBackendRequestCustom w unwrap onReq =
    performEventAsync $ ffor onReqWithKeys $ \(keys, req) cb ->
      backendRequest (keys, unwrap req) $ cb . (req,)
  where
    onReqWithKeys = attach (current $ _wallet_keys w) onReq

-- | Send a transaction via the /send endpoint.
--
--   And wait for its result via /listen. `performBackendRequest` is a little
--   more convenient to use if you don't need more elaborate request
--   information in your response.
--
--   Usage example:
--
-- @
--   let w = ourWallet
--   performEventAsync $ ffor (attachKeys w onReq) $ (\(keys, req)) cb ->
--     backendRequest (keys, buildBackendReq req) $ cb . (req,)
-- @
--
--   This primitive function is also useful if you happen to need to execute
--   multiple requests whose responses should be fed in the reflex network
--   simultaneously, because the logically belong together for example:
--
-- @
--   let w = ourWallet
--   performEventAsync $ ffor (attachKeys w onReq) $ (\(keys, req)) cb -> do
--     backendRequest (keys, buildBackendReq req) $ \res -> do
--       someMoreRequest someArg $ cb . (req, res,)
--
-- @
backendRequest :: forall t m
  . (MonadJSM m, HasJSContext m)
  => (KeyPairs, BackendRequest)
  -> (BackendErrorResult -> IO ())
  -> m ()
backendRequest (keys, req) cb = void . forkJSM $ do
    let
        callback = liftIO . cb
        uri = brUri $ _backendRequest_backend req
        signing = _backendRequest_signing req
    sendReq <- buildSendXhrRequest keys signing req
    void $ newXMLHttpRequestWithError sendReq $ \r -> case getResPayload r of
      Left e -> callback $ Left e
      Right send -> case _rkRequestKeys send of
        [] -> callback $ Left $ BackendError_Other "Response did not contain any RequestKeys"
        [key] -> do
          let listenReq = buildListenXhrRequest uri key
          void $ newXMLHttpRequestWithError listenReq $ \lr -> case getResPayload lr of
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
  :: (MonadIO m, MonadJSM m)
  => KeyPairs -> Set KeyName -> BackendRequest -> m (XhrRequest Text)
buildSendXhrRequest kps signing req = do
  fmap (xhrRequest "POST" (url (brUri $ _backendRequest_backend req) "/send")) $ do
    sendData <- encodeAsText . encode <$> buildSendPayload kps signing req
    pure $ def & xhrRequestConfig_sendData .~ sendData

-- | Build Xhr request for the /listen endpoint using the given URI and request
-- key.
buildListenXhrRequest :: BackendUri -> RequestKey -> XhrRequest Text
buildListenXhrRequest uri key = do
  xhrRequest "POST" (url uri "/listen") $ def
    & xhrRequestConfig_sendData .~ encodeAsText (encode $ object [ "listen" .= key ])

-- | Build payload as expected by /send endpoint.
buildSendPayload :: (MonadIO m, MonadJSM m) => KeyPairs -> Set KeyName -> BackendRequest -> m Value
buildSendPayload keys signing req = do
  cmd <- buildCmd keys signing req
  pure $ object
    [ "cmds" .= [ cmd ]
    ]

-- | Build a single cmd as expected in the `cmds` array of the /send payload.
--
-- As specified <https://pact-language.readthedocs.io/en/latest/pact-reference.html#send here>.
buildCmd :: (MonadIO m, MonadJSM m) => KeyPairs -> Set KeyName -> BackendRequest -> m Value
buildCmd keys signing req = do
  cmd <- encodeAsText . encode <$> buildExecPayload req
  let
    cmdHash = hash (T.encodeUtf8 cmd)
  sigs <- buildSigs cmdHash keys signing
  pure $ object
    [ "hash" .= cmdHash
    , "sigs" .= sigs
    , "cmd"  .= cmd
    ]

-- | Build signatures for a single `cmd`.
buildSigs :: MonadJSM m => Hash -> KeyPairs -> Set KeyName -> m Value
buildSigs cmdHash keys signing = do
  let
    -- isJust filter is necessary so indices are guaranteed stable even after
    -- the following `mapMaybe`:
    isForSigning (name, (KeyPair _ priv)) = Set.member name signing && isJust priv

    signingPairs = filter isForSigning . Map.assocs $ keys
    signingKeys = mapMaybe _keyPair_privateKey $ map snd signingPairs

  sigs <- traverse (mkSignature (unHash cmdHash)) signingKeys

  let
    mkSigPubKey :: KeyPair -> Signature -> Value
    mkSigPubKey kp sig = object
      [ "sig" .= sig
      , "pubKey" .= _keyPair_publicKey kp
      ]
  pure . toJSON $ zipWith mkSigPubKey (map snd signingPairs) sigs


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
    status = _xhrResponse_status r
    statusText = _xhrResponse_statusText r
    respText = _xhrResponse_responseText r
    tooLarge = 413 -- Status code for too large body requests.
    mRespT = BSL.fromStrict . T.encodeUtf8 <$> _xhrResponse_responseText r

  -- TODO: This does not really work, when testing we got an
  -- `XhrException` with no further details.
  when (status == tooLarge)
    $ throwError BackendError_ReqTooLarge
  when (status /= 200 && status /= 201)
    $ throwError $ BackendError_BackendError statusText

  respT <- maybe (throwError BackendError_NoResponse) pure mRespT
  pactRes <- case eitherDecode respT of
    Left e
      -> Left $ BackendError_ParseError $ T.pack e <> fromMaybe "" respText
    Right v
      -> Right v
  case pactRes of
    ApiFailure str ->
      throwError $ BackendError_Failure . T.pack $ str
    ApiSuccess a -> pure a

-- Other utilities:

prettyPrintBackendError :: BackendError -> Text
prettyPrintBackendError = ("ERROR: " <>) . \case
  BackendError_BackendError msg -> "Error http response: " <> msg
  BackendError_ReqTooLarge-> "Request exceeded the allowed maximum size!"
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


-- Instances:

instance Semigroup BackendRequest where
  (<>) = mappenddefault

instance Monoid BackendRequest where
  mempty = memptydefault
  mappend = (<>)

instance Semigroup BackendRef where
  (<>) = mappenddefault

instance Monoid BackendRef where
  mempty = memptydefault
  mappend = (<>)

instance Reflex t => Semigroup (BackendCfg t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (BackendCfg t) where
  mempty = memptydefault
  mappend = (<>)
--

instance Flattenable (BackendCfg t) t where
  flattenWith doSwitch ev =
    BackendCfg
      <$> doSwitch never (_backendCfg_refreshModule <$> ev)
      <*> doSwitch never (_backendCfg_deployCode <$> ev)
