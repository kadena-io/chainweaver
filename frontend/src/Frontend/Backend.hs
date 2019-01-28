{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE CPP                        #-}
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
  , textBackendRefName
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
                                                    encode, object, withObject,
                                                    (.:), (.=))
import           Data.Aeson.Types                  (parseMaybe, typeMismatch)
import qualified Data.ByteString.Lazy              as BSL
import           Data.Coerce                       (coerce)
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
import           Language.Javascript.JSaddle.Monad (JSContextRef, JSM, askJSM)
import qualified Network.HTTP.Client               as HTTP
import           Network.HTTP.Client.TLS           (newTlsManager)
import           Pact.Server.Client
import           Pact.Types.API
#if !defined(ghcjs_HOST_OS)
import           Pact.Types.Crypto                 (PPKScheme(..))
#endif
import           Pact.Types.Command
import           Pact.Types.Hash                   (hash)
import           Pact.Types.RPC
import           Pact.Types.Util
import           Reflex.Dom.Class
import           Reflex.Dom.Xhr
import           Reflex.NotReady.Class
import qualified Servant.Client                    as S

import           Pact.Parse                        (ParsedDecimal (..),
                                                    ParsedInteger (..))
import           Pact.Types.Command                (PublicMeta (..))

import           Common.Api
import           Common.Route                      (pactServerListPath)
import           Frontend.Crypto.Ed25519
import           Frontend.Foundation
import           Frontend.Messages
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

-- | Shortcut for : `textBackendName . backendRefName`.
textBackendRefName :: BackendRef -> Text
textBackendRefName = textBackendName . backendRefName

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
  { _backendCfg_refreshModule :: Event t ()
    -- ^ We are unfortunately not notified by the pact backend when new
    -- contracts appear on the blockchain, so UI code needs to request a
    -- refresh at appropriate times.
    -- TODO: This should go to `ModuleExplorer`
  , _backendCfg_deployCode    :: Event t BackendRequest
    -- ^ Deploy some code to the backend. Response will be logged to `Messages`.
  , _backendCfg_setChainId    :: Event t Text
    -- ^ On what chain to deploy to (ignored on pact -s backend).
  , _backendCfg_setGasLimit   :: Event t ParsedInteger
    -- ^ Maximun amount of gas to use for this transaction.
  , _backendCfg_setGasPrice   :: Event t ParsedDecimal
    -- ^ Maximum gas price you are willing to accept for having your
    -- transaction executed.
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
   -- TODO: This should really go to the `ModuleExplorer` or should it?
  , _backend_deployed :: Event t ()
   -- ^ Event gets triggered whenever some code got deployed sucessfully.
  , _backend_meta     :: Dynamic t PublicMeta
   -- ^ Meta data used for deployments. Can be modified via above
   -- `_backendCfg_setChainId`, `_backendCfg_setGasLimit`, ...
  , _backend_httpManager :: HTTP.Manager
   -- ^ The HTTP manager used to perform requests.
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
    , MonadSample t (Performable m)
    , HasBackendModel model t
    , HasBackendModelCfg mConf t
    )
  => model
  -> BackendCfg t
  -> m (mConf, Backend t)
makeBackend w cfg = mfix $ \ ~(_, backendL) -> do
    bs <- getBackends

    (mConf, onDeployed) <- deployCode w backendL $ cfg ^. backendCfg_deployCode
    manager <- newTlsManager

    modules <- loadModules backendL $ leftmost [ onDeployed, cfg ^. backendCfg_refreshModule ]

    meta <- buildMeta cfg

    pure
      ( mConf
      , Backend
          { _backend_backends = bs
          , _backend_modules = modules
          , _backend_deployed = onDeployed
          , _backend_meta = meta
          , _backend_httpManager = manager
          }
      )

buildMeta
  :: (MonadHold t m, MonadFix m, Reflex t)
  => BackendCfg t -> m (Dynamic t PublicMeta)
buildMeta cfg = do
  let defaultMeta =
        PublicMeta
          { _pmChainId = "someChain"
          , _pmSender  = "someSender"
          , _pmGasLimit = ParsedInteger 10000 -- TODO: Better defaults!!!
          , _pmGasPrice = ParsedDecimal 10000
          , _pmFee = ParsedDecimal 1
          }

  foldDyn id defaultMeta $ leftmost
    [ (\u c -> c { _pmChainId = u})  <$> cfg ^. backendCfg_setChainId
    , (\u c -> c { _pmGasLimit = u}) <$> cfg ^. backendCfg_setGasLimit
    , (\u c -> c { _pmGasPrice = u}) <$> cfg ^. backendCfg_setGasPrice
    ]


deployCode
  :: forall t m model mConf
  . (MonadHold t m, PerformEvent t m, MonadFix m, NotReady t m, Adjustable t m
    , MonadJSM (Performable m), HasJSContext (Performable m)
    , TriggerEvent t m, PostBuild t m, MonadIO m
    , MonadSample t (Performable m)
    , HasBackendModel model t
    , HasBackendModelCfg mConf t
    )
  => model
  -> Backend t
  -> Event t BackendRequest
  -> m (mConf, Event t ())
deployCode w backendL onReq = do
    reqRes <- performBackendRequest (w ^. wallet) backendL onReq
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
    , MonadSample t (Performable m)
    , TriggerEvent t m, PostBuild t m
    )
  => Backend t
  -> Event t ()
  -> m (Dynamic t (Map BackendName (Maybe [Text])))
loadModules backendL onRefresh = do
  let
    bs = backendL ^. backend_backends
    req r = (BackendRequest "(list-modules)" H.empty r Set.empty)
  backendMap <- networkView $ ffor bs $ \case
    Nothing -> pure mempty
    Just bs' -> do
      onPostBuild <- getPostBuild
      bm <- flip Map.traverseWithKey bs' $ \_ r -> do
        onErrResp <- performBackendRequest emptyWallet backendL $
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


-- | Send a transaction via the /send endpoint.
--
--   This is a convenience wrapper around `backendRequest`, use that if you
--   need some richer request information attached to your response or if this is really all you need `performBackendRequestCustom`.
performBackendRequest
  :: forall t m
  . ( PerformEvent t m, MonadJSM (Performable m)
    , HasJSContext (Performable m), TriggerEvent t m
    , MonadSample t (Performable m)
    )
  => Wallet t
  -> Backend t
  -> Event t BackendRequest
  -> m (Event t (BackendRequest, BackendErrorResult))
performBackendRequest w backendL onReq =
  performBackendRequestCustom w backendL id onReq

-- | Send a transaction via the /send endpoint.
--
--   This is a convenience wrapper around `backendRequest`, attaching a custom
--   request type to the response.
performBackendRequestCustom
  :: forall t m req
  . ( PerformEvent t m, MonadJSM (Performable m)
    , HasJSContext (Performable m), TriggerEvent t m
    , MonadSample t (Performable m)
    )
  => Wallet t
  -> Backend t
  -> (req -> BackendRequest)
  -> Event t req
  -> m (Event t (req, BackendErrorResult))
performBackendRequestCustom w backendL unwrap onReq =
    performEventAsync $ ffor onReqWithKeys $ \(keys, req) cb -> do
      meta <- sample $ current $ backendL ^. backend_meta
      backendRequest (meta, keys, unwrap req) backendL $ cb . (req,)
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
  => (PublicMeta, KeyPairs, BackendRequest)
  -> Backend t
  -> (BackendErrorResult -> IO ())
  -> m ()
backendRequest (meta, keys, req) backend cb = void . forkJSM $ do
  let
      callback = liftIO . cb
      uri = brUri $ _backendRequest_backend req
      signing = _backendRequest_signing req
      manager = _backend_httpManager backend
  payload <- buildSendPayload meta keys signing req
  baseUrl <- S.parseBaseUrl $ T.unpack uri
  let clientEnv = S.mkClientEnv manager baseUrl
  sent <- liftIO $ S.runClientM (send payload) clientEnv
  void $ case sent of
    Left e -> callback $ Left (BackendError_BackendError $ T.pack $ show e)
    Right res -> case res of
      ApiFailure e -> callback $ Left (BackendError_BackendError $ T.pack e)
      ApiSuccess sentValue -> case _rkRequestKeys sentValue of
        [] -> callback $ Left $ BackendError_Other "Response did not contain any RequestKeys"
        [key] -> do
          listened <- liftIO $ S.runClientM (listen (ListenerRequest key)) clientEnv
          case listened of
            Left e -> callback $ Left (BackendError_BackendError $ T.pack $ show e)
            Right listenResult -> case listenResult of
              ApiFailure e -> callback $ Left (BackendError_BackendError $ T.pack e)
              ApiSuccess listenedValue -> callback $ Right $ _arResult listenedValue
        _ -> callback $ Left $ BackendError_Other "Response contained more than one RequestKey"

-- TODO: upstream this?
instance HasJSContext JSM where
  type JSContextPhantom JSM = JSContextRef
  askJSContext = JSContextSingleton <$> askJSM


-- Request building ....

-- | Build payload as expected by /send endpoint.
buildSendPayload :: (MonadIO m, MonadJSM m) => PublicMeta -> KeyPairs -> Set KeyName -> BackendRequest -> m SubmitBatch
buildSendPayload meta keys signing req = do
  cmd <- buildCmd meta keys signing req
  pure $ SubmitBatch [ cmd ]

-- | Build a single cmd as expected in the `cmds` array of the /send payload.
--
-- As specified <https://pact-language.readthedocs.io/en/latest/pact-reference.html#send here>.
buildCmd :: (MonadIO m, MonadJSM m) => PublicMeta -> KeyPairs -> Set KeyName -> BackendRequest -> m (Command Text)
buildCmd meta keys signing req = do
  cmd <- encodeAsText . encode <$> buildExecPayload meta req
  let
    cmdHash = hash (T.encodeUtf8 cmd)
  sigs <- buildSigs cmdHash keys signing
  pure $ Pact.Types.Command.Command
    { _cmdPayload = cmd
    , _cmdSigs = sigs
    , _cmdHash = cmdHash
    }

-- | Build signatures for a single `cmd`.
buildSigs :: MonadJSM m => Hash -> KeyPairs -> Set KeyName -> m [UserSig]
buildSigs cmdHash keys signing = do
  let
    -- isJust filter is necessary so indices are guaranteed stable even after
    -- the following `mapMaybe`:
    isForSigning (name, (KeyPair _ priv)) = Set.member name signing && isJust priv

    signingPairs = filter isForSigning . Map.assocs $ keys
    signingKeys = mapMaybe _keyPair_privateKey $ map snd signingPairs

  sigs <- traverse (mkSignature (unHash cmdHash)) signingKeys

  let
    mkSigPubKey :: KeyPair -> Signature -> UserSig
    mkSigPubKey kp sig = UserSig ED25519 (keyToText sig) (keyToText $ _keyPair_publicKey kp)
  pure $ zipWith mkSigPubKey (map snd signingPairs) sigs


-- | Build exec `cmd` payload.
--
--   As specified <https://pact-language.readthedocs.io/en/latest/pact-reference.html#cmd-field-and-payloads here>.
--   Use `encodedAsText` for passing it as the `cmd` payload.
buildExecPayload :: MonadIO m => PublicMeta -> BackendRequest -> m (Payload PublicMeta Text)
buildExecPayload meta req = do
  nonce <- getNonce
  let
    payload = ExecMsg
      { _pmCode = _backendRequest_code req
      , _pmData = Object $ _backendRequest_data req
      }
  pure $ Payload
    { _pPayload = Exec payload
    , _pNonce = nonce
    , _pMeta = meta
    }

-- Response handling ...

-- | Extract the response body from a Pact server response:
-- getResPayload
--   :: FromJSON resp
--   => Either XhrException XhrResponse
--   -> Either BackendError resp
-- getResPayload xhrRes = do
--   r <- left BackendError_XhrException xhrRes
--   let
--     status = _xhrResponse_status r
--     statusText = _xhrResponse_statusText r
--     respText = _xhrResponse_responseText r
--     tooLarge = 413 -- Status code for too large body requests.
--     mRespT = BSL.fromStrict . T.encodeUtf8 <$> _xhrResponse_responseText r

--   -- TODO: This does not really work, when testing we got an
--   -- `XhrException` with no further details.
--   when (status == tooLarge)
--     $ throwError BackendError_ReqTooLarge
--   when (status /= 200 && status /= 201)
--     $ throwError $ BackendError_BackendError statusText

--   respT <- maybe (throwError BackendError_NoResponse) pure mRespT
--   pactRes <- case eitherDecode respT of
--     Left e
--       -> Left $ BackendError_ParseError $ T.pack e <> fromMaybe "" respText
--     Right v
--       -> Right v
--   case pactRes of
--     ApiFailure str ->
--       throwError $ BackendError_Failure . T.pack $ str
--     ApiSuccess a -> pure a

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
  BackendCfg refreshA deployA setChainIdA setGasLimitA setGasPriceA <>
    BackendCfg refreshB deployB setChainIdB setGasLimitB setGasPriceB
      = BackendCfg
        { _backendCfg_refreshModule = leftmost [ refreshA, refreshB ]
        , _backendCfg_deployCode    = leftmost [ deployA, deployB ]
        , _backendCfg_setChainId    = leftmost [ setChainIdA, setChainIdB ]
        , _backendCfg_setGasLimit   = leftmost [ setGasLimitA, setGasLimitB ]
        , _backendCfg_setGasPrice   = leftmost [ setGasPriceA, setGasPriceB ]
        }

instance Reflex t => Monoid (BackendCfg t) where
  mempty = BackendCfg never never never never never
  mappend = (<>)
--

instance Flattenable (BackendCfg t) t where
  flattenWith doSwitch ev =
    BackendCfg
      <$> doSwitch never (_backendCfg_refreshModule <$> ev)
      <*> doSwitch never (_backendCfg_deployCode <$> ev)
      <*> doSwitch never (_backendCfg_setChainId <$> ev)
      <*> doSwitch never (_backendCfg_setGasLimit <$> ev)
      <*> doSwitch never (_backendCfg_setGasPrice <$> ev)
