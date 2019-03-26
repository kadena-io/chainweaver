{-# LANGUAGE CPP                        #-}
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
  , BackendUri
  , BackendRequestV (..), backendRequest_code, backendRequest_data, backendRequest_signing
  , BackendRequest
  {- , RawBackendRequest -}
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
  , performLocalReadCustom
  , performLocalRead
    -- * Utilities
  , prettyPrintBackendErrorResult
  , prettyPrintBackendError
  ) where

import           Control.Arrow                     (left, (&&&), (***))
import           Control.Lens                      hiding ((.=))
import           Control.Monad.Except
import           Data.Aeson                        (FromJSON (..), Object,
                                                    ToJSON (..), Value (..),
                                                    encode, genericParseJSON,
                                                    genericToEncoding,
                                                    genericToJSON, withObject,
                                                    (.:))
import           Data.Aeson.Types                  (typeMismatch)
import Control.Monad.Catch (MonadThrow)
import qualified Data.ByteString.Lazy              as BSL
import           Data.Coerce                       (coerce)
import           Data.Default                      (def)
import qualified Data.HashMap.Strict               as H
import qualified Data.Map                          as Map
import           Data.Map.Strict                   (Map)
import           Data.Set                          (Set)
import Control.Monad.Reader (ReaderT (..))
import qualified Data.Set                          as Set
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import           Data.Time.Clock                   (getCurrentTime)
import           Generics.Deriving.Monoid          (mappenddefault,
                                                    memptydefault)
import           Language.Javascript.JSaddle.Monad (JSContextRef, JSM, askJSM,
                                                    liftJSM)
import qualified Network.HTTP.Types                as HTTP
import           Reflex.Dom.Class
import           Reflex.Dom.Xhr
import           Reflex.NotReady.Class

import           Pact.Typed.Server.Client
import           Pact.Typed.Types.API
import           Pact.Typed.Types.Command

import           Pact.Types.Hash                   (hash)
import           Pact.Types.RPC
import           Pact.Types.Util
import           Pact.Parse                        (ParsedDecimal (..),
                                                    ParsedInteger (..))
import           Pact.Types.Exp                    (Literal (LString))
import           Pact.Types.Term                   (Name,
                                                    Term (TList, TLiteral))

#if !defined (ghcjs_HOST_OS)
import           Pact.Types.Crypto                 (PPKScheme (..))
#endif

import           Common.Api
import           Common.Route                      (pactServerListPath)
import           Frontend.Crypto.Ed25519
import           Frontend.Foundation
import           Frontend.Messages
import           Frontend.Wallet
import           Frontend.Storage                  (getItemLocal, setItemLocal)
import qualified Servant.Client.JSaddle            as S

import Frontend.ModuleExplorer.RefPath as MP

-- | URI for accessing a backend.
type BackendUri = Text

-- | Name that uniquely describes a valid backend.
newtype BackendName = BackendName
  { unBackendName :: Text
  }
  deriving (Generic, Eq, Ord, Show, Semigroup, Monoid)

instance ToJSON BackendName where
  toJSON = genericToJSON compactEncoding
  toEncoding = genericToEncoding compactEncoding

instance FromJSON BackendName where
  parseJSON = genericParseJSON compactEncoding

-- | Render a backend name as `Text`.
textBackendName :: BackendName -> Text
textBackendName = coerce


instance IsRefPath BackendName where
  renderRef = mkRefPath . unBackendName

  parseRef = BackendName <$> MP.anySingle


-- | Request data to be sent to the backend.
data BackendRequest = BackendRequest
  { _backendRequest_code    :: Text
    -- ^ Pact code to be deployed, the `code` field of the
    -- <https://pact-language.readthedocs.io/en/latest/pact-reference.html#cmd-field-and-payload
    -- exec> payload.
  , _backendRequest_data    :: Object
    -- ^ The data to be deployed (referenced by deployed code). This is the
    -- `data` field of the `exec` payload.
  , _backendRequest_backend    :: BackendName
    -- ^ The backend to deploy to, specified by Name.
  , _backendRequest_signing :: Set KeyName
    -- ^ With what keys the request should be signed.
  } deriving (Show, Generic)

makePactLensesNonClassy ''BackendRequest


data BackendError
  = BackendError_BackendError Text
  -- ^ Server responded with a non 200 status code.
  | BackendError_ReqTooLarge
  -- ^ Request size exceeded the allowed limit.
  | BackendError_ParseError Text
  -- ^ Parsing the JSON response failed.
  | BackendError_CommandFailure CommandError
  -- ^ The status in the /listen result object was `failure`.
  | BackendError_DoesNotExist BackendName
  -- ^ The request could not be processed as the given `BackendName` no longer
  -- exists.
  | BackendError_InvalidUri BackendName
  -- ^ The given backend has no valid URI.
  | BackendError_Other Text
  -- ^ Other errors that should really never happen.
  deriving Show

-- | We either have a `BackendError` or some `Term Name`.
type BackendErrorResult = Either BackendError (Term Name)

-- | Config for creating a `Backend`.
data BackendCfg t = BackendCfg
  { _backendCfg_refreshModule :: Event t ()
    -- ^ We are unfortunately not notified by the pact backend when new
    -- contracts appear on the blockchain, so UI code needs to request a
    -- refresh at appropriate times.
  , _backendCfg_deployCode    :: Event t BackendRequest
    -- ^ Deploy some code to the backend. Response will be logged to `Messages`.
  , _backendCfg_setChainId    :: Event t Text
    -- ^ On what chain to deploy to (ignored on pact -s backend).
  , _backendCfg_setSender     :: Event t Text
    -- ^ What user wants to pay for this transaction?
  , _backendCfg_setGasLimit   :: Event t ParsedInteger
    -- ^ Maximum amount of gas to use for this transaction.
  , _backendCfg_setGasPrice   :: Event t ParsedDecimal
    -- ^ Maximum gas price you are willing to accept for having your
    -- transaction executed.
  }
  deriving Generic

makePactLenses ''BackendCfg

-- | HasBackendCfg with additional constraints to make it behave like a proper
-- config.
type IsBackendCfg cfg t = (HasBackendCfg cfg t, Monoid cfg, Flattenable cfg t)

data Backend t = Backend
  { _backend_backends :: Dynamic t (Maybe (Map BackendName BackendUri))
    -- ^ All available backends that can be selected.
  , _backend_modules  :: Dynamic t (Map BackendName (Maybe [Text]))
   -- ^ Available modules on all backends. `Nothing` if not loaded yet.
   -- TODO: This should really go to the `ModuleExplorer` or should it?
  , _backend_deployed :: Event t ()
   -- ^ Event gets triggered whenever some code got deployed sucessfully.
  , _backend_meta     :: Dynamic t PublicMeta
   -- ^ Meta data used for deployments. Can be modified via above
   -- `_backendCfg_setChainId`, `_backendCfg_setGasLimit`, ...
  }

makePactLenses ''Backend

-- | Model/dependencies of Backend.
type HasBackendModel model t = HasWallet model t

-- | Model config needed by Backend.
type HasBackendModelCfg mConf t = (Monoid mConf, HasMessagesCfg mConf t)

-- | Things we want to store to local storage.
data StoreBackend a where
  StoreBackend_GasSettings  :: StoreBackend PublicMeta

deriving instance Show (StoreBackend a)

-- NEXT STEPS:
--   - Get rid of RequestType
--   - No longer abstract /local and /send, but treat them differently
--   - Implement /send in conjunction with /poll - separated.
--   - Re-implement deploy in terms of /send and /poll and a dynamic holding state.
--   - Expose /local and make it not need wallet and keysets.
-- | What endpoint to use for a backend request.

makeBackend
  :: forall t m model mConf
  . ( MonadHold t m, PerformEvent t m, MonadFix m, NotReady t m, Adjustable t m
    , MonadJSM (Performable m), HasJSContext (Performable m), MonadJSM m
    , TriggerEvent t m, PostBuild t m
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

    modules <- loadModules backendL $ leftmost
      [ onDeployed
      , cfg ^. backendCfg_refreshModule
      , () <$ updated bs
      ]

    meta <- buildMeta cfg

    pure
      ( mConf
      , Backend
          { _backend_backends = bs
          , _backend_modules = modules
          , _backend_deployed = onDeployed
          , _backend_meta = meta
          }
      )


buildMeta
  :: ( MonadHold t m, MonadFix m, MonadJSM m
     , PerformEvent t m, MonadJSM (Performable m), TriggerEvent t m
     )
  => BackendCfg t -> m (Dynamic t PublicMeta)
buildMeta cfg = do
  let defaultMeta =
        PublicMeta
          { _pmChainId = "1"
          , _pmSender  = "someSender"
          , _pmGasLimit = ParsedInteger 100 -- TODO: Better defaults!!!
          , _pmGasPrice = ParsedDecimal 0.001
          , _pmFee = ParsedDecimal 1
          }
  m <- fromMaybe defaultMeta <$> liftJSM (getItemLocal StoreBackend_GasSettings)

  r <- foldDyn id m $ leftmost
    [ (\u c -> c { _pmChainId = u})  <$> cfg ^. backendCfg_setChainId
    , (\u c -> c { _pmSender = u})   <$> cfg ^. backendCfg_setSender
    , (\u c -> c { _pmGasLimit = u}) <$> cfg ^. backendCfg_setGasLimit
    , (\u c -> c { _pmGasPrice = u}) <$> cfg ^. backendCfg_setGasPrice
    ]

  onStore <- throttle 2 $ updated r
  performEvent_ $ liftJSM . setItemLocal StoreBackend_GasSettings <$> onStore

  pure r


deployCode
  :: forall t m model mConf
  . ( MonadHold t m, PerformEvent t m
    , MonadJSM (Performable m)
    , TriggerEvent t m
    , MonadSample t (Performable m)
    , HasBackendModel model t
    , HasBackendModelCfg mConf t
    )
  => model
  -> Backend t
  -> Event t BackendRequest
  -> m (mConf, Event t ())
deployCode w backendL onReq = do
  onErrKeys <- performSend (w ^. wallet) backendL onReq
  let
    onSendErr = fmapMaybe (traverse $ preview _Left) onErrKeys
    onReqKeys = fmapMaybe (traverse $ preview _Right) onErrKeys

    onReqsKeys :: Event t [(BackendRequest, RequestKey)]
    onReqsKeys = uncurry (map . (,)) . second coerce <$> onReqKeys

    onNewEntries :: Event t (MonoidalMap BackendName (Map RequestKey BackendRequest))
    onNewEntries =
      MMap.fromListWith (<>) (_backendRequest_backend . fst &&& Map.fromList swap) <$> onReqsKeys

  sentReqs <- foldDyn id Map.empty $ leftmost
    [ mappend <$> onNewEntries
    ]

  now <- getCurrentTime
  onTick <- fmap (const ()) <$> tickLossy 10 now
  onFirstPoll <- fmap (const ()) <$> delay 2 onReqKeys
  let
    onPollReqs = fanMap $ tag current sentReqs $ leftmost
      [ onTick, onFirstPoll ]
    backends = MMap.keys <$> current sentReqs

  networkView $ traverse (getReqEvents onPollReqs) <$> backends

  onErrResps <- performPoll backendL onPollReq

  let
    onClearSent = (traverse $ preview _Right)
    reqRes = leftmost [ fmap Left <$> onSendErr, onErrResps ]


  pure $ ( mempty & messagesCfg_send .~ fmap renderReqRes reqRes
         , () <$ ffilter (either (const False) (const True) . snd) reqRes
         )
  where
    getReqEvents :: EventSelector t (Const2 BackendName) -> BackendName ->

    renderReqRes :: (BackendRequest, BackendErrorResult) -> Text
    renderReqRes (req, res) =
      T.unlines [renderReq req, prettyPrintBackendErrorResult res]

    renderReq :: BackendRequest -> Text
    renderReq req =
      let
        backendName = _backendRequest_backend req
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
  => m (Dynamic t (Maybe (Map BackendName BackendUri)))
getBackends = do
  let
    buildUrl = ("http://localhost:" <>) . getPactInstancePort
    buildName = BackendName . ("dev-" <>) . T.pack . show
    buildServer =  buildName &&& buildUrl
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
parsePactServerList :: Text -> Map BackendName BackendUri
parsePactServerList raw =
  let
    rawEntries = map (fmap (T.dropWhile (== ':')) . T.breakOn ":") . T.lines $ raw
    stripped = map (BackendName . T.strip *** T.strip) rawEntries
  in
    Map.fromList stripped

-- | Load modules on startup and on every occurrence of the given event.
loadModules
  :: forall t m
  . ( MonadHold t m, PerformEvent t m, MonadFix m, NotReady t m, Adjustable t m
    , MonadJSM (Performable m)
    , MonadSample t (Performable m)
    , TriggerEvent t m, PostBuild t m
    )
  => Backend t
  -> Event t ()
  -> m (Dynamic t (Map BackendName (Maybe [Text])))
loadModules backendL onRefresh = do
      let
        bs = backendL ^. backend_backends
        req n = (BackendRequest "(list-modules)" H.empty n Set.empty)
      backendMap <- networkView $ ffor bs $ \case
        Nothing -> pure mempty
        Just bs' -> do
          onPostBuild <- getPostBuild
          bm <- flip Map.traverseWithKey bs' $ \n _ -> do
            onErrResp <- performLocalRead backendL $
              leftmost [ req n <$ onRefresh
                       , req n <$ onPostBuild
                       ]
            let
              (onErr, onResp) = fanEither $ snd <$> onErrResp

              onModules :: Event t (Maybe [Text])
              onModules =  getModuleList <$> onResp
            performEvent_ $ liftIO . putStrLn . ("ERROR: " <>) . show <$> onErr

            holdUniqDyn =<< holdDyn Nothing onModules
          pure $ sequence bm

      join <$> holdDyn mempty backendMap
    where
      getModuleList :: Term Name -> Maybe [Text]
      getModuleList = \case
        TList terms _ _ -> traverse getStringLit terms
        _               -> Nothing

      getStringLit :: Term Name -> Maybe Text
      getStringLit = \case
        TLiteral (LString v) _ -> Just v
        _         -> Nothing

-- | Monad transformer stack for doing backend requests.
type BackendReqT m = ReaderT S.ClientEnv (ExceptT BackendError m)

liftClientM :: S.ClientM a -> BackendReqT m a
liftClientM m = reThrowWith packHttpErr . S.runClientM m =<< ask


-- | Perform a read or non persisted request to the /local endpoint.
--
--   Use `performLocalReadCustom` for more flexibility.
performLocalRead
  :: forall t m
  . ( PerformEvent t m, MonadJSM (Performable m)
    , TriggerEvent t m
    , MonadSample t (Performable m)
    )
  => Backend t
  -> Event t BackendRequest
  -> m (Event t (BackendRequest, BackendErrorResult))
performLocalRead backendL onReq = performLocalReadCustom backendL id onReq


-- | Perform a read or other non persisted request to the /local endpoint.
--
--   Use this function if you want to retrieve data from the backend. It does
--   not sign the sent messages and uses some fake meta data to make sure the user
--   won't get charged for request made via `performLocalReadCustom`.
performLocalReadCustom
  :: forall t m req
  . ( PerformEvent t m, MonadJSM (Performable m)
    , TriggerEvent t m
    , MonadSample t (Performable m)
    )
  => Backend t
  -> (req -> BackendRequest)
  -> Event t req
  -> m (Event t (req, BackendErrorResult))
performLocalReadCustom backendL bName unwrap onReq =
  let
    myMeta = pure $ PublicMeta
      { _pmChainId = "1"
      , _pmSender  = "someSender"
      , _pmGasLimit = ParsedInteger 100000
      , _pmGasPrice = ParsedDecimal 1.0
      , _pmFee = ParsedDecimal 100
      }
  in
    performReqTagged backendL bName onReq (_backendRequest_backend . unwrap) $ \req -> do
      payload <- buildCmd myMeta mempty mempty req
      r <- liftClientM $ local pactServerApiClient cmd
      fromCommandValue r


-- | Send a transaction via the /send endpoint.
--
--   The resulting event contains the triggering event so you can keep track of
--   response/request relations easily.
performSend
  :: forall t m req
  . ( PerformEvent t m, MonadJSM (Performable m)
    , TriggerEvent t m
    , MonadSample t (Performable m)
    )
  => Wallet t
  -> Backend t
  -> Event t req
  -> m (Event t (req, Either BackendError RequestKeys))
performSend w backendL onReq = performSendCustom w backendL id onReq


-- | Send a transaction via the /send endpoint.
performSendCustom
  :: forall t m req
  . ( PerformEvent t m, MonadJSM (Performable m)
    , TriggerEvent t m
    , MonadSample t (Performable m)
    )
  => Wallet t
  -> Backend t
  -> (req -> BackendRequest)
  -> Event t req
  -> m (Event t (req, Either BackendError RequestKeys))
performSendCustom w backendL bName unwrap onReq =
  performReqTagged backendL onReq (_backendRequest_backend . unwrap) $ \req -> do
    payload <- backendBuildCmd w backendL (unwrap req)
    liftClientM $ send pactServerApiClient $ SubmitBatch . pure $ payload


-- | Send a transaction via the /send endpoint.
performPoll
  :: forall t m req
  . ( PerformEvent t m, MonadJSM (Performable m)
    , TriggerEvent t m
    , MonadSample t (Performable m)
    )
  => Backend t
  -> Event t (BackendName, Poll)
  -> m (Event t (Either BackendError PollResponses))
performPoll w backendL bName unwrap onReq =
  performReqTagged backendL onReq fst $ \(_, req) -> do
    liftClientM $ poll pactServerApiClient req


-- | Build a `Command Text` given the current backend and the wallet from a
--   `BackendRequest`.
backendBuildCmd
  :: forall t m req
  . ( MonadSample t m)
  => Wallet t
  -> Backend t
  -> BackendRequest
  -> m (Command Text)
backendBuildCmd w backendL req = do
  meta <- sample $ current $ backendL ^. backend_meta
  keys <- sample $ current $ _wallet_keys w
  let signing = _backendRequest_signing rawReq
  buildCmd meta keys signing req


-- | Perform a request in the `BackendReqT`.
--
--   The resulting event also contains the initiating request, so you can
--   relate responses with requests easily.
performReqTagged
  :: Backend t
  -> Event t req
  -> (req -> BackendName) -- ^ Get the backend name to use.
  -> (req -> BackendReqT (Performable m) a)
  -> m (Event t (req, Either BackendError a))
performReqTagged backendL onReq getBName doReq =
  performEventAsync $ ffor onReq $ \req cb -> void . forkIO $ do
    res <- runBackendReqT backendL (getBName req) $ doReq req
    liftIO $ cb (req, res)


-- | Run a `BackendReqT` transformer stack.
runBackendReqT
  :: (MonadJSM m, MonadSample t m)
  => Backend t
  -> BackendName
  -> BackendReqT m a -- ^ The action to run.
  -> m (Either BackendError a)
runBackendReqT backendL bName action = runExceptT $ do
  env <- getClientEnv backendL bName
  runReaderT action env

-------------------- Helper functions for request handling ----------------------------

-- | Get `ClientEnv` for running requests on a given backend.
getClientEnv
  :: (MonadSample t m)
  => Backend t
  -> BackendName
  -> ExceptT BackendError m S.ClientEnv
getClientEnv backendL bName = do

  cBackends <- sample . current $ backendL ^. backend_backends

  uri <- maybe (throwError $ BackendError_DoesNotExist bName) pure $
    cBackends ^. at bName

  baseUrl <- maybe (throwError $ BackendError_InvalidUri bName) pure $
    S.parseBaseUrl $ T.unpack uri

  pure $ S.mkClientEnv baseUrl


-- | Rethrow an error value by wrapping it with f.
reThrowWith
  :: (Monad m, MonadTrans tr, MonadError BackendError (tr m))
  => (e -> BackendError)
  -> m (Either e a)
  -> tr m a
reThrowWith f x = either (throwError . f) pure =<< lift x


packHttpErr :: S.ServantError -> BackendError
packHttpErr e = case e of
  S.FailureResponse response ->
    if S.responseStatusCode response == HTTP.status413
       then BackendError_ReqTooLarge
       else BackendError_BackendError $ T.pack $ show response
  _ -> BackendError_BackendError $ T.pack $ show e

fromCommandValue :: MonadError BackendError m => CommandValue -> m (Term Name)
fromCommandValue = \case
  CommandFailure e -> throwError $ BackendError_CommandFailure e
  CommandSuccess v -> pure v

getRequestKey :: MonadError BackendError m => RequestKeys -> m RequestKey
getRequestKey r =
  case _rkRequestKeys r of
    []    -> throwError $ BackendError_Other "Response did not contain any RequestKeys."
    [key] -> pure key
    _     -> throwError $ BackendError_Other "Response contained more than one RequestKey."

---------------------------------------------------------------------------------------


-- Request building ....

-- | Build a single cmd as expected in the `cmds` array of the /send payload.
--
-- As specified <https://pact-language.readthedocs.io/en/latest/pact-reference.html#send here>.
buildCmd :: (MonadIO m, MonadJSM m) => PublicMeta -> KeyPairs -> Set KeyName -> BackendRequest -> m (Command Text)
buildCmd meta keys signing req = do
  cmd <- encodeAsText . encode <$> buildExecPayload meta req
  let
    cmdHash = hash (T.encodeUtf8 cmd)
  sigs <- buildSigs cmdHash keys signing
  pure $ Pact.Typed.Types.Command.Command
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
    mkSigPubKey kp sig = UserSig ED25519 (keyToText $ _keyPair_publicKey kp) (keyToText sig)
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

-- | Pretty print a `BackendError`.
prettyPrintBackendError :: BackendError -> Text
prettyPrintBackendError = ("ERROR: " <>) . \case
  BackendError_BackendError msg -> "Error http response: " <> msg
  BackendError_ReqTooLarge -> "Request exceeded the allowed maximum size!"
  BackendError_ParseError m -> "Server response could not be parsed: " <> m
  BackendError_CommandFailure (CommandError e d) -> T.pack e <> ": " <> maybe "" T.pack d
  BackendError_DoesNotExist (BackendName n) -> "Backend named '" <> n <> "' no longer exists."
  BackendError_InvalidUri (BackendName n) -> "Backend named '" <> n <> "' has no proper URI."
  BackendError_Other m -> "Some unknown problem: " <> m


-- | Pretty print a `BackendErrorResult`.
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

-- Instances:

-- TODO: upstream this?
instance HasJSContext JSM where
  type JSContextPhantom JSM = JSContextRef
  askJSContext = JSContextSingleton <$> askJSM

instance Semigroup BackendRequest where
  (<>) = mappenddefault

instance Monoid BackendRequest where
  mempty = memptydefault
  mappend = (<>)

instance Reflex t => Semigroup (BackendCfg t) where
  BackendCfg refreshA deployA setChainIdA setSenderA setGasLimitA setGasPriceA <>
    BackendCfg refreshB deployB setChainIdB setSenderB setGasLimitB setGasPriceB
      = BackendCfg
        { _backendCfg_refreshModule = leftmost [ refreshA, refreshB ]
        , _backendCfg_deployCode    = leftmost [ deployA, deployB ]
        , _backendCfg_setChainId    = leftmost [ setChainIdA, setChainIdB ]
        , _backendCfg_setSender    = leftmost [ setSenderA, setSenderB ]
        , _backendCfg_setGasLimit   = leftmost [ setGasLimitA, setGasLimitB ]
        , _backendCfg_setGasPrice   = leftmost [ setGasPriceA, setGasPriceB ]
        }

instance Reflex t => Monoid (BackendCfg t) where
  mempty = BackendCfg never never never never never never
  mappend = (<>)
--

instance Flattenable (BackendCfg t) t where
  flattenWith doSwitch ev =
    BackendCfg
      <$> doSwitch never (_backendCfg_refreshModule <$> ev)
      <*> doSwitch never (_backendCfg_deployCode <$> ev)
      <*> doSwitch never (_backendCfg_setChainId <$> ev)
      <*> doSwitch never (_backendCfg_setSender <$> ev)
      <*> doSwitch never (_backendCfg_setGasLimit <$> ev)
      <*> doSwitch never (_backendCfg_setGasPrice <$> ev)
