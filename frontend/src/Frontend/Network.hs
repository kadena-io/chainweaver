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
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Interface for accessing Pact networks (pact -s, chainweb, kadena).
--
--   The module offers the possibility of selecting a particular network and
--   sending commands to it, by making use of the Pact REST API.
module Frontend.Network
  ( -- * Types & Classes
    NetworkName
  , textNetworkName
  , NetworkRequest (..), networkRequest_code, networkRequest_data, networkRequest_signing
  , Endpoint (..)
  , displayEndpoint
  , NetworkError (..)
  , NetworkErrorResult
  , NetworkCfg (..)
  , HasNetworkCfg (..)
  , IsNetworkCfg
  , Network (..)
  , HasNetwork (..)
    -- * Definitions from Common
  , module Common.Network
    -- * NodeInfo
  , module Frontend.Network.NodeInfo
    -- * Creation
  , makeNetwork
    -- * Perform requests
  , performLocalReadCustom
  , performLocalRead
    -- * Utilities
  , prettyPrintNetworkErrorResult
  , prettyPrintNetworkError
  ) where

import           Control.Arrow                     (left, (&&&), (***))
import           Control.Arrow                     (first, second)
import           Control.Arrow                     (right)
import           Control.Lens                      hiding ((.=))
import           Control.Monad.Except
import           Data.Aeson                        (FromJSON (..), Object,
                                                    ToJSON (..), Value (..),
                                                    encode, genericParseJSON,
                                                    genericToEncoding,
                                                    genericToJSON)
import qualified Data.ByteString.Lazy              as BSL
import           Data.Coerce                       (coerce)
import           Data.Default                      (def)
import           Data.Either                       (lefts, rights)
import qualified Data.HashMap.Strict               as H
import qualified Data.Map                          as Map
import           Data.Map.Strict                   (Map)
import           Data.Set                          (Set)
import qualified Data.Set                          as Set
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import qualified Data.Text.IO                      as T
import           Data.Time.Clock                   (getCurrentTime)
import           Language.Javascript.JSaddle.Monad (JSM, liftJSM)
import qualified Network.HTTP.Types                as HTTP
import           Reflex.Dom.Class
import           Reflex.Dom.Xhr
import           Reflex.NotReady.Class
import           Safe                              (fromJustNote)
import           System.IO                         (stderr)
import           Text.URI                          (URI)
import qualified Text.URI                          as URI

import           Pact.Typed.Server.Client
import           Pact.Typed.Types.API
import           Pact.Typed.Types.Command

import           Pact.Parse                        (ParsedDecimal (..),
                                                    ParsedInteger (..))
import           Pact.Types.Exp                    (Literal (LString))
import           Pact.Types.Hash                   (hash)
import           Pact.Types.RPC
import           Pact.Types.Term                   (Name,
                                                    Term (TList, TLiteral),
                                                    tStr)
import           Pact.Types.Util
import qualified Servant.Client.JSaddle            as S

#if !defined (ghcjs_HOST_OS)
import           Pact.Types.Crypto                 (PPKScheme (..))
#endif

import           Common.Api
import           Common.Network                    (ChainId, NetworkName (..),
                                                    getNetworksConfig,
                                                    getPactInstancePort,
                                                    numPactInstances,
                                                    textNetworkName)
import           Common.Route                      (pactDynServerListPath)
import           Frontend.Crypto.Ed25519
import           Frontend.Foundation
import           Frontend.Messages
import           Frontend.Network.NodeInfo
import           Frontend.Storage                  (getItemStorage,
                                                    localStorage,
                                                    setItemStorage)
import           Frontend.Wallet



-- | What endpoint to use for a network request.
data Endpoint
  = Endpoint_Send
  | Endpoint_Local
  deriving (Show, Read, Generic, Eq, Ord, Bounded, Enum)

-- | Get string representation of `Endpoint` suitable for being displayed to an end user.
displayEndpoint :: Endpoint -> Text
displayEndpoint = \case
  Endpoint_Send -> "/send"
  Endpoint_Local -> "/local"

-- | Request data to be sent to the current network.
data NetworkRequest = NetworkRequest
  { _networkRequest_code     :: Text
    -- ^ Pact code to be deployed, the `code` field of the
    -- <https://pact-language.readthedocs.io/en/latest/pact-reference.html#cmd-field-and-payload
    -- exec> payload.
  , _networkRequest_data     :: Object
    -- ^ The data to be deployed (referenced by deployed code). This is the
    -- `data` field of the `exec` payload.
  , _networkRequest_chainId  :: ChainId
    -- ^ To what chain should this request go to.
  , _networkRequest_endpoint :: Endpoint
    -- ^ Where shall this request go? To /local or to /send?
  , _networkRequest_signing  :: Set KeyName
    -- ^ With what keys the request should be signed. Don't sign with any keys
    -- when calling to `performLocalReadCustom` and similar.
  } deriving (Show, Generic)

makePactLensesNonClassy ''NetworkRequest


data NetworkError
  = NetworkError_NetworkError Text
  -- ^ Server could not be reached.
  | NetworkError_Status HTTP.Status Text
  -- ^ Server responded with a non 200 status code.
  | NetworkError_ReqTooLarge
  -- ^ Request size exceeded the allowed limit.
  | NetworkError_CommandFailure CommandError
  -- ^ The status in the /listen result object was `failure`.
  | NetworkError_NoNetwork Text
  -- ^ An action requiring a network was about to be performed, but we don't
  -- (yet) have any selected network.
  | NetworkError_Other Text
  -- ^ Other errors that should really never happen.
  deriving Show

-- | We either have a `NetworkError` or some `Term Name`.
type NetworkErrorResult = Either NetworkError (Term Name)


-- | Config for creating a `Network`.
data NetworkCfg t = NetworkCfg
  { _networkCfg_refreshModule :: Event t ()
    -- ^ We are unfortunately not notified by the pact network when new
    -- contracts appear on the blockchain, so UI code needs to request a
    -- refresh at appropriate times.
  , _networkCfg_deployCode    :: Event t [NetworkRequest]
    -- ^ Deploy some code to the network. Response will be logged to `Messages`.
  , _networkCfg_setSender     :: Event t Text
    -- ^ What user wants to pay for this transaction?
  , _networkCfg_setGasLimit   :: Event t ParsedInteger
    -- ^ Maximum amount of gas to use for this transaction.
  , _networkCfg_setGasPrice   :: Event t ParsedDecimal
    -- ^ Maximum gas price you are willing to accept for having your
    -- transaction executed.
  }

makePactLenses ''NetworkCfg


-- | HasNetworkCfg with additional constraints to make it behave like a proper
-- config.
type IsNetworkCfg cfg t = (HasNetworkCfg cfg t, Monoid cfg, Flattenable cfg t)

data Network t = Network
  { _network_networks        :: Dynamic t (Map NetworkName [URI.Authority])
    -- ^ List of available networks. Each network has a name and a number of nodes.
  , _network_selectedNetwork :: Dynamic t (NetworkName, [Either Text NodeInfo])
    -- ^ The currently selected network, the first `Right` `NodeInfo` will be
    -- used for deployments and such.
  , _network_modules         :: Dynamic t (Map ChainId [Text])
   -- ^ Available modules on all chains.
  , _network_deployed        :: Event t ()
   -- ^ Event gets triggered whenever some code got deployed sucessfully.
  , _network_meta            :: Dynamic t PublicMeta
   -- ^ Meta data used for deployments. Can be modified via above
   -- `_networkCfg_setGasLimit` and similar. The chainid in this field gets
   -- effectively ignored as it gets overridden on request handling, by the
   -- value `_networkRequest_chainId` of the handled `NetworkRequest`.
   --
   -- Note: We currently only keep one meta data for all possible networks. Having a
   -- persisted `PublicMeta` per network, would probably be useful, but is not
   -- yet implemented.
  }

makePactLenses ''Network

-- | Model/dependencies of Network.
type HasNetworkModel model t = HasWallet model t

-- | Model config needed by Network.
type HasNetworkModelCfg mConf t = (Monoid mConf, HasMessagesCfg mConf t)

-- | Things we want to store to local storage.
data StoreNetwork a where
  StoreNetwork_PublicMeta  :: StoreNetwork PublicMeta

deriving instance Show (StoreNetwork a)


makeNetwork
  :: forall t m model mConf
  . ( MonadHold t m, PerformEvent t m, MonadFix m, NotReady t m, Adjustable t m
    , MonadJSM (Performable m), HasJSContext (Performable m), MonadJSM m
    , TriggerEvent t m, PostBuild t m
    , MonadSample t (Performable m)
    , HasNetworkModel model t
    , HasNetworkModelCfg mConf t
    )
  => model
  -> NetworkCfg t
  -> m (mConf, Network t)
makeNetwork w cfg = mfix $ \ ~(_, networkL) -> do

    (mConf, onDeployed) <- deployCode w networkL $ cfg ^. networkCfg_deployCode

    cNameNetworks <- getNetworks
    let cName = fst <$> cNameNetworks
    onCName <- tagOnPostBuild cName
    onNodeInfos <- performEvent $ getNetworkNodeInfos networkL <$> leftmost
      [ onCName
        -- Refresh info on deployments and well on refresh (Refreshed node
        -- info triggers re-loading of modules too.):
      , tag (current cName) $ onDeployed
      , tag (current cName) $ cfg ^. networkCfg_refreshModule
      ]
    performEvent_ $ traverse_ reportNodeInfoError <$> fmap lefts onNodeInfos
    nodeInfos <- holdDyn [Left "No node infos fetched yet!"] onNodeInfos

    modules <- loadModules networkL

    meta <- buildMeta cfg

    pure
      ( mConf
      , Network
          { _network_networks = snd <$> cNameNetworks
          , _network_selectedNetwork = (,) <$> cName <*> nodeInfos
          , _network_modules = modules
          , _network_deployed = onDeployed
          , _network_meta = meta
          }
      )
  where
    reportNodeInfoError err =
      liftIO $ T.putStrLn $ "Fetching node info failed: " <> err


-- | Retrieve the node information for the given `NetworkName`
getNetworkNodeInfos
  :: (Reflex t, MonadSample t m, MonadJSM m)
  => Network t
  -> NetworkName
  -> m [Either Text NodeInfo]
getNetworkNodeInfos nw netName = do
    cNets <- sample $ current $ nw ^. network_networks
    let hosts = fromMaybe [] $ cNets ^. at netName
    runJSM (traverse discoverNode hosts) =<< askJSM


-- | Get the currently selected network.
--
--   Error out, if there is none.
getSelectedNetworkInfos
  :: (Reflex t, MonadSample t m)
  => Network t
  -> m [NodeInfo]
getSelectedNetworkInfos networkL = do
    errNets <- fmap snd . sample $ current $ networkL ^. network_selectedNetwork
    pure $ rights errNets


buildMeta
  :: ( MonadHold t m, MonadFix m, MonadJSM m
     , PerformEvent t m, MonadJSM (Performable m), TriggerEvent t m
     )
  => NetworkCfg t -> m (Dynamic t PublicMeta)
buildMeta cfg = do
  let defaultMeta =
        PublicMeta
          { _pmChainId = "1" -- Gets overridden always!
          , _pmSender  = "sender00"
          , _pmGasLimit = ParsedInteger 100 -- TODO: Better defaults!!!
          , _pmGasPrice = ParsedDecimal 0.001
          }
  m <- fromMaybe defaultMeta <$>
    liftJSM (getItemStorage localStorage StoreNetwork_PublicMeta)

  r <- foldDyn id m $ leftmost
    [ (\u c -> c { _pmSender = u})   <$> cfg ^. networkCfg_setSender
    , (\u c -> c { _pmGasLimit = u}) <$> cfg ^. networkCfg_setGasLimit
    , (\u c -> c { _pmGasPrice = u}) <$> cfg ^. networkCfg_setGasPrice
    ]

  onStore <- throttle 2 $ updated r
  performEvent_ $
    liftJSM . setItemStorage localStorage StoreNetwork_PublicMeta <$> onStore

  pure r


deployCode
  :: forall t m model mConf
  . ( MonadHold t m, PerformEvent t m
    , MonadJSM (Performable m)
    , TriggerEvent t m
    , MonadSample t (Performable m)
    , HasNetworkModel model t
    , HasNetworkModelCfg mConf t
    )
  => model
  -> Network t
  -> Event t [NetworkRequest]
  -> m (mConf, Event t ())
deployCode w networkL onReq = do
    reqRes <- performNetworkRequest (w ^. wallet) networkL onReq
    pure $ ( mempty & messagesCfg_send .~ fmap (map renderReqRes) reqRes
           , () <$ ffilter (or . map (either (const False) (const True) . snd)) reqRes
           )
  where
    renderReqRes :: (NetworkRequest, NetworkErrorResult) -> Text
    renderReqRes (req, res) =
      T.unlines [renderReq req, prettyPrintNetworkErrorResult res]

    renderReq :: NetworkRequest -> Text
    renderReq req =
      let
        chainId = _networkRequest_chainId req
        code = _networkRequest_code req
        -- Not really helpful to display deployed code if it is large:
        mkMsg msg = if T.length code < 100 then msg else ""
      in
        mkMsg $ T.unlines
          [ "Sent code to chain '" <> tshow chainId <> "':"
          , ""
          , code
          ]


-- | Available networks:
getNetworks
  :: ( MonadJSM (Performable m), HasJSContext (Performable m)
     , PerformEvent t m, TriggerEvent t m, PostBuild t m, MonadIO m
     , MonadHold t m
     )
  => m (Dynamic t (NetworkName, Map NetworkName [URI.Authority]))
getNetworks = do
  let
    buildAuthority =
      either (\e -> error $ "Parsing of dev networks failed: " <> T.unpack e) id
      . parseAuthority
      . ("localhost:" <>)
      . getPactInstancePort

    buildName = NetworkName . ("dev-" <>) . tshow
    buildNetwork =  buildName &&& pure . buildAuthority
    devNetworks = Map.fromList $ map buildNetwork [1 .. numPactInstances]
  onPostBuild <- getPostBuild

  errProdCfg <- liftIO $ getNetworksConfig
  pure $ case errProdCfg of
    Left _ -> -- Development mode
      pure $ (buildName 1, devNetworks)
    Right c -> -- Production mode
      pure c


-- | Load modules on startup and on every occurrence of the given event.
loadModules
  :: forall t m
  . ( MonadHold t m, PerformEvent t m, MonadFix m, NotReady t m, Adjustable t m
    , MonadJSM (Performable m)
    , MonadSample t (Performable m)
    , TriggerEvent t m, PostBuild t m
    )
  => Network t
  -> m (Dynamic t (Map ChainId [Text]))
loadModules networkL = do

      let nodeInfos = (rights . snd) <$> (networkL ^. network_selectedNetwork)
      onNodeInfos <- tagOnPostBuild nodeInfos

      let onReqs = map mkReq . maybe [] getChains . listToMaybe  <$> onNodeInfos
      onErrResps <- performLocalRead networkL onReqs

      let
        onByChainId :: Event t [ Either NetworkError (ChainId, Term Name) ]
        onByChainId = map byChainId <$> onErrResps

        onErrs = ffilter (not . null) $ fmap lefts onByChainId
        onResps = fmap rights onByChainId

        onModules :: Event t [(ChainId, [Text])]
        onModules =  map (second getModuleList) <$> onResps

      performEvent_ $ liftIO . T.hPutStrLn stderr . renderErrs <$> onErrs

      holdUniqDyn <=< holdDyn mempty $ Map.fromList <$> onModules

    where

      mkReq n = NetworkRequest "(list-modules)" H.empty n Endpoint_Local Set.empty

      byChainId :: (NetworkRequest, NetworkErrorResult) -> Either NetworkError (ChainId, Term Name)
      byChainId = sequence . first _networkRequest_chainId

      renderErrs :: [NetworkError] -> Text
      renderErrs =
        T.unlines
        .  ("Error, retrieving modules for one ore more chains: " :)
        . map prettyPrintNetworkError


      getModuleList :: Term Name -> [Text]
      getModuleList = \case
        TList terms _ _ -> mapMaybe getStringLit $ toList terms
        _               -> []

      getStringLit :: Term Name -> Maybe Text
      getStringLit = \case
        TLiteral (LString v) _ -> Just v
        _         -> Nothing


-- | Perform a read or non persisted request to the /local endpoint.
--
--   Use `performLocalReadCustom` for more flexibility.
--
--   Note: See note for `performLocalReadCustom`.
performLocalRead
  :: forall t m
  . ( PerformEvent t m, MonadJSM (Performable m)
    , TriggerEvent t m
    , MonadSample t (Performable m)
    )
  => Network t
  -> Event t [NetworkRequest]
  -> m (Event t [(NetworkRequest, NetworkErrorResult)])
performLocalRead networkL onReqs = performLocalReadCustom networkL id onReqs


-- | Perform a read or other non persisted request to the /local endpoint.
--
--   Use this function if you want to retrieve data from the network. It does
--   not sign the sent messages and uses some fake meta data to make sure the user
--   won't get charged for request made via `performLocalReadCustom`.
--
--   Note: _networkRequest_endpoint will be ignored and set to
--   Endpoint_Local also the meta data used will be a dummy payload.
performLocalReadCustom
  :: forall t m req
  . ( PerformEvent t m, MonadJSM (Performable m)
    , TriggerEvent t m
    , MonadSample t (Performable m)
    )
  => Network t
  -> (req -> NetworkRequest)
  -> Event t [req]
  -> m (Event t [(req, NetworkErrorResult)])
performLocalReadCustom networkL unwrapUsr onReqs =
  let
    unwrap = (networkRequest_endpoint .~ Endpoint_Local) <$> unwrapUsr
    fakeNetwork = networkL
      { _network_meta = pure $ PublicMeta
          { _pmChainId = "1"
          , _pmSender  = "someSender"
          , _pmGasLimit = ParsedInteger 100000
          , _pmGasPrice = ParsedDecimal 1.0
          }
      }
  in
    performNetworkRequestCustom emptyWallet fakeNetwork unwrap onReqs


-- | Send a transaction via the /send endpoint.
--
--   This is a convenience wrapper around `networkRequest`, use that if you
--   need some richer request information attached to your response or if this is really all you need `performNetworkRequestCustom`.
performNetworkRequest
  :: forall t m
  . ( PerformEvent t m, MonadJSM (Performable m)
    , TriggerEvent t m
    , MonadSample t (Performable m)
    )
  => Wallet t
  -> Network t
  -> Event t [NetworkRequest]
  -> m (Event t [(NetworkRequest, NetworkErrorResult)])
performNetworkRequest w networkL onReq =
  performNetworkRequestCustom w networkL id onReq


-- | Send a transaction via the /send endpoint.
--
--   This is a convenience wrapper around `networkRequest`, attaching a custom
--   request type to the response.
performNetworkRequestCustom
  :: forall t m req
  . ( PerformEvent t m, MonadJSM (Performable m)
    , TriggerEvent t m
    , MonadSample t (Performable m)
    )
  => Wallet t
  -> Network t
  -> (req -> NetworkRequest)
  -> Event t [req]
  -> m (Event t [(req, NetworkErrorResult)])
performNetworkRequestCustom w networkL unwrap onReqs =
    performEventAsync $ ffor onReqs $ \reqs cb ->
      reportError cb reqs $ do
        keys <- sample $ current $ _wallet_keys w
        metaTemplate <- sample $ current $ networkL ^. network_meta
        nodeInfos <- getSelectedNetworkInfos networkL
        void $ forkJSM $ do
          r <- traverse (doReqFailover metaTemplate nodeInfos keys) reqs
          liftIO $ cb r
  where

    reportError cb reqs m = do
      er <- runExceptT m
      case er of
        Left err -> liftIO $ cb $ map (, Left err) reqs
        Right () -> pure ()

    doReqFailover metaTemplate nodeInfos keys req =
      (req,) <$> go (NetworkError_Other "All nodes were offline!") nodeInfos
        where
          go lastErr = \case
            n:ns -> do
              errRes <- doReq metaTemplate n keys req
              case errRes of
                Left err -> if shouldFailOver err
                               then go err ns
                               else pure errRes
                Right res -> pure errRes
            [] -> pure $ Left lastErr


    doReq metaTemplate nodeInfo keys req = do
      let
        unwrapped = unwrap req
        metaChainId = toPmChainId $ _networkRequest_chainId unwrapped
        meta = metaTemplate { _pmChainId = metaChainId }
        signing = _networkRequest_signing unwrapped
        chainUrl = getChainBaseUrl (unwrapped ^. networkRequest_chainId) nodeInfo
      payload <- buildCmd meta keys signing unwrapped
      liftJSM $ networkRequest chainUrl (unwrapped ^. networkRequest_endpoint) payload


-- | Send a transaction via the /send endpoint.
--   And wait for its result via /listen. `performNetworkRequest` is a little
--   more convenient to use if you don't need more elaborate request
--   information in your response.
--
--   Usage example:
--
-- @
--   let w = ourWallet
--   performEventAsync $ ffor (attachKeys w onReq) $ (\(keys, req)) cb ->
--     networkRequest (keys, buildNetworkReq req) $ cb . (req,)
-- @
--
--   This primitive function is also useful if you happen to need to execute
--   multiple requests whose responses should be fed in the reflex network
--   simultaneously, because the logically belong together for example:
--
-- @
--   let w = ourWallet
--   performEventAsync $ ffor (attachKeys w onReq) $ (\(keys, req)) cb -> do
--     networkRequest (keys, buildNetworkReq req) $ \res -> do
--       someMoreRequest someArg $ cb . (req, res,)
--
-- @
networkRequest
  :: URI
  -> Endpoint
  -> Command Text
  -> JSM NetworkErrorResult
networkRequest baseUri endpoint cmd = do
    baseUrl <- S.parseBaseUrl $ URI.renderStr baseUri
    let clientEnv = S.mkClientEnv baseUrl

    liftJSM . runExceptT $ do
      v <- performReq clientEnv
      ExceptT . pure $ fromCommandValue v

  where
    performReq clientEnv = case endpoint of
      Endpoint_Send -> do
        res <- runReq clientEnv $ send pactServerApiClient $ SubmitBatch . pure $ cmd
        key <- getRequestKey $ res
        -- TODO: If we no longer wait for /listen, we should change the type instead of wrapping that message in `Term Name`.
        pure $ CommandSuccess $ tStr $ T.dropWhile (== '"') . T.dropWhileEnd (== '"') . tshow $ key
        {- key <- getRequestKey $ res -}
        {- v <- runReq clientEnv $ listen pactServerApiClient $ ListenerRequest key -}
        {- case preview (Aeson.key "result" . Aeson.key "hlCommandResult" . _JSON) v of -}
        {-   Just cr -> pure cr -}
        {-   Nothing -> case fromJSON v of -}
        {-     Error str -> throwError $ NetworkError_ParseError $ T.pack str -}
        {-     Success ar -> pure $ _arResult ar -}
      Endpoint_Local ->
         runReq clientEnv  $ local pactServerApiClient cmd

    -- | Rethrow an error value by wrapping it with f.
    reThrowWith :: (e -> NetworkError) -> JSM (Either e a) -> ExceptT NetworkError JSM a
    reThrowWith f = ExceptT . fmap (left f)

    runReq :: S.ClientEnv -> S.ClientM a -> ExceptT NetworkError JSM a
    runReq env = reThrowWith packHttpErr . flip S.runClientM env

    packHttpErr :: S.ServantError -> NetworkError
    packHttpErr e = case e of
      S.FailureResponse response ->
        if S.responseStatusCode response == HTTP.status413
           then NetworkError_ReqTooLarge
           else NetworkError_Status (S.responseStatusCode response) (T.pack $ show response)
      _ -> NetworkError_NetworkError $ T.pack $ show e

    fromCommandValue :: MonadError NetworkError m => CommandValue -> m (Term Name)
    fromCommandValue = \case
      CommandFailure e -> throwError $ NetworkError_CommandFailure e
      CommandSuccess v -> pure v

    getRequestKey :: MonadError NetworkError m => RequestKeys -> m RequestKey
    getRequestKey r =
      case _rkRequestKeys r of
        []    -> throwError $ NetworkError_Other "Response did not contain any RequestKeys."
        [key] -> pure key
        _     -> throwError $ NetworkError_Other "Response contained more than one RequestKey."


-- Request building ....

-- | Build a single cmd as expected in the `cmds` array of the /send payload.
--
-- As specified <https://pact-language.readthedocs.io/en/latest/pact-reference.html#send here>.
buildCmd :: (MonadIO m, MonadJSM m) => PublicMeta -> KeyPairs -> Set KeyName -> NetworkRequest -> m (Command Text)
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
    mkSigPubKey kp sig = UserSig ED25519 pubKey pubKey (keyToText sig)
      where
        pubKey = keyToText $ _keyPair_publicKey kp

  pure $ zipWith mkSigPubKey (map snd signingPairs) sigs


-- | Build exec `cmd` payload.
--
--   As specified <https://pact-language.readthedocs.io/en/latest/pact-reference.html#cmd-field-and-payloads here>.
--   Use `encodedAsText` for passing it as the `cmd` payload.
buildExecPayload :: MonadIO m => PublicMeta -> NetworkRequest -> m (Payload PublicMeta Text)
buildExecPayload meta req = do
  nonce <- getNonce
  let
    payload = ExecMsg
      { _pmCode = _networkRequest_code req
      , _pmData = Object $ _networkRequest_data req
      }
  pure $ Payload
    { _pPayload = Exec payload
    , _pNonce = nonce
    , _pMeta = meta
    }


-- Response handling ...

-- | Is the given error one, where trying another host makes sense?
shouldFailOver :: NetworkError -> Bool
shouldFailOver = \case
  -- Failover on connectivity problems.
  NetworkError_NetworkError err -> True
  -- Failover on server errors:
  NetworkError_Status (HTTP.Status code _) _ -> code >= 500 && code < 600
  _ -> False


-- | Pretty print a `NetworkError`.
prettyPrintNetworkError :: NetworkError -> Text
prettyPrintNetworkError = ("ERROR: " <>) . \case
  NetworkError_NetworkError msg -> "Network error: " <> msg
  NetworkError_Status _ msg -> "Error HTTP response: " <> msg
  NetworkError_ReqTooLarge-> "Request exceeded the allowed maximum size!"
  NetworkError_CommandFailure (CommandError e d) -> T.pack e <> ": " <> maybe "" T.pack d
  NetworkError_NoNetwork t -> "An action requiring a network was about to be performed, but we don't (yet) have any selected network: '" <> t <> "'"
  NetworkError_Other m -> "Some unknown problem: " <> m


-- | Pretty print a `NetworkErrorResult`.
prettyPrintNetworkErrorResult :: NetworkErrorResult -> Text
prettyPrintNetworkErrorResult = \case
  Left e -> prettyPrintNetworkError e
  Right r -> "Server result: " <> prettyTextPretty r


-- | Get unique nonce, based on current time.
getNonce :: MonadIO m => m Text
getNonce = do
  t <- liftIO getCurrentTime
  pure $ T.pack . show $ t

-- | Treat encoded JSON as a Text value which can be encoded again.
--
--   This way you get stringified JSON.
encodeAsText :: BSL.ByteString -> Text
encodeAsText = safeDecodeUtf8 . BSL.toStrict

-- Instances:

instance Reflex t => Semigroup (NetworkCfg t) where
  NetworkCfg refreshA deployA setSenderA setGasLimitA setGasPriceA <>
    NetworkCfg refreshB deployB setSenderB setGasLimitB setGasPriceB
      = NetworkCfg
        { _networkCfg_refreshModule = leftmost [ refreshA, refreshB ]
        , _networkCfg_deployCode    =  deployA <> deployB
        , _networkCfg_setSender    = leftmost [ setSenderA, setSenderB ]
        , _networkCfg_setGasLimit   = leftmost [ setGasLimitA, setGasLimitB ]
        , _networkCfg_setGasPrice   = leftmost [ setGasPriceA, setGasPriceB ]
        }

instance Reflex t => Monoid (NetworkCfg t) where
  mempty = NetworkCfg never never never never never
  mappend = (<>)
--

instance Flattenable (NetworkCfg t) t where
  flattenWith doSwitch ev =
    NetworkCfg
      <$> doSwitch never (_networkCfg_refreshModule <$> ev)
      <*> doSwitch never (_networkCfg_deployCode <$> ev)
      <*> doSwitch never (_networkCfg_setSender <$> ev)
      <*> doSwitch never (_networkCfg_setGasLimit <$> ev)
      <*> doSwitch never (_networkCfg_setGasPrice <$> ev)
