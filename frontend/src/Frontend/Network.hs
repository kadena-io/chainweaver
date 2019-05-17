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
{-# LANGUAGE RecursiveDo                #-}
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
    NetworkRequest (..), networkRequest_code, networkRequest_data, networkRequest_signing
  , Endpoint (..)
  , displayEndpoint
  , NetworkError (..)
  , NetworkErrorResult
  , NetworkCfg (..)
  , HasNetworkCfg (..)
  , IsNetworkCfg
  , Network (..)
  , HasNetwork (..)
    -- * Useful helpers
  , updateNetworks
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

import           Control.Arrow                     (first, left, second, (&&&))
import           Control.Lens                      hiding ((.=))
import           Control.Monad.Except
import           Data.Aeson                        (Object, Value (..), encode)
import qualified Data.ByteString.Lazy              as BSL
import           Data.Either                       (lefts, rights)
import qualified Data.HashMap.Strict               as H
import qualified Data.IntMap                       as IntMap
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

import           Common.Network                    (ChainId, ChainRef (..), NetworkName (..),
                                                    NodeRef, getNetworksConfig,
                                                    getPactInstancePort,
                                                    numPactInstances,
                                                    textNetworkName)
import           Frontend.Crypto.Ed25519
import           Frontend.Foundation
import           Frontend.Messages
import           Frontend.Network.NodeInfo
import           Frontend.Storage                  (getItemStorage,
                                                    localStorage,
                                                    removeItemStorage,
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
  , _networkRequest_chainRef  :: ChainRef
    -- ^ To what chain should this request go to.
  , _networkRequest_endpoint :: Endpoint
    -- ^ Where shall this request go? To /local or to /send?
  , _networkRequest_signing  :: Set KeyName
    -- ^ With what keys the request should be signed. Don't sign with any keys
    -- when calling to `performLocalReadCustom` and similar.
  } deriving (Show, Generic, Eq)

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
  , _networkCfg_setNetworks   :: Event t (Map NetworkName [NodeRef])
    -- ^ Provide a new networks configuration.
  , _networkCfg_resetNetworks :: Event t ()
    -- ^ Reset networks to default (provided at deployment).
  , _networkCfg_selectNetwork :: Event t NetworkName
    -- ^ Switch to a different network.
  }

makePactLenses ''NetworkCfg


-- | HasNetworkCfg with additional constraints to make it behave like a proper
-- config.
type IsNetworkCfg cfg t = (HasNetworkCfg cfg t, Monoid cfg, Flattenable cfg t)

data Network t = Network
  { _network_networks        :: Dynamic t (Map NetworkName [NodeRef])
    -- ^ List of available networks. Each network has a name and a number of nodes.
    --   This list gets persisted and can be edited by the user.
    --   TODO: This should be a Maybe. While we are technically able to always
    --   provide a value, it does not always make sense. (E.g. when user
    --   deleted all available networks.)
  , _network_selectedNetwork :: Dynamic t NetworkName
    -- ^ The currently selected network
  , _network_selectedNodes   :: Dynamic t [Either Text NodeInfo]
    -- ^ Node information for the nodes in the currently selected network.
    --  The first `Right` `NodeInfo` will be used for deployments and such.
  , _network_modules         :: Dynamic t (Map ChainId [Text])
   -- ^ Available modules on all chains.
  , _network_deployed        :: Event t ()
   -- ^ Event gets triggered whenever some code got deployed sucessfully.
  , _network_meta            :: Dynamic t PublicMeta
   -- ^ Meta data used for deployments. Can be modified via above
   -- `_networkCfg_setGasLimit` and similar. The chainid in this field gets
   -- effectively ignored as it gets overridden on request handling, by the
   -- value `_networkRequest_chainRef` of the handled `NetworkRequest`.
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
  StoreNetwork_Networks    :: StoreNetwork (Map NetworkName [NodeRef])
  StoreNetwork_SelectedNetwork :: StoreNetwork NetworkName

deriving instance Show (StoreNetwork a)


makeNetwork
  :: forall t m model mConf
  . ( MonadHold t m, PerformEvent t m, MonadFix m
    , MonadJSM (Performable m), MonadJSM m
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

    (cName, networks) <- getNetworks cfg
    onCName <- tagOnPostBuild cName
    nodeInfos <-
      getNetworkNodeInfosIncremental (current networks) (current cName) $ leftmost
        [ onCName
          -- Refresh info on deployments and well on refresh (Refreshed node
          -- info triggers re-loading of modules too):
        , tag (current cName) $ onDeployed
        , tag (current cName) $ cfg ^. networkCfg_refreshModule
        ]

    performEvent_ $ traverse_ reportNodeInfoError . lefts <$> updated nodeInfos

    modules <- loadModules networkL

    meta <- buildMeta cfg

    pure
      ( mConf
      , Network
          { _network_networks = networks
          , _network_selectedNetwork = cName
          , _network_selectedNodes = nodeInfos
          , _network_modules = modules
          , _network_deployed = onDeployed
          , _network_meta = meta
          }
      )
  where
    reportNodeInfoError err =
      liftIO $ T.putStrLn $ "Fetching node info failed: " <> err


-- | Update networks, given an updating event.
updateNetworks
  :: (Reflex t, Monoid mConf, HasNetwork model t, HasNetworkCfg mConf t)
  => model
  -> Event t (Map NetworkName [NodeRef] -> Map NetworkName [NodeRef])
  -> mConf
updateNetworks m onUpdate =
  let
    onNew = attachWith (&) (current $ m ^. network_networks) onUpdate
  in
    mempty & networkCfg_setNetworks .~ onNew


-- | Retrieve `NodeInfo`s asynchronously and incremental.
--
--   This means the returned dynamic list will grow as responses come in.
getNetworkNodeInfosIncremental
  :: forall t m.
    ( Reflex t, MonadSample t m, TriggerEvent t m, PerformEvent t m
    , MonadHold t m, MonadFix m, MonadJSM (Performable m)
    )
  => Behavior t (Map NetworkName [NodeRef])
  -> Behavior t NetworkName -- The currently user selected network.
  -> Event t NetworkName -- Start NodeInfo retrieval for new selection.
  -> m (Dynamic t [Either Text NodeInfo])
getNetworkNodeInfosIncremental cNets currentName onNetName = do
    let
      onNodes :: Event t (NetworkName, [(Int, NodeRef)])
      onNodes = attachWith (flip getIndexedNodes) cNets onNetName

    onInfo <- performEventAsync $ uncurry getNodeInfosAsync <$> onNodes

    infoMap <- foldDyn id IntMap.empty $ leftmost
      [ const IntMap.empty <$ onNetName
      , fmap (uncurry IntMap.insert) . filterValidUpdates $ onInfo
      ]
    pure $ IntMap.elems <$> infoMap
  where
    -- Ignore updates if currently selected network no longer matches:
    filterValidUpdates = fmap snd . ffilter fst . attachWith checkSelected currentName

    checkSelected :: NetworkName -> (NetworkName, a) -> (Bool, a)
    checkSelected n = first (== n)

    getIndexedNodes name = (name, ) . zip [0..] . fromMaybe [] . Map.lookup name


-- | Retrieve the node information for the given `NetworkName`
--
--   As we are sending those requests asynchronously, we also return the
--   NetworkName corresponding to our response, so we get the matching right.
getNodeInfosAsync
  :: MonadJSM m
  => NetworkName
  -> [(Int, NodeRef)]
  -> ((NetworkName, (Int, Either Text NodeInfo)) -> IO ())
  -> m ()
getNodeInfosAsync netName nodes cb = traverse_ discoverNodeAsync nodes
  where
    discoverNodeAsync (i, nodeRef) = void $ forkJSM $ do
      r <- discoverNode nodeRef
      liftIO $ cb (netName, (i, r))


-- | Get the currently selected network.
--
--   Error out, if there is none.
getSelectedNetworkInfos
  :: (Reflex t, MonadSample t m)
  => Network t
  -> m [NodeInfo]
getSelectedNetworkInfos networkL = do
    errNets <- sample $ current $ networkL ^. network_selectedNodes
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
        chainId = _chainRef_chain . _networkRequest_chainRef $ req
        code = _networkRequest_code req
        -- Not really helpful to display deployed code if it is large:
        mkMsg msg = if T.length code < 100 then msg else ""
      in
        mkMsg $ T.unlines
          [ "Sent code to chain '" <> tshow chainId <> "':"
          , ""
          , code
          ]


-- | Get the available networks and the currently selected network.
--
--   This function also takes care of persistence of those values.
getNetworks
  :: forall t m cfg. ( MonadJSM (Performable m)
     , PerformEvent t m, TriggerEvent t m
     , MonadHold t m, MonadJSM m, MonadFix m
     , HasNetworkCfg cfg t
     )
  => cfg -> m (Dynamic t NetworkName, Dynamic t (Map NetworkName [NodeRef]))
getNetworks cfg = do
    (defName, defNets) <- getConfigNetworks
    initialNets <- fromMaybe defNets <$>
      liftJSM (getItemStorage localStorage StoreNetwork_Networks)
    initialName <- fromMaybe defName <$>
      liftJSM (getItemStorage localStorage StoreNetwork_SelectedNetwork)

    networks <- holdDyn initialNets $ leftmost
      [ defNets <$ (cfg ^. networkCfg_resetNetworks)
      , cfg ^. networkCfg_setNetworks
      ]

    rec
      sName <- holdUniqDyn <=< holdDyn initialName $ leftmost
        [ cfg ^. networkCfg_selectNetwork
        , fmapMaybe id . attachWith getSelectedFix (current sName) $ updated networks
        ]

    -- Important: Don't use updated networks here, as we want to clear
    -- localstorage in case of reset:
    onNetworksStore <- throttle 2 $ cfg ^. networkCfg_setNetworks
    onSelectedStore <- throttle 2 $ updated sName

    performEvent_ $
      liftJSM . setItemStorage localStorage StoreNetwork_Networks <$> onNetworksStore
    performEvent_ $
      liftJSM (removeItemStorage localStorage StoreNetwork_Networks) <$ (cfg ^. networkCfg_resetNetworks)
    performEvent_ $
      liftJSM . setItemStorage localStorage StoreNetwork_SelectedNetwork <$> onSelectedStore

    pure (sName, networks)

  where
    getSelectedFix
      :: forall a. NetworkName -> Map NetworkName a -> Maybe NetworkName
    getSelectedFix nName networks =
      if Map.member nName networks
         then Nothing
         else fst <$> Map.lookupMin networks



-- | Get networks from Obelisk config.
getConfigNetworks
  :: ( PerformEvent t m, MonadIO m )
  => m (NetworkName, Map NetworkName [NodeRef])
getConfigNetworks = do
  let
    buildNodeRef =
      either (\e -> error $ "Parsing of dev networks failed: " <> T.unpack e) id
      . parseNodeRef
      . ("localhost:" <>)
      . getPactInstancePort

    buildName = NetworkName . ("dev-" <>) . tshow
    buildNetwork =  buildName &&& pure . buildNodeRef
    devNetworks = Map.fromList $ map buildNetwork [1 .. numPactInstances]

  errProdCfg <- liftIO $ getNetworksConfig
  pure $ case errProdCfg of
    Left _ -> -- Development mode
      (buildName 1, devNetworks)
    Right c -> c -- Production mode


-- | Load modules on startup and on every occurrence of the given event.
loadModules
  :: forall t m
  . ( MonadHold t m, PerformEvent t m, MonadFix m
    , MonadJSM (Performable m)
    , MonadSample t (Performable m)
    , TriggerEvent t m, PostBuild t m
    )
  => Network t
  -> m (Dynamic t (Map ChainId [Text]))
loadModules networkL = do

      let nodeInfos = traceDyn "Infos: " $ rights <$> (networkL ^. network_selectedNodes)
      rec
        onNodeInfosAll <- tagOnPostBuild nodeInfos
        let onNodeInfos = traceEvent "Interesting Infos: " $ push (getInterestingInfos lastUsed) onNodeInfosAll
        lastUsed <- hold [] onNodeInfos

      let onReqs = map mkReq . maybe [] getChains . listToMaybe  <$> onNodeInfos
      onErrResps <- performLocalReadLatest networkL onReqs

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

      -- Make sure we only refresh when it makes sense => not just more backup nodes discovered.
      getInterestingInfos
        :: forall ms. MonadSample t ms
        => Behavior t [NodeInfo]
        -> [NodeInfo]
        -> ms (Maybe [NodeInfo])
      getInterestingInfos currentInfos newInfos = do
        oldInfos <- sample currentInfos
        pure $
          case (oldInfos, newInfos) of
            (o:_, n:_) -> if o /= n then Just newInfos else Nothing
            _          -> Just newInfos

      mkReq n =
        NetworkRequest "(list-modules)" H.empty (ChainRef Nothing n) Endpoint_Local Set.empty

      byChainId :: (NetworkRequest, NetworkErrorResult) -> Either NetworkError (ChainId, Term Name)
      byChainId = sequence . first (_chainRef_chain . _networkRequest_chainRef)

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
--   This call differs from `performLocalRead` in that it skip responses to
--   earlier requests if more current responses have been delivered already.
--
--   Note: See note for `performLocalReadCustom`.
performLocalReadLatest
  :: forall t m
  . ( PerformEvent t m, MonadJSM (Performable m)
    , TriggerEvent t m
    , MonadHold t m, MonadFix m
    , MonadSample t (Performable m)
    )
  => Network t
  -> Event t [NetworkRequest]
  -> m (Event t [(NetworkRequest, NetworkErrorResult)])
performLocalReadLatest networkL onReqs = do
    counterDyn <- foldDyn (const (+1)) 0 onReqs
    let
      counter = current counterDyn
      onCounted = attach counter onReqs
    onResp <- performLocalReadCustom networkL snd onCounted
    -- TODO: If we are not interested in earlier responses, we could cancel the
    -- corresponding requests ...
    pure $ uncurry zip <$> getLatest counter onResp
  where
    getLatest counter =
      fmap snd . ffilter fst . attachWith isLatest counter
    isLatest c ((respC, reqs), resp) = (respC + 1 == c, (reqs, resp))


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
performLocalRead networkL onReqs =
  fmap (uncurry zip) <$> performLocalReadCustom networkL id onReqs


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
  -> (req -> [NetworkRequest])
  -> Event t req
  -> m (Event t (req, [NetworkErrorResult]))
performLocalReadCustom networkL unwrapUsr onReqs =
  let
    unwrap = map (networkRequest_endpoint .~ Endpoint_Local) . unwrapUsr
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
  fmap (uncurry zip) <$> performNetworkRequestCustom w networkL id onReq


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
  -> (req -> [NetworkRequest])
  -> Event t req
  -> m (Event t (req, [NetworkErrorResult]))
performNetworkRequestCustom w networkL unwrap onReqs =
    performEventAsync $ ffor onReqs $ \reqs cb ->
      reportError cb reqs $ do
        keys <- sample $ current $ _wallet_keys w
        metaTemplate <- sample $ current $ networkL ^. network_meta
        nodeInfos <- getSelectedNetworkInfos networkL
        void $ forkJSM $ do
          r <- traverse (doReqFailover metaTemplate nodeInfos keys) $ unwrap reqs
          liftIO $ cb (reqs, r)
  where

    reportError cb reqs m = do
      er <- runExceptT m
      case er of
        Left err -> liftIO $ cb $ (reqs, map (const $ Left err) $ unwrap reqs)
        Right () -> pure ()

    doReqFailover metaTemplate nodeInfos keys req =
      go nodeInfos
        where
          go = \case
            n:ns -> do
              errRes <- doReq metaTemplate (Just n) keys req
              case errRes of
                Left err -> if shouldFailOver err
                               then go ns
                               else pure errRes
                Right _ -> pure errRes
            [] -> doReq metaTemplate Nothing keys req

    doReq metaTemplate mNodeInfo keys req = runExceptT $ do
      let
        metaChainId = toPmChainId . _chainRef_chain $ _networkRequest_chainRef req
        meta = metaTemplate { _pmChainId = metaChainId }
        signing = _networkRequest_signing req
      chainUrl <- ExceptT $ getBaseUrlErr (req ^. networkRequest_chainRef) mNodeInfo
      payload <- buildCmd meta keys signing req
      ExceptT $ liftJSM $ networkRequest chainUrl (req ^. networkRequest_endpoint) payload

    getBaseUrlErr ref info = left NetworkError_Other <$> getChainRefBaseUrl ref info


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
  NetworkError_NetworkError _ -> True
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
  NetworkError_Other m -> m


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
  NetworkCfg
    refreshA deployA setSenderA setGasLimitA setGasPriceA setNetworksA resetNetworksA selectNetworkA
    <>
    NetworkCfg
      refreshB deployB setSenderB setGasLimitB setGasPriceB setNetworksB resetNetworksB selectNetworkB
      = NetworkCfg
        { _networkCfg_refreshModule = leftmost [ refreshA, refreshB ]
        , _networkCfg_deployCode    =  deployA <> deployB
        , _networkCfg_setSender    = leftmost [ setSenderA, setSenderB ]
        , _networkCfg_setGasLimit   = leftmost [ setGasLimitA, setGasLimitB ]
        , _networkCfg_setGasPrice   = leftmost [ setGasPriceA, setGasPriceB ]
        , _networkCfg_setNetworks = leftmost [setNetworksA, setNetworksB ]
        , _networkCfg_resetNetworks = leftmost [resetNetworksA, resetNetworksB ]
        , _networkCfg_selectNetwork = leftmost [selectNetworkA, selectNetworkB ]
        }

instance Reflex t => Monoid (NetworkCfg t) where
  mempty = NetworkCfg never never never never never never never never
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
      <*> doSwitch never (_networkCfg_setNetworks <$> ev)
      <*> doSwitch never (_networkCfg_resetNetworks <$> ev)
      <*> doSwitch never (_networkCfg_selectNetwork <$> ev)
