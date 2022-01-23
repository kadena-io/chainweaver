{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE DataKinds #-}

-- | Interface for accessing Pact networks (pact -s, chainweb, kadena).
--
--   The module offers the possibility of selecting a particular network and
--   sending commands to it, by making use of the Pact REST API.
module Frontend.Network
  ( -- * Types & Classes
    NetworkRequest (..), networkRequest_cmd
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
  , getNetworkInfoTriple
    -- * Definitions from Common
  , module Common.Network
    -- * NodeInfo
  , module Frontend.Network.NodeInfo
    -- * Creation
  , makeNetwork
  , packHttpErr
    -- * Perform requests
  , performLocalReadCustom
  , performLocalRead
  , doReqFailover
  , doReqFailoverTagged
  , mkClientEnvs
    -- * Utilities
  , parseNetworkErrorResult
  , prettyPrintNetworkErrorResult
  , prettyPrintNetworkError
  , prettyPrintNetworkErrors
  , networkErrorResultToEither
  , buildCmd
  , buildContPayload
  , buildCmdWithPayload
  , buildCmdWithPactKey
  , buildExecPayload
  , simpleLocal
  , mkSimpleReadReq
  , getChainsFromHomogenousNetwork
  , getNetworkNameAndMeta
  , getCreationTime
  , encodeAsText
  , buildSigDataWithPayload 
    -- * Defaults
  , chainwebGasLimitMaximum
  , defaultTransactionGasLimit
  , defaultTransactionGasPrice
  , maxCoinPrecision
  , defaultTransactionTTL
    -- * Re-export
  , HasTransactionLogger(..)
  ) where

import           Control.Error                     (hush, headMay)
import           Control.Exception                 (fromException)
import           Control.Arrow                     (left, second, (&&&))
import           Control.Lens                      hiding ((.=))
import           Control.Monad.Except
import           GHC.Word                          (Word8)
import           Data.Aeson                        (Object, Value (..), encode, eitherDecodeStrict')
import qualified Data.Bifunctor                    as BiF
import           Data.Functor                      (($>))
import qualified Data.ByteString.Lazy              as BSL
import           Data.Decimal                      (DecimalRaw (..))
import           Data.Either                       (lefts, rights)
import qualified Data.List                         as L
import           Data.List.NonEmpty                (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty                as NEL
import qualified Data.Map                          as Map
import           Data.Map.Strict                   (Map)
import           Data.Set                          (Set)
import qualified Data.Set                          as Set
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import           Data.These                        (These(..), these)
import           Data.Time.Clock                   (getCurrentTime)
import           Data.Time.Clock.POSIX             (getPOSIXTime)
import           Data.Traversable                  (for)
import           Foreign.JavaScript.TH             (HasJSContext)
import           Language.Javascript.JSaddle.Monad (JSM, liftJSM)
import qualified Network.HTTP.Types                as HTTP
import           Reflex.Dom.Core                   (def, XhrRequest(..), XhrResponse(..), performRequestAsync)
import           Text.URI                          (URI)
import qualified Text.URI                          as URI
import qualified Text.URI.Lens                     as URIL

import           Pact.Parse                        (ParsedDecimal (..), parsePact)
import           Pact.Server.ApiClient
import           Pact.Types.API
import           Pact.Types.Capability
import           Pact.Types.Command
import           Pact.Types.Runtime                (PactError (..), GasLimit (..), GasPrice (..), Gas (..), ModuleName)
import           Pact.Types.ChainMeta              (PublicMeta (..), TTLSeconds (..), TxCreationTime (..))
import qualified Pact.Types.ChainMeta              as Pact
import           Pact.Types.ChainId                (NetworkId (..))
import           Pact.Types.Exp                    (Literal (LString))
import           Pact.Types.Hash                   (hash, Hash (..), TypedHash (..), toUntypedHash)
import           Pact.Types.PactValue
import           Pact.Types.RPC
import           Pact.Types.SigData
import qualified Pact.Types.Term                   as Pact
import qualified Servant.Client.JSaddle            as S
import qualified Servant.Client.Internal.JSaddleXhrClient as S

#if !defined (ghcjs_HOST_OS)
import           Pact.Types.Crypto                 (PPKScheme (..))
#endif

import           Common.Modules
import           Common.Network
import           Common.Wallet
import           Frontend.Crypto.Class
import           Frontend.Crypto.Ed25519
import           Frontend.Crypto.Signature
import           Frontend.Foundation
import           Frontend.Messages
import           Frontend.Network.NodeInfo
import           Frontend.Storage
import           Frontend.VersionedStore
import           Frontend.Log


-- | What endpoint to use for a network request.
data Endpoint
  = Endpoint_Send AccountName ChainId
  | Endpoint_Local
  deriving (Show, Generic, Eq, Ord)

-- | Get string representation of `Endpoint` suitable for being displayed to an end user.
displayEndpoint :: Endpoint -> Text
displayEndpoint = \case
  Endpoint_Send _ _ -> "Transact"
  Endpoint_Local -> "Read"

-- | Request data to be sent to the current network.
data NetworkRequest = NetworkRequest
  { _networkRequest_cmd :: Command Text
    -- ^ Pact command
  , _networkRequest_chainRef :: ChainRef
    -- ^ To what chain should this request go to.
  , _networkRequest_endpoint :: Endpoint
    -- ^ Where shall this request go? To /local or to /send?
  } deriving (Show, Generic, Eq)

makePactLensesNonClassy ''NetworkRequest


data NetworkError
  = NetworkError_NetworkError Text
  -- ^ Server could not be reached.
  | NetworkError_Status HTTP.Status Text
  -- ^ Server responded with a non 200 status code.
  | NetworkError_Decoding Text
  -- ^ Server responded with a 200 status code, but decoding the response failed.
  | NetworkError_ReqTooLarge
  -- ^ Request size exceeded the allowed limit.
  | NetworkError_CommandFailure PactError
  -- ^ The status in the /listen result object was `failure`.
  | NetworkError_NoNetwork Text
  -- ^ An action requiring a network was about to be performed, but we don't
  -- (yet) have any selected network.
  | NetworkError_Other Text
  -- ^ Other errors that should really never happen.
  deriving Show

-- | We either have a `NetworkError`, some `Term Name` or both if we failed
-- over but eventually got to a successful result.
type NetworkErrorResult = These (NonEmpty (Maybe URI, NetworkError)) (Maybe Gas, PactValue)

-- | Use this sparingly as it throws away the errors that could have happened in
-- the failovers. There could be something important in there
networkErrorResultToEither :: NetworkErrorResult -> Either (NonEmpty (Maybe URI, NetworkError)) (Maybe Gas, PactValue)
networkErrorResultToEither = these Left Right (\_ -> Right)

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
  , _networkCfg_setGasLimit   :: Event t GasLimit
    -- ^ Maximum amount of gas to use for this transaction.
  , _networkCfg_setGasPrice   :: Event t GasPrice
    -- ^ Maximum gas price you are willing to accept for having your
    -- transaction executed.
  , _networkCfg_setTTL   :: Event t TTLSeconds
    -- ^ TTL for this transaction
  , _networkCfg_refreshNodes  :: Event t ()
  , _networkCfg_trackNodes    :: Event t (Set NodeRef)
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
  , _network_selectedNodes   :: Dynamic t [Either Text NodeInfo] -- TODO: redundant field; replace with util
    -- ^ Node information for the nodes in the currently selected network.
    --  The first `Right` `NodeInfo` will be used for deployments and such.
  , _network_trackedNodes    :: Dynamic t (Map NodeRef (Either Text NodeInfo))
    -- ^ All nodes we have cared about
  , _network_modules         :: Dynamic t (Map ChainId [ModuleName])
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

-- | Model config needed by Network.
type HasNetworkModelCfg mConf t = (Monoid mConf, HasMessagesCfg mConf t)

makeNetwork
  :: forall key t m mConf model
  . ( MonadHold t m, PerformEvent t m, MonadFix m
    , MonadJSM (Performable m), MonadJSM m
    , TriggerEvent t m, PostBuild t m
    , MonadSample t (Performable m)
    , HasNetworkModelCfg mConf t
    , HasConfigs m
    , HasJSContext (Performable m)
    , HasStorage m, HasStorage (Performable m)
    , HasCrypto key (Performable m)
    , HasLogger model t
    , HasTransactionLogger m
    )
  => model
  -> NetworkCfg t
  -> m (mConf, Network t)
makeNetwork model cfg = mfix $ \ ~(_, networkL) -> do
    pb <- getPostBuild

    (mConf, onDeployed) <- deployCode (model ^. logger) networkL $ cfg ^. networkCfg_deployCode

    (cName, networks) <- getNetworks cfg

    let onRefresh = leftmost [ onDeployed, cfg ^. networkCfg_refreshModule ]


    let
      refreshTracking = Set.fromList . join . Map.elems <$> current networks <@ leftmost [pb, cfg^.networkCfg_refreshNodes]
      requestTracking = cfg ^. networkCfg_trackNodes
      markTrackingStale = ffor (cfg ^. networkCfg_trackNodes) $ \ns m -> Map.restrictKeys m (Set.difference (Map.keysSet m) ns)
    tracks <- performEventAsync $ fmap (getNodeInfosAsync . Set.toList) $ leftmost
      [ requestTracking
      , refreshTracking
      ]
    tracked <- foldDyn ($) mempty $ leftmost
      [ ffor tracks $ uncurry Map.insert
      , markTrackingStale
      ]
    let nodeInfos = ffor3 cName networks tracked $ \n nets nodes ->
          let nodeRefs = fromMaybe [] $ Map.lookup n nets
          in fforMaybe nodeRefs $ flip Map.lookup nodes


    performEvent_ $ traverse_ reportNodeInfoError . lefts <$> updated nodeInfos

    modules <- loadModules (model ^. logger) networkL onRefresh

    meta <- buildMeta cfg

    pure
      ( mConf
      , Network
          { _network_networks = networks
          , _network_selectedNetwork = cName
          , _network_selectedNodes = nodeInfos
          , _network_trackedNodes = tracked
          , _network_modules = modules
          , _network_deployed = onDeployed
          , _network_meta = meta
          }
      )
  where
    reportNodeInfoError err =
      putLog model LevelWarn $ "Fetching node info failed: " <> err

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

getNodeInfosAsync
  :: MonadJSM m
  => [NodeRef]
  -> ((NodeRef, Either Text NodeInfo) -> IO ())
  -> m ()
getNodeInfosAsync nodes cb = for_ nodes $ \nodeRef -> void $ liftJSM $ forkJSM $ do
  r <- discoverNode nodeRef
  liftIO $ cb (nodeRef, r)

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

defaultTransactionGasPrice :: GasPrice
defaultTransactionGasPrice = GasPrice $ ParsedDecimal $ Decimal 6 1

-- | This is the minimum precision allowed by the Pact language, as defined in the coin contract:
-- https://github.com/kadena-io/chainweb-node/commit/ee8a0db079869b39e23be1ef6737f0a7795eff87#diff-6c59a5fb9f1b0b8b470cb50e8bd643ebR54
maxCoinPrecision :: Word8
maxCoinPrecision = 12

defaultTransactionTTL :: TTLSeconds
defaultTransactionTTL = TTLSeconds (8 * 60 * 60) -- 8 hours

-- Taken from https://github.com/kadena-io/chainweb-node/blob/85688ea0182d1b1ab0d8d784a48b4851a950ec7a/src/Chainweb/Chainweb.hs#L344
chainwebGasLimitMaximum :: Num a => a
chainwebGasLimitMaximum = 1e5

defaultTransactionGasLimit :: GasLimit
defaultTransactionGasLimit = GasLimit 600

getNetworkInfoTriple
  :: Reflex t
  => Network t
  -> Dynamic t (Maybe ([Either Text NodeInfo], PublicMeta, NetworkName))
getNetworkInfoTriple nw = do
  nodes <- nw ^. network_selectedNodes
  meta <- nw ^. network_meta
  let networkId = mkNetworkName . nodeVersion <$> headMay (rights nodes)
  pure $ (nodes, meta, ) <$> networkId

buildMeta
  :: ( MonadHold t m, MonadFix m, MonadJSM m
     , PerformEvent t m, MonadJSM (Performable m), TriggerEvent t m
     , HasStorage m, HasStorage (Performable m)
     )
  => NetworkCfg t -> m (Dynamic t PublicMeta)
buildMeta cfg = do
  time <- getCreationTime
  let defaultMeta =
        PublicMeta
          { _pmChainId = "1" -- Gets overridden always!
          , _pmSender  = "sender00"
          , _pmGasLimit = defaultTransactionGasLimit
          , _pmGasPrice = defaultTransactionGasPrice
          , _pmTTL = defaultTransactionTTL
          , _pmCreationTime = time
          }
  m <- fromMaybe defaultMeta <$>
    getItemStorage localStorage StoreFrontend_Network_PublicMeta

  r <- foldDyn id m $ leftmost
    [ set Pact.pmSender <$> cfg ^. networkCfg_setSender
    , set Pact.pmGasLimit <$> cfg ^. networkCfg_setGasLimit
    , set Pact.pmGasPrice <$> cfg ^. networkCfg_setGasPrice
    , set Pact.pmTTL <$> cfg ^. networkCfg_setTTL
    ]

  onStore <- throttle 2 $ updated r
  performEvent_ $
    setItemStorage localStorage StoreFrontend_Network_PublicMeta <$> onStore

  pure r


deployCode
  :: forall t m mConf
  . ( MonadHold t m, PerformEvent t m
    , MonadJSM (Performable m)
    , TriggerEvent t m
    , MonadSample t (Performable m)
    , HasNetworkModelCfg mConf t
    , HasTransactionLogger m
    )
  => Logger t
  -> Network t
  -> Event t [NetworkRequest]
  -> m (mConf, Event t ())
deployCode logL networkL onReq = do
    reqRes <- performNetworkRequest logL networkL onReq
    pure $ ( mempty & messagesCfg_send .~ fmap (map renderReqRes) reqRes
           , () <$ ffilter (or . map (these (const False) (const True) (const . const $ True) . snd)) reqRes
           )
  where
    renderReqRes :: (NetworkRequest, NetworkErrorResult) -> Text
    renderReqRes (req, res) =
      T.unlines [renderReq req, prettyPrintNetworkErrorResult res]

    renderReq :: NetworkRequest -> Text
    renderReq req =
      let
        chainId = _chainRef_chain . _networkRequest_chainRef $ req
        code = _cmdPayload $ _networkRequest_cmd req
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
     , MonadHold t m, MonadFix m
     , HasNetworkCfg cfg t, HasConfigs m
     , PostBuild t m, HasJSContext (Performable m)
     , HasStorage m, HasStorage (Performable m)
     )
  => cfg -> m (Dynamic t NetworkName, Dynamic t (Map NetworkName [NodeRef]))
getNetworks cfg = do
    (defName, defNets, mRemoteSource) <- getConfigNetworks
    initialNets <- fromMaybe defNets <$>
      getItemStorage localStorage StoreFrontend_Network_Networks
    initialName <- fromMaybe defName <$>
      getItemStorage localStorage StoreFrontend_Network_SelectedNetwork

    -- Hit the remote-source for network configs, if applicable
    mRemoteUpdate <- for mRemoteSource $ \url -> do
      pb <- getPostBuild
      go <- throttle 10 (pb <> cfg ^. networkCfg_resetNetworks)
      let req = XhrRequest "GET" url def <$ go
      resp <- (fmap . fmap) _xhrResponse_responseText $ performRequestAsync req
      let resp' = fmap parseNetworks <$> resp
      pure $ fmapMaybe (either (const Nothing) Just) $ fmapMaybe id resp'

    networks <- foldDyn ($) initialNets $ leftmost
      [ const defNets <$ (cfg ^. networkCfg_resetNetworks)
      , const <$> cfg ^. networkCfg_setNetworks
      , Map.unionWith (\x -> L.nub . mappend x) . snd <$> fromMaybe never mRemoteUpdate
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
      setItemStorage localStorage StoreFrontend_Network_Networks <$> onNetworksStore
    performEvent_ $
      removeItemStorage localStorage StoreFrontend_Network_Networks <$ (cfg ^. networkCfg_resetNetworks)
    performEvent_ $
      setItemStorage localStorage StoreFrontend_Network_SelectedNetwork <$> onSelectedStore

    pure (sName, networks)

  where
    getSelectedFix
      :: forall a. NetworkName -> Map NetworkName a -> Maybe NetworkName
    getSelectedFix nName networks =
      if Map.member nName networks
         then Nothing
         else fst <$> Map.lookupMin networks

-- Assumes all nodes have exactly the same chains
-- TODO: Make the network dialog enforce that assumption, pull chains out of NodeInfo and remove this method
getChainsFromHomogenousNetwork :: Reflex t => HasNetwork model t => model -> Dynamic t [ChainId]
getChainsFromHomogenousNetwork m =
  let mNodeInfo = (^? to rights . _head) <$> m ^. network_selectedNodes
  in maybe [] getChains <$> mNodeInfo

-- | Get networks from Obelisk config.
getConfigNetworks
  :: ( PerformEvent t m, HasConfigs m )
  => m (NetworkName, Map NetworkName [NodeRef], Maybe Text)
getConfigNetworks = do
  let
    buildNodeRef =
      either (\e -> error $ "Parsing of dev networks failed: " <> T.unpack e) id
      . parseNodeRef
      . ("localhost:" <>)
      . getPactInstancePort

    buildName = mkNetworkName . ("dev-" <>) . tshow
    buildNetwork = buildName &&& pure . buildNodeRef
    devNetworks = Map.fromList $ map buildNetwork [1 .. numPactInstances]

  errProdCfg <- getNetworksConfig
  pure $ case errProdCfg of
    Left (Left _) -> -- Development mode
      (buildName 1, devNetworks, Nothing)
    Left (Right x) -> -- Mac app remote list
      (mkNetworkName "pact", mempty, Just x)
    Right (x,y) -> (x,y, Nothing) -- Production mode

getNetworkNameAndMeta
  :: ( Reflex t
     , HasNetwork model t
     )
  => model
  -> Dynamic t (NetworkName, PublicMeta)
getNetworkNameAndMeta model = (,)
  <$> (model ^. network_selectedNetwork)
  <*> (model ^. network_meta)

mkSimpleReadReq
  :: (MonadIO m, MonadJSM m, HasCrypto key m)
  => Text -> NetworkName -> PublicMeta -> ChainRef -> m NetworkRequest
mkSimpleReadReq code networkName pm cRef = do
  cmd <- buildCmd Nothing networkName (pm { _pmChainId = _chainRef_chain cRef }) [] [] code mempty mempty
  pure $ NetworkRequest
    { _networkRequest_cmd = cmd
    , _networkRequest_chainRef = cRef
    , _networkRequest_endpoint = Endpoint_Local
    }

-- | Load modules on startup and on every occurrence of the given event.
loadModules
  :: forall key t m
  . ( MonadHold t m, PerformEvent t m, MonadFix m
    , MonadJSM (Performable m), MonadIO m
    , MonadSample t (Performable m)
    , TriggerEvent t m, PostBuild t m
    , HasCrypto key (Performable m)
    , HasTransactionLogger m
    )
  => Logger t
  -> Network t
  -> Event t ()
  -> m (Dynamic t (Map ChainId [ModuleName]))
loadModules logL networkL onRefresh = do

      let nodeInfos = rights <$> (networkL ^. network_selectedNodes)
      rec
        onNodeInfosAll <- tagOnPostBuild nodeInfos
        let onNodeInfos = push (getInterestingInfos lastUsed) onNodeInfosAll
        lastUsed <- hold [] onNodeInfos

      onReqs <- performEvent $ attachWith (\f m -> traverse f $ maybe [] getChains $ listToMaybe m)
        (current $ uncurry mkReq <$> getNetworkNameAndMeta networkL)
        (leftmost
          [ onNodeInfos
          , tag (current nodeInfos) onRefresh
          ])
      onErrResps <- performLocalReadLatest logL networkL onReqs

      let
        onByChainId :: Event t [ Either (NonEmpty NetworkError) (ChainId, PactValue) ]
        onByChainId = map byChainId <$> onErrResps

        onErrs = ffilter (not . null) $ fmap lefts onByChainId
        onResps = fmap rights onByChainId

        onModules :: Event t [(ChainId, [ModuleName])]
        onModules =  map (second getModuleList) <$> onResps

      performEvent_ $ traverse_ (putLog logL LevelWarn . renderErrs . toList) <$> onErrs

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

      -- We need a minimum GasLimit to ensure that this call cannot fail. As the
      -- PublicMeta stores the users last transaction configuration and may not be
      -- sufficient for this function.
      mkReq netName pm = mkSimpleReadReq "(list-modules)" netName (pm & Pact.pmGasLimit .~ 200) . ChainRef Nothing

      byChainId :: (NetworkRequest, NetworkErrorResult) -> Either (NonEmpty NetworkError) (ChainId, PactValue)
      byChainId = sequence . bimap (_chainRef_chain . _networkRequest_chainRef) (bimap (fmap snd) snd . networkErrorResultToEither)

      renderErrs :: [NetworkError] -> Text
      renderErrs =
        T.unlines
        .  ("Error, retrieving modules for one or more chains: " :)
        . map prettyPrintNetworkError


      getModuleList :: PactValue -> [ModuleName]
      getModuleList = \case
        PList terms -> mapMaybe getStringLit $ toList terms
        _               -> []

      getStringLit :: PactValue -> Maybe ModuleName
      getStringLit = \case
        PLiteral (LString v) -> hush $ parseModuleName v
        _         -> Nothing


-- | Parse a NetworkErrorResult into something that the frontend can use
parseNetworkErrorResult
  :: MonadIO m
  => Logger t
  -> (PactValue -> Either Text a)
  -> NetworkErrorResult
  -> m (These Text a)
parseNetworkErrorResult logL parse = \case
  That (_gas, pactValue) -> doParse pactValue That
  This e -> pure $ This $ prettyPrintNetworkErrors e
  These errs (_gas, pactValue) -> doParse pactValue (These (prettyPrintNetworkErrors errs))
  where
    doParse pactValue f = case parse pactValue of
      Left e -> putLog logL LevelWarn e $> This "Error parsing the response"
      Right v -> pure $ f v

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
    , TriggerEvent t m, MonadIO m
    , MonadHold t m, MonadFix m
    , MonadSample t (Performable m)
    , HasTransactionLogger m
    )
  => Logger t
  -> Network t
  -> Event t [NetworkRequest]
  -> m (Event t [(NetworkRequest, NetworkErrorResult)])
performLocalReadLatest logL networkL onReqs = do
    counterDyn <- foldDyn (const (+1)) (0 :: Int) onReqs
    let
      counter = current counterDyn
      onCounted = attach counter onReqs
    onResp <- performLocalReadCustom logL networkL snd onCounted
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
    , TriggerEvent t m, MonadIO m
    , MonadSample t (Performable m)
    , HasTransactionLogger m
    )
  => Logger t
  -> Network t
  -> Event t [NetworkRequest]
  -> m (Event t [(NetworkRequest, NetworkErrorResult)])
performLocalRead logL networkL onReqs =
  fmap (uncurry zip) <$> performLocalReadCustom logL networkL id onReqs

getCreationTime :: MonadIO m => m TxCreationTime
getCreationTime = mkCt <$> liftIO getPOSIXTime
  where
    mkCt x = TxCreationTime $ round (x - 15)

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
    , TriggerEvent t m, MonadIO m
    , MonadSample t (Performable m)
    , HasTransactionLogger m
    )
  => Logger t
  -> Network t
  -> (req -> [NetworkRequest])
  -> Event t req
  -> m (Event t (req, [NetworkErrorResult]))
performLocalReadCustom logL networkL unwrapUsr onReqs = do
  time <- getCreationTime
  let
    unwrap = map (networkRequest_endpoint .~ Endpoint_Local) . unwrapUsr
    fakeNetwork = networkL & network_meta . mapped . Pact.pmCreationTime .~ time
  performNetworkRequestCustom logL fakeNetwork unwrap onReqs


-- | Send a transaction via the /send endpoint.
--
--   This is a convenience wrapper around `networkRequest`, use that if you
--   need some richer request information attached to your response or if this is really all you need `performNetworkRequestCustom`.
performNetworkRequest
  :: forall t m
  . ( PerformEvent t m, MonadJSM (Performable m)
    , TriggerEvent t m
    , MonadSample t (Performable m)
    , HasTransactionLogger m
    )
  => Logger t
  -> Network t
  -> Event t [NetworkRequest]
  -> m (Event t [(NetworkRequest, NetworkErrorResult)])
performNetworkRequest logL networkL onReq =
  fmap (uncurry zip) <$> performNetworkRequestCustom logL networkL id onReq

-- | Turn some node URLs into chain specific servant envs
mkClientEnvs :: [NodeInfo] -> ChainId -> [S.ClientEnv]
mkClientEnvs nodeInfos chain = fforMaybe nodeInfos $ \nodeInfo ->
  let chainUrl = getChainBaseUrl chain nodeInfo
  in S.mkClientEnv <$> S.parseBaseUrl (URI.renderStr chainUrl)

-- | Perform some servant request by stepping through the given envs
doReqFailover :: MonadJSM m => [S.ClientEnv] -> S.ClientM a -> m (Either [S.ClientError] a)
doReqFailover envs = fmap (fmap snd) . doReqFailoverTagged (fmap ((),) envs)

-- | Perform some servant request by stepping through the given envs
doReqFailoverTagged :: MonadJSM m => [(tag, S.ClientEnv)] -> S.ClientM a -> m (Either [S.ClientError] (tag, a))
doReqFailoverTagged [] _ = pure $ Left []
doReqFailoverTagged ((t, c):cs) request = liftJSM $ S.runClientM request c >>= \case
  Left e -> BiF.first (e:) <$> doReqFailoverTagged cs request
  Right r -> pure $ Right (t, r)


-- | Send a transaction via the /send endpoint.
--
--   This is a convenience wrapper around `networkRequest`, attaching a custom
--   request type to the response.
performNetworkRequestCustom
  :: forall t m req
  . ( PerformEvent t m, MonadJSM (Performable m)
    , TriggerEvent t m
    , MonadSample t (Performable m)
    , HasTransactionLogger m
    )
  => Logger t
  -> Network t
  -> (req -> [NetworkRequest])
  -> Event t req
  -> m (Event t (req, [NetworkErrorResult]))
performNetworkRequestCustom logL networkL unwrap onReqs = do
  transactionLog <- askTransactionLogger
  performEventAsync $ ffor onReqs $ \reqs cb -> do
    nodeInfos <- getSelectedNetworkInfos networkL
    void $ liftJSM $ forkJSM $ flip runTransactionLoggerT transactionLog $ do
      r <- traverse (doReqFailover' nodeInfos) $ unwrap reqs
      liftIO $ cb (reqs, r)
  where
    doReqFailover' :: [NodeInfo] -> NetworkRequest -> TransactionLoggerT JSM NetworkErrorResult
    doReqFailover' nodeInfos req =
      go [] nodeInfos
        where
          go :: [(Maybe URI,NetworkError)] -> [NodeInfo] -> TransactionLoggerT JSM NetworkErrorResult
          go errs = \case
            n:ns -> do
              errRes <- doReq (Just n) req
              case errRes of
                Left (chainUrl,err) -> do
                  putLog logL LevelWarn $ "Got err: " <> tshow err
                  if shouldFailOver err
                               then go ((chainUrl,err) : errs) ns
                               else pure $ mkResult errs (Left (chainUrl,err))
                Right res -> pure $ mkResult errs (Right res)
            [] -> mkResult errs <$> doReq Nothing req
          mkResult errs (Left e) = This (NEL.reverse $ e :| errs)
          mkResult errs (Right res) = maybe (That res) (flip These res) . nonEmpty . reverse $ errs

    doReq :: Maybe NodeInfo -> NetworkRequest -> TransactionLoggerT JSM (Either (Maybe URI,NetworkError) (Maybe Gas, PactValue))
    doReq mNodeInfo req = runExceptT $ do
      chainUrl <- ExceptT $ getBaseUrlErr (req ^. networkRequest_chainRef) mNodeInfo
      ExceptT $ fmap (BiF.first (Just chainUrl,)) $ networkRequest chainUrl (req ^. networkRequest_endpoint) (_networkRequest_cmd req)

    getBaseUrlErr ref info = left ((Nothing,) . NetworkError_Other) <$> getChainRefBaseUrl ref info


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
  :: (HasTransactionLogger m, MonadJSM m)
  => URI
  -> Endpoint
  -> Command Text
  -> m (Either NetworkError (Maybe Gas, PactValue))
networkRequest baseUri endpoint cmd = case S.parseBaseUrl $ URI.renderStr baseUri of
  Nothing -> pure $ Left $ NetworkError_NetworkError $ T.pack $ "Invalid url: " <> URI.renderStr baseUri
  Just baseUrl -> runExceptT $ performReq $ S.mkClientEnv baseUrl
  where
    performReq clientEnv = case endpoint of
      Endpoint_Send sender chain -> do
        transactionLogger <- lift askTransactionLogger
        res <- runReq clientEnv $ send apiV1Client transactionLogger (unAccountName sender) chain $ SubmitBatch . pure $ cmd
        key <- getRequestKey $ res
        -- TODO: If we no longer wait for /listen, we should change the type instead of wrapping that message in `PactValue`.
        pure $ (Nothing,) $ PLiteral . LString $
          T.dropWhile (== '"') . T.dropWhileEnd (== '"') . tshow $ key
      Endpoint_Local ->
        fromCommandResult <=< runReq clientEnv $ local apiV1Client cmd

    -- | Rethrow an error value by wrapping it with f.
    reThrowWith :: Functor m => (e -> NetworkError) -> m (Either e a) -> ExceptT NetworkError m a
    reThrowWith f = ExceptT . fmap (left f)

    runReq :: (MonadJSM m, HasTransactionLogger m) => S.ClientEnv -> TransactionLoggerT S.ClientM a -> ExceptT NetworkError m a
    runReq env t = do
      transactionLog <- lift askTransactionLogger
      reThrowWith packHttpErr . liftJSM . flip S.runClientM env $ runTransactionLoggerT t transactionLog

    fromCommandResult :: MonadError NetworkError m => CommandResult a -> m (Maybe Gas, PactValue)
    fromCommandResult r = case _crResult r of
      PactResult (Left e) -> throwError $ NetworkError_CommandFailure e
      PactResult (Right v) -> pure (Just $ _crGas r, v)

    getRequestKey :: MonadError NetworkError m => RequestKeys -> m RequestKey
    getRequestKey r =
      case _rkRequestKeys r of
        key :| [] -> pure key
        _         -> throwError $ NetworkError_Other "Response contained more than one RequestKey."

packHttpErr :: S.ClientError -> NetworkError
packHttpErr e = case e of
  S.FailureResponse _ response ->
    if S.responseStatusCode response == HTTP.status413
       then NetworkError_ReqTooLarge
       else NetworkError_Status (S.responseStatusCode response) (tshow $ S.responseBody response)
  S.ConnectionError ex
    -- This is a tad more robust than a text pattern match.
    | Just S.JSaddleConnectionError <- fromException ex
    -> NetworkError_NetworkError "Node unreachable"
  S.ConnectionError t -> NetworkError_NetworkError (tshow t)
  _ -> NetworkError_Decoding $ T.pack $ show e

-- Request building ....

-- | Build a single cmd as expected in the `cmds` array of the /send payload.
--
-- As specified <https://pact-language.readthedocs.io/en/latest/pact-reference.html#send here>.
buildCmd
  :: ( MonadJSM m
     , HasCrypto key m
     )
  => Maybe Text
  -- ^ Nonce. When missing, uses the current time.
  -> NetworkName
  -- ^ The network that we are targeting
  -> PublicMeta
  -- ^ Assorted information for the payload. The time is overridden.
  -> [KeyPair key]
  -- ^ Keys which we are signing with
  -> [PublicKey]
  -- ^ Keys which should be added to `signers`, but not used to sign
  -> Text
  -- ^ Code
  -> Object
  -- ^ Data object
  -> Map PublicKey [SigCapability]
  -- ^ Capabilities for each public key
  -> m (Command Text)
buildCmd =
  buildCmdWithSigs (buildSigs Nothing)

buildCmdWithPactKey
  :: ( MonadJSM m
     , HasCrypto key m
     )
  => PactKey
  -- ^ Extra signing key using legacy ED255219 pact key.
  -> Maybe Text
  -- ^ Nonce. When missing, uses the current time.
  -> NetworkName
  -- ^ The network that we are targeting
  -> PublicMeta
  -- ^ Assorted information for the payload. The time is overridden.
  -> [KeyPair key]
  -- ^ Keys which we are signing with
  -> [PublicKey]
  -- ^ Keys which should be added to `signers`, but not used to sign
  -> Text
  -- ^ Code
  -> Object
  -- ^ Data object
  -> Map PublicKey [SigCapability]
  -- ^ Capabilities for each public key
  -> m (Command Text)
buildCmdWithPactKey pactKey =
  buildCmdWithSigs (buildSigsWithPactKey pactKey)

-- | Build a single cmd as expected in the `cmds` array of the /send payload.
--
-- As specified <https://pact-language.readthedocs.io/en/latest/pact-reference.html#send here>.
buildCmdWithSigs
  :: ( MonadIO m
     , MonadJSM m
     , HasCrypto key m
     )
  => (forall h. HasCrypto key m => TypedHash h -> [KeyPair key] -> m [UserSig])
  -- ^ Function for generating our signatures
  -> Maybe Text
  -- ^ Nonce. When missing, uses the current time.
  -> NetworkName
  -- ^ The network that we are targeting
  -> PublicMeta
  -- ^ Assorted information for the payload. The time is overridden.
  -> [KeyPair key]
  -- ^ Keys which we are signing with
  -> [PublicKey]
  -- ^ Keys which should be added to `signers`, but not used to sign
  -> Text
  -- ^ Code
  -> Object
  -- ^ Data object
  -> Map PublicKey [SigCapability]
  -- ^ Capabilities for each public key
  -> m (Command Text)
buildCmdWithSigs signingFn mNonce networkName meta signingKeys extraKeys code dat caps = do
  cmd <- encodeAsText . encode <$> buildExecPayload mNonce networkName meta signingKeys extraKeys code dat caps
  let
    cmdHashL = hash (T.encodeUtf8 cmd)
  sigs <- signingFn cmdHashL signingKeys
  pure $ Command
    { _cmdPayload = cmd
    , _cmdSigs = sigs
    , _cmdHash = cmdHashL
    }

buildSigDataWithPayload
  :: (
       MonadJSM m
     , HasCrypto key m
     )
  => Payload PublicMeta Text
  -> [KeyPair key]
  -- ^ Keys which we are signing with
  -> m (Either String (SigData Text))
buildSigDataWithPayload payload signingKeys = do
  let cmd = encodeAsText $ encode payload
      cmdHashL = hash (T.encodeUtf8 cmd)
      pkt2pk = fromPactPublicKey . Pact.PublicKey . T.encodeUtf8
      keySets = Map.fromList $ fmap (_keyPair_publicKey &&& id) signingKeys
      signingKeysets = ffor (payload^.pSigners) $ \signer ->
        let pk = pkt2pk $ signer^.siPubKey
        in (signer, pk `Map.lookup` keySets)
  sigs <- buildSigsPreserveOrder cmdHashL signingKeysets
  pure $ BiF.second (\sd' -> sd' { _sigDataSigs = sigs }) $
    unsignedCommandToSigData $ Command
      { _cmdPayload = cmd
      , _cmdSigs = []
      , _cmdHash = cmdHashL
      }

-- |Copied from Pact.Types.SigData in anticipation of the function `commandToSigData` being
--  changed and silently breaking any use of it that relies on the fact that it ignores the
--  _cmdSig field
unsignedCommandToSigData :: Command Text -> Either String (SigData Text)
unsignedCommandToSigData c = do
  let ep = traverse parsePact =<< (eitherDecodeStrict' $ T.encodeUtf8 $ _cmdPayload c)
  case ep :: Either String (Payload Value ParsedCode) of
    Left e -> Left $ "Error decoding payload: " <> e
    Right p -> do
      let sigs = map (\s -> (PublicKeyHex $ _siPubKey s, Nothing)) $ _pSigners p
      Right $ SigData (_cmdHash c) sigs (Just $ _cmdPayload c)

-- |Only accepts a list of signer/keypair tuples so that the ordering of sigs has the same ordering
-- as signers, which is required by pact
buildSigsPreserveOrder
  :: ( MonadJSM m
     , HasCrypto key m
     )
  => TypedHash h
  -> [(Signer, Maybe (KeyPair key))]
  -> m [(PublicKeyHex, Maybe UserSig)]
buildSigsPreserveOrder cmdHashL signingPairs =
  forM signingPairs $ \(signer, mkp)-> case mkp of
    Just (KeyPair _ (Just privKey)) -> do
      sig <- cryptoSign (unHash . toUntypedHash $ cmdHashL) privKey
      pure (toPubKeyHex signer, Just $ toPactSig sig )
    _ -> pure (toPubKeyHex signer, Nothing)
  where
    toPactSig sig = UserSig $ keyToText sig
    toPubKeyHex = PublicKeyHex . _siPubKey

-- | Build a single cmd as expected in the `cmds` array of the /send payload.
--
-- As specified <https://pact-language.readthedocs.io/en/latest/pact-reference.html#send here>.
buildCmdWithPayload
  :: ( MonadIO m
     , MonadJSM m
     , HasCrypto key m
     )
  => Payload PublicMeta Text
  -> [KeyPair key]
  -- ^ Keys which we are signing with
  -> m (Command Text)
buildCmdWithPayload payload signingKeys = do
  let cmd = encodeAsText $ encode payload
  let cmdHashL = hash (T.encodeUtf8 cmd)
  sigs <- buildSigs Nothing cmdHashL signingKeys
  pure $ Command
    { _cmdPayload = cmd
    , _cmdSigs = sigs
    , _cmdHash = cmdHashL
    }

buildSigsWithPactKey
  :: ( MonadJSM m
     , HasCrypto key m
     )
  => PactKey
  -> TypedHash h
  -> [KeyPair key]
  -> m [UserSig]
buildSigsWithPactKey pk cmdHashL signingPairs = do
  pactKeySig <- cryptoSignWithPactKey (unHash . toUntypedHash $ cmdHashL) pk
  buildSigs (Just pactKeySig) cmdHashL signingPairs

-- | Build signatures for a single `cmd`.
buildSigs
  :: ( MonadJSM m
     , HasCrypto key m
     )
  => Maybe Signature
  -> TypedHash h
  -> [KeyPair key]
  -> m [UserSig]
buildSigs mSig cmdHashL signingPairs = do
  let signingKeys = mapMaybe _keyPair_privateKey signingPairs
  sigs <- traverse (cryptoSign (unHash . toUntypedHash $ cmdHashL)) signingKeys
  pure $ map toPactSig (maybe sigs (:sigs) mSig)
  where
    toPactSig :: Signature -> UserSig
    toPactSig sig = UserSig $ keyToText sig


-- | Build cont `cmd` payload.
--
--   As specified <https://pact-language.readthedocs.io/en/latest/pact-reference.html#cmd-field-and-payloads here>.
--   Use `encodedAsText` for passing it as the `cmd` payload.
buildContPayload
  :: MonadIO m
  => NetworkName
  -- ^ The network that we are targeting
  -> PublicMeta
  -- ^ Assorted information for the payload. The time is overridden.
  -> [KeyPair key]
  -- ^ Keys which we are signing with
  -> ContMsg
  -- ^ Continuation payload
  -> m (Payload PublicMeta Text)
buildContPayload networkName meta signingKeys payload = do
    time <- getCreationTime
    nonce <- getNonce
    pure $ Payload
      { _pPayload = Continuation payload
      , _pNonce = "CW:" <> nonce
      , _pMeta = meta { _pmCreationTime = time }
      , _pSigners = map mkSigner (map _keyPair_publicKey signingKeys)
      , _pNetworkId = pure $ NetworkId $ textNetworkName networkName
      }
  where
    mkSigner pubKey = Signer
      { _siScheme = pure ED25519
      , _siPubKey = keyToText pubKey
      , _siAddress = pure $ keyToText pubKey
      , _siCapList = []
      }


-- | Build exec `cmd` payload.
--
--   As specified <https://pact-language.readthedocs.io/en/latest/pact-reference.html#cmd-field-and-payloads here>.
--   Use `encodedAsText` for passing it as the `cmd` payload.
buildExecPayload
  :: MonadIO m
  => Maybe Text
  -- ^ Nonce. When missing, uses the current time.
  -> NetworkName
  -- ^ The network that we are targeting
  -> PublicMeta
  -- ^ Assorted information for the payload. The time is overridden.
  -> [KeyPair key]
  -- ^ Keys which we are signing with
  -> [PublicKey]
  -- ^ Keys which should be added to `signers`, but not used to sign
  -> Text
  -- ^ Code
  -> Object
  -- ^ Data object
  -> Map PublicKey [SigCapability]
  -- ^ Capabilities for each public key
  -> m (Payload PublicMeta Text)
buildExecPayload mNonce networkName meta signingKeys extraKeys code dat caps = do
    time <- getCreationTime
    nonce <- maybe getNonce pure mNonce
    let
      payload = ExecMsg
        { _pmCode = code
        , _pmData = Object dat
        }
    pure $ Payload
      { _pPayload = Exec payload
      , _pNonce = "CW:" <> nonce
      , _pMeta = meta { _pmCreationTime = time }
      , _pSigners = map mkSigner (map _keyPair_publicKey signingKeys ++ extraKeys)
      , _pNetworkId = pure $ NetworkId $ textNetworkName networkName
      }
  where
    mkSigner pubKey = Signer
      { _siScheme = pure ED25519
      , _siPubKey = keyToText pubKey
      , _siAddress = pure $ keyToText pubKey
      , _siCapList = Map.findWithDefault [] pubKey caps
      }

-- no signing of any kind here
simpleLocal
  :: (MonadIO m)
  => Maybe Text
  -> NetworkName
  -> PublicMeta
  -> Text
  -> m (Command Text)
simpleLocal nonce networkName meta code = do
  cmd <- encodeAsText . encode <$> buildExecPayload nonce networkName meta mempty mempty code mempty mempty
  let cmdHashL = hash (T.encodeUtf8 cmd)
  pure $ Pact.Types.Command.Command
    { _cmdPayload = cmd
    , _cmdSigs = mempty
    , _cmdHash = cmdHashL
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
prettyPrintNetworkError = \case
  NetworkError_NetworkError msg -> "Network error: " <> msg
  NetworkError_Status c msg -> "Error HTTP response (" <> tshow c <> "):" <> msg
  NetworkError_Decoding msg -> "Decoding server response failed: " <> msg
  NetworkError_ReqTooLarge -> "Request exceeded the allowed maximum size!"
  NetworkError_CommandFailure e -> tshow e
  NetworkError_NoNetwork t -> "An action requiring a network was about to be performed, but we don't (yet) have any selected network: '" <> t <> "'"
  NetworkError_Other m -> m

prettyPrintNetworkErrors :: NonEmpty (Maybe URI, NetworkError) -> Text
prettyPrintNetworkErrors = T.intercalate "\n" . toList . fmap (\(mu,e) ->
  renderMaybeUri mu <> prettyPrintNetworkError e)
  where
    renderMaybeUri = maybe "" (\h -> ("Error from (" <> h <> "): ")) . (extractHost =<<)
    extractHost = (^? URIL.uriAuthority . _Right . URIL.authHost . URIL.unRText)

-- | Pretty print a `NetworkErrorResult`.
prettyPrintNetworkErrorResult :: NetworkErrorResult -> Text
prettyPrintNetworkErrorResult = \case
  This errs -> prettyPrintNetworkErrors errs
  That (_gas, r) -> "Successful Server result: " <> prettyTextPretty r
  (These errs (_gas, r)) -> "Successful Server result: " <> prettyTextPretty r <> "\n  These nodes failed: \n" <> prettyPrintNetworkErrors errs


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
    refreshA deployA setSenderA setGasLimitA setGasPriceA setTtlA refreshNodesA trackNodesA setNetworksA resetNetworksA selectNetworkA
    <>
    NetworkCfg
      refreshB deployB setSenderB setGasLimitB setGasPriceB setTtlB refreshNodesB trackNodesB setNetworksB resetNetworksB selectNetworkB
      = NetworkCfg
        { _networkCfg_refreshModule = leftmost [ refreshA, refreshB ]
        , _networkCfg_deployCode    = deployA <> deployB
        , _networkCfg_setSender     = leftmost [ setSenderA, setSenderB ]
        , _networkCfg_setGasLimit   = leftmost [ setGasLimitA, setGasLimitB ]
        , _networkCfg_setGasPrice   = leftmost [ setGasPriceA, setGasPriceB ]
        , _networkCfg_setTTL        = leftmost [ setTtlA, setTtlB ]
        , _networkCfg_refreshNodes  = leftmost [ refreshNodesA, refreshNodesB ]
        , _networkCfg_trackNodes    = trackNodesA <> trackNodesB
        , _networkCfg_setNetworks   = leftmost [setNetworksA, setNetworksB ]
        , _networkCfg_resetNetworks = leftmost [resetNetworksA, resetNetworksB ]
        , _networkCfg_selectNetwork = leftmost [selectNetworkA, selectNetworkB ]
        }

instance Reflex t => Monoid (NetworkCfg t) where
  mempty = NetworkCfg never never never never never never never never never never never
  mappend = (<>)

instance Flattenable (NetworkCfg t) t where
  flattenWith doSwitch ev =
    NetworkCfg
      <$> doSwitch never (_networkCfg_refreshModule <$> ev)
      <*> doSwitch never (_networkCfg_deployCode <$> ev)
      <*> doSwitch never (_networkCfg_setSender <$> ev)
      <*> doSwitch never (_networkCfg_setGasLimit <$> ev)
      <*> doSwitch never (_networkCfg_setGasPrice <$> ev)
      <*> doSwitch never (_networkCfg_setTTL <$> ev)
      <*> doSwitch never (_networkCfg_refreshNodes <$> ev)
      <*> doSwitch never (_networkCfg_trackNodes <$> ev)
      <*> doSwitch never (_networkCfg_setNetworks <$> ev)
      <*> doSwitch never (_networkCfg_resetNetworks <$> ev)
      <*> doSwitch never (_networkCfg_selectNetwork <$> ev)
