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
  , mkSimpleReadReq
  , getNetworkNameAndMeta
  , getCreationTime
    -- * Defaults
  , chainwebGasLimit
  , defaultTransactionGasLimit
  , defaultTransactionGasPrice
  , maxCoinPrecision
  , defaultTransactionTTL
  ) where

import           Control.Arrow                     (first, left, second, (&&&))
import           Control.Lens                      hiding ((.=))
import           Control.Monad.Except
import           GHC.Word                          (Word8)
import           Data.Aeson                        (Object, Value (..), encode)
import qualified Data.Bifunctor                    as BiF
import qualified Data.ByteString.Lazy              as BSL
import           Data.Decimal                      (DecimalRaw (..))
import           Data.Either                       (lefts, rights)
import qualified Data.IntMap                       as IntMap
import qualified Data.List                         as L
import           Data.List.NonEmpty                (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty                as NEL
import qualified Data.Map                          as Map
import           Data.Map.Strict                   (Map)
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import qualified Data.Text.IO                      as T
import           Data.These                        (These(..), these)
import           Data.Time.Clock                   (getCurrentTime)
import           Data.Time.Clock.POSIX             (getPOSIXTime)
import           Data.Traversable                  (for)
import           Foreign.JavaScript.TH             (HasJSContext)
import           Language.Javascript.JSaddle.Monad (JSM, liftJSM)
import qualified Network.HTTP.Types                as HTTP
import           Reflex.Dom.Core                   (def, XhrRequest(..), XhrResponse(..), performRequestAsync)
import           System.IO                         (stderr)
import           Text.URI                          (URI)
import qualified Text.URI                          as URI
import qualified Text.URI.Lens                     as URIL

import           Pact.Parse                        (ParsedDecimal (..))
import           Pact.Server.ApiV1Client
import           Pact.Types.API
import           Pact.Types.Capability
import           Pact.Types.Command
import           Pact.Types.Runtime                (PactError (..), GasLimit (..), GasPrice (..), Gas (..))
import           Pact.Types.ChainMeta              (PublicMeta (..), TTLSeconds (..), TxCreationTime (..))
import qualified Pact.Types.ChainMeta              as Pact
import           Pact.Types.ChainId                (NetworkId (..))
import           Pact.Types.Exp                    (Literal (LString))
import           Pact.Types.Hash                   (hash, Hash (..), TypedHash (..), toUntypedHash)
import           Pact.Types.RPC
import           Pact.Types.PactValue
import qualified Servant.Client.JSaddle            as S

#if !defined (ghcjs_HOST_OS)
import           Pact.Types.Crypto                 (PPKScheme (..))
#endif

import           Common.Network
import           Common.Wallet
import           Frontend.Crypto.Class
import           Frontend.Crypto.Ed25519
import           Frontend.Foundation
import           Frontend.Messages
import           Frontend.Network.NodeInfo
import           Frontend.Storage                  (getItemStorage,
                                                    HasStorage,
                                                    localStorage,
                                                    removeItemStorage,
                                                    setItemStorage)


-- | What endpoint to use for a network request.
data Endpoint
  = Endpoint_Send
  | Endpoint_Local
  deriving (Show, Read, Generic, Eq, Ord, Bounded, Enum)

-- | Get string representation of `Endpoint` suitable for being displayed to an end user.
displayEndpoint :: Endpoint -> Text
displayEndpoint = \case
  Endpoint_Send -> "Transact"
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

-- | Model config needed by Network.
type HasNetworkModelCfg mConf t = (Monoid mConf, HasMessagesCfg mConf t)

-- | Things we want to store to local storage.
data StoreNetwork a where
  StoreNetwork_PublicMeta  :: StoreNetwork PublicMeta
  StoreNetwork_Networks    :: StoreNetwork (Map NetworkName [NodeRef])
  StoreNetwork_SelectedNetwork :: StoreNetwork NetworkName

deriving instance Show (StoreNetwork a)


makeNetwork
  :: forall key t m mConf
  . ( MonadHold t m, PerformEvent t m, MonadFix m
    , MonadJSM (Performable m), MonadJSM m
    , TriggerEvent t m, PostBuild t m
    , MonadSample t (Performable m)
    , HasNetworkModelCfg mConf t
    , HasNetworkCfg mConf t
    , HasConfigs m
    , HasJSContext (Performable m)
    , HasStorage m, HasStorage (Performable m)
    , HasCrypto key (Performable m)
    )
  => NetworkCfg t
  -> m (mConf, Network t)
makeNetwork cfg = mfix $ \ ~(_, networkL) -> do

    (mConf, onDeployed) <- deployCode networkL $ cfg ^. networkCfg_deployCode

    (cName, networks) <- getNetworks cfg
    onCName <- tagOnPostBuild cName

    let onRefresh = leftmost [ onDeployed, cfg ^. networkCfg_refreshModule ]
    nodeInfos <-
      getNetworkNodeInfosIncremental (current networks) (current cName) $ leftmost
        [ Just <$> onCName
          -- Refresh info on deployments and well on refresh (Refreshed node
          -- info triggers re-loading of modules too):
        , Nothing <$ onRefresh
        ]

    performEvent_ $ traverse_ reportNodeInfoError . lefts <$> updated nodeInfos

    modules <- loadModules networkL onRefresh

    meta <- buildMeta cfg

    pure
      ( mConf & networkCfg_refreshModule <>~ void (updated networks)
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
  -> Behavior t NetworkName -- ^ The currently user selected network.
  -> Event t (Maybe NetworkName) -- ^ Start NodeInfo retrieval for new selection/ or just do a refresh (`Nothing` case).
  -> m (Dynamic t [Either Text NodeInfo])
getNetworkNodeInfosIncremental cNets currentName onRefreshLoad = do
    let
      onNetName = attachWith fromMaybe currentName onRefreshLoad
      onLoad = fmapMaybe id onRefreshLoad

      onNodes :: Event t (NetworkName, [(Int, NodeRef)])
      onNodes = attachWith (flip getIndexedNodes) cNets onNetName

    onInfo <- performEventAsync $ uncurry getNodeInfosAsync <$> onNodes

    infoMap <- foldDyn id IntMap.empty $ leftmost
      [ const IntMap.empty <$ onLoad -- Only clear if network selection changed, not on refresh!
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
    discoverNodeAsync (i, nodeRef) = void $ liftJSM $ forkJSM $ do
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

-- This is the minimum precision allowed by the Pact language:
-- https://github.com/kadena-io/chainweb-node/commit/ee8a0db079869b39e23be1ef6737f0a7795eff87#diff-6c59a5fb9f1b0b8b470cb50e8bd643ebR54
defaultTransactionGasPrice :: GasPrice
defaultTransactionGasPrice = GasPrice $ ParsedDecimal $ Decimal maxCoinPrecision 1

-- | As defined in the coin contract
maxCoinPrecision :: Word8
maxCoinPrecision = 12

defaultTransactionTTL :: TTLSeconds
defaultTransactionTTL = TTLSeconds (8 * 60 * 60) -- 8 hours

-- Taken from https://github.com/kadena-io/chainweb-node/blob/85688ea0182d1b1ab0d8d784a48b4851a950ec7a/src/Chainweb/Chainweb.hs#L344
chainwebGasLimit :: Num a => a
chainwebGasLimit = 1e5

defaultTransactionGasLimit :: GasLimit
defaultTransactionGasLimit = GasLimit 1e5


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
    getItemStorage localStorage StoreNetwork_PublicMeta

  r <- foldDyn id m $ leftmost
    [ set Pact.pmSender <$> cfg ^. networkCfg_setSender
    , set Pact.pmGasLimit <$> cfg ^. networkCfg_setGasLimit
    , set Pact.pmGasPrice <$> cfg ^. networkCfg_setGasPrice
    , set Pact.pmTTL <$> cfg ^. networkCfg_setTTL
    ]

  onStore <- throttle 2 $ updated r
  performEvent_ $
    setItemStorage localStorage StoreNetwork_PublicMeta <$> onStore

  pure r


deployCode
  :: forall t m mConf
  . ( MonadHold t m, PerformEvent t m
    , MonadJSM (Performable m)
    , TriggerEvent t m
    , MonadSample t (Performable m)
    , HasNetworkModelCfg mConf t
    )
  => Network t
  -> Event t [NetworkRequest]
  -> m (mConf, Event t ())
deployCode networkL onReq = do
    reqRes <- performNetworkRequest networkL onReq
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
     , MonadHold t m, MonadJSM m, MonadFix m
     , HasNetworkCfg cfg t, HasConfigs m
     , PostBuild t m, HasJSContext (Performable m)
     , HasStorage m, HasStorage (Performable m)
     )
  => cfg -> m (Dynamic t NetworkName, Dynamic t (Map NetworkName [NodeRef]))
getNetworks cfg = do
    (defName, defNets, mRemoteSource) <- getConfigNetworks
    initialNets <- fromMaybe defNets <$>
      getItemStorage localStorage StoreNetwork_Networks
    initialName <- fromMaybe defName <$>
      getItemStorage localStorage StoreNetwork_SelectedNetwork

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
      setItemStorage localStorage StoreNetwork_Networks <$> onNetworksStore
    performEvent_ $
      removeItemStorage localStorage StoreNetwork_Networks <$ (cfg ^. networkCfg_resetNetworks)
    performEvent_ $
      setItemStorage localStorage StoreNetwork_SelectedNetwork <$> onSelectedStore

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
  :: ( PerformEvent t m, HasConfigs m )
  => m (NetworkName, Map NetworkName [NodeRef], Maybe Text)
getConfigNetworks = do
  let
    buildNodeRef =
      either (\e -> error $ "Parsing of dev networks failed: " <> T.unpack e) id
      . parseNodeRef
      . ("localhost:" <>)
      . getPactInstancePort

    buildName = uncheckedNetworkName . ("dev-" <>) . tshow
    buildNetwork = buildName &&& pure . buildNodeRef
    devNetworks = Map.fromList $ map buildNetwork [1 .. numPactInstances]

  errProdCfg <- getNetworksConfig
  pure $ case errProdCfg of
    Left (Left _) -> -- Development mode
      (buildName 1, devNetworks, Nothing)
    Left (Right x) -> -- Mac app remote list
      (uncheckedNetworkName "pact", mempty, Just x)
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
    )
  => Network t
  -> Event t ()
  -> m (Dynamic t (Map ChainId [Text]))
loadModules networkL onRefresh = do

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
      onErrResps <- performLocalReadLatest networkL onReqs

      let
        onByChainId :: Event t [ Either (NonEmpty NetworkError) (ChainId, PactValue) ]
        onByChainId = map byChainId <$> onErrResps

        onErrs = ffilter (not . null) $ fmap lefts onByChainId
        onResps = fmap rights onByChainId

        onModules :: Event t [(ChainId, [Text])]
        onModules =  map (second getModuleList) <$> onResps

      performEvent_ $ liftIO . traverse_ (T.hPutStrLn stderr . renderErrs . toList) <$> onErrs

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

      mkReq netName pm = mkSimpleReadReq "(list-modules)" netName pm . ChainRef Nothing

      byChainId :: (NetworkRequest, NetworkErrorResult) -> Either (NonEmpty NetworkError) (ChainId, PactValue)
      byChainId = sequence . bimap (_chainRef_chain . _networkRequest_chainRef) (bimap (fmap snd) snd . networkErrorResultToEither)

      renderErrs :: [NetworkError] -> Text
      renderErrs =
        T.unlines
        .  ("Error, retrieving modules for one or more chains: " :)
        . map prettyPrintNetworkError


      getModuleList :: PactValue -> [Text]
      getModuleList = \case
        PList terms -> mapMaybe getStringLit $ toList terms
        _               -> []

      getStringLit :: PactValue -> Maybe Text
      getStringLit = \case
        PLiteral (LString v) -> Just v
        _         -> Nothing


-- | Parse a NetworkErrorResult into something that the frontend can use
parseNetworkErrorResult :: (MonadIO m) => (PactValue -> Either Text a) -> NetworkErrorResult -> m (These Text a)
parseNetworkErrorResult parse = \case
  That (_gas, pactValue) -> doParse pactValue That
  This e -> pure $ This $ prettyPrintNetworkErrors e
  These errs (_gas, pactValue) -> doParse pactValue (These (prettyPrintNetworkErrors errs))
  where
    doParse pactValue f = case parse pactValue of
      Left e -> do
        liftIO $ T.putStrLn e
        pure $ This "Error parsing the response"
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
    )
  => Network t
  -> Event t [NetworkRequest]
  -> m (Event t [(NetworkRequest, NetworkErrorResult)])
performLocalReadLatest networkL onReqs = do
    counterDyn <- foldDyn (const (+1)) (0 :: Int) onReqs
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
    , TriggerEvent t m, MonadIO m
    , MonadSample t (Performable m)
    )
  => Network t
  -> Event t [NetworkRequest]
  -> m (Event t [(NetworkRequest, NetworkErrorResult)])
performLocalRead networkL onReqs =
  fmap (uncurry zip) <$> performLocalReadCustom networkL id onReqs

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
    )
  => Network t
  -> (req -> [NetworkRequest])
  -> Event t req
  -> m (Event t (req, [NetworkErrorResult]))
performLocalReadCustom networkL unwrapUsr onReqs = do
  time <- getCreationTime
  let
    unwrap = map (networkRequest_endpoint .~ Endpoint_Local) . unwrapUsr
    fakeNetwork = networkL & network_meta . mapped . Pact.pmCreationTime .~ time
  performNetworkRequestCustom fakeNetwork unwrap onReqs


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
  => Network t
  -> Event t [NetworkRequest]
  -> m (Event t [(NetworkRequest, NetworkErrorResult)])
performNetworkRequest networkL onReq =
  fmap (uncurry zip) <$> performNetworkRequestCustom networkL id onReq


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
  => Network t
  -> (req -> [NetworkRequest])
  -> Event t req
  -> m (Event t (req, [NetworkErrorResult]))
performNetworkRequestCustom networkL unwrap onReqs =
    performEventAsync $ ffor onReqs $ \reqs cb -> do
      nodeInfos <- getSelectedNetworkInfos networkL
      void $ liftJSM $ forkJSM $ do
        r <- traverse (doReqFailover nodeInfos) $ unwrap reqs
        liftIO $ cb (reqs, r)
  where
    doReqFailover :: [NodeInfo] -> NetworkRequest -> JSM NetworkErrorResult
    doReqFailover nodeInfos req =
      go [] nodeInfos
        where
          go :: [(Maybe URI,NetworkError)] -> [NodeInfo] -> JSM NetworkErrorResult
          go errs = \case
            n:ns -> do
              errRes <- doReq (Just n) req
              case errRes of
                Left (chainUrl,err) -> do
                  liftIO $ putStrLn $ "Got err: " <> show err
                  if shouldFailOver err
                               then go ((chainUrl,err) : errs) ns
                               else pure $ mkResult errs (Left (chainUrl,err))
                Right res -> pure $ mkResult errs (Right res)
            [] -> mkResult errs <$> doReq Nothing req
          mkResult errs (Left e) = This (NEL.reverse $ e :| errs)
          mkResult errs (Right res) = maybe (That res) (flip These res) . nonEmpty . reverse $ errs

    doReq :: Maybe NodeInfo -> NetworkRequest -> JSM (Either (Maybe URI,NetworkError) (Maybe Gas, PactValue))
    doReq mNodeInfo req = runExceptT $ do
      chainUrl <- ExceptT $ getBaseUrlErr (req ^. networkRequest_chainRef) mNodeInfo
      ExceptT $ liftJSM $ fmap (BiF.first (Just chainUrl,)) $ networkRequest chainUrl (req ^. networkRequest_endpoint) (_networkRequest_cmd req)

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
  :: URI
  -> Endpoint
  -> Command Text
  -> JSM (Either NetworkError (Maybe Gas, PactValue))
networkRequest baseUri endpoint cmd = do
    baseUrl <- S.parseBaseUrl $ URI.renderStr baseUri
    let clientEnv = S.mkClientEnv baseUrl

    liftJSM . runExceptT $ do
      performReq clientEnv

  where
    performReq clientEnv = case endpoint of
      Endpoint_Send -> do
        res <- runReq clientEnv $ send apiV1Client $ SubmitBatch . pure $ cmd
        key <- getRequestKey $ res
        -- TODO: If we no longer wait for /listen, we should change the type instead of wrapping that message in `PactValue`.
        pure $ (Nothing,) $ PLiteral . LString $
          T.dropWhile (== '"') . T.dropWhileEnd (== '"') . tshow $ key
        {- key <- getRequestKey $ res -}
        {- v <- runReq clientEnv $ listen pactServerApiClient $ ListenerRequest key -}
        {- case preview (Aeson.key "result" . Aeson.key "hlCommandResult" . _JSON) v of -}
        {-   Just cr -> pure cr -}
        {-   Nothing -> case fromJSON v of -}
        {-     Error str -> throwError $ NetworkError_ParseError $ T.pack str -}
        {-     Success ar -> pure $ _arResult ar -}
      Endpoint_Local ->
         fromCommandResult <=< runReq clientEnv  $ local apiV1Client cmd

    -- | Rethrow an error value by wrapping it with f.
    reThrowWith :: (e -> NetworkError) -> JSM (Either e a) -> ExceptT NetworkError JSM a
    reThrowWith f = ExceptT . fmap (left f)

    runReq :: S.ClientEnv -> S.ClientM a -> ExceptT NetworkError JSM a
    runReq env = reThrowWith packHttpErr . flip S.runClientM env

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
      , _pNonce = nonce
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
      , _pNonce = nonce
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
    refreshA deployA setSenderA setGasLimitA setGasPriceA setTtlA setNetworksA resetNetworksA selectNetworkA
    <>
    NetworkCfg
      refreshB deployB setSenderB setGasLimitB setGasPriceB setTtlB setNetworksB resetNetworksB selectNetworkB
      = NetworkCfg
        { _networkCfg_refreshModule = leftmost [ refreshA, refreshB ]
        , _networkCfg_deployCode    =  deployA <> deployB
        , _networkCfg_setSender     = leftmost [ setSenderA, setSenderB ]
        , _networkCfg_setGasLimit   = leftmost [ setGasLimitA, setGasLimitB ]
        , _networkCfg_setGasPrice   = leftmost [ setGasPriceA, setGasPriceB ]
        , _networkCfg_setTTL        = leftmost [ setTtlA, setTtlB ]
        , _networkCfg_setNetworks = leftmost [setNetworksA, setNetworksB ]
        , _networkCfg_resetNetworks = leftmost [resetNetworksA, resetNetworksB ]
        , _networkCfg_selectNetwork = leftmost [selectNetworkA, selectNetworkB ]
        }

instance Reflex t => Monoid (NetworkCfg t) where
  mempty = NetworkCfg never never never never never never never never never
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
      <*> doSwitch never (_networkCfg_setTTL <$> ev)
      <*> doSwitch never (_networkCfg_setNetworks <$> ev)
      <*> doSwitch never (_networkCfg_resetNetworks <$> ev)
      <*> doSwitch never (_networkCfg_selectNetwork <$> ev)
