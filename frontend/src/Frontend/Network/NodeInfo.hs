{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}


-- | Information about a chainweb like node.
--
--   A node (hostname) can either be a node of a chainweb network in some
--   version with some number of available chains or a single `pact -s` node.
--   This module provides the necessary abstractions to work with such nodes.
--
--   In particular it provides `NodeInfo` which can retrieved from an `Authority`
--   by means of `discoverNode`.
--
module Frontend.Network.NodeInfo
  ( -- * Types & Classes
    ChainId
  , NodeInfo (..)
  , nodeVersion
  , nodeInfoRef
    -- * Discover
  , parseNodeRef
  , discoverNode
    -- * Get node/network information.
  , getChainRefBaseUrl
  , getChainBaseUrl
  , getChains
    -- * More details
  , NodeType (..)
  , ChainwebInfo (..)
  ) where

import           Control.Applicative         ((<|>))
import           Control.Arrow               (right)
import           Control.Arrow               (left)
import           Control.Error.Safe          (headErr, maximumErr)
import           Control.Lens
import           Control.Monad
import           Control.Monad               (void)
import           Control.Monad.Except        (ExceptT (..), MonadError,
                                              liftEither, runExceptT,
                                              throwError)
import           Control.Monad.IO.Unlift
import           Data.Aeson                  (Value)
import qualified Data.Aeson                  as Aeson
import qualified Data.Aeson.Lens             as AL
import           Data.Default
import qualified Data.HashMap.Lazy           as HM
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Void                   (Void)
import           Language.Javascript.JSaddle (JSM, MonadJSM, liftJSM)
import           Reflex.Dom.Class            (HasJSContext (..))
import           Reflex.Dom.Xhr
import           Safe                        (fromJustNote)
import qualified Text.Megaparsec             as MP
import qualified Text.Megaparsec.Char        as MP
import           Text.Read                   (readMaybe)
import           Text.URI                    (URI (URI))
import qualified Text.URI                    as URI hiding (uriPath)
import           Text.URI.Lens               as URI
import qualified Text.URI.QQ                 as URI
import           UnliftIO.Async
import           UnliftIO.Exception          (catch)
import           UnliftIO.MVar
import qualified Pact.Types.ChainId          as Pact

import           Common.Network              (ChainId (..), ChainRef (..),
                                              NodeRef (..), parseNodeRef)
import           Frontend.Foundation


data ChainwebInfo = ChainwebInfo
  { _chainwebInfo_version        :: Text
    -- ^ What chainweb version is running on the node.
  , _chainwebInfo_networkVersion :: Text
    -- ^ What version of the network is running.
  , _chainwebInfo_numberOfChains :: Word
    -- ^ How many chains do we have.
  }
  deriving (Eq, Ord, Show)


data NodeType =
    NodeType_Pact Text -- ^ A pact -s node with the provided version string.
  | NodeType_Chainweb  ChainwebInfo -- ^ A chainweb node.
  deriving (Eq, Ord, Show)

-- | Internaly used Uri type, which diverges from URI mostly for a mandatory
-- instead of optional Authority. (Thus we can avoid pointless `Maybe`s or
-- partial functions.)
data NodeUri = NodeUri
  { _nodeUri_scheme    :: URI.RText 'URI.Scheme
  , _nodeUri_authority :: URI.Authority
  }
  deriving (Eq, Ord, Show, Generic)

data NodeInfo = NodeInfo
  { _nodeInfo_baseUri :: NodeUri
  , _nodeInfo_type    :: NodeType
  }
  deriving (Eq, Ord, Show, Generic)

nodeVersion :: NodeInfo -> Text
nodeVersion ni = case _nodeInfo_type ni of
                   NodeType_Pact _v -> ""
                   NodeType_Chainweb ci -> _chainwebInfo_networkVersion ci

-- | Retrive the `NodeInfo` for a given host by quering its API.
discoverNode :: forall m. (MonadJSM m, MonadUnliftIO m, HasJSContext m) => NodeRef -> m (Either Text NodeInfo)
discoverNode (NodeRef auth) = do
    httpsReqs <- async $ discoverChainwebOrPact httpsUri
    httpReqs <- async $ discoverChainwebOrPact httpUri

    -- For some http only servers waiting for a https response will take ages
    -- on the other hand we need to prefer chainweb detection over pact -s
    -- detection (as the former is more reliable). Therefore we group them by
    -- protocol and go with the first success result.
    waitSuccess [httpsReqs, httpReqs]

  where
    waitSuccess :: [Async (Either Text NodeInfo)] -> m (Either Text NodeInfo)
    waitSuccess = \case
      [] -> pure $ Left ""
      xs -> do
        (finished, r) <- waitAny xs
        case r of
          Left err -> do
            left ((err <> "\n\n") <>) <$> waitSuccess (filter (/= finished) xs)
          Right success -> pure $ Right success

    httpsUri = uriFromSchemeAuth [URI.scheme|https|]
    httpUri = uriFromSchemeAuth [URI.scheme|http|]

    uriFromSchemeAuth scheme =  NodeUri scheme auth


-- | The node this node info is for.
nodeInfoRef :: NodeInfo -> NodeRef
nodeInfoRef = NodeRef . _nodeUri_authority . _nodeInfo_baseUri


-- | Get a base `URI` from a `NodeUri`.
nodeToBaseUri :: NodeUri -> URI
nodeToBaseUri (NodeUri scheme auth) = URI (Just scheme) (Right auth) Nothing [] Nothing


-- | Base url to use for a particular `ChainRef`.
--
--   Note in case the `ChainRef` provides a `_chainRef_node` we have to run a
--   node detection, so this call might fail in that case.
getChainRefBaseUrl :: MonadJSM m => ChainRef -> Maybe NodeInfo -> m (Either Text URI)
getChainRefBaseUrl (ChainRef mNode chainId) mInfo = do
    jsm <- askJSM
    errInfo <- getInfo `runJSM` jsm
    pure $ right (getChainBaseUrl chainId) errInfo
  where
    getInfo = fromMaybe uInfo <$> traverse discoverNode mNode
    uInfo = maybe (Left "No network node available!") Right mInfo

-- | Base url to use for a particular chain.
--
--   This is the url where you can append /send, /local, /listen ...
getChainBaseUrl :: ChainId -> NodeInfo -> URI
getChainBaseUrl chainId (NodeInfo base nType) =
    nodeToBaseUri base & uriPath .~ getChainBasePath chainId nType


-- | Get a list of available chains.
--
getChains :: NodeInfo -> [Pact.ChainId]
getChains (NodeInfo _ nType) = Pact.ChainId . tshow <$>
  case nType of
    NodeType_Pact _ -> [ 0 ]
    NodeType_Chainweb info -> [ 0 .. (_chainwebInfo_numberOfChains info -1)]


-- | Get the path for a given chain id.
getChainBasePath :: ChainId -> NodeType -> [URI.RText 'URI.PathPiece]
getChainBasePath (ChainId chainId) = buildPath . \case
    NodeType_Pact _
      -> [] -- ["api", "v1"]
    NodeType_Chainweb (ChainwebInfo cwVersion netVersion _)
      -> ["chainweb", cwVersion, netVersion, "chain", chainId, "pact"]
  where
    buildPath = fromJustNote "Building chain base path failed!" . traverse URI.mkPathPiece


-- | Find out whether the given host and scheme are either a Pact or a Chainweb node.
discoverChainwebOrPact :: (MonadJSM m, HasJSContext m, MonadUnliftIO m) => NodeUri -> m (Either Text NodeInfo)
discoverChainwebOrPact uri = do
  (chainwebResp, pactResp) <- discoverChainwebNode uri  `concurrently` discoverPactNode uri
  -- pure $ chainwebResp <|> pactResp
  -- No `Error` instance for Text:
  pure $ left T.pack $ left T.unpack chainwebResp <|> left T.unpack pactResp


-- | Retrieve the `NodeInfo` for a given host by quering its API.
--
--   This function will only succeed for chainweb nodes.
discoverChainwebNode :: (MonadJSM m, HasJSContext m, MonadUnliftIO m) => NodeUri -> m (Either Text NodeInfo)
discoverChainwebNode baseUri = runExceptT $ do

    let req = mkSwaggerReq $ nodeToBaseUri baseUri
    resp <- ExceptT . fmap (left tshow) $ runReq req

    when (_xhrResponse_status resp /= 200) $
      throwError $ "Received non 200 status: " <> tshow (_xhrResponse_status resp)

    swaggerI <- note "Parsing swagger.json failed" $
      Aeson.decodeStrict . T.encodeUtf8 <=< _xhrResponse_responseText $ resp

    info <- parseChainwebInfo swaggerI
    pure $ NodeInfo
      { _nodeInfo_baseUri = baseUri
      , _nodeInfo_type = NodeType_Chainweb info
      }


-- | Find out whether the node could be a valid pact -s node.
--
--   WARNING: The check is pretty basic and could easily confuse a `pact -s` with a chainweb node, when
--   chainweb or pact -s evolve a bit, therefore, always run
--   `discoverChainwebNode` first, which is more reliable.
discoverPactNode :: (MonadJSM m, HasJSContext m, MonadUnliftIO m) => NodeUri -> m (Either Text NodeInfo)
discoverPactNode baseUri = runExceptT $ do
    let req = mkVersionReq $ nodeToBaseUri baseUri
    resp <- ExceptT . fmap (left tshow) $ runReq req
    when (_xhrResponse_status resp /= 200) $
      throwError $ "Received non 200 status: " <> tshow (_xhrResponse_status resp)
    pure $ NodeInfo
      { _nodeInfo_baseUri = baseUri
      , _nodeInfo_type = NodeType_Pact $ fromMaybe "" $ _xhrResponse_responseText resp
      }


-- | Parse `ChainwebInfo` given a `Value` representing /swagger.json.
{- parseChainwebInfo :: forall m. MonadPlus m => Value -> m ChainwebInfo -}
parseChainwebInfo :: forall m. (MonadError Text m) => Value -> m ChainwebInfo
parseChainwebInfo v = do

    allPaths <- note "Found no paths object" $ v ^? AL.key "paths" . AL._Object . to HM.keys

    let
      chainFilter = (&&) <$> T.isPrefixOf "/chainweb/" <*> T.isInfixOf "/chain/"
      sendFilter = (&&) <$> T.isSuffixOf "/send" <*> chainFilter
      sendPaths = filter sendFilter allPaths

    examplePath <- liftEither $ headErr "No send paths found." sendPaths
    (chainwebVersion, networkVersion) <- getVersions examplePath

    chainIds <- traverse getChainId sendPaths
    numberOfChains <- fmap (+1) . liftEither $ maximumErr "No chainids found." chainIds

    pure $ ChainwebInfo
      { _chainwebInfo_version = chainwebVersion
      , _chainwebInfo_networkVersion = networkVersion
      , _chainwebInfo_numberOfChains = numberOfChains
      }

  where
    -- Get chainweb and network version from path:
    getVersions :: forall mp. MonadError Text mp => Text -> mp (Text, Text)
    getVersions = parseLifted versionsP

    versionsP :: MP.Parsec Void Text (Text, Text)
    versionsP = do
      void $ MP.string "/chainweb/"
      chainwebVersion <- MP.takeWhile1P (Just "chainweb version") (/= '/')
      void $ MP.char '/'
      networkVersion <- MP.takeWhile1P (Just "network version") (/= '/')
      pure (chainwebVersion, networkVersion)

    getChainId :: forall mp. MonadError Text mp => Text -> mp Word
    getChainId = parseLifted chainIdP

    chainIdP :: MP.Parsec Void Text Word
    chainIdP = do
      void versionsP
      void $ MP.string "/chain/"
      digitsP

    digitsP :: MP.Parsec Void Text Word
    digitsP = maybe (fail "parseChainwebInfo: digitsP: no parse") pure . readMaybe =<< MP.some MP.digitChar

    parseLifted :: forall a mp. MonadError Text mp => MP.Parsec Void Text a -> Text -> mp a
    parseLifted p s = liftEither . left (T.pack . show) $ MP.runParser p "swagger.json" s


runReq
  :: (HasJSContext m, MonadJSM m, MonadUnliftIO m, IsXhrPayload a)
  => XhrRequest a
  -> m (Either XhrException XhrResponse)
runReq req = do
  resp <- newEmptyMVar
  void $ newXMLHttpRequestWithErrorSane req (liftIO . putMVar resp)
  takeMVar resp


-- Sane version of newXMLHttpRequestWithError: Report all errors via callback,
-- including those that are thrown in JS. We return () instead of the
-- XmlHttpRequest as we can't get access to the that object in case of an
-- exception and it does not really make sense to throw it, as we are reporting
-- the error via the callback.
newXMLHttpRequestWithErrorSane
    :: forall m a.
      ( HasJSContext m, MonadJSM m, IsXhrPayload a, MonadUnliftIO m)
    => XhrRequest a
    -- ^ The request to make.
    -> (Either XhrException XhrResponse -> JSM ())
    -- ^ A continuation to be called once a response comes back, or in
    -- case of error.
    -> m ()
newXMLHttpRequestWithErrorSane req cb =
    void (newXMLHttpRequestWithError req cb) `catch` handleException
  where
    handleException :: XhrException -> m ()
    handleException e = liftJSM $ cb $ Left e


-- | Get requests for retrieving the swagger.json from a given node.
mkSwaggerReq :: URI -> XhrRequest ()
mkSwaggerReq uri =
  let
    swaggerURI = uri & URI.uriPath .~ [ [URI.pathPiece|swagger.json|] ]
  in
    xhrRequest "GET" (URI.render swaggerURI) def


-- | Get requests for retrieving /version from a given pact -s node.
mkVersionReq :: URI -> XhrRequest ()
mkVersionReq uri =
  let
    versionURI = uri & URI.uriPath .~ [ [URI.pathPiece|version|] ]
  in
    xhrRequest "GET" (URI.render versionURI) def
