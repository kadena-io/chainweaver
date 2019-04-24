{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
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


-- | Information about a chainweb like node.
--
--   A node (hostname) can either be a node of a chainweb network in some
--   version with some number of available chains or a single `pact -s` node.
--   This module provides the necessary abstractions to work with such nodes.
--
--   In particular it provides `NodeInfo` which can retrieved from an `Authority`
--   by means of `discoverNode`.
--
module Frontend.Backend.NodeInfo
  ( -- * Types & Classes
  ) where


import           Control.Lens
import Control.Monad.Trans.Class
import           Control.Monad
import           Control.Monad               (void)
import           Control.Monad.Except        (MonadError)
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Maybe
import           Data.Aeson                  (Value)
import qualified Data.Aeson                  as Aeson
import           Data.Default
import           Data.Either                 (rights)
import           Data.Text                   (Text)
import qualified Data.Text.Encoding          as T
import           Language.Javascript.JSaddle (JSM, MonadJSM, askJSM, liftJSM,
                                              runJSM)
import           Reflex.Dom.Class            (HasJSContext (..),
                                              JSContextSingleton (..))
import           Reflex.Dom.Xhr
import           Text.URI                    (URI (URI))
import qualified Text.URI                    as URI hiding (uriPath)
import           Text.URI.Lens               as URI
import qualified Text.URI.QQ                 as URI
import           UnliftIO.Async
import           UnliftIO.Exception          (catch)
import           UnliftIO.MVar

data ChainwebInfo = ChainwebInfo
  { _chainwebInfo_version        :: Text
    -- ^ What chainweb version is running on the node.
  , _chainwebInfo_networkVersion :: Text
    -- ^ What version of the network is running.
  , _chainwebInfo_numberOfChains :: Int
    -- ^ How many chains do we have.
  }


data NodeType =
    NodeType_Pact -- ^ A pact -s node
  | NodeType_Chainweb  ChainwebInfo -- ^ A chainweb node.


data NodeInfo = NodeInfo
  { _nodeInfo_baseUri   :: URI
  , _nodeInfo_type      :: NodeType
  }


-- | Retrive the `NodeInfo` for a given host by quering its API.
discoverNode :: (MonadJSM m, MonadUnliftIO m, HasJSContext m) => URI.Authority -> m (Maybe NodeInfo)
discoverNode auth = do
    let uris = flip uriFromSchemeAuth auth <$> schemes
    resps <- mapConcurrently discoverChainwebNode uris
    error "Not yet implemented"
  where
    schemes = [ [URI.scheme|https|], [URI.scheme|http|] ]
    uriFromSchemeAuth scheme auth = URI (Just scheme) (Right auth) Nothing [] Nothing


-- | Retrive the `NodeInfo` for a given host by quering its API.
--
--   This function will only succeed for chainweb nodes.
discoverChainwebNode :: (MonadJSM m, HasJSContext m, MonadUnliftIO m) => URI -> m (Maybe NodeInfo)
discoverChainwebNode baseUri = runMaybeT $ do
    let req = mkSwaggerReq baseUri
    resp <- rightZ =<< lift (runReq req)
    when (_xhrResponse_status resp /= 200) mzero
    swaggerI <- justZ $ Aeson.decodeStrict . T.encodeUtf8 <=< _xhrResponse_responseText $ resp
    info <- parseChainwebInfo swaggerI
    pure $ NodeInfo
      { _nodeInfo_baseUri = baseUri
      , _nodeInfo_type = NodeType_Chainweb info
      }
  where
    rightZ = either (const mzero) pure
    justZ = maybe mzero pure

parseChainwebInfo :: MonadPlus m => Value -> m ChainwebInfo
parseChainwebInfo v = do
  error "Not yet implemented"

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


-- | Get requests for retrieving the swagger.json from a given
mkSwaggerReq :: URI -> XhrRequest ()
mkSwaggerReq uri =
  let
    swaggerURI = uri & URI.uriPath .~ [ [URI.pathPiece| "swagger.json"|] ]
  in
    xhrRequest "GET" (URI.render swaggerURI) def
