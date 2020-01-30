{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE QuasiQuotes #-}

-- |
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.ReplGhcjs where

import Control.Lens
import Control.Monad.State.Strict
import Data.Text (Text)
import Language.Javascript.JSaddle (liftJSM)
import Obelisk.Route (R)
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Common.Route

import qualified Servant.Client.JSaddle            as S

import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as T
import Safe (fromJustNote)
import Pact.Server.ApiV1Client as Pact
import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Hash as Pact
import qualified Text.URI as URI hiding (uriPath)
import qualified Text.URI.QQ as URI
import Text.URI.Lens (uriPath)

app
  :: forall t m.
     ( MonadWidget t m
     )
  => RoutedT t (R FrontendRoute) m ()
app = do
  let
    args :: [Text]
    args = ["chainweb", "0.0", "testnet04", "chain", "8", "pact"]
    path :: [URI.RText 'URI.PathPiece]
    path = fromJustNote "Building chain base path failed!" . traverse URI.mkPathPiece $ args
    host :: URI.RText 'URI.Host
    host = fromJustNote "HOOOST" $ URI.mkHost "eu1.testnet.chainweb.com"
    auth :: URI.Authority
    auth = URI.Authority Nothing host Nothing
    uri :: URI.URI
    uri = URI.URI (Just [URI.scheme|https|]) (Right auth) Nothing [] Nothing
      & uriPath .~ path

    payload :: ByteString
    payload = "{\"networkId\":\"testnet\",\"payload\":{\"exec\":{\"data\":{},\"code\":\"(list-modules)\"}},\"signers\":[],\"meta\":{\"creationTime\":1580337414,\"ttl\":28800,\"gasLimit\":600,\"chainId\":\"0\",\"gasPrice\":1.0e-5,\"sender\":\"5d0e99e446a078a356e86934c4b1d223f482e9f8888fb215502d3543b4abfdcf\"},\"nonce\":\"2020-01-29 22:37:09.307237957 UTC\"}"

  liftIO $ do
    putStrLn "=============================="
  r <- case S.parseBaseUrl $ URI.renderStr uri of
    Nothing -> error "nope"
    Just baseUrl -> do
       let env = S.mkClientEnv baseUrl
           cmd = fmap T.decodeUtf8 $ Pact.Command payload [] $ Pact.hash payload
       liftJSM $ flip S.runClientM env $ Pact.local apiV1Client cmd
  liftIO $ do
    putStrLn "=============================="
    print r
