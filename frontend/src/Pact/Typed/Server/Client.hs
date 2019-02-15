{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

-- Workaround for untyped and unparsable Pact API. It should get properly fixed in Pact at some point, see:
-- https://github.com/kadena-io/pact/pull/375
-- Note: As those types are not shared with pact, we don't really gain much*)
-- safety/guarantees by those types - it just makes handling of the API easier. At
-- some point the above mentioned PR or something similar should really get merged.

-- *) not much but not nothing, as some types are shared.

module Pact.Typed.Server.Client
  ( PactServerAPI
  , PactServerAPIClient(..)
  , pactServerAPI
  , pactServerApiClient
  ) where

import Data.Proxy
import Servant.API
import Servant.Client.Core
import qualified Pact.Analyze.Remote.Types as Analyze
import Data.Text (Text)

import Pact.Typed.Server.API
import Pact.Typed.Types.API
import Pact.Typed.Types.Command

data PactServerAPIClient m = PactServerAPIClient
  { send :: SubmitBatch -> m RequestKeys
  , poll :: Poll -> m PollResponses
  , listen :: ListenerRequest -> m ApiResult
  , local :: Command Text -> m CommandValue
  , verify :: Analyze.Request -> m Analyze.Response
  , version :: m Text
  }

pactServerApiClient :: forall m. RunClient m => PactServerAPIClient m
pactServerApiClient = let
  (send :<|> poll :<|> listen :<|> local) :<|> verify :<|> version =
    clientIn pactServerAPI (Proxy :: Proxy m)
  in PactServerAPIClient{ send, poll, listen, local, verify, version }
