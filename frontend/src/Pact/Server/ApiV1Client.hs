{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

-- Limit API to the parts that are common to chainweb and `pact -s`.
module Pact.Server.ApiV1Client
  ( ApiV1API
  , ApiV1Client(..)
  , apiV1Client
  ) where

import Data.Proxy
import Servant.API
import Servant.Client.Core
import Pact.Types.API
import Pact.Types.Command
import Data.Text (Text)
import Pact.Types.Hash (Hash)

import Pact.Server.API

data ApiV1Client m = ApiV1Client
  { send :: SubmitBatch -> m RequestKeys
  , poll :: Poll -> m PollResponses
  , listen :: ListenerRequest -> m ListenResponse
  , local :: Command Text -> m (CommandResult Hash)
  }

{- apiV1API :: Proxy ApiV1API -}
{- apiV1API = Proxy -}

apiV1Client :: forall m. RunClient m => ApiV1Client m
apiV1Client = let
  send :<|> poll :<|> listen :<|> local =
    clientIn apiV1API (Proxy :: Proxy m)
  in ApiV1Client { send, poll, listen, local }
