{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

-- Workaround for untyped and unparsable Pact API. It should get properly fixed in Pact at some point, see:
-- https://github.com/kadena-io/pact/pull/375
-- Note: As those types are not shared with pact, we don't really gain much*)
-- safety/guarantees by those types - it just makes handling of the API easier. At
-- some point the above mentioned PR or something similar should really get merged.

--  *) not much but not nothing, as some types are shared.

module Pact.Typed.Server.API
  ( ApiV1API
  , PactServerAPI
  , pactServerAPI
  ) where

import Data.Proxy
import Servant.API
import qualified Pact.Analyze.Remote.Types as Analyze
import Pact.Typed.Types.API
import Pact.Typed.Types.Command
import Data.Text (Text)

type ApiV1API =
  (    "send" :> ReqBody '[JSON] SubmitBatch :>
    Post '[JSON] RequestKeys
  :<|> "poll" :> ReqBody '[JSON] Poll :>
    Post '[JSON] PollResponses
  :<|> "listen" :> ReqBody '[JSON] ListenerRequest :>
    Post '[JSON] ApiResult
  :<|> "local" :> ReqBody '[JSON] (Command Text) :>
    Post '[JSON] CommandValue
  )

type PactServerAPI =
       "api" :> "v1" :> ApiV1API
  :<|> "verify" :> ReqBody '[JSON] Analyze.Request :> Post '[JSON] Analyze.Response
  :<|> "version" :> Get '[PlainText] Text

pactServerAPI :: Proxy PactServerAPI
pactServerAPI = Proxy
