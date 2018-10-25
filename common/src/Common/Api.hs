{-# LANGUAGE OverloadedStrings #-}

module Common.Api where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Semigroup ((<>))
import Obelisk.ExecutableConfig (get)



-- | How many Pact instances to launch in development mode.
numPactInstances :: Int
numPactInstances = 4

-- | Get the port number a given pact instance is launched on.
--
--   Numeration starts at 1. So for `numPactInstances` == 2 we would have
--   instances with numbers 1 and 2.
getPactInstancePort :: Int -> Text
getPactInstancePort num = T.pack . show $ 7020 + num



-- | Retrieve the list of pact servers as specified in the static config.
--
--   If that config file is present, then we are running in `production` mode:
--   The frontend will retrieve the dynamic server list at `pactServerListPath`
--   and will fall back to this static one, if not found.
--
--   When this function returns `Nothing` (no config file found), the frontend
--   and backend will run in dev mode. Meaning, the backend will spawn
--   `numPactInstances` pact instances and the frontend will list and connect
--   to those.
getPactServerList :: IO (Maybe (Text))
getPactServerList = get "config/common/pact-server-list"
