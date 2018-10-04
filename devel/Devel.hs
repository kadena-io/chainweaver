module Devel where

import           Control.Concurrent.Async (withAsync)
import           Control.Monad
import           Obelisk.Run
import qualified Pact.Server.Server       as Pact


import           Backend
import           Frontend


main :: Int -> IO ()
main port = do
  let
    runPact = Pact.serve "config/common/pact.yaml"
    runServer = run port backend frontend
  withAsync runPact (const runServer)

