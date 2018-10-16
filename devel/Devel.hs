module Devel where

import           Control.Concurrent.Async (withAsync)
import qualified Control.Concurrent.Async as Async
import           Control.Exception        (bracket)
import           Obelisk.Run
import qualified Pact.Server.Server       as Pact


import           Backend
import           Frontend


main :: Int -> IO ()
main port = do
  let
    runPact = pactSafeServe "config/common/pact.yaml"
    runPact2 = pactSafeServe "config/common/pact-2.yaml"
    runServer = run port backend frontend
  withAsync runPact $ \_ -> withAsync runPact2 $ \_ -> runServer

pactSafeServe :: String -> IO ()
pactSafeServe configFile = bracket
  (Pact.setupServer configFile)
  (\(_, cmd, hist) -> do
    Async.uninterruptibleCancel cmd
    Async.uninterruptibleCancel hist
  )
  (\(runServer, cmd, hist) -> do
    Async.link cmd
    Async.link hist
    runServer
  )
