module Devel where

import           Control.Exception
import qualified Control.Concurrent.Async as Async
import           Control.Monad
import           Obelisk.Run
import qualified Pact.Server.Server       as Pact


import           Backend
import           Frontend


main :: Int -> IO ()
main port = do
  Async.withAsync (pactSafeServe "config/common/pact.yaml") $ \s -> do
    run port backend frontend
    Async.wait s

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
