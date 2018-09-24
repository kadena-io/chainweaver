module Devel where

import           Control.Concurrent.Async (async, wait)
import           Control.Monad
import           Obelisk.Run
import qualified Pact.Server.Server       as Pact


import           Backend
import           Frontend


main :: Int -> IO ()
main port = do
  s <- async $ Pact.serve "config/common/pact.yaml"
  run port backend frontend
  wait s

