{-# LANGUAGE OverloadedStrings #-}
module Frontend.Network.NodeInfoSpec where

import qualified Pact.Types.ChainId as Pact
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Frontend.Network.NodeInfo (sortChainIds)

tests :: TestTree
tests = testGroup "NodeInfoSpec"
  [ testCase "Sort ChainIds" $
    sortChainIds (Pact.ChainId <$> ["8","9","werd","4","5","10","11","24","6","2 words","7","0","1","2","3"]) @?=
      (Pact.ChainId <$> ["0","1","2","3","4","5","6","7","8","9","10","11","24","2 words","werd"])
  ]

