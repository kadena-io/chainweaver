{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Except (runExcept)

import Test.Tasty

import Common.Network (ChainId (..), mkNetworkName, parseNodeRef)
import Common.OAuth (OAuthProvider (..))

import Common.Wallet (AccountBalance (..), AccountName (..), UnfinishedCrossChainTransfer (..), parsePublicKey)

import Pact.Types.Command (RequestKey (..))
import qualified Pact.Types.Hash as Hash

import Common.Tests.GoldenHelper

goldenTests :: [GTest]
goldenTests =
  let
    chain = ChainId "0"

    (Right (node, pubkey)) = runExcept $ (,)
      <$> parseNodeRef "api.testnet.chainweb.com"
      <*> parsePublicKey "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"

    rk = RequestKey $ Hash.pactHash "this is a test"

    unfinishedXChainTfr = UnfinishedCrossChainTransfer rk chain (AccountName "donatello") 1e-8
  in
    [ mkGTest "ChainId" "chainid" chain
    , mkGTest "NetworkName" "networkname" (mkNetworkName "testnet")
    , mkGTest "NodeRef" "noderef" node
    , mkGTest "OAuthProvider" "oauth" OAuthProvider_GitHub
    , mkGTest "PublicKey" "pubkey" pubkey
    , mkGTest "AccountBalance_Zero" "accbalance_zero" (AccountBalance 0)
    , mkGTest "AccountBalance_NonZero" "accbalance_nonzero" (AccountBalance 13e-5)
    , mkGTest "UnfinishedCrossChainTransfer" "unfinxchaintfr" unfinishedXChainTfr
    ]

main :: IO ()
main = defaultMain
  $ testGroup "Golden Tests - Common" $ fmap toGoldenTest goldenTests
