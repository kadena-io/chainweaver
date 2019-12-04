{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Except (runExcept)

import Test.Tasty
import Test.Tasty.Golden

import Data.ByteString (ByteString)
import Data.String (IsString)

import Data.Aeson (ToJSON, FromJSON, Value)
import qualified Data.Aeson as Aeson

import Common.Network (NodeRef, ChainId (..), NetworkName, uncheckedNetworkName, parseNodeRef)
import Common.OAuth (OAuthProvider (..))

import Common.Wallet (PublicKey, AccountBalance (..), AccountName (..), UnfinishedCrossChainTransfer (..), parsePublicKey)

import Pact.Types.Command (RequestKey (..))
import qualified Pact.Types.Hash as Hash

goldenFilePath :: IsString a => a
goldenFilePath = "tests/golden-files"

goldenFileExt :: IsString a => a
goldenFileExt = "golden.json"

outputFileExt :: IsString a => a
outputFileExt = "json"

outputFilePath :: IsString a => a
outputFilePath = "tests/golden-outputs"

(</>) :: (Semigroup a, IsString a) => a -> a -> a
(</>) a b = a <> "/" <> b

(<.>) :: (Semigroup a, IsString a) => a -> a -> a
(<.>) a b = a <> "." <> b

data GTest = GTest
  { _gTest_name :: String
  , _gTest_golden :: FilePath
  , _gTest_input :: Value
  }

mkGTest :: ToJSON a => String -> FilePath -> a -> GTest
mkGTest lbl gfile a = GTest lbl gfile (Aeson.toJSON a)

toGoldenTest :: GTest -> TestTree
toGoldenTest (GTest name gold v) = goldenVsFile name
  (goldenFilePath </> gold <.> goldenFileExt)
  (outputFilePath </> gold <.> outputFileExt)
  (Aeson.encodeFile (outputFilePath </> gold <.> outputFileExt) v)

--- ABOVE: Will probably be moved to something I can using in the different packages

--- BELOW : Will live in common|backend|frontend as needed

goldenTests :: [GTest]
goldenTests =
  let
    chain = ChainId "0"

    (Right (node, pubkey)) = runExcept $ (,)
      <$> parseNodeRef "us1.testnet.chainweb.com"
      <*> parsePublicKey "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"

    rk = RequestKey $ Hash.pactHash "this is a test"

    unfinishedXChainTfr = UnfinishedCrossChainTransfer rk chain (AccountName "donatello") 1e-8
  in
    [ mkGTest "ChainId" "chainid" chain
    , mkGTest "NetworkName" "networkname" (uncheckedNetworkName "testnet")
    , mkGTest "NodeRef" "noderef" node
    , mkGTest "OAuthProvider" "oauth" OAuthProvider_GitHub
    , mkGTest "PublicKey" "pubkey" pubkey
    , mkGTest "AccountBalance_Zero" "accbalance_zero" (AccountBalance 0)
    , mkGTest "AccountBalance_NonZero" "accbalance_nonzero" (AccountBalance 13e-5)
    , mkGTest "UnfinishedCrossChainTransfer" "unfinxchaintfr" unfinishedXChainTfr

-- Needs 'key' instantiated, will defer tests to 'desktop' and 'frontend'
-- ToJSON (KeyPair key)
-- ToJSON (Account key)

-- AccountGuard -- Wrapper over pact types, tests needed?
    ]

main :: IO ()
main = defaultMain
  $ testGroup "Golden Tests" $ fmap toGoldenTest goldenTests

