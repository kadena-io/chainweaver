{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty

import Text.Printf (printf)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Aeson as Aeson

import Common.Network (ChainId (..), uncheckedNetworkName)

import Common.Wallet (UnfinishedCrossChainTransfer (..), Account (..), KeyPair (..), AccountBalance (..), AccountName (..))

import qualified Cardano.Crypto.Wallet as Crypto
import Pact.Types.Command (RequestKey (..))
import qualified Pact.Types.Hash as Hash

import Desktop.Frontend
import GoldenHelper

testBIPRoot :: LBS.ByteString
testBIPRoot = "\"020ce294062e88e49c9daa5b917384eba6175a50a229c115b5795b242b866d375b0cf1f9d477e3fdc5f277c47aa3f389ac4f13e6e58830c30e4113013eb7654de55fd721249c2c8e5188051756d9673e006d83d1e1344564efbdcbf3fcd64101188a32b7919161fb9c18650f483ee7efcd4bb684f31a5a3d9c8471f04efd0f0e\""

testBIPPass :: Text
testBIPPass = "1234567890" -- Awww yisss, security

goldenTests :: [GTest]
goldenTests =
  let
    (Right testXPrv) = Aeson.eitherDecode testBIPRoot
    (privKey, pubKey) = bipCryptoGenPair testXPrv testBIPPass 0

    keyPairJust = KeyPair pubKey (Just privKey)
    keyPairNothing = KeyPair pubKey (Nothing :: Maybe Crypto.XPrv)

    justAccBalance = Just $ AccountBalance 30

    chain = ChainId "0"

    rk = RequestKey $ Hash.pactHash "this is a test"
    justXchaintfr = Just $ UnfinishedCrossChainTransfer rk chain (AccountName "donatello") 1e-8

    accountW kp bal xchaintfr = Account
      { _account_name = AccountName "fred"
      , _account_chainId = chain
      , _account_key = kp
      , _account_network = uncheckedNetworkName "testnet"
      , _account_notes = mempty
      , _account_balance = bal
      , _account_unfinishedCrossChainTransfer = xchaintfr
      }

    mkAccTest (a, b, c) =
      let g = maybe 'N' (const 'Y')
          tag = printf "[PrivKey_%c|Balance_%c|XChainTfr_%c]"
            (g $ _keyPair_privateKey a)
            (g b)
            (g c)
      in
        mkGTest ("Account-" <> tag) ("account-" <> tag) $ accountW a b c

    -- Lazy persons smallcheck
    someAccountPermutations =
      [ (a,b,c)
      | a <- [keyPairJust, keyPairNothing],
        b <- [Nothing, justAccBalance],
        c <- [Nothing, justXchaintfr]
      ]
  in
      -- KeyPair
    [ mkGTest "KeyPair - Just" "keypair-just" keyPairJust
    , mkGTest "KeyPair - Nothing" "keypair-nothing" keyPairNothing
      -- Account
    ] <> fmap mkAccTest someAccountPermutations

main :: IO ()
main = defaultMain $ testGroup "Golden Tests - Desktop" $ fmap toGoldenTest goldenTests
