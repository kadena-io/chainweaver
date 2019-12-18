{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty

import Text.Printf (printf)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Map as Map
import qualified Data.Aeson as Aeson

import Common.Network (ChainId (..), uncheckedNetworkName)
import Common.Wallet (UnfinishedCrossChainTransfer (..), Account (..), KeyPair (..),
                      AccountBalance (..), AccountName (..), parsePublicKey)

import Frontend.JsonData (Keyset, KeysetV (..))
import Frontend.Wallet (SomeAccount (..), mkAccountNotes)

import qualified Cardano.Crypto.Wallet as Crypto
import Pact.Types.Command (RequestKey (..))
import qualified Pact.Types.Hash as Hash

import Desktop.Frontend
import Common.Tests.GoldenHelper

testBIPRoot :: LBS.ByteString
testBIPRoot = "\"020ce294062e88e49c9daa5b917384eba6175a50a229c115b5795b242b866d375b0cf1f9d477e3fdc5f277c47aa3f389ac4f13e6e58830c30e4113013eb7654de55fd721249c2c8e5188051756d9673e006d83d1e1344564efbdcbf3fcd64101188a32b7919161fb9c18650f483ee7efcd4bb684f31a5a3d9c8471f04efd0f0e\""

testBIPPass :: Text
testBIPPass = "1234567890"

goldenTests :: [GTest]
goldenTests =
  let
    -- TODO: Find a better way to do this
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
      , _account_notes = mkAccountNotes mempty
      , _account_balance = bal
      , _account_unfinishedCrossChainTransfer = xchaintfr
      }

    mkTag a b c =
      let tagChar = maybe 'N' (const 'Y')
      in printf "%c-%c-%c" (tagChar $ _keyPair_privateKey a) (tagChar b) (tagChar c)

    mkAccTest a b c =
      let tag = mkTag a b c
      in mkGTest ("Account-" <> tag) ("account-" <> tag) $ accountW a b c

    -- Lazy persons smallcheck
    accountPermutations
      :: (KeyPair Crypto.XPrv -> Maybe AccountBalance -> Maybe UnfinishedCrossChainTransfer -> b)
      -> [b]
    accountPermutations f =
      [ f a b c
      | a <- [keyPairJust, keyPairNothing],
        b <- [Nothing, justAccBalance],
        c <- [Nothing, justXchaintfr]
      ]

    someAccountPermutations =
      [ mkGTest ("SomeAccount-" <> t) ("someaccount-" <> t) s
      | (t, s) <-
          ("deleted", SomeAccount_Deleted)
          : accountPermutations (\k b x -> (mkTag k b x, SomeAccount_Account $ accountW k b x))
      ]

    -- Example taken from Kadena Pact Reference
    -- https://pact-language.readthedocs.io/en/latest/pact-reference.html#keysets-and-authorization
    --
    -- {
    --   "fully-specified-with-native-pred":
    --   { "keys": ["abc6bab9b88e08d","fe04ddd404feac2"], "pred": "keys-2" },
    --   "fully-specified-with-qual-custom":
    --   { "keys": ["abc6bab9b88e08d","fe04ddd404feac2"], "pred": "my-module.custom-pred" },
    --   "keysonly":
    --   { "keys": ["abc6bab9b88e08d","fe04ddd404feac2"] },
    --   "keylist": ["abc6bab9b88e08d","fe04ddd404feac2"]
    -- }

    (Right pubkey1) = parsePublicKey "b7a3c12dc0c8c748ab07525b701122b88bd78f600c76342d27f25e5f92444cde"
    keysetkeys = Map.fromList [("alice", pubkey1) , ("fred", pubkey1)]

    fullySpecifiedWithNativePred,fullySpecifiedWithQualifiedCustomPred,keyList :: Keyset

    fullySpecifiedWithNativePred =
      Keyset keysetkeys $ Just "keys-2"

    fullySpecifiedWithQualifiedCustomPred =
      Keyset keysetkeys $ Just "my-module.custom-pred"

    keyList =
      Keyset keysetkeys Nothing

  in
    [ mkGTest "KeyPair - Just" "keypair-just" keyPairJust
    , mkGTest "KeyPair - Nothing" "keypair-nothing" keyPairNothing
    , mkGTest "Keyset - Native Predicate" "fullySpecifiedWithNativePred" fullySpecifiedWithNativePred
    , mkGTest "Keyset - Qualified Custom Predicate" "fullySpecifiedWithQualifiedCustomPred" fullySpecifiedWithQualifiedCustomPred
    , mkGTest "Keyset - Key list" "keylist" keyList
    ]
    <> accountPermutations mkAccTest
    <> someAccountPermutations

main :: IO ()
main = defaultMain
  $ testGroup "Golden Tests - Desktop - [PrivateKey-Balance-XChainTfr]"
  $ fmap toGoldenTest goldenTests
