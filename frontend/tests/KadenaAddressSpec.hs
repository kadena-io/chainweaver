{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module KadenaAddressSpec where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Monad (when)
import Control.Lens ((^.))
import Control.Error (hush,isRight,isLeft)
import Data.Bifunctor (first)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Char as C
import qualified Data.ByteString.Char8 as BS8
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Kadena.SigningApi (mkAccountName,unAccountName)

import Pact.Types.ChainId (chainId)
import Common.Network (ChainId (..))
import qualified Frontend.KadenaAddress as KA

printableLatin1 = Gen.filterT isPrintable Gen.latin1
  where
    isPrintable char = not
      ( C.isSpace char ||
        C.isControl char ||
        C.ord char == fromIntegral KA.humanReadableDelimiter
      )

genKadenaAddress :: Gen KA.KadenaAddress
genKadenaAddress = KA.mkKadenaAddress <$> genCreated <*> genChainId <*> genAccountName
  where
    genCreated = Gen.element [KA.AccountCreated_No, KA.AccountCreated_Yes]
    genAccountName = Gen.just $ hush . mkAccountName <$> Gen.text (Range.linear 3 256) printableLatin1
    genChainId = ChainId . T.singleton <$> Gen.digit

prop_parse_kadenaAddress_roundtrip_Created_Yes :: Property
prop_parse_kadenaAddress_roundtrip_Created_Yes = property $ do
  ka <- forAll genKadenaAddress
  tripping ka KA.encodeKadenaAddress KA.decodeKadenaAddress

prop_parse_kadenaAddress_encoding :: Property
prop_parse_kadenaAddress_encoding = property $ do
  ka <- forAll genKadenaAddress
  let
    isCreated = KA._kadenaAddress_accountCreated ka == KA.AccountCreated_Yes
    encoded = KA.encodeKadenaAddress ka
    [name, chain, chksumOrEncoding] = BS8.split (C.chr (fromIntegral KA.humanReadableDelimiter)) encoded

  TE.decodeLatin1 name === unAccountName (KA._kadenaAddress_accountName ka)
  TE.decodeLatin1 chain === (KA._kadenaAddress_chainId ka ^. chainId)

  if isCreated
    then KA.decodeKadenaAddress chksumOrEncoding === Right ka
    else chksumOrEncoding === KA.bytestringChecksum (KA._kadenaAddress_checksum ka)

tests :: TestTree
tests = testGroup "Kadena Address Spec"
  [ testProperty "Encoding" prop_parse_kadenaAddress_encoding
  , testProperty "Round Trip" prop_parse_kadenaAddress_roundtrip_Created_Yes
  ]
