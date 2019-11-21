{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Error (hush)
import qualified Data.Text as T
import qualified Data.Char as C
import Kadena.SigningApi (mkAccountName)

import Common.Network (ChainId (..))
import qualified Frontend.KadenaAddress as KA

prop_parse_kadenaAddress_roundtrip :: Property
prop_parse_kadenaAddress_roundtrip = property $ do
  ka <- forAll $ KA.mkKadenaAddress KA.KadenaAddressPrefix_Keep KA.KadenaAddress_AccountCreated_No <$> genChainId <*> genAccountName

  tripping ka KA.encodeKadenaAddress KA.decodeKadenaAddress
  where
    genAccountName = Gen.just $ hush . mkAccountName <$> Gen.text (Range.linear 3 256) printableLatin1
    genChainId = ChainId . T.singleton <$> Gen.digit

    printableLatin1 = Gen.filterT isPrintable Gen.latin1

    isPrintable char =
      not (C.isSpace char || C.isControl char || C.ord char == fromIntegral KA.humanReadableDelimiter)

main :: IO Bool
main = checkParallel $$(discover)
