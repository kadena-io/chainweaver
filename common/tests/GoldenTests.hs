{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Except (runExceptT)

import Test.Tasty
import Test.Tasty.Golden

import Data.ByteString (ByteString)
import Data.String (IsString)

import Data.Aeson (ToJSON, FromJSON, Value)
import qualified Data.Aeson as Aeson

import Common.Network (NodeRef, ChainId (..), NetworkName, uncheckedNetworkName, parseNodeRef)

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

goldenTests :: NodeRef -> [GTest]
goldenTests noderef =
  [ mkGTest "ChainId" "chainid" (ChainId "0")
  , mkGTest "NetworkName" "networkname" (uncheckedNetworkName "testnet")
  , mkGTest "NodeRef" "noderef" noderef
  ]

main :: IO ()
main = do
  (Right node) <- runExceptT $ parseNodeRef "us1.testnet.chainweb.com"
  defaultMain
    $ testGroup "Golden Tests" $ fmap toGoldenTest (goldenTests node)
