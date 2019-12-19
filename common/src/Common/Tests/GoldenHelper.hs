{-# LANGUAGE OverloadedStrings #-}
module Common.Tests.GoldenHelper where

import Test.Tasty
import Test.Tasty.Golden

import Data.String (IsString)

import Data.Aeson (ToJSON, Value)
import qualified Data.Aeson as Aeson

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
