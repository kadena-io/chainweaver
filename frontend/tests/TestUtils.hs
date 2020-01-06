{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TestUtils where

import Data.Aeson(ToJSON,FromJSON,Value, encode, eitherDecode)
import Data.Algorithm.DiffContext (prettyContextDiff, getContextDiff)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit (assertBool, testCase)
import qualified Text.PrettyPrint as Doc
import Text.Show.Pretty (ppShow)

-- The person that wrote this really should put it in a library...
-- https://github.com/feuerbach/tasty/issues/226#issuecomment-508868067
(@?~)
  :: (Eq a, Show a)
  => a
  -> a
  -> IO ()
got @?~ expected = assertBool (show diff) (got == expected)
  where
    gotPurdy = lines $ ppShow got
    expectedPurdy = lines $ ppShow expected
    diff = prettyContextDiff "Got" "Expected" Doc.text (getContextDiff 3 gotPurdy expectedPurdy)
