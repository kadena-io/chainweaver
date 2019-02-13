{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE ExtendedDefaultRules   #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

-- | References that can be rendered as some kind of path (string
-- representation) and parsed again.
--
-- Copyright   :  (C) 2019 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.ModuleExplorer.RefPath
  ( -- * Types and classes
    -- ** Paths
    RefPath
  , PathSegment
    -- ** Reference parsing
  , IsRefPath (..)
  , tryParseRef
  , RefParser
    -- * Handle `RefPath`s
  , parsePath
  , renderPath
    -- * Handle `PathSegment`s
  , mkPathSegment
  , unPathSegment
  ) where

------------------------------------------------------------------------------
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Void            (Void)
import           Text.Megaparsec      as MP
------------------------------------------------------------------------------

type RefPath = [ PathSegment ]

-- | A `PathSegment` is some `Text` with all occurrences of "\\" being escaped with "\\".
newtype PathSegment = PathSegment {unsafeUnPathSegment :: Text}
  deriving (Eq, Ord, Show)

-- | Parser parsing a path.
type RefParser = Parsec Void RefPath


-- | References that can be rendered to and parsed from a `RefPath` should have an instance of this class.
class IsRefPath r where
  -- | Render a reference to a `RefPath`.
  renderRef :: r -> RefPath

  -- | Parse a reference from a `RefPath`.
  parseRef :: RefParser r


-- | Try to pase a ref.
--
--   Same as `try parseRef`
tryParseRef :: RefParser r
tryParseRef = try parseRef

-- | RefPath separator.
--   We use \ as it won't get percent encoded in url encoding.
pathSepChar :: Char
pathSepChar = '\\'

-- | RefPath separator as Text.
pathSep :: Text
pathSep = T.singleton pathSepChar

mkPathSegment :: Text -> PathSegment
mkPathSegment = PathSegment . T.replace pathSep (pathSep <> pathSep)

-- | Get back the original text unescaped.
unPathSegment :: PathSegment -> Text
unPathSegment (PathSegment x) =
  T.replace (pathSep <> pathSep) pathSep x

-- | Split a given `Text` by occurrences of `pathSep`.
parsePath :: Text -> RefPath
parsePath = map (PathSegment . T.pack . reverse) . splitIt "" . T.unpack
  where
    splitIt :: String -> String -> [String]
    splitIt building (x:xsA@(y:xs))
      -- Not escaped path separation character - do split:
      | x == pathSepChar && y /= pathSepChar = building : splitIt "" xsA
      | x == pathSepChar && y == pathSepChar = splitIt (y:x:building) xs
      | otherwise                            = splitIt (x:building) xsA
    splitIt building (x:[])
      | x == pathSepChar = building : []
      | otherwise = (x:building) : []
    splitIt building []   = building : []

-- | Render a given `RefPath` as Text.
renderPath :: RefPath -> Text
renderPath = T.intercalate pathSep . map unsafeUnPathSegment


instance IsString PathSegment where
  fromString = mkPathSegment . T.pack
