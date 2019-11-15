{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Definitions common to frontend and backend.
--
--   And commonly used imports.

module Common.Foundation
  ( -- * Aeson encodings
    compactEncoding
    -- * Helpers that should really not be here
  , tshow
  , prettyTextCompact
  , prettyTextPretty
  , note
  , safeDecodeUtf8
    -- * Re-exports
  , module Data.Maybe
  , module Data.Semigroup
  , module Data.Foldable
  , module Control.Monad.IO.Class
  , module Control.Monad.Fix
  , module GHC.Generics
  ) where

import           Control.Monad.Except                  (MonadError, throwError)
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Aeson                            as A
import           Data.ByteString                       (ByteString)
import           Data.Foldable
import qualified Data.List.Split                       as L
import           Data.Semigroup
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as T
import qualified Data.Text.Encoding.Error              as T
import           GHC.Generics                          (Generic)

import           Data.Maybe                            hiding (mapMaybe)

import qualified Data.Text.Prettyprint.Doc             as Pretty (defaultLayoutOptions,
                                                                  layoutCompact,
                                                                  layoutPretty)
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Pact.Types.Pretty                     as Pretty


-- | Aeson encoding options for compact encoding.
--
--   We pass on the most compact sumEncoding as it could be unsound for certain types.
--
--   But we assume the following naming of constructor names (sum typs) and
--   field names (records): _TypeName_Blah and _typename_blah.
--
--   In particular we assume that only the string after the last underscore is
--   significant for distinguishing field names/constructor names. If this
--   assumption is not met this encoding might not result in the same decoding.
compactEncoding :: A.Options
compactEncoding = defaultOptions
    { fieldLabelModifier = shortener
    , allNullaryToStringTag = True
    , constructorTagModifier = shortener
    , omitNothingFields = True
    , sumEncoding = ObjectWithSingleField
    , unwrapUnaryRecords = True
    , tagSingleConstructors = False
    }
  where
    -- As long as names are not empty or just underscores this head should be fine:
    shortener = head . reverse . filter (/= "") . L.splitOn "_"

tshow :: Show a => a -> Text
tshow = T.pack . show

prettyTextCompact :: Pretty.Pretty a => a -> Text
prettyTextCompact = Pretty.renderStrict . Pretty.layoutCompact . Pretty.pretty

prettyTextPretty :: Pretty.Pretty a => a -> Text
prettyTextPretty = Pretty.renderStrict . Pretty.layoutPretty Pretty.defaultLayoutOptions . Pretty.pretty

note :: MonadError e m => e -> Maybe a -> m a
note e = maybe (throwError e) pure

safeDecodeUtf8 :: ByteString -> Text
safeDecodeUtf8 = T.decodeUtf8With T.lenientDecode
