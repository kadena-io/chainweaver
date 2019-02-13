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

-- | References for things that can be loaded in some way to the `Editor`.
--
--   In short, what can be loaded to the editor are files and single modules.
--
-- Copyright   :  (C) 2019 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.ModuleExplorer.LoadedRef
  ( -- * Filenames and References
    LoadedRef (..)
  , _LoadedRef_File
  , _LoadedRef_Module
  , loadedRefToText
  , loadedRefFromText
  ) where

------------------------------------------------------------------------------
import qualified Data.Aeson                        as A
import qualified Data.ByteString.Lazy              as BSL
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
------------------------------------------------------------------------------
import           Frontend.Backend                  (HasBackend)
import           Frontend.Foundation
import           Frontend.ModuleExplorer.File
import           Frontend.ModuleExplorer.ModuleRef
import           Frontend.ModuleExplorer.Example
import           Frontend.ModuleExplorer.RefPath


-- | Reference something that can be loaded to the `Editor`.
data LoadedRef
  = LoadedRef_File FileRef -- ^ A file
  | LoadedRef_Module ModuleRef -- ^ or a module.
  deriving (Generic, Show, Eq, Ord)

makePactPrisms ''LoadedRef

instance A.ToJSON LoadedRef where
  toJSON = A.genericToJSON compactEncoding
  toEncoding = A.genericToEncoding compactEncoding

instance A.FromJSON LoadedRef where
  parseJSON = A.genericParseJSON compactEncoding


instance IsRefPath LoadedRef where
  renderRef = \case
    LoadedRef_File f -> renderRef f
    LoadedRef_Module m -> renderRef m

  parseRef = fmap LoadedRef_Module tryParseRef <|> fmap LoadedRef_File tryParseRef
