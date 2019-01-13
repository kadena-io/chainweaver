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

-- | File abstraction in `ModuleExplorer`.
--
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.ModuleExplorer.File
  ( -- * Filenames and References
    FileName
  , textFileName
  , FileRef (..)
  , _FileRef_Example
  , _FileRef_Stored
   -- * A PactFile
  , PactFile
  -- * Storing and Retrieval
  , fetchFile
  , fetchFileCached
  -- * Retrieve contents:
  , fileModules
  ) where

------------------------------------------------------------------------------
import Control.Lens
import Control.Arrow ((***))
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Text                (Text)
import           Generics.Deriving.Monoid (mappenddefault, memptydefault)
import           GHC.Generics             (Generic)
import           Reflex
import           Data.Set                     (Set)
import Control.Monad
import           Reflex.Dom.Core             (HasJSContext, MonadHold,
                                              PostBuild, XhrResponse (..),
                                              newXMLHttpRequest, xhrRequest)
------------------------------------------------------------------------------
import           Obelisk.Generated.Static
import           Pact.Types.Lang          (DefType, FunType, ModuleName, Name,
                                         Term (TModule), Code (..), Module)
import qualified Pact.Compile                as Pact
import qualified Pact.Parse                  as Pact
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Foundation
import           Frontend.Wallet
import           Frontend.ModuleExplorer.Example
import           Frontend.ModuleExplorer.Module

-- | The name of a file stored by the user.
newtype FileName = FileName { unFileName :: Text }
  deriving (Show, Eq, Ord)

-- | Get textual representation of a `FileName`
textFileName :: FileName -> Text
textFileName = unFileName

-- | A `FileRef` either points to a stored file or an `ExampleRef`.
data FileRef
  = FileRef_Example ExampleRef
  | FileRef_Stored FileName
  deriving (Eq, Ord, Show)

makePactPrisms ''FileRef

{- -- | A selected file. -}
{- data PactFile = PactFile -}
{-   { _pactFile_content :: Code -- * Full content of the file, no matter what. -}
{-   , _pactFile_modules :: [Module] -- * Any modules contained in the file. (Parsed content) -}
{-   } deriving (Show) -}

{- makePactLenses ''PactFile -}

-- | A Pact file is simply some piece of `Code`.
type PactFile = Code


-- | Fetch a `File` given a `FileRef`.
--
--   TODO: Implement support for `FileRef_Stored`.
fetchFile
  :: ( PerformEvent t m, TriggerEvent t m, MonadJSM (Performable m)
     , HasJSContext (Performable m)
     )
  => Event t FileRef -> m (Event t (FileRef, PactFile))
fetchFile onFileRef = do
    onExample <- fetchExample $ fmapMaybe (^? _FileRef_Example)  onFileRef
    pure $ wrapExample <$> onExample
  where
    wrapExample = FileRef_Example *** Code . fst 

-- | Fetch a `File`, with cache.
--
--   This function maintains a cache of size 1, that means if the fetched file
--   was fetched last it will be delivered immediately.
fetchFileCached
  :: ( PerformEvent t m, TriggerEvent t m, MonadJSM (Performable m)
     , HasJSContext (Performable m), MonadFix m, MonadHold t m
     )
  => Event t FileRef -> m (Event t (FileRef, PactFile))
fetchFileCached onFileRef = mdo
    let onNewFileRef = pushAlways (onlyNew lastFile) onFileRef
    onFetched <- fetchFile $ fmapMaybe id onNewFileRef
    lastFile <- hold Nothing $ Just <$> onFetched

    let onCached = fmapMaybe id . tag lastFile $ ffilter isNothing onNewFileRef
    pure $ leftmost [ onFetched, onCached ]
  where
    onlyNew cache req = do
      l <- sample cache
      if fmap fst l == Just req
         then pure Nothing
         else pure $ Just req

-- | Get the `Module`s contained in `PactFile`.
fileModules :: PactFile -> Map ModuleName Module
fileModules code = case Pact.compileExps Pact.mkEmptyInfo <$> Pact.parseExprs (_unCode code) of
  Right (Right terms) -> Map.fromList $ mapMaybe getModule terms
  _                   -> Map.empty


-- | Get module from a `Term`
getModule :: MonadPlus m => Term Name -> m (ModuleName, Module)
getModule = \case
  TModule m _ _ -> pure (nameOfModule m, m)
  {- TModule m _ _ -> pure $ (m, getFunctions $ Bound.instantiate undefined body) -}
  _             -> mzero


-- Instances:
instance Semigroup FileRef where
  a <> _ = a
