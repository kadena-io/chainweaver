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
-- Copyright   :  (C) 2019 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.ModuleExplorer.File
  ( -- * Filenames and References
    FileName
  , textFileName
  , FileRef (..)
  , _FileRef_Example
  , _FileRef_Stored
    -- ** Retrieving information about `FileRef`:
  , fileRefName
  , textFileType
   -- * A PactFile
  , PactFile
  -- * Storing and Retrieval
  , fetchFile
  , fetchFileCached
  -- * Retrieve contents:
  , fileModules
  ) where

------------------------------------------------------------------------------
import           Control.Arrow                   ((***))
import           Control.Lens
import           Control.Monad
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Text                       (Text)
import           Reflex
import           Reflex.Dom.Core                 (HasJSContext, MonadHold)
------------------------------------------------------------------------------
import qualified Pact.Compile                    as Pact
import qualified Pact.Parse                      as Pact
import           Pact.Types.Lang                 (Code (..), Module, ModuleName,
                                                  Name, Term (TModule))
------------------------------------------------------------------------------
import           Frontend.Foundation
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

-- | Text representation of a `FileRef` suitable for showing to the user.
fileRefName :: FileRef -> Text
fileRefName = \case
  FileRef_Example e -> exampleName e
  FileRef_Stored s  -> textFileName s

-- | Show the file type, suitable for getting displayed to the user.
textFileType :: FileRef -> Text
textFileType = \case
  FileRef_Example _ -> "Example File"
  FileRef_Stored _ -> "User File"


-- | Fetch a `File` given a `FileRef`.
--
--   TODO: Implement support for `FileRef_Stored`.
fetchFile
  :: ( PerformEvent t m, TriggerEvent t m, MonadJSM (Performable m)
     , HasJSContext (Performable m), HasJSContext JSM
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
     , HasJSContext JSM
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
fileModules (Code c) = case Pact.compileExps (Pact.mkTextInfo c) <$> Pact.parseExprs c of
  Right (Right terms) -> Map.fromList $ mapMaybe getModule terms
  _                   -> Map.empty


-- | Get module from a `Term`
getModule :: MonadPlus m => Term Name -> m (ModuleName, Module)
getModule = \case
  TModule m _ _ -> pure (m ^. nameOfModule, m)
  _             -> mzero


-- Instances:
instance Semigroup FileRef where
  a <> _ = a
