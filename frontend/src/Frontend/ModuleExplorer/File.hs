{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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
  {- , fetchFileCached -}
  -- * Retrieve contents:
  , fileModules
  ) where

------------------------------------------------------------------------------
import           Control.Arrow                   ((***), (&&&))
import           Control.Lens
import           Control.Monad
import qualified Data.Aeson                      as A
import           Data.List.NonEmpty              (NonEmpty ((:|)))
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Text                       (Text)
import           Reflex
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Reflex.Dom.Core                 (HasJSContext)
import qualified Text.Megaparsec      as MP
import Network.GitHub.Types.Gist.Core as G
------------------------------------------------------------------------------
import qualified Pact.Compile                    as Pact
import qualified Pact.Parse                      as Pact
import           Pact.Types.Lang                 (Code (..), ModuleName,
                                                  Name, Term (TModule))
------------------------------------------------------------------------------
import           Frontend.Foundation
import           Frontend.ModuleExplorer.Example
import           Frontend.ModuleExplorer.Module
import           Frontend.GistStore
import           Common.RefPath as MP

-- | The name of a file stored by the user.
newtype FileName = FileName { unFileName :: Text }
  deriving (Show, Eq, Ord, Generic)

instance A.ToJSON FileName
instance A.FromJSON FileName

-- | Get textual representation of a `FileName`
textFileName :: FileName -> Text
textFileName = unFileName

-- | A `FileRef` either points to a stored file or an `ExampleRef`.
data FileRef
  = FileRef_Example ExampleRef
  | FileRef_Stored FileName
  | FileRef_Gist GistRef
  deriving (Eq, Ord, Show, Generic)

makePactPrisms ''FileRef

instance A.ToJSON FileRef where
  toJSON = A.genericToJSON compactEncoding
  toEncoding = A.genericToEncoding compactEncoding

instance A.FromJSON FileRef where
  parseJSON = A.genericParseJSON compactEncoding


instance IsRefPath FileRef where
  renderRef = \case
    FileRef_Example ex ->
      "example" <> renderRef ex
    FileRef_Stored  (FileName n) ->
      "stored" <> mkRefPath n -- Not really implemented yet.
    FileRef_Gist r ->
      "gist" <> renderRef r

  parseRef = do
    r <- MP.anySingle
    case r of
      "example" -> FileRef_Example <$> parseRef
      "stored"  -> FileRef_Stored  <$> parseRef
      "gist"    -> FileRef_Gist <$> parseRef
      _         -> MP.unexpected $ MP.Tokens $ r :| []


instance IsRefPath FileName where
  renderRef = mkRefPath . unFileName

  parseRef = FileName <$> MP.anySingle


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
  FileRef_Gist _ -> "Some Gist"

-- | Show the file type, suitable for getting displayed to the user.
textFileType :: FileRef -> Text
textFileType = \case
  FileRef_Example _ -> "Example File"
  FileRef_Stored _ -> "User File"
  FileRef_Gist _ -> "GitHub Gist"


-- | Fetch a `File` given a `FileRef`.
--
--   TODO: Implement support for `FileRef_Stored`.
--   TODO: Handle error case (file could not be loaded) properly.
fetchFile
  :: ( PerformEvent t m, TriggerEvent t m, MonadJSM (Performable m)
     , HasJSContext JSM, MonadHold t m
     , HasGistStore model t, HasGistStoreCfg mConf t, Monoid mConf
     )
  => model -> Event t FileRef -> m (mConf, Event t (FileRef, PactFile))
fetchFile m onFileRef = do
    onExample <- fetchExample $ fmapMaybe (^? _FileRef_Example) onFileRef
    let
      onGist = m ^. gistStore_loaded
      onGistReq = fmapMaybe (^? _FileRef_Gist) onFileRef
    -- TODO: We should use a proper `Requester` workflow or similar, this is a
    -- hack to get this release out:
    requested <- hold False $ leftmost
      [ False <$ onGist
      , True <$ onGistReq
      ]
    pure
      ( mempty & gistStoreCfg_load .~ onGistReq
      , leftmost
          [ wrapExample <$> onExample
          , fmap (wrapGist . snd) . ffilter fst $ attach requested onGist
          ]
      )
  where
    wrapExample = FileRef_Example *** Code . fst
    wrapGist = FileRef_Gist . gistId &&& Code . getGistFile

    getGistFile = fromMaybe "Error loading gist: Gist was not a valid chainweaver gist." .
      fmap fileContent . listToMaybe . HM.elems . HM.filterWithKey (\k _ -> T.isSuffixOf ".pact" k) . gistFiles

-- | Fetch a `File`, with cache.
--
--   This function maintains a cache of size 1, that means if the fetched file
--   was fetched last it will be delivered immediately.
{- fetchFileCached -}
{-   :: ( PerformEvent t m, TriggerEvent t m, MonadJSM (Performable m) -}
{-      , MonadFix m, MonadHold t m -}
{-      , HasJSContext JSM -}
{-      , HasGistStore model t, HasGistStoreCfg mConf t, Monoid mConf -}
{-      ) -}
{-   => model -> Event t FileRef -> m (mConf, Event t (FileRef, PactFile)) -}
{- fetchFileCached onFileRef = mdo -}
{-     let onNewFileRef = pushAlways (onlyNew lastFile) onFileRef -}
{-     (fCfg, onFetched) <- fetchFile $ fmapMaybe id onNewFileRef -}
{-     lastFile <- hold Nothing $ Just <$> onFetched -}

{-     let onCached = fmapMaybe id . tag lastFile $ ffilter isNothing onNewFileRef -}
{-     pure $ leftmost [ onFetched, onCached ] -}
{-   where -}
{-     onlyNew cache req = do -}
{-       l <- sample cache -}
{-       if fmap fst l == Just req -}
{-          then pure Nothing -}
{-          else pure $ Just req -}

-- | Get the `Module`s contained in `PactFile`.
fileModules :: PactFile -> Map ModuleName (ModuleDef (Term Name))
fileModules (Code c) = case Pact.compileExps (Pact.mkTextInfo c) <$> Pact.parseExprs c of
  Right (Right terms) -> Map.fromList $ mapMaybe getModule terms
  _                   -> Map.empty


-- | Get module from a `Term`
getModule :: MonadPlus m => Term Name -> m (ModuleName, ModuleDef (Term Name))
getModule = \case
  TModule m _ _ -> pure (m ^. nameOfModule, m)
  _             -> mzero


-- Instances:
instance Semigroup FileRef where
  a <> _ = a
