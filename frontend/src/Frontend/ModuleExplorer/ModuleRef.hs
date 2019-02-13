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

-- | Refererencing `Module`s.
--
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.ModuleExplorer.ModuleRef
  ( -- * Re-exported Module and ModuleName
    module PactTerm
    -- * ModuleRefs
  , ModuleSource (..)
  , _ModuleSource_Deployed
  , _ModuleSource_File
  , ModuleRefV (..)
  , moduleRef_source
  , moduleRef_name
  , ModuleRef
  , DeployedModuleRef
  , FileModuleRef
    -- * Get more specialized references:
  , getDeployedModuleRef
  , getFileModuleRef
    -- * Get hold of a `Module`
  , fetchModule
    -- * Pretty printing
  , textModuleRefSource
  , textModuleRefName
  , textModuleName
  ) where

------------------------------------------------------------------------------
import           Control.Arrow                   (left, (***))
import qualified Text.Megaparsec      as MP
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except            (throwError)
import           Data.Coerce                     (coerce)
import qualified Data.Map                        as Map
import qualified Data.Set                        as Set
import           Data.Text
import qualified Data.Text                       as T
import           Reflex.Dom.Core                 (MonadHold)
------------------------------------------------------------------------------
import           Pact.Types.Exp                  (Literal (LString))
import           Pact.Types.Info                 (Code (..))
import           Pact.Types.Lang                 (ModuleName)
import           Pact.Types.Term                 as PactTerm (Module (..),
                                                              ModuleName (..),
                                                              Name,
                                                              NamespaceName (..),
                                                              Term (TList, TLiteral, TModule, TObject),
                                                              tStr)
import qualified Data.Aeson                        as A
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Foundation
import           Frontend.ModuleExplorer.Example
import           Frontend.ModuleExplorer.File
import           Frontend.Wallet

-- | A `Module` can come from a number of sources.
--
--   A `Module` either comes from some kind of a file or a backend (blockchain).
--
--   We don't treet backends as file, because they can't contain arbitrary code
--   and data as files. So while they appear to be quite similar conceptually
--   to files, they should in practice be treated differently:
--
--   - Backends can only contain modules and keysets, not arbitrary Pact code
--     and even less random arbitrary data.
--   - Backends will usually hold much more data than typical files, so we need
--     things like search/filter and maybe later on some proper pagination and
--     stuff.
--   - We combine several backends in a view and let the user filter by
--     backend, we don't do that with files.
data ModuleSource
  = ModuleSource_Deployed BackendRef -- ^ A module that already got deployed and loaded from there
  | ModuleSource_File FileRef
  deriving (Eq, Ord, Show, Generic)

makePactPrisms ''ModuleSource

instance A.ToJSON ModuleSource where
  toJSON = A.genericToJSON compactEncoding
  toEncoding = A.genericToEncoding compactEncoding

instance A.FromJSON ModuleSource where
  parseJSON = A.genericParseJSON compactEncoding

-- | A Module is uniquely idendified by its name and its origin.
data ModuleRefV s = ModuleRef
  { _moduleRef_source :: s -- ^ Where does the module come from.
  , _moduleRef_name   :: ModuleName   -- ^ Fully qualified name of the module.
  }
  deriving (Eq, Ord, Show, Generic)

makePactLensesNonClassy ''ModuleRefV

instance A.ToJSON s => A.ToJSON (ModuleRefV s) where
  toJSON = A.genericToJSON compactEncoding
  toEncoding = A.genericToEncoding compactEncoding

instance A.FromJSON s => A.FromJSON (ModuleRefV s) where
  parseJSON = A.genericParseJSON compactEncoding


-- | Most general `ModuleRef`.
--
--   Can refer to a deployed module or to a file module.
type ModuleRef = ModuleRefV ModuleSource

-- | `ModuleRefV` that refers to some deployed module.
type DeployedModuleRef = ModuleRefV BackendRef

-- | `ModuleRefV` that refers to a module coming from some file.
type FileModuleRef = ModuleRefV FileRef


-- | Parse and render a `ModuleRef`.
instance IsRefPath ModuleRef where
  renderRef r =
    renderRef (_moduleRef_source r) <> renderRef (_moduleRef_name r)

  parseRef = ModuleRef <$> parseRef <*> parseRef


instance IsRefPath ModuleSource where
  renderRef = \case
    ModuleSource_Deployed bs -> "deployed" : renderRef bs
    ModuleSource_File f -> renderRef f

  parseRef =
      fmap ModuleSource_File tryParseRef <|> fmap ModuleSource_Deployed parseDeployed
    where
      parseDeployed = do
        r <- MP.anySingle
        case unPathSegment r of
          "deployed" -> FileRef_Deployed <$> parseRef
          _         -> MP.unexpected $ r :| []


instance IsRefPath ModuleName where
  renderRef (ModuleName n mNamespace) = mkPathSegment $
    case mNamespace of
      Nothing -> n
      Just (Namespace ns) -> ns <> "." <> n

  parseRef = do
    fn <- unPathSegment <$> MP.anySingle
    pure $ case reverse $ T.splitOn "." fn of
      []   ->
        MP.failure
          (Just . MP.Tokens . $ "" :| [])
          (Set.singleton $ Tokens $ "module name" :| [])
      n:[] -> ModuleName n Nothing
      n:ns -> ModuleName n (Just . T.intercalate "." . reverse $ ns)


-- | Get a `DeployedModuleRef` if it is one.
getDeployedModuleRef :: ModuleRef -> Maybe DeployedModuleRef
getDeployedModuleRef = traverse (^? _ModuleSource_Deployed)

-- | Get a `FileModuleRef` if it is one.
getFileModuleRef :: ModuleRef -> Maybe FileModuleRef
getFileModuleRef = traverse (^? _ModuleSource_File)

-- | Get the module name of a `ModuleRefV` as `Text`.
--
--   Same as:
--
-- @
--   textModuleName . _moduleRef_name
-- @
textModuleRefName :: ModuleRefV a -> Text
textModuleRefName = textModuleName . _moduleRef_name

-- | Render a `ModuleName` as `Text`.
textModuleName :: ModuleName -> Text
textModuleName = \case
  ModuleName n Nothing  -> n
  ModuleName n (Just s) -> coerce s <> "." <> n

-- | Show the type of the selected module.
--
--   It is either "Example Module" or "Deployed Module"
--   The first parameter tells whether we are really dealing with a `Module` or with an `Interface`. It is `True` for modules, `False` otherwise.
textModuleRefSource :: Bool -> ModuleRef -> Text
textModuleRefSource isModule m =
    case _moduleRef_source m of
      ModuleSource_File (FileRef_Example n)
        -> printPretty "Example" (exampleName n)
      ModuleSource_File (FileRef_Stored n)
        -> printPretty "Stored" (textFileName n)
      ModuleSource_Deployed b
        -> printPretty "Deployed" (textBackendName . backendRefName $ b)
  where
    printPretty n d = mconcat [ n, " ", moduleText, " [ " , d , " ]" ]
    moduleText = if isModule then "Module" else "Interface"

-- Get hold of a deployed module:

-- | Fetch a `Module` from a backend where it has been deployed.
--
--   Resulting Event is either an error msg or the loaded module.
fetchModule
  :: forall m t model
  . ( MonadHold t m, PerformEvent t m, MonadJSM (Performable m)
    , TriggerEvent t m
    , MonadSample t (Performable m)
    , HasBackend model t
    )
  => model
  -> Event t DeployedModuleRef
  -> m (Event t (DeployedModuleRef, Either Text Module))
fetchModule backendL onReq = do
    deployedResult :: Event t (DeployedModuleRef, BackendErrorResult)
      <- performBackendRequestCustom emptyWallet (backendL ^. backend) mkReq onReq

    pure $ ffor deployedResult $
      id *** (getModule <=< left (T.pack . show))

  where
    mkReq mRef = BackendRequest
      { _backendRequest_code = mconcat
        [ defineNamespace . _moduleRef_name $ mRef
        , "(describe-module '"
        , _mnName . _moduleRef_name $ mRef
        , ")"
        ]
      , _backendRequest_data = mempty
      , _backendRequest_backend = _moduleRef_source mRef
      , _backendRequest_signing = Set.empty
      }

    getModule :: Term Name -> Either Text Module
    getModule  = \case
      TObject terms _ _ -> do
        mods <- fmap fileModules $ getCode terms
        case Map.elems mods of
          []   -> throwError "No module in response"
          m:[] -> pure m
          _    -> throwError "More than one module in response?"
      _ -> throwError "Server response did not contain a  TObject module description."

    getCode :: [(Term Name, Term Name)] -> Either Text Code
    getCode props =
        case lookup (tStr "code") props of
          Nothing -> throwError "No code property in module description object!"
          Just (TLiteral (LString c) _) -> pure $ Code c
          _ -> throwError "No code found, but something else!"

    defineNamespace =
      maybe "" (\n -> "(namespace '" <> coerce n <> ")") . _mnNamespace


-- Instances:

instance Functor ModuleRefV where
  fmap f (ModuleRef s n) = ModuleRef (f s) n

instance Foldable ModuleRefV where
  foldMap f (ModuleRef s _) = f s

instance Traversable ModuleRefV where
  traverse f (ModuleRef s n)  = flip ModuleRef n <$> f s

instance Semigroup (ModuleRefV a) where
  a <> _ = a
