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

-- | Module, ModuleRef related functions and types.
--
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.ModuleExplorer.Module
  ( -- * Re-exported Module and ModuleName
    module PactTerm
  , -- * ModuleRefs
  , ModuleRefV (..)
  , HasModuleRefV (..)
  , ModuleRef
  , DeployedModuleRef
  , FileModuleRef
    -- * Get more specialized references:
  , getDeployedModuleRef
  , getFileModuleRef
    -- * Utitlity functions when working with `Module`
  , nameOfModule
  , codeOfModule
    -- * Get hold of modules and functions:
  , PactFunction (..)
  , HasPactFunction (..)
  , fetchModule
  , getModule
  , getFunctions
    -- * Pretty printing
  , textModuleRefSource
  ) where

------------------------------------------------------------------------------
------------------------------------------------------------------------------
import           Obelisk.Generated.Static
import           Pact.Types.Lang          (DefType, FunType, ModuleName, Name,
                                         Term)
import           Pact.Types.Term           (Module (..), ModuleName (..)) as PactTerm
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Foundation
import           Frontend.Wallet
import           Frontend.ModuleExplorer.Example

-- | A module can come from a number of sources.
--
--   A module either comes from some kind of a file or a backend (blockchain).
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

makePactPrisms ''ModuleSource


-- | A Module is uniquely idendified by its name and its origin.
data ModuleRefV s = ModuleRef
  { _moduleRef_source :: s -- ^ Where does the module come from.
  , _moduleRef_name   :: ModuleName   -- ^ Fully qualified name of the module.
  }

makePactLenes ''ModuleRef

-- | Most general `ModuleRef`.
--
--   Can refer to a deployed module or to a file module.
type ModuleRef = ModuleRefV ModuleSource

-- | `ModuleRefV` that refers to some deployed module.
type DeployedModuleRef = ModuleRefV BackendRef

-- | `ModuleRefV` that refers to a module coming from some file.
type FileModuleRef = ModuleRefV FileRef


-- | Get a `DeployedModuleRef` if it is one.
getDeployedModuleRef :: ModuleRef -> Maybe DeployedModuleRef
getDeployedModuleRef r = traverse (^? _ModuleSource_Deployed)

-- | Get a `FileModuleRef` if it is one.
getFileModuleRef :: ModuleRef -> Maybe FileModuleRef
getFileModuleRef r = traverse (^? _ModuleSource_File)


-- | Show the type of the selected module.
--
--   It is either "Example Contract" or "Deployed Contract"
textModuleRefSource :: ModuleRef -> Text
textModuleRefSource m =
    case _moduleRef_source m of
      ModuleSource_File (FileRef_Example n)
        -> withDetails "Example Module" (exampleName n)
      ModuleSource_File (FileRef_Stored n)
        -> withDetails "Stored Module" (textFileName n)
      ModuleSource_Deployed b
        -> withDetails "Deployed Module" (textBackendName . backendRefName $ b)
  where
    withDetails n d = mconcat [ n <> " [ " , d , " ]" ]


-- Module utility functions:

-- | Get the `ModuleName` of a `Module`.
nameOfModule :: Module -> ModuleName
nameOfModule = \case
  Interface {..} -> _interfaceName
  Module {..}    -> _mName

-- | Get the `ModuleName` of a `Module`.
codeOfModule :: Module -> Code
codeOfModule = \case
  Interface {..} -> _interfaceCode
  Module {..}    -> _mCode

-- Get hold of modules and functions:

-- | Useful data about a pact function.
data PactFunction = PactFunction
  { _pactFunction_module        :: ModuleName
  , _pactFunction_name          :: Text
  , _pactFunction_defType       :: DefType
  , _pactFunction_documentation :: Maybe Text
  , _pactFunction_type          :: FunType (Term Name)
  }
  deriving Show

makePactLenses ''PactFunction

-- | Fetch a `Module` from a backend where it has been deployed.
--
--   Resulting Event is either an error msg or the loaded module.
fetchModule
  :: forall m t
  . ( MonadHold t m, PerformEvent t m, MonadJSM (Performable m)
    , HasJSContext (Performable m), TriggerEvent t m
    )
  => Event t DeployedModuleRef
  -> m (Event t (DeployedModuleRef, Either Text Module))
fetchModule onReq = do
    deployedResult :: Event t (DeployedModuleRef, BackendErrorResult)
      <- performBackendRequestCustom emptyWallet mkReq onReq

    pure $ ffor deployedResult $
      id *** (fromJsonEither <=< left (T.pack . show))

  where
    mkReq mRef = BackendRequest
      { _backendRequest_code = mconcat
        [ defineNamespace . _moduleRef_name $ mRef
        , "(describe-module '"
        , _mnName . _moduleRef_name $ mRef
        , ")"
        ]
      , _backendRequest_data = mempty
      , _backendRequest_backend = _moduleRef_source dm
      , _backendRequest_signing = Set.empty
      }

    fromJsonEither :: FromJSON a => Value -> Either Text a
    fromJsonEither v = case fromJSON v of
        Aeson.Error e -> throwError . T.pack $ e
        Aeson.Success a -> pure a

    defineNamespace =
      maybe "" (\n -> "(namespace '" <> n <> ")") .  _mnNamespace


-- | Get module and its function of current term, if it is a `Module`
getModule :: MonadPlus m => Term Name -> m (Module, [PactFunction])
getModule = \case
  TModule m body _ -> pure $ (m, getFunctions $ Bound.instantiate undefined body)
  _                -> mzero

-- | Get the top level functions from a 'Term'
getFunctions :: Term Name -> [PactFunction]
getFunctions (TModule _ body _) = getFunctions $ Bound.instantiate undefined body
getFunctions (TDef (Def (DefName name) moduleName defType funType _body meta _) _) =
  [PactFunction moduleName name defType (_mDocs meta) funType]
getFunctions (TList list1 _ _) = getFunctions =<< list1
getFunctions _ = []


-- Instances:

instance Functor ModuleRefV where
  fmap f (ModuleRef s n) = ModuleRef (f s) n

instance Foldable ModuleRefV where
  foldMap f (ModuleRef s _) = f s

instance Traversable ModuleRefV where
  traverse f (ModuleRef s n)  = flip ModuleRef n <$> f s
