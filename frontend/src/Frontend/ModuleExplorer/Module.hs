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


-- Instances:

instance Functor ModuleRefV where
  fmap f (ModuleRef s n) = ModuleRef (f s) n

instance Foldable ModuleRefV where
  foldMap f (ModuleRef s _) = f s

instance Traversable ModuleRefV where
  traverse f (ModuleRef s n)  = flip ModuleRef n <$> f s
