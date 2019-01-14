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
import Control.Lens
import Control.Monad.Except (throwError)
import           Data.Coerce             (coerce)
import           Control.Arrow               ((***), left)
import Control.Monad
import Data.Text
import qualified Data.Text as T
import qualified Data.Set as Set
import           Data.Aeson                  as Aeson (Result (..), fromJSON, FromJSON, Value)
import           Reflex.Dom.Core          (HasJSContext, MonadHold)
------------------------------------------------------------------------------
import           Pact.Types.Lang          (ModuleName)
import           Pact.Types.Term         as PactTerm  (Module (..), ModuleName (..), NamespaceName (..))
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Foundation
import           Frontend.Wallet
import           Frontend.ModuleExplorer.File
import           Frontend.ModuleExplorer.Example

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
  deriving (Eq, Ord, Show)

makePactPrisms ''ModuleSource


-- | A Module is uniquely idendified by its name and its origin.
data ModuleRefV s = ModuleRef
  { _moduleRef_source :: s -- ^ Where does the module come from.
  , _moduleRef_name   :: ModuleName   -- ^ Fully qualified name of the module.
  }
  deriving (Eq, Ord, Show)

makePactLensesNonClassy ''ModuleRefV

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


-- Get hold of a deployed module:

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
      , _backendRequest_backend = _moduleRef_source mRef
      , _backendRequest_signing = Set.empty
      }

    fromJsonEither :: FromJSON a => Value -> Either Text a
    fromJsonEither v = case fromJSON v of
        Aeson.Error e -> throwError . T.pack $ e
        Aeson.Success a -> pure a

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
