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

-- | ModuleExplorer: Browse and load modules from examples or backends.
--
--   In the future also load modules and data from server storage or local
--   storage.
--
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.ModuleExplorer
  ( -- * Types and Classes
    -- ** The basic Model and ModelConfig types
    ModuleExplorerCfg (..)
  , HasModuleExplorerCfg (..)
  , ModuleExplorer (..)
  , HasModuleExplorer (..)
  -- * Re-exports
  , module Example
  , module Module
  , module File
  -- * Constants
  , demos
  , exampleData
  -- ** Auxiliary Types
  , ModuleSel (..)
  , _ModuleSel_Example
  , _ModuleSel_Deployed
  , ExampleModule (..)
  , DeployedModule (..)
  , PactFunction (..)
  , TransactionInfo (..)
   -- *** SelectedModule
  , SelectedModule (..)
  , HasSelectedModule (..)
  , selectedModuleName
  , showSelectedModuleType
  ) where

------------------------------------------------------------------------------
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Text                (Text)
import           Generics.Deriving.Monoid (mappenddefault, memptydefault)
import           GHC.Generics             (Generic)
import           Reflex
import           Data.Set                     (Set)
------------------------------------------------------------------------------
import           Obelisk.Generated.Static
import           Pact.Types.Lang          (DefType, FunType, ModuleName, Name,
                                         Term)
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Foundation
import           Frontend.Wallet
import           Frontend.ModuleExplorer.Example as Example
import           Frontend.ModuleExplorer.Module  as Module

-- | Data needed to send transactions to the server.
data TransactionInfo = TransactionInfo
  { _transactionInfo_keys    :: Set KeyName
    -- ^ The keys to sign the message with.
  , _transactionInfo_backend :: BackendName
    -- ^ The backend to deploy to.
  } deriving (Eq, Ord, Show)


-- | Configuration for ModuleExplorer
--
--   State is controlled via this configuration.
data ModuleExplorerCfg t = ModuleExplorerCfg
  { _moduleExplorerCfg_pushModule  :: Event t ModuleRef
    -- ^ Push a module to our `_moduleExplorer_selectedModule` stack.
  , _moduleExplorerCfg_popModule   :: Event t ()
    -- ^ Pop a module from our `_moduleExplorer_selectedModule` stack. If the
    -- stack is empty, this `Event` does nothing.
  , _moduleExplorerCfg_selectFile  :: Event t (Maybe FileRef)
    -- ^ Select or deselect (`Nothing`) a given file.
  , _moduleExplorerCfg_loadModule :: Event t ModuleRef
    -- ^ Load a module into the editor.
  , _moduleExplorerCfg_loadFile :: Event t FileRef
    -- ^ Load some file into the `Editor`.
  , _moduleExplorerCfg_deployEditor :: Event t TransactionInfo
    -- ^ Deploy code that is currently in `Editor`.
  , _moduleExplorerCfg_deployCode :: Event t (Text, TransactionInfo)
    -- ^ Deploy given Pact code, usually a function call.
  }
  deriving Generic

makePactLenses ''ModuleExplorerCfg

-- | Current ModuleExploer state.
data ModuleExplorer t = ModuleExplorer
  { _moduleExplorer_moduleStack :: Dynamic t [(ModuleRef, Module)]
  -- ^ The stack of currently selected modules.
  , _moduleExplorer_selectedFile :: MDynamic t (FileRef, PactFile)
  -- ^ The currently selected file if any.
  , _moduleExplorer_loaded   :: MDynamic t ModuleSource
  -- ^ Where did the data come from that got loaded last into the `Editor`?
  , _moduleExplorer_deployedModules :: Dynamic t (Map BackendName (Maybe [Text]))
  }
  deriving Generic

makePactLenses ''ModuleExplorer

-- Instances:

instance Reflex t => Semigroup (ModuleExplorerCfg t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (ModuleExplorerCfg t) where
  mempty = memptydefault
  mappend = (<>)

instance Semigroup DeployedModule where
  sel1 <> _ = sel1

instance Semigroup ModuleSel where
  sel1 <> _ = sel1

instance Semigroup TransactionInfo where
  sel1 <> _ = sel1


instance Flattenable (ModuleExplorerCfg t) t where
  flattenWith doSwitch ev =
    ModuleExplorerCfg
      <$> doSwitch never (_moduleExplorerCfg_selModule <$> ev)
      <*> doSwitch never (_moduleExplorerCfg_loadModule <$> ev)
      <*> doSwitch never (_moduleExplorerCfg_deployEditor <$> ev)
      <*> doSwitch never (_moduleExplorerCfg_deployCode <$> ev)
