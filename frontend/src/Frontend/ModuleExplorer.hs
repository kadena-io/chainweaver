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
------------------------------------------------------------------------------
import           Obelisk.Generated.Static
import           Pact.Types.Lang (ModuleName, DefType, FunType, Term, Name)
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Foundation

data ExampleModule = ExampleModule
  { _exampleModule_name :: Text
  , _exampleModule_code :: Text
  , _exampleModule_data :: Text
  } deriving Show

makePactLenses ''ExampleModule

data DeployedModule = DeployedModule
  { _deployedModule_name        :: Text
  , _deployedModule_backendName :: BackendName
  , _deployedModule_backendUri  :: BackendUri
  } deriving (Eq, Ord, Show)

makePactLenses ''DeployedModule

-- | Selector for loading modules.
data ModuleSel
  = ModuleSel_Example ExampleModule
  | ModuleSel_Deployed DeployedModule

makePactPrisms ''ModuleSel

-- | Useful data about a pact function.
data PactFunction = PactFunction
  { _pactFunction_module        :: ModuleName
  , _pactFunction_name          :: Text
  , _pactFunction_defType       :: DefType
  , _pactFunction_documentation :: Maybe Text
  , _pactFunction_type          :: FunType (Term Name)
  }
  deriving Show

-- | Information about the currently selected deployed module.
--
data SelectedModule = SelectedModule
  { _selectedModule_module :: ModuleSel
    -- ^ The module that is currently selected.
  , _selectedModule_code :: Text
    -- ^ Source code of the currently selected module.
  , _selectedModule_functions :: Maybe [PactFunction]
    -- ^ The available functions of that module. `Nothing` in case function
    -- fetching failed for some reason.
  }

makePactLenses ''SelectedModule

-- | Configuration for ModuleExplorer
--
--   State is controlled via this configuration.
data ModuleExplorerCfg t = ModuleExplorerCfg
  { _moduleExplorerCfg_selModule :: Event t (Maybe ModuleSel)
    -- ^ Select a module for viewing its functions and further details.
  , _moduleExplorerCfg_loadModule :: Event t ModuleSel
    -- ^ Load a module into the editor.
  }
  deriving Generic

makePactLenses ''ModuleExplorerCfg

-- | Current ModuleExploer state.
data ModuleExplorer t = ModuleExplorer
  { _moduleExplorer_selectedModule :: MDynamic t SelectedModule
  -- ^ Information about the currently selected module, if available.
  , _moduleExplorer_loadedModule :: MDynamic t ModuleSel
  -- ^ The module that was loaded last into the editor.
  }
  deriving Generic

makePactLenses ''ModuleExplorer

-- | Get the name of a selected module.
selectedModuleName :: SelectedModule -> Text
selectedModuleName selected =
  case _selectedModule_module selected of
    ModuleSel_Example ex -> _exampleModule_name ex
    ModuleSel_Deployed ex -> _deployedModule_name ex

-- | Show the type of the selected module.
--
--   It is either "Example Contract" or "Deployed Contract"
showSelectedModuleType :: SelectedModule -> Text
showSelectedModuleType selected =
  case _selectedModule_module selected of
    ModuleSel_Example _ -> "Example Contract"
    ModuleSel_Deployed _ -> "Deployed Contract"


-- | Available example modules.
exampleData :: [ExampleModule]
exampleData =
  [ ExampleModule "Hello World"
    (static @ "examples/helloWorld-1.0.repl")
    (static @ "examples/helloWorld-1.0.data.json")
  , ExampleModule "Simple Payment"
    (static @ "examples/simplePayments-1.0.repl")
    (static @ "examples/simplePayments-1.0.data.json")
  , ExampleModule "International Payment"
    (static @ "examples/internationalPayments-1.0.repl")
    (static @ "examples/internationalPayments-1.0.data.json")

  {- , ExampleModule "Commercial Paper" "examples/commercialPaper-1.0" -}
  ]

-- | Examples as Map from Index to actual data.
demos :: Map Int ExampleModule
demos = Map.fromList $ zip [0..] exampleData

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

instance Flattenable (ModuleExplorerCfg t) t where
  flattenWith doSwitch ev =
    ModuleExplorerCfg
      <$> doSwitch never (_moduleExplorerCfg_selModule <$> ev)
      <*> doSwitch never (_moduleExplorerCfg_loadModule <$> ev)
