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
    -- ** Additonal quick viewing functions
  , moduleExplorer_selection
  -- * Re-exports
  , module Example
  , module Module
  , module File
  -- ** Auxiliary Types
  , TransactionInfo (..)
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Set                          (Set)
import           Data.Text                         (Text)
import           Generics.Deriving.Monoid          (mappenddefault,
                                                    memptydefault)
import           GHC.Generics                      (Generic)
import           Reflex
------------------------------------------------------------------------------
import           Pact.Types.Lang                   (ModuleName)
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Foundation
import           Frontend.ModuleExplorer.Example   as Example
import           Frontend.ModuleExplorer.File      as File
import           Frontend.ModuleExplorer.Module    as Module
import           Frontend.ModuleExplorer.ModuleRef as Module
import           Frontend.Wallet

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
  { _moduleExplorerCfg_pushModule   :: Event t ModuleRef
    -- ^ Push a module to our `_moduleExplorer_selectedModule` stack.
  , _moduleExplorerCfg_popModule    :: Event t ()
    -- ^ Pop a module from our `_moduleExplorer_selectedModule` stack. If the
    -- stack is empty, this `Event` does nothing.
  , _moduleExplorerCfg_selectFile   :: Event t (Maybe FileRef)
    -- ^ Select or deselect (`Nothing`) a given file.
  , _moduleExplorerCfg_loadModule   :: Event t ModuleRef
    -- ^ Load a module into the editor.
  , _moduleExplorerCfg_loadFile     :: Event t FileRef
    -- ^ Load some file into the `Editor`.
  , _moduleExplorerCfg_deployEditor :: Event t TransactionInfo
    -- ^ Deploy code that is currently in `Editor`.
  , _moduleExplorerCfg_deployCode   :: Event t (Text, TransactionInfo)
    -- ^ Deploy given Pact code, usually a function call.
  }
  deriving Generic

makePactLenses ''ModuleExplorerCfg

-- | Current ModuleExploer state.
data ModuleExplorer t = ModuleExplorer
  { _moduleExplorer_moduleStack  :: Dynamic t [(ModuleRef, Module)]
  -- ^ The stack of currently selected modules.
  , _moduleExplorer_selectedFile :: MDynamic t (FileRef, PactFile)
  -- ^ The currently selected file if any.
  , _moduleExplorer_loaded       :: MDynamic t ModuleSource
  -- ^ Where did the data come from that got loaded last into the `Editor`?
  {- , _moduleExplorer_deployedModules :: Dynamic t (Map BackendName (Maybe [Text])) -}
  }
  deriving Generic

makePactLenses ''ModuleExplorer

-- | Quick check whether the current selection is a `File` or a `Module`.
--
--   If a file and a module is selected, then the selected module is a module
--   of that file, thus it takes precedence and will be the result of this
--   function call.
moduleExplorer_selection
  :: (Reflex t, HasModuleExplorer explr t)
  => explr
  -> MDynamic t (Either (FileRef, PactFile) (ModuleRef, Module))
moduleExplorer_selection explr = do
  stk <- explr ^. moduleExplorer_moduleStack
  fileL <- explr ^. moduleExplorer_selectedFile
  pure $ case stk of
    []  -> Left <$> fileL
    s:_ -> Just . Right $ s


-- Instances:

instance Reflex t => Semigroup (ModuleExplorerCfg t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (ModuleExplorerCfg t) where
  mempty = memptydefault
  mappend = (<>)

instance Semigroup TransactionInfo where
  sel1 <> _ = sel1


instance Flattenable (ModuleExplorerCfg t) t where
  flattenWith doSwitch ev =
    ModuleExplorerCfg
      <$> doSwitch never (_moduleExplorerCfg_pushModule <$> ev)
      <*> doSwitch never (_moduleExplorerCfg_popModule <$> ev)
      <*> doSwitch never (_moduleExplorerCfg_selectFile <$> ev)
      <*> doSwitch never (_moduleExplorerCfg_loadModule <$> ev)
      <*> doSwitch never (_moduleExplorerCfg_loadFile <$> ev)
      <*> doSwitch never (_moduleExplorerCfg_deployEditor <$> ev)
      <*> doSwitch never (_moduleExplorerCfg_deployCode <$> ev)
