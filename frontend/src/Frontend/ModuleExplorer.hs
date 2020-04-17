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

-- | ModuleExplorer: Browse and load modules from examples or backends.
--
--   In the future also load modules and data from server storage or local
--   storage.
--
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.ModuleExplorer
  ( -- * Types and Classes
    -- ** The basic Model and ModelConfig types
    ModuleExplorerCfg (..)
  , HasModuleExplorerCfg (..)
  , ModuleExplorer (..)
  , HasModuleExplorer (..)
    -- ** Other types
  , GistMeta (..)
    -- ** Additonal quick viewing functions
  , moduleExplorer_selection
  -- * Re-exports
  , module Example
  , module Module
  , module File
  , module ModuleList
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Generics.Deriving.Monoid           (mappenddefault,
                                                     memptydefault)
import           GHC.Generics                       (Generic)
import           Reflex
------------------------------------------------------------------------------
import           Pact.Types.Lang                    (Code, ModuleName)
------------------------------------------------------------------------------
import           Common.GistStore                  (GistMeta (..))
import           Frontend.Foundation
import           Frontend.ModuleExplorer.Example    as Example
import           Frontend.ModuleExplorer.File       as File
import           Frontend.ModuleExplorer.LoadedRef  as Module
import           Frontend.ModuleExplorer.Module     as Module
import           Frontend.ModuleExplorer.ModuleList as ModuleList
import           Frontend.ModuleExplorer.ModuleRef  as Module

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
  , _moduleExplorerCfg_goHome       :: Event t ()
    -- ^ Clear selectedFile and module stack.
  , _moduleExplorerCfg_loadModule   :: Event t ModuleRef
    -- ^ Load a module into the editor.
  , _moduleExplorerCfg_loadFile     :: Event t FileRef
    -- ^ Load some file into the `Editor`.
  , _moduleExplorerCfg_clearLoaded  :: Event t ()
    -- ^ Set `_moduleExplorer_loaded` to Nothing and clear editor contents.
  , _moduleExplorerCfg_createGist   :: Event t GistMeta
    -- ^ Create a github gist with the contents of the `Editor`.
  , _moduleExplorerCfg_modules      :: ModuleListCfg t
    -- ^ Configuration for the deployed module list.
  }
  deriving Generic

makePactLenses ''ModuleExplorerCfg

-- | Current ModuleExploer state.
data ModuleExplorer t = ModuleExplorer
  { _moduleExplorer_moduleStack     :: Dynamic t [(ModuleRef, ModuleDef (Term Name))]
    -- ^ The stack of currently selected modules.
  , _moduleExplorer_selectedFile    :: MDynamic t (FileRef, Code)
    -- ^ The currently selected file if any.
  , _moduleExplorer_selectionGrowth :: Dynamic t Ordering
    -- ^ Whether the stack is currently growing/shrinking.
  , _moduleExplorer_loaded          :: MDynamic t LoadedRef
    -- ^ Where did the data come from that got loaded last into the `Editor`?
    -- We can load files and we can load modules, so this is either a `FileRef`
    -- or a `ModuleRef`.
  , _moduleExplorer_modules         :: ModuleList t
    -- ^ List of deployed modules, with user given filters applied.
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
  -> MDynamic t (Either (FileRef, Code) (ModuleRef, ModuleDef (Term Name)))
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


instance Flattenable (ModuleExplorerCfg t) t where
  flattenWith doSwitch ev =
    ModuleExplorerCfg
      <$> doSwitch never (_moduleExplorerCfg_pushModule <$> ev)
      <*> doSwitch never (_moduleExplorerCfg_popModule <$> ev)
      <*> doSwitch never (_moduleExplorerCfg_selectFile <$> ev)
      <*> doSwitch never (_moduleExplorerCfg_goHome <$> ev)
      <*> doSwitch never (_moduleExplorerCfg_loadModule <$> ev)
      <*> doSwitch never (_moduleExplorerCfg_loadFile <$> ev)
      <*> doSwitch never (_moduleExplorerCfg_clearLoaded <$> ev)
      <*> doSwitch never (_moduleExplorerCfg_createGist <$> ev)
      <*> flattenWith doSwitch (_moduleExplorerCfg_modules <$> ev)
