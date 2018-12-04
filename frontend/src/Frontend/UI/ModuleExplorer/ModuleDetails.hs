{-# LANGUAGE ConstraintKinds        #-}
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

-- | Details view for a particular module.
--
--   This view shows details about a particular Pact module and in particular
--   offers an interface for calling its functions.
--
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.UI.ModuleExplorer.ModuleDetails where

------------------------------------------------------------------------------
import           Control.Lens
import qualified Data.List                as L
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Traversable         (for)
import           Reflex
import           Reflex.Dom
import           Reflex.Network
import           Reflex.Network.Extended
------------------------------------------------------------------------------
import           Obelisk.Generated.Static
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.ModuleExplorer
import           Frontend.UI.Button
import           Frontend.UI.Widgets
import           Frontend.UI.Icon
------------------------------------------------------------------------------

type HasUIModuleDetailsModel model t =
  (HasModuleExplorer model t, HasBackend model t)

type HasUIModuleDetailsModelCfg mConf t =
  ( Monoid mConf, Flattenable mConf t, HasModuleExplorerCfg mConf t, HasBackendCfg mConf t
  )

moduleDetails
  :: forall t m model mConf
  . ( MonadWidget t m
    , HasUIModuleDetailsModel model t
    , HasUIModuleDetailsModelCfg mConf t
    )
  => model
  -> SelectedModule
  -> m mConf
moduleDetails m selected = do
    headerCfg <- elClass "div" "control-block" $ do
      (onBack, onLoad) <- elClass "h2" "back-button-header" $
        (,) <$> backButton <*> elClass "div" "load-button" loadToEditorButton

      moduleTitle
      pure $ mempty
        & moduleExplorerCfg_selModule .~ (Nothing <$ onBack)
        & moduleExplorerCfg_loadModule .~ (_selectedModule_module selected <$ onLoad)
    bodyCfg <- elClass "div" "module-details-content" $ do
      elClass "h3" "module-details" $ text "Functions"
      mayFunctionList $ _selectedModule_functions selected
    pure (headerCfg <> bodyCfg)
  where
    mayFunctionList :: Maybe [PactFunction] -> m mConf
    mayFunctionList = maybe noFunctions functionList

    noFunctions = do
      elClass "div" "error" $ text "Error while loading functions."
      pure mempty

    moduleTitle = elClass "h2" "module-details-title" $ do
      text $ selectedModuleName selected
      elClass "span" "module-details-type" $ text $ showSelectedModuleType selected


functionList
  :: (MonadWidget t m, HasUIModuleDetailsModelCfg mConf t)
  => [PactFunction] -> m mConf
functionList functions = do
    divClass "functions" $ elClass "ol" "functions-list" $ do
      onView <- fmap leftmost . for functions $ \f -> el "li" $ do
        divClass "function-name" $ text $ _pactFunction_name f 
        divClass "function-desc" $ text $ fromMaybe "" $ _pactFunction_documentation f
        divClass "function-view" $ fmap (const f) <$> loadToEditorButton
      pure $ mempty

