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
import           Data.Maybe
import           Data.Traversable         (for)
import           Reflex
import           Reflex.Dom
import           Reflex.Network.Extended
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.ModuleExplorer
import           Frontend.UI.Button
import           Frontend.Foundation
import           Frontend.UI.Dialogs.CallFunction
import           Frontend.UI.Modal
------------------------------------------------------------------------------

type HasUIModuleDetailsModel model t =
  (HasModuleExplorer model t, HasBackend model t, HasUICallFunctionModel model t)

type HasUIModuleDetailsModelCfg mConf m t =
  ( Monoid mConf, Flattenable mConf t, HasModuleExplorerCfg mConf t, HasBackendCfg mConf t
  , HasModalCfg mConf (Modal mConf m t) t
  , HasUICallFunctionModelCfg (ModalCfg mConf t) t
  )

moduleDetails
  :: forall t m model mConf
  . ( MonadWidget t m
    , HasUIModuleDetailsModel model t
    , HasUIModuleDetailsModelCfg mConf m t
    )
  => model
  -> SelectedModule
  -> m mConf
moduleDetails m selected = do
    headerCfg <- elClass "div" "segment" $ do
      (onBack, onLoad) <- elClass "h2" "heading heading_type_h2" $
        (,) <$> backButton <*> openButton mempty

      moduleTitle
      pure $ mempty
        & moduleExplorerCfg_selModule .~ (Nothing <$ onBack)
        & moduleExplorerCfg_loadModule .~ (_selectedModule_module selected <$ onLoad)
    bodyCfg <- elClass "div" "segment" $ do
      elClass "h3" "heading heading_type_h3" $ text "Functions"
      mayFunctionList $ _selectedModule_functions selected
    pure (headerCfg <> bodyCfg)
  where
    mayDeployed = selected ^? selectedModule_module . _ModuleSel_Deployed
    mayFunctionList :: Maybe [PactFunction] -> m mConf
    mayFunctionList = maybe noFunctions (functionList m mayDeployed)

    noFunctions = do
      elClass "div" "error" $ text "Error while loading functions."
      pure mempty

    moduleTitle = elClass "h2" "heading heading_type_h2" $ do
      text $ selectedModuleName selected
      elClass "div" "heading__type-details" $
        text $ showSelectedModuleType selected


functionList
  :: forall t m mConf model
  .  ( MonadWidget t m, HasUIModuleDetailsModelCfg mConf m t
     , HasUIModuleDetailsModel model t
     )
  => model -> Maybe DeployedModule -> [PactFunction] -> m mConf
functionList m moduleL functions =
    elClass "ol" "table table_type_primary" $ do
      onView <- fmap leftmost . for functions $ \f ->
        elClass "li" "table__row table__row_type_primary" $ do
          divClass "table__text-cell table__cell_size_side title" $
            text $ _pactFunction_name f
          divClass "table__text-cell table__cell_size_double-main description" $
            text $ fromMaybe "" $ _pactFunction_documentation f
          divClass "table__cell_size_flex" $ do
            let btnCls = "table__action-button"
            fmap (const f) <$> maybe (viewButton btnCls) (const $ callButton btnCls) moduleL
      pure $ mempty & modalCfg_setModal .~ (Just . uiCallFunction m moduleL <$> onView)

