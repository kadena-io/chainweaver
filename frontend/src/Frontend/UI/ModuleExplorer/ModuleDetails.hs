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
import           Control.Monad                         (when)
import           Data.Bool                             (bool)
import           Data.Maybe
import           Data.Traversable                      (for)
import           Reflex
import           Reflex.Dom
import           Reflex.Network.Extended
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.ModuleExplorer
import           Frontend.UI.Button
import           Frontend.UI.Dialogs.CallFunction
import           Frontend.UI.Modal
import           Frontend.UI.ModuleExplorer.ModuleList
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
  -> (ModuleRef, Module)
  -> m mConf
moduleDetails m (selectedRef, selected) = do
    headerCfg <- elClass "div" "segment" $ do
      ((onHome, onBack), onLoad) <- elClass "h2" "heading heading_type_h2" $ do
        hb <- el "div" $
          (,) <$> homeButton "heading__left-double-button" <*> backButton
        (hb,) <$> openButton mempty

      moduleTitle
      pure $ mempty
        & moduleExplorerCfg_goHome .~ onHome
        & moduleExplorerCfg_popModule .~  onBack
        & moduleExplorerCfg_loadModule .~ (selectedRef <$ onLoad)

    bodyCfg1 <-
      let
        interfaceRefs = map (ModuleRef (_moduleRef_source selectedRef)) $
          selected ^. interfacesOfModule
      in
        case interfaceRefs of
          [] -> pure mempty
          _  -> elClass "div" "segment" $ do
            elClass "h3" "heading heading_type_h3" $ text "Implemented Interfaces"
            onSel <- uiModuleList . pure $ interfaceRefs
            pure $ mempty & moduleExplorerCfg_pushModule .~ onSel

    bodyCfg2 <- elClass "div" "segment" $ do
      elClass "h3" "heading heading_type_h3" $ text "Functions"
      mkFunctionList $ functionsOfModule selected
    pure (headerCfg <> bodyCfg1 <> bodyCfg2)

  where
    mkFunctionList :: [PactFunction] -> m mConf
    mkFunctionList = functionList m $
      if isModule selected
         then getDeployedModuleRef selectedRef
         else Nothing

    moduleTitle = elClass "h2" "heading heading_type_h2" $ do
      text $ textModuleName $ _moduleRef_name selectedRef
      elClass "div" "heading__type-details" $ do
        text $ textModuleRefSource (isModule selected) selectedRef


functionList
  :: forall t m mConf model
  .  ( MonadWidget t m, HasUIModuleDetailsModelCfg mConf m t
     , HasUIModuleDetailsModel model t
     )
  => model -> Maybe DeployedModuleRef -> [PactFunction] -> m mConf
functionList m mDeployed functions =
    elClass "ol" "table table_type_primary" $ do
      onView <- fmap leftmost . for functions $ \f ->
        elClass "li" "table__row table__row_type_primary" $ do
          divClass "table__text-cell table__cell_size_side title" $
            text $ _pactFunction_name f
          divClass "table__text-cell table__cell_size_double-main description" $
            text $ fromMaybe "" $ _pactFunction_documentation f
          divClass "table__cell_size_flex table__last-cell" $ do
            let btnCls = "table__action-button"
            let isDeployed = isJust mDeployed
            fmap (const f) <$> bool (viewButton btnCls) (callButton btnCls) isDeployed
      pure $ mempty & modalCfg_setModal .~ (Just . uiCallFunction m mDeployed <$> onView)

