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
import           Data.Bool                             (bool)
import           Data.Maybe
import           Data.Text                             (Text)
import           Data.Traversable                      (for)
import           Reflex
import           Reflex.Dom
import           Reflex.Network.Extended
------------------------------------------------------------------------------
import           Frontend.ModuleExplorer
import           Frontend.Network
import           Frontend.UI.Button
import           Frontend.UI.Dialogs.CallFunction
import           Frontend.UI.Modal
import           Frontend.UI.ModuleExplorer.ModuleList
------------------------------------------------------------------------------

-- | Constraints on the `Model` we have for our details screen.
type HasUIModuleDetailsModel model t =
  (HasModuleExplorer model t, HasNetwork model t, HasUICallFunctionModel model t)

-- | Constraints on the model config we have for implementing this screen.
type HasUIModuleDetailsModelCfg mConf m t =
  ( Monoid mConf, Flattenable mConf t, HasModuleExplorerCfg mConf t
  , HasNetworkCfg mConf t
  , HasModalCfg mConf (Modal mConf m t) t
  , HasUICallFunctionModelCfg (ModalCfg mConf t) t
  )

-- | Details screen for a Module/Interface
--
--   User can see and call the module's function and browse implemented
--   interfaces and used modules.
moduleDetails
  :: forall t m model mConf
  . ( MonadWidget t m
    , HasUIModuleDetailsModel model t
    , HasUIModuleDetailsModelCfg mConf m t
    )
  => model
  -> (ModuleRef, ModuleDef (Term Name))
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

    bodyCfg1 <- showDeps "Implemented Interfaces" $
      selected ^. interfacesOfModule

    bodyCfg2 <- showDeps "Used Modules" $
      selected ^. importNamesOfModule

    functionConfigs <-
      traverse uiDefTypeSegment . functionsByDefType $ functionsOfModule selected

    pure (headerCfg <> bodyCfg1 <> bodyCfg2 <> mconcat functionConfigs)

  where
    uiDefTypeSegment :: (DefType, [PactFunction]) -> m mConf
    uiDefTypeSegment (t, fs) =
      case fs of
        [] -> pure mempty
        _  -> elClass "div" "segment" $ do
          elClass "h3" "heading heading_type_h3" $
            text $ defTypeHeading t
          mkFunctionList fs

    defTypeHeading :: DefType -> Text
    defTypeHeading = \case
      Defun -> "Functions"
      Defpact -> "Pacts"
      Defcap  -> "Capabilities"

    mkFunctionList :: [PactFunction] -> m mConf
    mkFunctionList = functionList m $
      if isModule selected
         then getDeployedModuleRef selectedRef
         else Nothing

    moduleTitle = elClass "h2" "heading heading_type_h2" $ do
      text $ textModuleName $ _moduleRef_name selectedRef
      elClass "div" "heading__type-details" $ do
        text $ textModuleRefSource (isModule selected) selectedRef

    showDeps :: Text -> [ModuleName] -> m mConf
    showDeps n deps =
      let
        refs = map (ModuleRef (_moduleRef_source selectedRef)) deps
      in
        case refs of
          [] -> pure mempty
          _  -> elClass "div" "segment" $ do
            elClass "h3" "heading heading_type_h3" $ text n
            onSel <- uiModuleList . pure $ refs
            pure $ mempty & moduleExplorerCfg_pushModule .~ onSel

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
            let isCallable = isDeployed && functionIsCallable f
            fmap (const f) <$> bool (viewButton btnCls) (callButton btnCls) isCallable
      pure $ mempty & modalCfg_setModal .~ (Just . uiCallFunction m mDeployed <$> onView)

