{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Details view for a file, lists contained modules.
--
--   This view shows details about a particular Pact file, it shows contained
--   modules and offers loading to editor.
--
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.UI.ModuleExplorer.FileDetails where

------------------------------------------------------------------------------
import           Control.Lens
import           Reflex.Dom
import           Reflex.Network.Extended
import qualified Data.Map as Map
------------------------------------------------------------------------------
import           Frontend.Network
import           Frontend.ModuleExplorer
import           Frontend.UI.ModuleExplorer.ModuleList
import           Frontend.UI.Button
import           Frontend.UI.Dialogs.CallFunction
import           Frontend.UI.Modal
------------------------------------------------------------------------------

type HasUIFileDetailsModelCfg mConf m t =
  ( Monoid mConf, Flattenable mConf t, HasModuleExplorerCfg mConf t, HasNetworkCfg mConf t
  , HasModalCfg mConf (Modal mConf m t) t
  , HasUICallFunctionModelCfg (ModalCfg mConf t) t
  )

fileDetails
  :: forall t m mConf
  . ( MonadWidget t m
    , HasUIFileDetailsModelCfg mConf m t
    )
  => (FileRef, PactFile)
  -> m mConf
fileDetails (selectedRef, selected) = do
    headerCfg <- elClass "div" "segment" $ do
      (onBack, onLoad) <- elClass "h2" "heading heading_type_h2" $
        (,) <$> backButton <*> openButton mempty

      moduleTitle
      pure $ mempty
        & moduleExplorerCfg_selectFile .~ fmap (const Nothing) onBack
        & moduleExplorerCfg_loadFile .~ (selectedRef <$ onLoad)

    bodyCfg <- elClass "div" "segment" $ do
      elClass "h3" "heading heading_type_h3" $ text "Modules"
      onModSelect <- uiModuleList . pure $ moduleRefs
      pure $ mempty & moduleExplorerCfg_pushModule .~ onModSelect

    pure (headerCfg <> bodyCfg)
  where
    moduleRefs :: [ModuleRef]
    moduleRefs =
      map (ModuleRef (ModuleSource_File selectedRef)) . Map.keys . fileModulesDiscardingErrors $ selected

    moduleTitle = elClass "h2" "heading heading_type_h2" $ do
      text $ fileRefName selectedRef
      elClass "div" "heading__type-details" $
        text $ textFileType selectedRef


{- functionList -}
{-   :: forall t m mConf model -}
{-   .  ( MonadWidget t m, HasUIFileDetailsModelCfg mConf m t -}
{-      , HasUIFileDetailsModel model t -}
{-      ) -}
{-   => model -> Maybe DeployedModuleRef -> [PactFunction] -> m mConf -}
{- functionList m mDeployed functions = -}
{-     elClass "ol" "table table_type_primary" $ do -}
{-       onView <- fmap leftmost . for functions $ \f -> -}
{-         elClass "li" "table__row table__row_type_primary" $ do -}
{-           divClass "table__text-cell table__cell_size_side title" $ -}
{-             text $ _pactFunction_name f -}
{-           divClass "table__text-cell table__cell_size_double-main description" $ -}
{-             text $ fromMaybe "" $ _pactFunction_documentation f -}
{-           divClass "table__cell_size_flex" $ do -}
{-             let btnCls = "table__action-button" -}
{-             let isDeployed = isJust mDeployed -}
{-             fmap (const f) <$> bool (callButton btnCls) (viewButton btnCls) isDeployed -}
{-       pure $ mempty & modalCfg_setModal .~ (Just . uiCallFunction m mDeployed <$> onView) -}
