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

-- | Details view for a file, lists contained modules.
--
--   This view shows details about a particular Pact file, it shows contained
--   modules and offers loading to editor.
--
-- Copyright   :  (C) 2019 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.UI.ModuleExplorer.FileDetails where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Bool                        (bool)
import           Data.Maybe
import           Data.Traversable                 (for)
import           Reflex
import           Reflex.Dom
import           Reflex.Network.Extended
import qualified Data.Map as Map
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.ModuleExplorer
import           Frontend.UI.ModuleExplorer.ModuleList
import           Frontend.UI.Button
import           Frontend.UI.Dialogs.CallFunction
import           Frontend.UI.Modal
------------------------------------------------------------------------------

type HasUIFileDetailsModel model t =
  (HasModuleExplorer model t, HasBackend model t, HasUICallFunctionModel model t)

type HasUIFileDetailsModelCfg mConf m t =
  ( Monoid mConf, Flattenable mConf t, HasModuleExplorerCfg mConf t, HasBackendCfg mConf t
  , HasModalCfg mConf (Modal mConf m t) t
  , HasUICallFunctionModelCfg (ModalCfg mConf t) t
  )

fileDetails
  :: forall t m model mConf
  . ( MonadWidget t m
    , HasUIFileDetailsModel model t
    , HasUIFileDetailsModelCfg mConf m t
    )
  => model
  -> (FileRef, PactFile)
  -> m mConf
fileDetails m (selectedRef, selected) = do
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
      map (ModuleRef (ModuleSource_File selectedRef)) . Map.keys . fileModules $ selected

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

