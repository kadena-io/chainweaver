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

-- |
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.UI.ModuleExplorer where

------------------------------------------------------------------------------
import           Control.Lens
import           Reflex.Dom
import           Reflex.Network.Extended
------------------------------------------------------------------------------
import           Frontend.Network
import           Frontend.ModuleExplorer
import           Frontend.UI.Button
import           Frontend.UI.ModuleExplorer.FileDetails
import           Frontend.UI.ModuleExplorer.ModuleDetails
import           Frontend.UI.ModuleExplorer.ModuleList
import           Frontend.UI.Widgets
------------------------------------------------------------------------------

type HasUIModuleExplorerModel model t =
  (HasModuleExplorer model t, HasNetwork model t, HasUIModuleDetailsModel model t)

type HasUIModuleExplorerModelCfg mConf m t =
  ( Monoid mConf, Flattenable mConf t, HasModuleExplorerCfg mConf t, HasNetworkCfg mConf t
  , HasUIModuleDetailsModelCfg mConf m t
  )

moduleExplorer
  :: forall t m model mConf
  . ( MonadWidget t m
    , HasUIModuleExplorerModel model t
    , HasUIModuleExplorerModelCfg mConf m t
    )
  => model
  -> m mConf
moduleExplorer m = do
    let selected = moduleExplorer_selection m
    networkViewFlatten $ maybe browse showDetails <$> selected
  where
    browse = animatedDiv $ do
      exampleCfg <- browseExamples
      deplCfg <- browseDeployedTitle m
      pure $ mconcat [ exampleCfg, deplCfg ]

    showDetails x = animatedDiv $ case x of
      Left f     -> fileDetails f
      Right modL -> moduleDetails m modL

    animatedDiv c = do
      let growth = m ^. moduleExplorer_selectionGrowth
      elDynClass "div" (mkAnimationCls <$> growth) $ c

    mkAnimationCls = \case
      LT -> "fly-in fly-in_from_left"
      GT -> "fly-in fly-in_from_right"
      _  -> "fly-in"


browseExamples
  :: forall t m mConf
  . ( MonadWidget t m , HasUIModuleExplorerModelCfg mConf m t
    )
  => m mConf
browseExamples =
  accordionItem True "segment" "Examples" $ do
    let showExample c = do
          divClass "table__text-cell table__cell_size_main" $
            text $ exampleName c

    (onOpen, onView) <- fileList showExample $ examples

    let
      onExampleSel = fmap (Just . FileRef_Example) onView
      onExampleOpen = fmap FileRef_Example onOpen
    pure $ mempty
      & moduleExplorerCfg_selectFile .~ onExampleSel
      & moduleExplorerCfg_loadFile .~ onExampleOpen


-- | Browse deployed contracts
--
--   This includes the accordion and the refresh button at the top.
browseDeployedTitle
  :: forall t m model mConf
  . ( MonadWidget t m
    , HasUIModuleExplorerModel model t
    , HasUIModuleExplorerModelCfg mConf m t
    )
  => model
  -> m mConf
browseDeployedTitle m = do
  let
    title = el "span" $ do
      el "span" $ text "Deployed Contracts"
      refreshButton "accordion__title-button"
  (onRefrClick, (mListCfg, onSelected)) <-
    accordionItem' True "segment" title $
      uiDeployedModuleList m (m ^. moduleExplorer_modules)

  pure $ mempty
    & moduleExplorerCfg_pushModule .~ onSelected
    & networkCfg_refreshModule .~ onRefrClick
    & moduleExplorerCfg_modules .~ mListCfg
