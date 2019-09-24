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

-- | List of `Modules` UI component.
--
-- Copyright   :  (C) 2019 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.UI.ModuleExplorer.ModuleList where

------------------------------------------------------------------------------
import           Control.Arrow               ((&&&))
import           Control.Lens
import           Control.Monad
import           Data.Either                 (rights)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import           Data.Traversable            (for)
import           Pact.Types.ChainId          (_chainId)
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Network
import           Reflex.Network.Extended
------------------------------------------------------------------------------
import           Frontend.Foundation
import           Frontend.ModuleExplorer
import           Frontend.Network
import           Frontend.UI.Button
import           Frontend.UI.Widgets
------------------------------------------------------------------------------

type HasUIModuleListModel model t =
  (HasNetwork model t)

type HasUIModuleListModelCfg mConf t =
  ( Monoid mConf, Flattenable mConf t, HasModuleListCfg mConf t
  )


uiModuleList
  :: forall t m
  . ( MonadWidget t m)
  => Dynamic t [ModuleRef]
  -> m (Event t ModuleRef)
uiModuleList modules = do
  let
    showModules :: ModuleRef -> m ()
    showModules c = do
      divClass "table__text-cell table__cell_size_main" $
        text $ textModuleRefName c
      case _moduleRef_source c of
        ModuleSource_Deployed s ->
          divClass "table__text-cell table__cell_size_side" $
            text $ _chainId $ _chainRef_chain s
        _ ->
          blank

  listEv <- networkView $ viewList showModules <$> modules
  switchHold never listEv


-- | Browse deployed contracts and select one.
uiDeployedModuleList
  :: forall t m model mConf
  . ( MonadWidget t m
    , HasUIModuleListModel model t
    , HasUIModuleListModelCfg mConf t
    )
  => model
  -> ModuleList t
  -> m (mConf, Event t ModuleRef)
uiDeployedModuleList m mList = mdo
    cfg <- divClass "filter-bar" $ do
      onSearch <- tagOnPostBuild $ mList ^. moduleList_nameFilter
      ti <- uiInputElement $ def
          & initialAttributes .~ ("placeholder" =: "Search" <> "class" =: "input_type_search input_type_tertiary filter-bar__search")
          & inputElementConfig_setValue .~ onSearch

      -- dropdown is kinda loopy, therefore the delay.
      onNetworkName <- delay 0 <=< tagOnPostBuild $ mList ^. moduleList_chainIdFilter
      let mkMap = Map.fromList . map (Just &&& _chainId) . getChains
          mInfo = (^? to rights . _head) <$> m ^. network_selectedNodes
          opts = Map.insert Nothing "All chains" . maybe mempty mkMap <$> mInfo
          filterCfg = def & dropdownConfig_attributes %~ fmap (addToClassAttr $ "select_type_tertiary" <> "filter-bar__chain-filter")
                          & setValue .~ onNetworkName

      d <- uiDropdown Nothing opts filterCfg
      let
        onNewSearch :: Event t Text
        onNewSearch = _inputElement_input ti

        onChainIdL :: Event t (Maybe ChainId)
        onChainIdL = _dropdown_change d

      onUpdatePageL <- paginationWidget "filter-bar__pagination"
        (mList ^. moduleList_page)
        (mList ^. moduleList_pageCount)

      pure $ mempty
        & moduleListCfg_setPage .~ onUpdatePageL
        & moduleListCfg_setNameFilter .~ onNewSearch
        & moduleListCfg_setChainIdFilter .~ onChainIdL

    onSelect <- uiModuleList $ mList ^. moduleList_modules

    pure (cfg, onSelect)

viewList :: MonadWidget t m => (a -> m ()) -> [a] -> m (Event t a)
viewList rowFunc contracts = do
    elClass "ol" "table table_type_primary" $
      fmap leftmost . for contracts $ \c -> elClass "li" "table__row table__row_type_primary" $ do
        divClass "table__row-counter" blank
        rowFunc c
        divClass "table__cell_size_flex table__last-cell" $
          viewModButton c

-- TODO: Unify with viewList (copy & paste right now) - only difference, we
-- have a load button in addition to view.
fileList :: MonadWidget t m => (a -> m ()) -> [a] -> m (Event t a, Event t a)
fileList rowFunc contracts = do
    evs <- elClass "ol" "table table_type_primary" $
      for contracts $ \c -> elClass "li" "table__row table__row_type_primary" $ do
        divClass "table__row-counter" blank
        rowFunc c
        divClass "table__cell_size_flex table__last-cell" $ do
          (,) <$> openModButton c <*> viewModButton c
    let
      onOpen = leftmost . map fst $ evs
      onView = leftmost . map snd $ evs
    pure (onOpen, onView)


openModButton :: MonadWidget t m => a -> m (Event t a)
openModButton c = fmap (const c) <$> openButton "table__action-button table__left-action-button"

viewModButton :: MonadWidget t m => a -> m (Event t a)
viewModButton c = fmap (const c) <$> viewButton "table__action-button"

