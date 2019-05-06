{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

-- | Dialog for editing and selection of networks.
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.NetworkEdit
  ( uiNetworkEdit
  ) where

------------------------------------------------------------------------------
import           Control.Arrow                  ((&&&))
import           Control.Lens
import           Control.Monad
import           Data.Bifunctor
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Text
import           Data.Void                      (Void)
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.Foundation
import           Frontend.Ide
import           Frontend.ModuleExplorer        (HasModuleExplorerCfg (..),
                                                 TransactionInfo (..))
import           Frontend.Network
import           Frontend.UI.DeploymentSettings
import           Frontend.UI.Modal
import           Frontend.UI.Widgets
------------------------------------------------------------------------------


type HasUiNetworkEditModel model t =
  (HasNetwork model t)

type HasUiNetworkEditModelCfg mConf t =
  ( Monoid mConf, Flattenable mConf t
  , HasNetworkCfg mConf t
  )

-- | Confirmation dialog for deployments.
--
--   User can make sure to deploy to the right network, has the right keysets,
--   the right keys, ...
uiNetworkEdit
  :: forall t m model mConf a.
    ( MonadWidget t m, HasUiNetworkEditModel model t, HasUiNetworkEditModelCfg mConf t
    )
  => model
  -> m (mConf, Event t ())
uiNetworkEdit m = do
  onClose <- modalHeader $ text "Networks"
  modalMain $ do
    cfg <- modalBody $
      divClass "group" $ do
        onNewNet <- validatedInputWithButton "group__header" checkNetName "Enter network name." "Create"
        editCfg <- uiNetworkHeader m
        let newCfg = updateNetworks m $ (\n -> Map.insert (NetworkName n) []) <$> onNewNet
        pure $ mconcat [ newCfg, editCfg ]

    modalFooter $ do
      -- TODO: Is this really a "Cancel" button?!
      onReset <- cancelButton def "Reset to Defaults"
      text " "
      onConfirm <- confirmButton def "Ok"

      pure (cfg & networkCfg_resetNetworks .~ onReset, never)
  where
    checkNetName k = do
      keys <- sample $ current $ m ^. network_networks
      pure $ if Map.member (NetworkName k) keys then Just "This network already exists." else Nothing


uiNetworks
  :: (MonadWidget t m, HasUiNetworkEditModel model t, HasUiNetworkEditModelCfg mConf t)
  => model -> m mConf
uiNetworks m = do
    let
      networkNames = Map.keys <$> m ^. network_networks
      selName = fst <$> m ^. network_selectedNetwork
    evEv <- networkView $ traverse (mkEdit selName) <$> networkNames
    ev <- switchHold never $ leftmost <$> evEv
    let
      selCfg =  mempty & networkCfg_selectNetwork .~ fmapMaybe (^? _NetworkSelect) ev
      delCfg = updateNetworks m $ Map.delete <$> fmapMaybe (^? _NetworkDelete) ev
    pure $ mconcat [selCfg, delCfg]
  where
    mkEdit selected n = fmap fst $ accordionItem' False mempty (uiNetworkSelector selected n) (text "Body")


{- uiNetwork :: MonadWidget t m => Dynamic t (NetworkName, [NodeRef]) -> NetworkName -> m (Event t NetworkAction) -}
{- uiNetwork networkL self = do -}


uiNetworkSelector :: MonadWidget t m => Dynamic t NetworkName -> NetworkName -> m (Event t NetworkAction)
uiNetworkSelector val self = do
  onSelect <- fmap NetworkSelect <$> uiLabeledRadioView label val self
  onDelete <- fmap (const $ NetworkDelete self) <$> accordionDeleteBtn
  pure $ leftmost [ onSelect, onDelete ]
  where
    accordionDeleteBtn = deleteButtonNaked $ def & uiButtonCfg_class .~ "accordion__title-button"
    label cls = fmap fst . elKlass' "span" ("heading_type_h2" <> cls) $ text (textNetworkName self)


uiNodes :: MonadWidget t m => Dynamic t [NodeRef] -> m (Event t NodeAction)
uiNodes nodes = elClass "ol" "table table_type_primary" $ do
  dynNodes <- unjoinList nodes
  onEvAction <- networkView $ ffor dynNodes $ \nds -> do
    onEdits <- traverse (uiNode . Just) nds
    let onNumberedEdits = zipWith (\n e -> NodeUpdate n <$> e) [1..] onEdits
    onNewEdit <- fmapMaybe (fmap NodeNew) <$> uiNode Nothing
    pure $ leftmost $ onNewEdit : onNumberedEdits
  switchHold never onEvAction


uiNode :: MonadWidget t m => Maybe (Dynamic t NodeRef) -> m (Event t (Maybe NodeRef))
uiNode nRef = do
  elClass "li" "table__row table__row_type_primary" $ do
    divClass "table__row-counter" blank
    divClass "table__cell_size_flex" $ do
      onVal <- traverse tagOnPostBuild nRef
      nodeInput <- uiInputElement $ def { _inputElementConfig_setValue = fmap renderNodeRef <$> onVal }
      let onInput = _inputElement_input nodeInput
      pure $ either (const Nothing) Just . parseNodeRef <$> onInput


unjoinList :: forall t m a. (Reflex t, MonadHold t m, MonadFix m) => Dynamic t [a] -> m (Dynamic t [Dynamic t a])
unjoinList l = do
    let
      withIndex = zip [1..] <$> l
      byIndex = fanInt $ IntMap.fromList <$> updated withIndex
    lengthChanges :: Dynamic t [(Int, a)] <- holdUniqDynBy (\xs ys -> length xs == length ys) withIndex

    let
      onNewList = pushAlways (traverse (holdIndexValue byIndex)) (updated lengthChanges)
    cInitList <- traverse (holdIndexValue byIndex) <=< sample $ current lengthChanges

    holdDyn cInitList onNewList

  where
    holdIndexValue :: forall m1. MonadHold t m1 => EventSelectorInt t a -> (Int, a) -> m1 (Dynamic t a)
    holdIndexValue (EventSelectorInt selectInt) (i, initial) = holdDyn initial $ selectInt i




uiLabeledRadioView
  :: (MonadWidget t m, Eq a)
  => (CssClass -> m (Element EventResult (DomBuilderSpace m) t))
  -> Dynamic t a
  -> a
  -> m (Event t a)
uiLabeledRadioView mkLabel val self  = do
    onRadioChange <- uiRadioElementView val self
    l <- mkLabel $ "label" <> "label_for_radio"
    let onLabelClick = domEvent Click l
    pure $ leftmost
      [ onRadioChange
      , fmapMaybe selectUnselected . tag (current val) $ onLabelClick
      ]
  where
    selectUnselected v = if v /= self then Just self else Nothing


uiRadioElementView :: (MonadWidget t m, Eq a) => Dynamic t a -> a -> m (Event t a)
uiRadioElementView val self = do
  v <- tagOnPostBuild val
  let
    cfg = def
      & initialAttributes .~ ("type" =: "radio" <> "class" =: "input input_type_radio")
      & inputElementConfig_setChecked .~ fmap (== self) v
  e <- uiInputElement cfg
  pure $ fmap (const self) . ffilter id $ _inputElement_checkedChange e

