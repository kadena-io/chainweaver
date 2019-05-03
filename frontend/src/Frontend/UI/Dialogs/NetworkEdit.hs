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

      pure (cfg, never)
  where
    checkNetName k = do
      keys <- sample $ current $ m ^. network_networks
      pure $ if Map.member (NetworkName k) keys then Just "This network already exists." else Nothing

uiNetworkHeader
  :: (MonadWidget t m, HasUiNetworkEditModel model t, HasUiNetworkEditModelCfg mConf t)
  => model -> m mConf
uiNetworkHeader m = do
    let
      networkNames = Map.keys <$> m ^. network_networks
      selName = fst <$> m ^. network_selectedNetwork
    evEv <- networkView $ traverse (mkEdit selName) <$> networkNames
    ev <- switchHold never $ leftmost <$> evEv
    pure $ mempty & networkCfg_selectNetwork .~ ev
  where
    mkEdit selected n = fmap fst $ accordionItem' False mempty (uiNetworkSelector selected n) (text "Body")


uiNetworkSelector :: MonadWidget t m => Dynamic t NetworkName -> NetworkName -> m (Event t NetworkName)
uiNetworkSelector val self = uiLabeledRadioView label val self
  where
    label cls = fmap fst . elKlass' "span" ("heading_type_h2" <> cls) $ text (textNetworkName self)


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

