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
import Data.IntMap (IntMap)
import qualified Data.Text as T
import           Control.Lens
import           Control.Monad
import           Data.Bifunctor
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Text                      (Text)
import           Data.Void                      (Void)
import           Reflex.Extended
import           Reflex.Dom
import qualified Data.IntMap as IntMap
------------------------------------------------------------------------------
import           Frontend.Foundation
import           Frontend.Ide
import           Frontend.ModuleExplorer        (HasModuleExplorerCfg (..),
                                                 TransactionInfo (..))
import           Frontend.Network
import           Common.Network (renderNodeRef)
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


data NetworkAction =
      NetworkDelete NetworkName
    | NetworkSelect NetworkName
    | NetworkNode   NetworkName (Int, Maybe NodeRef)
      -- ^ Deletion or update of a node

makePrisms ''NetworkAction


-- | Confirmation dialog for deployments.
--
--   User can make sure to deploy to the right network, has the right keysets,
--   the right keys, ...
uiNetworkEdit
  :: forall t m model mConf.
    ( MonadWidget t m, HasUiNetworkEditModel model t, HasUiNetworkEditModelCfg mConf t
    )
  => model
  -> m (mConf, Event t ())
uiNetworkEdit m = do
  onClose <- modalHeader $ text "Networks"
  modalMain $ do
    cfg <- modalBody $
      divClass "group" $ do
        onNewNet <- validatedInputWithButton "group__header" checkNetName "Create new network." "Create"
        editCfg <- uiNetworks m
        let newCfg = updateNetworks m $ (\n -> Map.insert (NetworkName n) []) <$> onNewNet
        pure $ mconcat [ newCfg, editCfg ]

    modalFooter $ do
      -- TODO: Is this really a "Cancel" button?!
      onReset <- cancelButton def "Reset to Defaults"
      text " "
      onConfirm <- confirmButton def "Ok"

      pure (cfg & networkCfg_resetNetworks .~ onReset, leftmost [onConfirm, onClose])
  where
    checkNetName k = do
      keys <- sample $ current $ m ^. network_networks
      pure $ if Map.member (NetworkName k) keys then Just "This network already exists." else Nothing


uiNetworks
  :: forall t m model mConf.
    (MonadWidget t m, HasUiNetworkEditModel model t, HasUiNetworkEditModelCfg mConf t
    )
  => model -> m mConf
uiNetworks m = do
    let
      selName = fst <$> m ^. network_selectedNetwork
      networks = m ^. network_networks

    networkNames <- holdUniqDyn $ Map.keys <$> networks

    {- evEv <- networkView $ traverse (uiNetwork selName networks) <$> networkNames -}
    {- ev <- switchHold never $ leftmost <$> evEv -}

    -- Using above networkView causes spider to die, can be fixed with witgetHold and delay 0.
    -- TODO: Understand why this is needed. Obviously it does not like having
    -- parts of the widget getting updated while the widget is already being
    -- deleted ... but why?
    onNetworkNames <- delay 0 =<< tagOnPostBuild networkNames
    dynEv <- networkHold (pure []) $ traverse (uiNetwork selName networks) <$> onNetworkNames
    let ev = switchDyn $ leftmost <$> dynEv

    let
      selCfg =  mempty & networkCfg_selectNetwork .~ fmapMaybe (^? _NetworkSelect) ev
      netsCfg = updateNetworks m $ leftmost
        [ Map.delete <$> fmapMaybe (^? _NetworkDelete) ev
        , applyNodeUpdate <$> fmapMaybe (^? _NetworkNode) ev
        ]
    pure $ mconcat [selCfg, netsCfg]

  where

    applyNodeUpdate (netName, (i, mRef)) nets =
      nets & at netName . _Just %~ updateNodes i mRef

    updateNodes :: Int -> Maybe NodeRef -> [NodeRef] -> [NodeRef]
    updateNodes i mRef =
      catMaybes
      . imap (\ci old -> if ci == i then mRef else old)
      . (<> [Nothing]) -- So any new row will be handled properly
      . map Just


uiNetwork
  :: MonadWidget t m
  => Dynamic t NetworkName -- ^ Currently selected network
  -> Dynamic t (Map NetworkName [NodeRef]) -- ^ All networks.
  -> NetworkName -- ^ The network currently being rendered.
  -> m (Event t NetworkAction)
uiNetwork selected networks self = do
    let
      nodes = fromMaybe [] . Map.lookup self <$> networks

    (onHeadAction, onBodyAction) <- accordionItem' False "segment segment_type_secondary"
      (uiNetworkSelector selected self)
      (uiNodes nodes)

    pure $ leftmost
      [ onHeadAction
      , NetworkNode self <$> onBodyAction
      ]


uiNetworkSelector :: MonadWidget t m => Dynamic t NetworkName -> NetworkName -> m (Event t NetworkAction)
uiNetworkSelector val self = do
  onSelect <- fmap NetworkSelect <$> uiLabeledRadioView label val self
  onDelete <- fmap (const $ NetworkDelete self) <$> accordionDeleteBtn
  pure $ leftmost [ onSelect, onDelete ]
  where
    accordionDeleteBtn = deleteButtonNaked $ def & uiButtonCfg_class .~ "accordion__title-button"
    label cls = fmap fst . elKlass' "span" ("heading_type_h2" <> cls) $ text (textNetworkName self)


uiNodes :: forall t m. MonadWidget t m => Dynamic t [NodeRef] -> m (Event t (Int, Maybe NodeRef))
uiNodes nodes = elClass "ol" "table table_type_primary" $ do

    -- Append node for new entry (`Nothing`):
    (initMap, onUpdate) <- getListUpdates $ (<> [Nothing]) . map Just <$> nodes

    (initialResp, onRespUpdate) <-
      traverseIntMapWithKeyWithAdjust (\_ -> uiNode) initMap onUpdate

    responses :: Dynamic t (IntMap (Event t (Maybe NodeRef))) <-
       incrementalToDynamic <$> holdIncremental initialResp onRespUpdate
    pure $ switchDyn $ leftmost . toFunctorList <$> responses

  where

    toFunctorList :: forall f a. Functor f => IntMap (f a) -> [f (Int, a)]
    toFunctorList = map (uncurry $ fmap . (,)) . IntMap.toList


uiNode :: MonadWidget t m => (Dynamic t (Maybe NodeRef)) -> m (Event t (Maybe NodeRef))
uiNode nRef = do
  elClass "li" "table__row table__row_type_primary" $ do
    divClass "table__row-counter" blank
    divClass "table__cell_size_flex" $ do
      onVal <- tagOnPostBuild nRef
      nodeInput <- uiInputElement $ def & inputElementConfig_setValue .~ (maybe "" renderNodeRef <$> onVal)
      let
        onInput = _inputElement_input nodeInput
        onUpdate = fmapMaybe id $ either (const Nothing) Just . parseNodeRef <$> onInput
        onDelete = ffilter (T.null . T.strip) onInput
      pure $ leftmost
        [ Nothing <$ onDelete
        , Just <$> onUpdate
        ]


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

