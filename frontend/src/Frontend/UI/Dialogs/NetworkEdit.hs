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
import           Control.Lens
import           Data.IntMap                    (IntMap)
import qualified Data.IntMap                    as IntMap
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import qualified Data.Text                      as T
import           Data.Text                      (Text)
import qualified Data.Text.IO as T
import           Reflex.Dom
import           Reflex.Extended
import           Control.Monad (void)
------------------------------------------------------------------------------
import           Common.Network                 (renderNodeRef)
import           Frontend.Foundation
import           Frontend.Network
import           Frontend.UI.Modal
import           Frontend.UI.Widgets
import           Frontend.Network.NodeInfo
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
  onClose <- modalHeader $ text "Network Settings"
  modalMain $ do
    cfg <- modalBody $ do
      selCfg <- uiGroup "segment" $ do
        uiGroupHeader mempty $ do
          elClass "h2" "heading heading_type_h2" $
            text "Select Network"
        uiNetworkSelect m

      editCfg <- uiGroup "segment" $ do
        uiGroupHeader mempty $ elClass "h2" "heading heading_type_h2" $
          text "Edit Networks"
        onNewNet <- validatedInputWithButton "group__header" checkNetName "Create new network." "Create"
        editCfg <- uiNetworks m
        let newCfg = updateNetworks m $ (\n -> Map.insert (NetworkName n) []) <$> onNewNet
        pure $ mconcat [ newCfg, editCfg ]

      pure $ selCfg <> editCfg

    modalFooter $ do
      -- TODO: Is this really a "Cancel" button?!
      onReset <- cancelButton def "Restore Defaults"
      text " "
      onConfirm <- confirmButton def "Ok"

      pure
        ( cfg
           & networkCfg_resetNetworks .~ onReset
           & networkCfg_refreshModule .~ onConfirm
        , leftmost [onConfirm, onClose]
        )
  where
    checkNetName k = do
      keys <- sample $ current $ m ^. network_networks
      pure $ if Map.member (NetworkName k) keys
                then Just "This network already exists."
                else Nothing


uiNetworkSelect
  :: forall t m model mConf.
    (MonadWidget t m, HasUiNetworkEditModel model t, HasUiNetworkEditModelCfg mConf t
    )
  => model -> m mConf
uiNetworkSelect m = do
  selected <- holdUniqDyn $ m ^. network_selectedNetwork
  onNetworkDirect <- tagOnPostBuild selected
  -- Delay necessary until we have mount hooks. (SelectElement won't accept
  -- setting event until its children are properly rendered.)
  onNetworkDelayed <- delay 0 onNetworkDirect
  let
    networks = m ^. network_networks
  -- The refresh is necessary, because we currently always have a valid
  -- selection even if there are no networks. Now if a previously invalid
  -- network becomes valid - selected is not updated, therefore we have no
  -- event and the select element would display the wrong value (because it won't remember the previous valid once it became invalid):
  onNetworkRefresh <- delay 0 $ tag (current selected) (updated networks)
  let
    onNetwork = leftmost [onNetworkDelayed, onNetworkRefresh]
  networkNames <- holdUniqDyn $ Map.keys <$> networks

  let
    cfg = SelectElementConfig "" (Just $ textNetworkName <$> onNetwork) $
      def & initialAttributes .~ "class" =: "select_type_primary select_width_full"
    itemDom v = do
      elAttr "option" ("value" =: v) $ text v
      liftIO $ T.putStrLn $ "Network: " <> v

  (s, ()) <- uiSelectElement cfg $
    void $ networkView $ traverse_ (itemDom . textNetworkName) <$> networkNames
  pure $ mempty & networkCfg_selectNetwork .~ fmap NetworkName (_selectElement_change s)


uiNetworks
  :: forall t m model mConf.
    (MonadWidget t m, HasUiNetworkEditModel model t, HasUiNetworkEditModelCfg mConf t
    )
  => model -> m mConf
uiNetworks m = do
    let
      networks = m ^. network_networks

    networkNames <- holdUniqDyn $ Map.keys <$> networks

    {- evEv <- networkView $ traverse (uiNetwork selName networks) <$> networkNames -}
    {- ev <- switchHold never $ leftmost <$> evEv -}

    -- Using above networkView causes spider to die, can be fixed with witgetHold and delay 0.
    -- TODO: Understand why this is needed. Obviously it does not like having
    -- parts of the widget getting updated while the widget is already being
    -- deleted ...
    onNetworkNames <- delay 0 =<< tagOnPostBuild networkNames
    dynEv <- networkHold (pure []) $ traverse (uiNetwork networks) <$> onNetworkNames
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
  => Dynamic t (Map NetworkName [NodeRef]) -- ^ All networks.
  -> NetworkName -- ^ The network currently being rendered.
  -> m (Event t NetworkAction)
uiNetwork networks self = do
    let
      nodes = fromMaybe [] . Map.lookup self <$> networks

    (onHeadAction, onBodyAction) <- accordionItem' False "segment segment_type_secondary"
      (uiNetworkHeading self)
      (uiNodes nodes)

    pure $ leftmost
      [ onHeadAction
      , NetworkNode self <$> onBodyAction
      ]


uiNetworkHeading :: MonadWidget t m => NetworkName -> m (Event t NetworkAction)
uiNetworkHeading self = do
    text $ textNetworkName self
    fmap (const $ NetworkDelete self) <$> accordionDeleteBtn
  where
    accordionDeleteBtn = deleteButtonNaked $ def & uiButtonCfg_class .~ "accordion__title-button"


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


uiNode :: MonadWidget t m => Dynamic t (Maybe NodeRef) -> m (Event t (Maybe NodeRef))
uiNode nRef = do
  onVal <- tagOnPostBuild nRef

  elClass "li" "table__row table__row_type_primary" $ do
    divClass "table__row-counter" blank
    onEdit <- divClass "table__cell table__cell_size_flex" $ do
      let placeholderVal = maybe (Just "Add node") (const Nothing) <$> onVal
      nodeInput <- uiInputElement $ def
        & inputElementConfig_setValue .~ (maybe "" renderNodeRef <$> onVal)
        & initialAttributes .~ "class" =: "input_width_full"
        & modifyAttributes .~ fmap ("placeholder" =:) placeholderVal
      let
        onInput = _inputElement_input nodeInput
        onUpdate = fmapMaybe id $ either (const Nothing) Just . parseNodeRefFull <$> onInput
        onDelete = ffilter (T.null . T.strip) onInput
      pure $ leftmost
        [ Nothing <$ onDelete
        , Just <$> onUpdate
        ]
    uiNodeStatus "table__cell table__cell_size_tiny" onVal
    pure onEdit

  where
    parseNodeRefFull r =
      let
        res = parseNodeRef r
      in
        if fmap renderNodeRef res == Right r
           then res
           else Left "Input could not be fully parsed"


uiNodeStatus :: forall m t. MonadWidget t m => CssClass -> Event t (Maybe NodeRef) -> m ()
uiNodeStatus cls unthrottled = do
    mStatus <- throttle 2 unthrottled
    elKlass "div" ("signal" <> cls) $ do
      onAttrs <- performEventAsync $ buildStatusAttrsAsync <$> mStatus
      attrs :: Dynamic t (Map T.Text T.Text) <- holdDyn mempty onAttrs
      elDynAttr "div" attrs blank
  where
    buildStatusAttrsAsync ref cb =
      void $ forkJSM $ do
        jsm <- askJSM
        r <- buildStatusAttrs ref `runJSM` jsm
        liftIO $ cb r

    buildStatusAttrs :: Maybe NodeRef -> JSM (Map Text Text)
    buildStatusAttrs = \case
      Nothing -> pure $ "class" =: "signal__circle"
      Just r -> do
        er <- discoverNode r
        pure $ case er of
          Left err ->
            "title" =: ("Invalid node: " <> err)
            <> "class" =: "signal__circle signal__circle_status_problem"
          Right r ->
            "title" =: infoTitle r
            <> "class" =: "signal__circle signal__circle_status_ok"

    infoTitle :: NodeInfo -> Text
    infoTitle info =
      case _nodeInfo_type info of
        NodeType_Pact -> "Pact Node"
        NodeType_Chainweb cwInfo ->
          "Chainweb Node"
          <> "\nVersion: " <> _chainwebInfo_version cwInfo
          <> "\nNetwork version: " <> _chainwebInfo_networkVersion cwInfo
          <> "\nNumber of chains: " <> tshow (_chainwebInfo_numberOfChains cwInfo)



