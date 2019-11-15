{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Dialog for editing and selection of networks.
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.NetworkEdit
  ( uiNetworkEdit
  , uiNetworkStatus
  , uiNetworkSelect
  , queryNetworkStatus
  , NetworkStatus(..)
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad       (join, void)
import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as IntMap
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Reflex.Dom
import           Reflex.Extended
------------------------------------------------------------------------------
import           Common.Network      (renderNodeRef)
import           Frontend.Foundation
import           Frontend.Network
import           Frontend.UI.Modal
import           Frontend.UI.Widgets
------------------------------------------------------------------------------


type HasUiNetworkEditModel model t =
  (HasNetwork model t)

type HasUiNetworkEditModelCfg mConf t =
  ( Monoid mConf, Flattenable mConf t
  , HasNetworkCfg mConf t
  )


-- | Internal data type for manipulating networks, based on user edits.
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
  -> Event t () -> m (mConf, Event t ())
uiNetworkEdit m _onClose = do
  onClose <- modalHeader $ text "Network Settings"
  cfg <- modalMain $ do
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
        let newCfg = updateNetworks m $ (\n -> Map.insert n []) <$> onNewNet
        pure $ mconcat [ newCfg, editCfg ]

      pure $ selCfg <> editCfg

  modalFooter $ do
    -- TODO: Is this really a "Cancel" button?!
    onReset <- cancelButton def "Restore Defaults"
    onConfirm <- confirmButton def "Ok"

    pure
      ( cfg
          & networkCfg_resetNetworks .~ onReset
          & networkCfg_refreshModule .~ onConfirm
      , leftmost [onConfirm, onClose]
      )
  where
    checkNetName = getErr <$> m ^. network_networks
    getErr nets k =
      if Map.member (uncheckedNetworkName k) nets
         then Left "This network already exists."
         else mkNetworkName k


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
  -- event and the select element would display the wrong value (because it
  -- won't remember the previous valid, once it became invalid):
  onNetworkRefresh <- delay 0 $ tag (current selected) (updated networks)
  let
    onNetwork = leftmost [onNetworkDelayed, onNetworkRefresh]
  networkNames <- holdUniqDyn $ Map.keys <$> networks

  let
    cfg = SelectElementConfig "" (Just $ textNetworkName <$> onNetwork) $
      def & initialAttributes .~ "class" =: "select_type_primary select_width_full"
    itemDom v =
      elAttr "option" ("value" =: v) $ text v

  (s, ()) <- uiSelectElement cfg $
    void $ networkView $ traverse_ (itemDom . textNetworkName) <$> networkNames
  pure $ mempty & networkCfg_selectNetwork .~ fmap uncheckedNetworkName (_selectElement_change s)


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


-- | Overall `NetworkStatus` for a particular network.
data NetworkStatus =
    NetworkStatus_Good Text -- ^ All reachable nodes belong to the same network.
  | NetworkStatus_Invalid -- ^ Nodes coming from different networks.
  | NetworkStatus_Bad     -- ^ No node alive.

instance Semigroup NetworkStatus where
  s1 <> s2 = case (s1, s2) of
    (NetworkStatus_Good a ,  NetworkStatus_Good b) ->
      if a == b then NetworkStatus_Good a else NetworkStatus_Invalid
    (NetworkStatus_Invalid ,  _) -> NetworkStatus_Invalid
    (_ ,  NetworkStatus_Invalid) -> NetworkStatus_Invalid
    (NetworkStatus_Bad,  b) -> b
    (a,  NetworkStatus_Bad) -> a


-- | Render a single network.
--
--   Which is an accordion with header and line edits for each node.
uiNetwork
  :: MonadWidget t m
  => Dynamic t (Map NetworkName [NodeRef]) -- ^ All networks.
  -> NetworkName -- ^ The network currently being rendered.
  -> m (Event t NetworkAction)
uiNetwork networks self = do
    let
      nodes = fromMaybe [] . Map.lookup self <$> networks

    rec
      (onHeadAction, (onBodyAction, stats)) <- accordionItem' False "segment segment_type_secondary"
        (uiNetworkHeading self stats)
        (uiNodes nodes)

    pure $ leftmost
      [ onHeadAction
      , NetworkNode self <$> onBodyAction
      ]


-- | Display the heading of a `Network`.
--
--   This is an accordion with the `NetworkName` as heading, a delete button
--   and a status circle showing the network health.
uiNetworkHeading :: MonadWidget t m => NetworkName -> MDynamic t NetworkStatus -> m (Event t NetworkAction)
uiNetworkHeading self mStat = do
    text $ textNetworkName self
    uiNetworkStatus "accordion__collapsed-info table__row-right-aligned_type_primary" mStat
    fmap (const $ NetworkDelete self) <$> accordionDeleteBtn
  where
    accordionDeleteBtn = deleteButtonNaked $ def & uiButtonCfg_class .~ "accordion__title-button"


-- | Renders line edits for all nodes in a network.
--
--   Delivers change events and a `Dynamic` holding the current overall `NetworkStatus`.
uiNodes
  :: forall t m. MonadWidget t m
  => Dynamic t [NodeRef]
  -> m (Event t (Int, Maybe NodeRef), MDynamic t NetworkStatus)
uiNodes nodes = elClass "ol" "table table_type_primary" $ do

    -- Append node for new entry (`Nothing`):
    (initMap, onUpdate) <- getListUpdates $ (<> [Nothing]) . map Just <$> nodes

    (initialResp, onRespUpdate) <-
      traverseIntMapWithKeyWithAdjust (\_ -> uiNode) initMap onUpdate

    responses :: Dynamic t (IntMap (Event t (Maybe NodeRef), MDynamic t (Either Text NodeInfo))) <-
       incrementalToDynamic <$> holdIncremental initialResp onRespUpdate

    let
      onAction = switchDyn $ leftmost . toFunctorList . fmap fst <$> responses
      netState = calculateNetworkStatusDyn responses

    pure (onAction, netState)

  where

    calculateNetworkStatusDyn
      :: Dynamic t (IntMap (Event t (Maybe NodeRef), MDynamic t (Either Text NodeInfo)))
      -> MDynamic t NetworkStatus
    calculateNetworkStatusDyn responses =
      let
        dynStats :: Dynamic t (Dynamic t [Maybe (Either Text NodeInfo)])
        dynStats = sequence . IntMap.elems . fmap snd <$> responses

        stats :: Dynamic t [Maybe (Either Text NodeInfo)]
        stats = join dynStats
      in
        mconcat . map (fmap getNetworkStatus) <$> stats

toFunctorList :: forall f a. Functor f => IntMap (f a) -> [f (Int, a)]
toFunctorList = map (uncurry $ fmap . (,)) . IntMap.toList

getNetworkStatus :: Either Text NodeInfo -> NetworkStatus
getNetworkStatus = \case
    Left _ -> NetworkStatus_Bad
    Right i -> NetworkStatus_Good $ infoTitle i

-- | Render a line edit for a single node + `uiNodeStatus`.
uiNode
  :: MonadWidget t m
  => Dynamic t (Maybe NodeRef)
  -> m (Event t (Maybe NodeRef), MDynamic t (Either Text NodeInfo))
uiNode onVal = do
  pb <- getPostBuild

  elClass "li" "table__row table__row_type_primary" $ do
    divClass "table__row-counter" blank
    nodeInput <- divClass "table__cell table__cell_size_flex" $ uiInputElement $ def
      & inputElementConfig_setValue .~
        attachWith (\mv _ -> maybe "" renderNodeRef mv) (current onVal) pb
      & initialAttributes .~ mconcat
        [ "class" =: "input_width_full"
        , "placeholder" =: "Add node"
        ]
    let
      val = ffor (value nodeInput) $ \case
        t | T.null (T.strip t) -> Just Nothing
          | Right v <- parseNodeRefFull t -> Just (Just v)
          | otherwise -> Nothing
      onEdit = fmapMaybe id $ updated val
    stat <- uiNodeStatus "table__cell table__cell_size_tiny" $ join <$> val
    pure (onEdit, stat)

  where
    parseNodeRefFull r =
      let
        res = parseNodeRef r
      in
        if fmap renderNodeRef res == Right r
           then res
           else Left "Input could not be fully parsed"

queryNetworkStatus
  :: forall t m. MonadWidget t m
  => Dynamic t (Map NetworkName [NodeRef])
  -> Dynamic t NetworkName
  -> m (MDynamic t NetworkStatus)
queryNetworkStatus networks self = do
  let nodes = Map.findWithDefault [] <$> self <*> networks
  -- Append node for new entry (`Nothing`):
  (initMap, onUpdate) <- getListUpdates $ (<> [Nothing]) . map Just <$> nodes
  (initialResp, onRespUpdate) <-
    traverseIntMapWithKeyWithAdjust (\_ -> queryNodeStatus) initMap onUpdate
  responses :: Dynamic t (IntMap (MDynamic t (Either Text NodeInfo))) <-
      incrementalToDynamic <$> holdIncremental initialResp onRespUpdate
  pure $ fmap (mconcat . map (fmap getNetworkStatus)) $ join $ fmap (sequence . IntMap.elems) responses

queryNodeStatus
  :: MonadWidget t m
  => Dynamic t (Maybe NodeRef)
  -> m (Dynamic t (Maybe (Either Text NodeInfo)))
queryNodeStatus nodeRef = do
  pb <- getPostBuild
  mStatus <- throttle 2 $ leftmost [updated nodeRef, tag (current nodeRef) pb]
  onErrInfo <- performEventAsync $ getInfoAsync <$> mStatus
  holdDyn Nothing onErrInfo
  where
    getInfoAsync ref cb =
      void $ liftJSM $ forkJSM $ do
        r <- traverse discoverNode ref
        liftIO $ cb r


-- | Display status of a single node.
--
--   This is a circle, either not filled (no info yet) or red (something went
--   wrong) or green (everything is fine).
uiNodeStatus
  :: forall m t. MonadWidget t m
  => CssClass
  -> Dynamic t (Maybe NodeRef)
  -> m (MDynamic t (Either Text NodeInfo))
uiNodeStatus cls nodeRef = do
    errInfo <- queryNodeStatus nodeRef
    elKlass "div" ("signal" <> cls) $ do
      let attrs = buildStatusAttrs <$> errInfo
      elDynAttr "div" attrs blank
      pure errInfo
  where
    emptyAttrs = "class" =: "signal__circle"

    buildStatusAttrs :: Maybe (Either Text NodeInfo) -> (Map Text Text)
    buildStatusAttrs = \case
      Nothing -> emptyAttrs
      Just (Left err) ->
        "title" =: ("Invalid node: " <> err)
        <> "class" =: "signal__circle signal__circle_status_problem"
      Just (Right rT) ->
        "title" =: infoTitle rT
        <> "class" =: "signal__circle signal__circle_status_ok"


-- | Show a status circle for a whole network (red, yellow, green).
--
--   See `NetworkStatus` for details. `NetworkStatus_Invalid` is rendered as yellow.
uiNetworkStatus :: (DomBuilder t m, PostBuild t m) => Dynamic t CssClass -> MDynamic t NetworkStatus -> m ()
uiNetworkStatus cls mState = do
  elDynKlass "div" ("signal" <> cls) $ do
    elDynAttr "div" (buildStatusAttrs <$> mState) blank
  where
    emptyAttrs = "class" =: "signal__circle"

    buildStatusAttrs :: Maybe NetworkStatus -> (Map Text Text)
    buildStatusAttrs = \case
      Nothing -> emptyAttrs
      Just NetworkStatus_Bad ->
        "title" =: "No node available on this network"
        <> "class" =: "signal__circle signal__circle_status_problem"
      Just NetworkStatus_Invalid ->
        "title" =: "Network contains nodes from multiple networks"
        <> "class" =: "signal__circle signal__circle_status_warning"
      Just (NetworkStatus_Good msg) ->
        "title" =: msg
        <> "class" =: "signal__circle signal__circle_status_ok"


-- | Get some descriptive information (usable for a tooltip) from NodeInfo.
infoTitle :: NodeInfo -> Text
infoTitle info =
  case _nodeInfo_type info of
    NodeType_Pact v ->
      "Pact Network"
      <> "\nVersion: " <> v

    NodeType_Chainweb cwInfo ->
      "Chainweb Network"
      <> "\nVersion: " <> _chainwebInfo_version cwInfo
      <> "\nNetwork version: " <> _chainwebInfo_networkVersion cwInfo
      <> "\nNumber of chains: " <> tshow (_chainwebInfo_numberOfChains cwInfo)



