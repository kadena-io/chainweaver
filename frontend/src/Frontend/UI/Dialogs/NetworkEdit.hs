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
import           Control.Monad       (join, void, (<=<), guard)
import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as IntMap
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Reflex.Dom
import           Reflex.Extended
------------------------------------------------------------------------------
import           Common.Network
import           Frontend.Foundation
import           Frontend.Network
import           Frontend.UI.Modal
import           Frontend.UI.Widgets
import           Frontend.UI.Widgets.Helpers (dialogSectionHeading)
------------------------------------------------------------------------------

type HasUiNetworkEditModel model t =
  (HasNetwork model t)

type HasUiNetworkEditModelCfg mConf t =
  ( Monoid mConf, Flattenable mConf t
  , HasNetworkCfg mConf t
  )


-- | Internal data type for manipulating networks, based on user edits.
data NetworkAction
  = NetworkDelete NetworkName
  | NetworkNode NetworkName [NodeRef]
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
  (dNew, cfg) <- modalMain $ do
      selCfg <- uiGroup "segment" $ do
        uiGroupHeader mempty $ do
          dialogSectionHeading mempty "Select Network"
        uiNetworkSelect "select_type_primary select_width_full" m

      (dNew, editCfg) <- uiGroup "segment" $ do
        uiGroupHeader mempty $ dialogSectionHeading mempty "Edit Networks"
        onNewNet <- validatedInputWithButton "group__header" checkNetName "Create new network." "Create"
        dSelectNewNet <- holdDyn Nothing $ Just <$> onNewNet
        editCfg <- uiNetworks m
        let newCfg = updateNetworks m $ (\n -> Map.insert n []) <$> onNewNet

        pure $ (dSelectNewNet, mconcat [ newCfg, editCfg ])

      pure $ (dNew, selCfg <> editCfg)

  let
    dSelectNewest = (\nets mnet -> do
        net <- mnet
        nodes <- Map.lookup net nets
        guard $ not $ null nodes
        pure net
      )
      <$> (m ^. network_networks)
      <*> dNew

  modalFooter $ do
    -- TODO: Is this really a "Cancel" button?!
    onReset <- cancelButton def "Restore Defaults"
    onConfirm <- confirmButton def "Ok"

    pure
      ( cfg
          & networkCfg_resetNetworks .~ onReset
          & networkCfg_refreshModule .~ onConfirm
          & networkCfg_selectNetwork .~ tagMaybe (current dSelectNewest) onConfirm
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
  => Text -> model -> m mConf
uiNetworkSelect cls m = do
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
      def & initialAttributes .~ "class" =: cls
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
  let networks = m ^. network_networks
  evEv :: Dynamic t (Map NetworkName (Event t NetworkAction))
    <- listWithKey networks $ \networkName nodes -> do
      rec
        (deletions, (nodeUpdates, status)) <- accordionItem' False "segment segment_type_secondary"
          (uiNetworkHeading networkName status)
          (uiNodes nodes)
      pure $ leftmost
        [ deletions
        , NetworkNode networkName <$> updated nodeUpdates
        ]

  let ev = switch $ fmap (leftmost . Map.elems) $ current evEv

  pure $ mconcat
    [ updateNetworks m $ leftmost
      [ Map.delete <$> fmapMaybe (^? _NetworkDelete) ev
      , applyNodeUpdate <$> fmapMaybe (^? _NetworkNode) ev
      ]
    ]

  where

    applyNodeUpdate (netName, ns) nets =
      nets & at netName . _Just .~ ns


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


-- | Display the heading of a `Network`.
--
--   This is an accordion with the `NetworkName` as heading, a delete button
--   and a status circle showing the network health.
uiNetworkHeading :: MonadWidget t m => NetworkName -> MDynamic t NetworkStatus -> m (Event t NetworkAction)
uiNetworkHeading self mStat = do
    accordionHeaderBtn $ textNetworkName self
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
  -> m (Dynamic t [NodeRef], Dynamic t (Maybe NetworkStatus))
uiNodes nodes = elClass "ol" "table table_type_primary" $ do
  pb <- getPostBuild
  -- Build a patch which adds all the nodes, plus an extra empty node input
  let mkInitialNodes = PatchIntMap . fmap Just . IntMap.fromList . zip [0..] . (<> [Nothing]) . fmap Just
  rec
    responses <- uncurry (foldDyn applyAlways) <=<
      traverseIntMapWithKeyWithAdjust (const uiNode) IntMap.empty $ leftmost
        [ switch $ actionToPatch . fmap (view _1) <$> current responses
        , attachWith (const . mkInitialNodes) (current nodes) pb
        ]
  let updates = calculateUpdates $ fmap (view _2) <$> responses
      netState = calculateNetworkStatusDyn $ fmap (view _3) <$> responses

  pure (updates, netState)

  where
    calculateNetworkStatusDyn
      :: Dynamic t (IntMap (MDynamic t (Either Text NodeInfo)))
      -> Dynamic t (Maybe NetworkStatus)
    calculateNetworkStatusDyn responses =
      let stats :: Dynamic t [Maybe (Either Text NodeInfo)]
          stats = join $ sequenceA . IntMap.elems <$> responses
      in foldMap (fmap getNetworkStatus) <$> stats

    calculateUpdates :: Dynamic t (IntMap (Dynamic t (Maybe NodeRef))) -> Dynamic t [NodeRef]
    calculateUpdates r = join $ fmap catMaybes . sequenceA . IntMap.elems <$> r

    nextKey = maybe 0 (succ . fst) . IntMap.lookupMax

    actionToPatch :: IntMap (Event t NodeAction) -> Event t (PatchIntMap (Maybe NodeRef))
    actionToPatch m = mconcat $ fmap (handleNodeAction $ nextKey m) $ IntMap.toList m

    handleNodeAction :: Int -> (Int, Event t NodeAction) -> Event t (PatchIntMap (Maybe NodeRef))
    handleNodeAction s (i, e) = ffor e $ PatchIntMap . \case
      NodeAction_Deleted -> IntMap.singleton i Nothing
      NodeAction_Dirtied -> IntMap.singleton s $ Just Nothing

getNetworkStatus :: Either Text NodeInfo -> NetworkStatus
getNetworkStatus = \case
    Left _ -> NetworkStatus_Bad
    Right i -> NetworkStatus_Good $ infoTitle i

-- | Individual nodes can be deleted, or can cause the creation of an extra node
-- input.
data NodeAction
  = NodeAction_Deleted
  | NodeAction_Dirtied
  deriving Show

-- | Render a line edit for a single node + `uiNodeStatus`.
uiNode
  :: MonadWidget t m
  => Maybe NodeRef
  -> m (Event t NodeAction, Dynamic t (Maybe NodeRef), MDynamic t (Either Text NodeInfo))
uiNode initVal = do
  let
    uiNodeInput cfg = do
      ie <- uiInputElement cfg
      pure (ie, parseNodeRefFull . T.strip <$> _inputElement_input ie)

    showNodePopover =
      pure . fmap (either PopoverState_Error (const PopoverState_Disabled)) . snd

  elClass "li" "table__row table__row_type_primary" $ do
    divClass "table__row-counter" blank
    (nodeInput, _) <- divClass "table__cell table__cell_size_flex" $
      uiInputWithPopover uiNodeInput (_inputElement_raw . fst) showNodePopover $ def
        & inputElementConfig_initialValue .~ maybe "" renderNodeRef initVal
        & initialAttributes .~ mconcat
          [ "class" =: "input_width_full"
          , "placeholder" =: "Add node"
          ]
    let checkVal = \case
          t | T.null (T.strip t) -> Just Nothing
            | Right v <- parseNodeRefFull (T.strip t) -> Just (Just v)
            | otherwise -> Nothing
        checked = checkVal <$> value nodeInput
    stat <- uiNodeStatus "table__cell table__cell_size_tiny" $ join <$> checked
    -- Determine if we should take an action which deletes this node or adds a
    -- new one
    let mkAction isClean t
          | T.null t = (Just False, Just NodeAction_Deleted)
          | isClean = (Just False, Just NodeAction_Dirtied)
          | otherwise = (Nothing, Nothing)
    action <- mapAccumMaybe_ mkAction (isNothing initVal) $ _inputElement_input nodeInput
    pure (action, join <$> checked, stat)

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
  pure $ fmap (mconcat . map (fmap getNetworkStatus)) $ join $ fmap (sequenceA . IntMap.elems) responses

queryNodeStatus
  :: MonadWidget t m
  => Dynamic t (Maybe NodeRef)
  -> m (Dynamic t (Maybe (Either Text NodeInfo)))
queryNodeStatus nodeRef = do
  pb <- getPostBuild
  nodeUpdated <- debounce 0.5 $ updated nodeRef
  mStatus <- throttle 2 $ leftmost
    [ nodeUpdated
    , tag (current nodeRef) pb
    ]
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
      <> "\nNumber of chains: " <> tshow (length $ _chainwebInfo_chainIds cwInfo)
