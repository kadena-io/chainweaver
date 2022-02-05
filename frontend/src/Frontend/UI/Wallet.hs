{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Wallet management ui for handling private/public keys.
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.UI.Wallet
  ( -- * Key management widget
    uiWallet
  , uiAccountsTable
  , uiAvailableKeys
  , uiWalletRefreshButton
  , uiWatchRequestButton
  , uiGenerateKeyButton
    -- ** Filters for keys
  , hasPrivateKey
  , HasUiWalletModelCfg
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad               (when, (<=<))
import           Control.Error               (headMay)
import qualified Data.IntMap                 as IntMap
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Obelisk.Generated.Static
import           Obelisk.Route.Frontend
import           Reflex
import           Reflex.Dom hiding (Key)
import           Text.Read
------------------------------------------------------------------------------
import qualified Pact.Types.Pretty as Pact
import qualified Pact.Types.Term   as Pact
------------------------------------------------------------------------------
import           Common.Route
import           Frontend.Log (HasLogger, HasLogCfg)
import           Frontend.Crypto.Class
import           Frontend.Crypto.Ed25519     (keyToText)
import           Frontend.Wallet
import           Frontend.UI.Widgets
import           Frontend.Foundation
import           Frontend.JsonData (HasJsonData, HasJsonDataCfg)
import           Frontend.TxBuilder
import           Frontend.UI.Dialogs.AccountDetails
import           Frontend.UI.Dialogs.KeyDetails (uiKeyDetails)
import           Frontend.UI.Dialogs.Receive (uiReceiveModal)
import           Frontend.UI.Dialogs.WatchRequest (uiWatchRequestDialog)
-- import           Frontend.UI.Dialogs.Send (uiSendModal)
import           Frontend.UI.KeysetWidget
import           Frontend.UI.Modal
import           Frontend.Network
------------------------------------------------------------------------------

-- | Constraints on the model config we have for implementing this widget.
type HasUiWalletModelCfg model mConf key m t =
  ( Monoid mConf
  , Flattenable mConf t
  , IsWalletCfg mConf key t
  , HasModalCfg mConf (Modal mConf m t) t
  , HasWalletCfg (ModalCfg mConf t) key t
  , Flattenable (ModalCfg mConf t) t
  , Monoid (ModalCfg mConf t)
  , HasNetwork model t
  , HasWallet model key t
  , HasCrypto key m
  , HasCrypto key (Performable m)
  , HasJsonData model t
  , HasNetworkCfg (ModalCfg mConf t) t
  , HasJsonDataCfg (ModalCfg mConf t) t
  , HasLogger model t
  , HasLogCfg mConf t
  )

-- | Possible actions from an account
data AccountDialog
  = AccountDialog_DetailsChain (AccountName, ChainId, AccountDetails, Account)
  | AccountDialog_Details AccountName (Maybe AccountNotes)
  | AccountDialog_Receive AccountName ChainId (Maybe AccountDetails)
  | AccountDialog_TransferTo AccountName AccountDetails ChainId

  -- AccountDialog_Send (AccountName, ChainId, AccountDetails) (Maybe UnfinishedCrossChainTransfer)

uiWalletRefreshButton
  :: (MonadWidget t m, Monoid mConf, HasWalletCfg mConf key t)
  => m mConf
uiWalletRefreshButton = do
  eRefresh <- uiButton headerBtnCfg (text "Refresh")
  pure $ mempty & walletCfg_refreshBalances <>~ eRefresh

uiWatchRequestButton
  :: ( MonadWidget t m
     , Monoid mConf
     , Monoid (ModalCfg mConf t)
     , Flattenable (ModalCfg mConf t) t
     , HasModalCfg mConf (Modal mConf m t) t
     , HasNetwork model t
     )
  => model -> m mConf
uiWatchRequestButton model = do
  watch <- uiButton headerBtnCfg (text "Check Tx Status")
  pure $ mempty & modalCfg_setModal .~ (Just (uiWatchRequestDialog model) <$ watch)

-- | UI for managing the keys wallet.
uiWallet
  :: forall m t key model mConf
     . ( MonadWidget t m
       , HasUiWalletModelCfg model mConf key m t
       , SetRoute t (R FrontendRoute) m
       )
  => model
  -> m mConf
uiWallet = uiAvailableKeys

----------------------------------------------------------------------
-- Keys related helper widgets:
----------------------------------------------------------------------

-- | Check whether a given key does contain a private key.
hasPrivateKey :: (Text, KeyPair PrivateKey) -> Bool
hasPrivateKey = isJust . _keyPair_privateKey . snd

----------------------------------------------------------------------

uiAccountsTable
  :: forall t m model mConf key.
  (MonadWidget t m, HasUiWalletModelCfg model mConf key m t, HasTransactionLogger m, (SetRoute t (R FrontendRoute) m))
  => model -> Dynamic t (Maybe AccountName) -> m mConf
uiAccountsTable model dStartOpen = do
  let net = model ^. network_selectedNetwork
      networks = model ^. wallet_accounts
  mAccounts <- maybeDyn $ ffor2 net networks $ \n (AccountData m) -> case Map.lookup n m of
    Just accs | not (Map.null accs) -> Just accs
    _ -> Nothing
  flatten <=< dyn $ ffor mAccounts $ \case
    Nothing -> uiEmptyState $(static "img/menu/wallet.svg") "No Accounts Found" $ do
      el "p" $ do
        text "Create new Accounts or interact with existing Accounts by selecting the "
        el "strong" $ text "+ Add Account"
        text " button."
      pure mempty
    Just m -> divClass "wallet__keys-list" $ uiAccountItems model m dStartOpen


uiAccountItems
  :: forall t m model mConf key.
  (MonadWidget t m, HasUiWalletModelCfg model mConf key m t, HasTransactionLogger m, (SetRoute t (R FrontendRoute) m))
  => model -> Dynamic t (Map AccountName (AccountInfo Account)) -> Dynamic t (Maybe AccountName) -> m mConf
uiAccountItems model accountsMap dStartOpen = do
  let net = model ^. network_selectedNetwork
      tableAttrs = mconcat
        [ "style" =: "table-layout: fixed; width: 98%"
        , "class" =: "wallet table"
        ]

  events <- elAttr "table" tableAttrs $ do
    el "colgroup" $ do
      elAttr "col" ("style" =: "width: 25px") blank
      elAttr "col" ("style" =: "width: 40px") blank
      elAttr "col" ("style" =: "width: 200px") blank
      elAttr "col" ("style" =: "width: 80px") blank
      elAttr "col" ("style" =: "width: 180px") blank
      elAttr "col" ("style" =: "width: 15%") blank
      elAttr "col" ("style" =: "width: 200px") blank
      elAttr "col" ("style" =: "width: 25%") blank

    el "thead" $ el "tr" $ do
      let mkHeading = elClass "th" "wallet__table-heading" . text
      traverse_ mkHeading $
        [ ""
        , ""
        , "Account Name"
        , "Owner"
        , "Keyset Info"
        , "Notes"
        , "Balance (KDA)"
        , ""
        ]

    el "tbody" $ do
      let cwKeys = model ^. wallet_keys
          dMapAndOpenAcc = (,) <$> accountsMap <*> dStartOpen
          startsOpen = ffor dMapAndOpenAcc $ \(m, open) name -> Map.size m == 1 || (Just name == open)
      events <- listWithKey accountsMap (uiAccountItem cwKeys startsOpen)
      dyn_ $ ffor accountsMap $ \accs ->
        when (null accs) $
          elClass "tr" "wallet__table-row" $ elAttr "td" ("colspan" =: "5" <> "class" =: "wallet__table-cell") $
            text "No accounts ..."
      pure events

  let
    onAccountModal = switchDyn $ leftmost . Map.elems <$> events

    accModal n = Just . \case
      AccountDialog_Details acc notes -> uiAccountDetails n acc notes
      AccountDialog_DetailsChain acc -> uiAccountDetailsOnChain n acc
      AccountDialog_Receive name chain details -> uiReceiveModal "Receive" model name chain details
      AccountDialog_TransferTo name details chain -> uiReceiveModal "Transfer To" model name chain (Just details)
      -- AccountDialog_Send acc mucct -> uiSendModal model acc mucct

  refresh <- delay 1 =<< getPostBuild

  pure $ mempty
    & modalCfg_setModal .~ attachWith accModal (current net) onAccountModal
    & walletCfg_refreshBalances .~ refresh

-- | This function only exists to workaround a reflex-dom Adjustable bug.
-- It should be removed immediately when that bug is fixed.
accursedUnutterableListWithKey
  :: forall t k v m a. (Eq v, NotReady t m, Ord k, Adjustable t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Dynamic t (Map k a))
accursedUnutterableListWithKey dMap f = do
  dm <- holdUniqDyn dMap
  holdDyn mempty <=< dyn $ ffor dm $ \m -> Map.traverseWithKey (\k -> f k . pure) m

data AccountOwnership = SoleOwner | JointOwner | NotOwner
  deriving (Eq,Ord,Show,Read,Enum)

ownershipText :: AccountOwnership -> Text
ownershipText SoleOwner = "yes"
ownershipText JointOwner = "joint" -- shared?
ownershipText NotOwner = "no" -- other?

getAccountOwnership
  :: Reflex t
  => Dynamic t (KeyStorage key)
  -> Dynamic t (Maybe AccountDetails)
  -> Dynamic t (Maybe AccountOwnership)
getAccountOwnership dcwKeys dacctDetails = do
  cwKeys <- dcwKeys
  mad <- dacctDetails

  case mad of
    Nothing -> pure Nothing
    Just acctDetails -> do
      let cwKeySet = Set.fromList $ map (_keyPair_publicKey . _key_pair) $ IntMap.elems cwKeys
      pure $ Just $ case acctDetails ^? accountDetails_guard . _AccountGuard_KeySetLike of
        -- Keys can't own the non-keyset guards with the exception of GKeySetRef
        -- which we're not going to worry about for now. Erroneously flagging an
        -- account as NotOwner is less potentially damaging than erroneously
        -- flagging it as owned.
        Nothing -> NotOwner
        Just (KeySetHeritage acctKeys pred' _ref) -> do
          let numAcctKeys = Set.size acctKeys
              controlCount = case pred' of
                "keys-any" -> 1
                "keys-2" -> 2
                "keys-all" -> numAcctKeys
              calcOwnership cwks =
                  if numGoodKeys == 0
                     then NotOwner
                     else if numGoodKeys == numAcctKeys
                             then SoleOwner
                             else JointOwner
                where
                  numGoodKeys = Set.size (Set.intersection acctKeys cwks)
          calcOwnership cwKeySet

padChainId :: Int -> ChainId -> ChainId
padChainId n (ChainId c) =
  case readMaybe (T.unpack c) :: Maybe Int of
    Nothing -> ChainId c
    Just _ -> ChainId $ T.replicate (n - T.length c) "0" <> c

unPadChainId :: ChainId -> ChainId
unPadChainId (ChainId c) = if T.length c <= 1 then ChainId c else
  case headMay (T.unpack c) of
    -- recurse for when we have padded chain-lengths > 3
    -- Ex: 0001
    Just '0' -> unPadChainId $ ChainId $ T.tail c
    _ -> ChainId c

uiAccountItem
  :: forall key t m. MonadWidget t m
  => Dynamic t (KeyStorage key)
  -> Dynamic t (AccountName -> Bool)
  -> AccountName
  -> Dynamic t (AccountInfo Account)
  -> m (Event t AccountDialog)
uiAccountItem cwKeys startsOpen name accountInfo = do
  let chainMap = _accountInfo_chains <$> accountInfo

      -- Chains get sorted in text order without padding is wrong for more than 10 chains
      padKeys m = Map.mapKeys (padChainId maxKeyLen) m
        where
          maxKeyLen = maximum $ map (T.length . _chainId) $ Map.keys m
      orderedChainMap = padKeys <$> chainMap

      notes = _accountInfo_notes <$> accountInfo
  rec
    (clk, dialog) <- keyRow visible notes $ ffor balances $ \xs0 -> case catMaybes xs0 of
      [] -> "Does not exist"
      xs -> uiAccountBalance False $ Just $ sum xs

    v0 <- sample $ current $ ($ name) <$> startsOpen
    visible <- toggle v0 clk
    results <- accursedUnutterableListWithKey orderedChainMap $ accountRow visible

    let balances :: Dynamic t [Maybe AccountBalance]
        balances = fmap Map.elems $ joinDynThroughMap $ (fmap . fmap) fst results
  let dialogs = switch $ leftmost . fmap snd . Map.elems <$> current results
  pure $ leftmost [dialog, dialogs]
  where
  trKey = elClass "tr" "wallet__table-row wallet__table-row-account"
  trAcc = elClass "tr" "wallet__table-row"
  td = td' ""
  td' extraClass = elClass "td" ("wallet__table-cell" <> extraClass)
  buttons = divClass "wallet__table-buttons"
  cfg = def
    & uiButtonCfg_class <>~ "wallet__table-button-with-background"

  keyRow open notes balance = trKey $ do
    let accordionCell o = "wallet__table-cell" <> if o then "" else " accordion-collapsed"
        bcfg = btnCfgSecondary & uiButtonCfg_class <>~ "wallet__table-button" <> "button_border_none"
    clk <- elDynClass "td" (accordionCell <$> open) $ accordionButton def
    td $ copyButton' "" bcfg ButtonShade_Dark False (constant $ unAccountName name)
    elAttr "td" ("class" =: "wallet__table-cell") $ text $ unAccountName name
    td blank
    td blank
    td $ dynText $ maybe "" unAccountNotes <$> notes
    td' " wallet__table-cell-balance" $ dynText balance
    onDetails <- td $ do
      buttons $ detailsIconButton cfg
    pure (clk, AccountDialog_Details name <$> current notes <@ onDetails)


  accountRow
    :: Dynamic t Bool
    -> ChainId
    -> Dynamic t Account
    -> m (Dynamic t (Maybe AccountBalance), Event t AccountDialog)
  accountRow visible paddedChain dAccount = do
    let chain = unPadChainId paddedChain
    let details = (^? account_status . _AccountStatus_Exists) <$> dAccount
    let balance = _accountDetails_balance <$$> details
    -- Previously we always added all chain rows, but hid them with CSS. A bug
    -- somewhere between reflex-dom and jsaddle means we had to push this under
    -- a `dyn`.
    dialog <- switchHold never <=< dyn $ ffor visible $ \case
      False -> pure never
      True -> trAcc $ do
        td blank -- Arrow column
        td blank -- Copy column
        td $ text $ "Chain " <> _chainId chain
        td $ dynText $ maybe "" ownershipText <$> getAccountOwnership cwKeys details
        accStatus <- holdUniqDyn $ _account_status <$> dAccount
        elClass "td" "wallet__table-cell wallet__table-cell-keyset" $ dynText $ ffor accStatus $ \case
          AccountStatus_Unknown -> "Unknown"
          AccountStatus_DoesNotExist -> ""
          AccountStatus_Exists d -> accountGuardSummary $ _accountDetails_guard d
        td $ dynText $ maybe "" unAccountNotes . _vanityAccount_notes . _account_storage <$> dAccount
        td' " wallet__table-cell-balance" $ dynText $ fmap (uiAccountBalance' False) dAccount
        td $ buttons $ switchHold never <=< dyn $ ffor accStatus $ \case
          AccountStatus_Unknown -> pure never
          AccountStatus_DoesNotExist -> do
            receive <- receiveButton cfg
            pure $ AccountDialog_Receive name chain Nothing <$ receive
          AccountStatus_Exists d -> do
            let ks = d ^? accountDetails_guard . _AccountGuard_KeySetLike
            let uk = (\(KeySetHeritage k p _ref) -> UserKeyset k (parseKeysetPred p)) <$> ks

            let txb = TxBuilder name chain (userToPactKeyset <$> uk)
            let bcfg = btnCfgSecondary & uiButtonCfg_class <>~ "wallet__table-button-with-background" <> "button_border_none"
            copyAddress <- copyButton' "Copy Tx Builder" bcfg ButtonShade_Light False (constant $ prettyTxBuilder txb)

            receive <- receiveButton cfg
            onDetails <- detailsIconButton cfg
            pure $ leftmost
              [ AccountDialog_DetailsChain . (name, chain, d,) <$> current dAccount <@ onDetails
              , AccountDialog_Receive name chain (Just d) <$ receive
              ]
    pure (balance, dialog)


accountGuardSummary :: AccountGuard -> Text
accountGuardSummary (AccountGuard_Other pactGuard) =
  case pactGuard ^? Pact._GKeySetRef of
    Nothing -> gType <> " : " <> Pact.renderCompactText pactGuard
    Just (Pact.KeySetName name) -> "ref: " <> name
  where
    gType = pactGuardTypeText $ Pact.guardTypeOf pactGuard

accountGuardSummary (AccountGuard_KeySetLike (KeySetHeritage ksKeys ksPred ksRef)) = T.intercalate ", "
  [ tshow numKeys <> if numKeys == 1 then " key" else " keys"
  , ksPred
  , "[" <> T.intercalate ", " (keyToText <$> Set.toList ksKeys) <> "]"
  , maybe "" ("ref: " <>) ksRef
  ]
  where

    numKeys = Set.size ksKeys

-- | Widget listing all available keys.
uiAvailableKeys
  :: forall t m model mConf key.
     ( MonadWidget t m
     , HasUiWalletModelCfg model mConf key m t
     , HasWalletCfg mConf key t
     , SetRoute t (R FrontendRoute) m
     )
  => model
  -> m mConf
uiAvailableKeys model = do
  let keyMap' = model ^. wallet_keys
  mKeyMap <- maybeDyn $ ffor keyMap' $ \im -> case IntMap.toAscList im of
    [] -> Nothing
    xs -> Just $ Map.fromAscList xs
  flatten <=< dyn $ ffor mKeyMap $ \case
    Nothing -> uiEmptyState $(static "img/menu/keys.svg") "No Keys Found" $ do
      el "p" $ text "The first step towards transacting on the Kadena blockchain is to generate a key pair."
      el "p" $ do
        text "Begin by selecting the "
        el "strong" $ text "+ Generate Key"
        text " button, then continue to Accounts."
      pure mempty
    Just keyMap -> divClass "wallet__keys-list" $ do
      let netAccList  =
            do
              net <- model^.network_selectedNetwork
              accs <- model^.wallet_accounts
              pure $ (net, fmap fst $ Map.toList $ fromMaybe mempty $ accs^? _AccountData . ix net)
      uiKeyItems keyMap netAccList

-- | Render a list of key items.
--
-- Does not include the surrounding `div` tag. Use uiAvailableKeys for the
-- complete `div`.
uiKeyItems
  :: ( MonadWidget t m
     , Monoid mConf, Monoid (ModalCfg mConf t)
     , HasModalCfg mConf (Modal mConf m t) t
     , HasCrypto key m
     , HasWalletCfg mConf key t
     , SetRoute t (R FrontendRoute) m
     )
  => Dynamic t (Map Int (Key key))
  -> Dynamic t (NetworkName, [AccountName])
  -> m mConf
uiKeyItems keyMap netAndAccs = do
  let
    tableAttrs =
      "style" =: "table-layout: fixed; width: calc(100% - 22px);"
      <> "class" =: "wallet table"
  events <- elAttr "table" tableAttrs $ do
    el "colgroup" $ do
      elAttr "col" ("style" =: "width: 70%") blank
      elAttr "col" ("style" =: "width: 30%") blank
    el "thead" $ el "tr" $ do
      let mkHeading = elClass "th" "wallet__table-heading" . text
      traverse_ mkHeading $
        [ "Public Key"
        , ""
        ]

    el "tbody" $ do
      events <- listWithKey keyMap uiKeyItem
      dyn_ $ ffor keyMap $ \keys ->
        when (null keys) $
          elClass "tr" "wallet__table-row" $ elAttr "td" ("colspan" =: "5" <> "class" =: "wallet__table-cell") $
            text "No keys ..."
      pure events

  let
    keyEvents = (fmap . fmap) fst events
    addAccMap = (fmap . fmap) snd events
    modalEvents = switch $ leftmost . Map.elems <$> current keyEvents
    accEvents = switch $ leftmost . Map.elems <$> current addAccMap

    eAddAccWithKey = attach (current netAndAccs) accEvents
    addAcc ((net, accList), k) =
      let accName = AccountName $ "k:" <> k
       in case accName `elem` accList of
            True -> Nothing
            False -> Just (net, accName)

  pure $ mempty
    & modalCfg_setModal .~ fmap keyModal modalEvents
    & walletCfg_importAccount .~ fmapMaybe addAcc eAddAccWithKey

  where
    keyModal = Just . \case
      KeyDialog_Details i key -> uiKeyDetails i key

-- | Dialogs which can be launched from keys.
data KeyDialog key
  = KeyDialog_Details IntMap.Key (Key key)

------------------------------------------------------------------------------
-- | Display a key as list item together with its name.
uiKeyItem
  :: forall key t m. (MonadWidget t m, (SetRoute t (R FrontendRoute) m))
  => IntMap.Key
  -> Dynamic t (Key key)
  -> m (Event t (KeyDialog key), Event t Text)
uiKeyItem keyIndex key = trKey $ do
  let dKeyText = keyToText . _keyPair_publicKey . _key_pair <$> key
  td $ dynText dKeyText
  td $ buttons $ do
    copyButton' "" bcfg ButtonShade_Dark False (current $ keyToText . _keyPair_publicKey . _key_pair <$> key)
    eAddK <- addKAccountButton $ cfg & uiButtonCfg_class <>~ "wallet__table-button--hamburger" <> "wallet__table-button-key"
    onDetails <- detailsButton (cfg & uiButtonCfg_class <>~ "wallet__table-button--hamburger" <> "wallet__table-button-key")
    let addK = current dKeyText <@ eAddK
    setRoute $ ffor addK $ \k -> FrontendRoute_Accounts :/ ("open" =: (Just $ "k:" <> k))
    pure $ (KeyDialog_Details keyIndex <$> current key <@ onDetails, addK)
  where
    bcfg = btnCfgSecondary & uiButtonCfg_class <>~ "wallet__table-button-with-background" <> "button_border_none"
    trKey = elClass "tr" "wallet__table-row wallet__table-row-key"
    td = elClass "td" "wallet__table-cell"
    buttons = divClass "wallet__table-buttons"
    cfg = def
      & uiButtonCfg_class <>~ "wallet__table-button-with-background"

uiGenerateKeyButton
  :: (MonadWidget t m, Monoid mConf, HasWalletCfg mConf key t)
  => m mConf
uiGenerateKeyButton = do
  e <- uiButton headerBtnCfgPrimary (text "+ Generate Key")
  pure $ mempty & walletCfg_genKey .~ e
