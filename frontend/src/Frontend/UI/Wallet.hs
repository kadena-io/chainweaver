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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Wallet management ui for handling private/public keys.
-- Copyright   :  (C) 2018 Kadena
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
import           Control.Error               (hush)
import           Control.Lens
import           Control.Monad               (when, (<=<))
import qualified Data.IntMap                 as IntMap
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Set (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Obelisk.Generated.Static
import           Reflex
import           Reflex.Dom hiding (Key)
------------------------------------------------------------------------------
import           Frontend.Log (HasLogger, HasLogCfg)
import           Frontend.Crypto.Class
import           Frontend.Crypto.Ed25519     (keyToText)
import           Frontend.Wallet
import           Frontend.UI.Widgets
import           Frontend.Foundation
import           Frontend.JsonData (HasJsonData, HasJsonDataCfg)
import           Frontend.UI.Dialogs.AccountDetails
import           Frontend.UI.Dialogs.AddVanityAccount (uiCreateAccountButton, uiCreateAccountDialog)
import           Frontend.UI.Dialogs.KeyDetails (uiKeyDetails)
import           Frontend.UI.Dialogs.Receive (uiReceiveModal)
import           Frontend.UI.Dialogs.Send (uiSendModal)
import           Frontend.UI.Dialogs.WatchRequest (uiWatchRequestDialog)
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
  = AccountDialog_DetailsChain (AccountName, ChainId, Account)
  | AccountDialog_Details AccountName (Maybe AccountNotes)
  | AccountDialog_Receive AccountName ChainId
  | AccountDialog_Send (AccountName, ChainId, Account)
  | AccountDialog_Create AccountName ChainId (Maybe PublicKey)

uiWalletRefreshButton
  :: (MonadWidget t m, Monoid mConf, HasWalletCfg mConf key t)
  => m mConf
uiWalletRefreshButton = do
  eRefresh <- uiButton (def & uiButtonCfg_class <>~ " main-header__wallet-refresh-button")  (text "Refresh")
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
  watch <- uiButton (def & uiButtonCfg_class <>~ " main-header__wallet-refresh-button")  (text "Watch Request")
  pure $ mempty & modalCfg_setModal .~ (Just (uiWatchRequestDialog model) <$ watch)

-- | UI for managing the keys wallet.
uiWallet
  :: forall m t key model mConf
     . ( MonadWidget t m
       , HasUiWalletModelCfg model mConf key m t
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
  (MonadWidget t m, HasUiWalletModelCfg model mConf key m t, HasTransactionLogger m)
  => model -> m mConf
uiAccountsTable model = do
  let net = model ^. network_selectedNetwork
      networks = model ^. wallet_accounts
  mAccounts <- maybeDyn $ ffor2 net networks $ \n (AccountData m) -> case Map.lookup n m of
    Just accs | not (Map.null accs) -> Just accs
    _ -> Nothing
  flatten <=< dyn $ ffor mAccounts $ \case
    Nothing -> uiEmptyState (static @"img/menu/wallet.svg") "No Accounts Found" $ do
      el "p" $ do
        text "Create new Accounts or interact with existing Accounts by selecting the "
        el "strong" $ text "+ Add Account"
        text " button."
      pure mempty
    Just m -> divClass "wallet__keys-list" $ uiAccountItems model m


uiAccountItems
  :: forall t m model mConf key.
  (MonadWidget t m, HasUiWalletModelCfg model mConf key m t, HasTransactionLogger m)
  => model -> Dynamic t (Map AccountName (AccountInfo Account)) -> m mConf
uiAccountItems model accountsMap = do
  let net = model ^. network_selectedNetwork
      tableAttrs = mconcat
        [ "style" =: "table-layout: fixed; width: 98%"
        , "class" =: "wallet table"
        ]

  events <- elAttr "table" tableAttrs $ do
    el "colgroup" $ do
      elAttr "col" ("style" =: "width: 5%") blank
      elAttr "col" ("style" =: "width: 22.5%") blank
      elAttr "col" ("style" =: "width: 20.5%") blank
      elAttr "col" ("style" =: "width: 15%") blank
      elAttr "col" ("style" =: "width: 12%") blank
      elAttr "col" ("style" =: "width: 25%") blank

    el "thead" $ el "tr" $ do
      let mkHeading = elClass "th" "wallet__table-heading" . text
      traverse_ mkHeading $
        [ ""
        , "Account Name"
        , "Keyset Info"
        , "Notes"
        , "Balance (KDA)"
        , ""
        ]

    el "tbody" $ do
      let keys = Set.fromList . fmap (_keyPair_publicKey . _key_pair) . IntMap.elems <$> model ^. wallet_keys
      events <- listWithKey accountsMap $ uiAccountItem keys
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
      AccountDialog_Receive name chain -> uiReceiveModal model name (Just chain)
      AccountDialog_Send acc -> uiSendModal model acc
      AccountDialog_Create name chain mKey -> uiCreateAccountDialog model name chain mKey

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

uiAccountItem
  :: forall t m. MonadWidget t m
  => Dynamic t (Set PublicKey)
  -> AccountName
  -> Dynamic t (AccountInfo Account)
  -> m (Event t AccountDialog)
uiAccountItem keys name accountInfo = do
  let chainMap = _accountInfo_chains <$> accountInfo
      notes = _accountInfo_notes <$> accountInfo
  rec
    (clk, dialog) <- keyRow visible notes $ ffor balances $ \xs0 -> case catMaybes xs0 of
      [] -> "Does not exist"
      xs -> uiAccountBalance False $ Just $ sum xs

    visible <- toggle False clk
    results <- accursedUnutterableListWithKey chainMap $ accountRow visible
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
    & uiButtonCfg_class <>~ "wallet__table-button"

  keyRow open notes balance = trKey $ do
    let accordionCell o = "wallet__table-cell" <> if o then "" else " accordion-collapsed"
    clk <- elDynClass "td" (accordionCell <$> open) $ accordionButton def
    td $ text $ unAccountName name
    td blank -- Keyset info column
    td $ dynText $ maybe "" unAccountNotes <$> notes
    td' " wallet__table-cell-balance" $ dynText balance
    onDetails <- td $ buttons $ detailsIconButton cfg
    pure (clk, AccountDialog_Details name <$> current notes <@ onDetails)

  accountRow
    :: Dynamic t Bool
    -> ChainId
    -> Dynamic t Account
    -> m (Dynamic t (Maybe AccountBalance), Event t AccountDialog)
  accountRow visible chain dAccount = do
    let balance = (^? account_status . _AccountStatus_Exists . accountDetails_balance) <$> dAccount
    -- Previously we always added all chain rows, but hid them with CSS. A bug
    -- somewhere between reflex-dom and jsaddle means we had to push this under
    -- a `dyn`.
    dialog <- switchHold never <=< dyn $ ffor visible $ \case
      False -> pure never
      True -> trAcc $ do
        td blank -- Arrow column
        td $ text $ "Chain ID: " <> _chainId chain
        accStatus <- holdUniqDyn $ _account_status <$> dAccount
        elClass "td" "wallet__table-cell wallet__table-cell-keyset" $ dynText $ ffor accStatus $ \case
          AccountStatus_Unknown -> "Unknown"
          AccountStatus_DoesNotExist -> ""
          AccountStatus_Exists d -> keysetSummary $ _accountDetails_keyset d
        td $ dynText $ maybe "" unAccountNotes . _vanityAccount_notes . _account_storage <$> dAccount
        td' " wallet__table-cell-balance" $ dynText $ fmap (uiAccountBalance' False) dAccount
        td $ buttons $ switchHold never <=< dyn $ ffor accStatus $ \case
          AccountStatus_Unknown -> pure never
          AccountStatus_DoesNotExist -> do
            create <- uiCreateAccountButton cfg
            let keyFromName = hush $ parsePublicKey $ unAccountName name
            pure $ AccountDialog_Create name chain keyFromName <$ create
          AccountStatus_Exists d -> do
            owned <- holdUniqDyn $ (_addressKeyset_keys (_accountDetails_keyset d) `Set.isSubsetOf`) <$> keys
            switchHold never <=< dyn $ ffor owned $ \case
              True -> do
                recv <- receiveButton cfg
                send <- sendButton cfg
                onDetails <- detailsIconButton cfg
                pure $ leftmost
                  [ AccountDialog_Receive name chain <$ recv
                  , AccountDialog_Send . (name, chain, ) <$> current dAccount <@ send
                  , AccountDialog_DetailsChain . (name, chain, ) <$> current dAccount <@ onDetails
                  ]
              False -> do
                transferTo <- transferToButton cfg
                onDetails <- detailsIconButton cfg
                pure $ leftmost
                  [ AccountDialog_Receive name chain <$ transferTo
                  , AccountDialog_DetailsChain . (name, chain, ) <$> current dAccount <@ onDetails
                  ]
    pure (balance, dialog)

keysetSummary :: AddressKeyset -> Text
keysetSummary ks = T.intercalate ", "
  [ tshow numKeys <> if numKeys == 1 then " key" else " keys"
  , _addressKeyset_pred ks
  , "[" <> T.intercalate ", " (fmap keyToText . Set.toList $ _addressKeyset_keys ks) <> "]"
  ]
    where numKeys = Set.size $ _addressKeyset_keys ks

-- | Widget listing all available keys.
uiAvailableKeys
  :: forall t m model mConf key.
     ( MonadWidget t m
     , HasUiWalletModelCfg model mConf key m t
     )
  => model
  -> m mConf
uiAvailableKeys model = do
  let keyMap' = model ^. wallet_keys
  mKeyMap <- maybeDyn $ ffor keyMap' $ \im -> case IntMap.toAscList im of
    [] -> Nothing
    xs -> Just $ Map.fromAscList xs
  flatten <=< dyn $ ffor mKeyMap $ \case
    Nothing -> uiEmptyState (static @"img/menu/keys.svg") "No Keys Found" $ do
      el "p" $ text "The first step towards transacting on the Kadena blockchain is to generate a key pair."
      el "p" $ do
        text "Begin by selecting the "
        el "strong" $ text "+ Generate Key"
        text " button, then continue to Accounts."
      pure mempty
    Just keyMap -> divClass "wallet__keys-list" $ uiKeyItems keyMap

-- | Render a list of key items.
--
-- Does not include the surrounding `div` tag. Use uiAvailableKeys for the
-- complete `div`.
uiKeyItems
  :: ( MonadWidget t m
     , Monoid mConf, Monoid (ModalCfg mConf t)
     , HasModalCfg mConf (Modal mConf m t) t
     , HasCrypto key m
     )
  => Dynamic t (Map Int (Key key))
  -> m mConf
uiKeyItems keyMap = do
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

  let modalEvents = switch $ leftmost . Map.elems <$> current events

  pure $ mempty
    & modalCfg_setModal .~ fmap keyModal modalEvents
  where
    keyModal = Just . \case
      KeyDialog_Details i key -> uiKeyDetails i key

-- | Dialogs which can be launched from keys.
data KeyDialog key
  = KeyDialog_Details IntMap.Key (Key key)

------------------------------------------------------------------------------
-- | Display a key as list item together with its name.
uiKeyItem
  :: forall key t m. MonadWidget t m
  => IntMap.Key
  -> Dynamic t (Key key)
  -> m (Event t (KeyDialog key))
uiKeyItem keyIndex key = trKey $ do
  td $ uiPublicKeyShrunkDyn $ _keyPair_publicKey . _key_pair <$> key
  td $ buttons $ do
    onDetails <- detailsButton (cfg & uiButtonCfg_class <>~ "wallet__table-button--hamburger" <> "wallet__table-button-key")
    pure $ KeyDialog_Details keyIndex <$> current key <@ onDetails
  where
    trKey = elClass "tr" "wallet__table-row wallet__table-row-key"
    td = elClass "td" "wallet__table-cell"
    buttons = divClass "wallet__table-buttons"
    cfg = def
      & uiButtonCfg_class <>~ "wallet__table-button"

uiGenerateKeyButton
  :: (MonadWidget t m, Monoid mConf, HasWalletCfg mConf key t)
  => m mConf
uiGenerateKeyButton = do
  e <- uiButton (def & uiButtonCfg_class <>~ " main-header__add-account-button")  (text "+ Generate Key")
  pure $ mempty & walletCfg_genKey .~ e
