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
  , uiGenerateKeyButton
  , uiPublicKeyDropdown
    -- ** Filters for keys
  , hasPrivateKey
  , HasUiWalletModelCfg
  ) where

------------------------------------------------------------------------------
import           Control.Applicative         (liftA2)
import           Control.Lens
import           Control.Monad               (when, (<=<), join)
import qualified Data.IntMap                 as IntMap
import qualified Data.Map                    as Map
import           Data.Set (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
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
  ( MonadWidget t m, HasUiWalletModelCfg model mConf key m t)
  => model -> m mConf
uiAccountsTable = divClass "wallet__keys-list" . uiAccountItems

uiAccountItems
  :: forall t m model mConf key.
  ( MonadWidget t m, HasUiWalletModelCfg model mConf key m t)
  => model -> m mConf
uiAccountItems model = do
  let net = model ^. network_selectedNetwork
      accountsMap = liftA2 (Map.findWithDefault mempty) net (unAccountData <$> model ^. wallet_accounts)
      tableAttrs = mconcat
        [ "style" =: "table-layout: fixed; width: 98%"
        , "class" =: "wallet table"
        ]

  events <- elAttr "table" tableAttrs $ do
    el "colgroup" $ do
      elAttr "col" ("style" =: "width: 5%") blank
      elAttr "col" ("style" =: "width: 20%") blank
      elAttr "col" ("style" =: "width: 35%") blank
      elAttr "col" ("style" =: "width: 20%") blank
      elAttr "col" ("style" =: "width: 20%") blank

    el "thead" $ el "tr" $ do
      let mkHeading = elClass "th" "wallet__table-heading" . text
      traverse_ mkHeading $
        [ ""
        , "Account Name"
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
    (clk, dialog) <- keyRow visible notes $ sum . catMaybes <$> balances
    visible <- toggle False clk
    results <- listWithKey chainMap $ accountRow visible
    let balances :: Dynamic t [Maybe AccountBalance]
        balances = join $ traverse fst . Map.elems <$> results
  let dialogs = switch $ leftmost . fmap snd . Map.elems <$> current results
  pure $ leftmost [dialog, dialogs]
  where
  trKey = elClass "tr" "wallet__table-row wallet__table-row-account"
  trAcc visible = elDynAttr "tr" $ ffor visible $ \v -> mconcat
    [ "class" =: "wallet__table-row"
    , if v then mempty else "style" =: "display:none"
    ]
  td = elClass "td" "wallet__table-cell"
  buttons = divClass "wallet__table-buttons"
  cfg = def
    & uiButtonCfg_class <>~ "wallet__table-button"

  keyRow open notes balance = trKey $ do
    let accordionCell o = "wallet__table-cell" <> if o then "" else " accordion-collapsed"
    clk <- elDynClass "td" (accordionCell <$> open) $ accordionButton def
    td $ text $ unAccountName name
    td $ dynText $ maybe "" unAccountNotes <$> notes
    td $ dynText $ uiAccountBalance False . Just <$> balance
    onDetails <- td $ buttons $ detailsIconButton cfg
    pure (clk, AccountDialog_Details name <$> current notes <@ onDetails)

  accountRow visible chain dAccount = trAcc visible $ do
    td blank -- Arrow column
    td $ text $ "Chain ID: " <> _chainId chain
    td $ dynText $ maybe "" unAccountNotes . _vanityAccount_notes . _account_storage <$> dAccount
    td $ dynText $ fmap (uiAccountBalance' False) dAccount
    td $ buttons $ do
      accStatus <- holdUniqDyn $ _account_status <$> dAccount
      action <- switchHold never <=< dyn $ ffor accStatus $ \case
        AccountStatus_Unknown -> pure never
        AccountStatus_DoesNotExist -> do
          create <- uiCreateAccountButton cfg
          let keyFromName = textToKey $ unAccountName name
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
      let balance = (^? account_status . _AccountStatus_Exists . accountDetails_balance) <$> dAccount
      pure (balance, action)


uiPublicKeyDropdown
  :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m, HasWallet model key t)
  => model -> CssClass -> m (Dynamic t (Maybe PublicKey))
uiPublicKeyDropdown model cls = do
  let toPair k = let pk = _keyPair_publicKey $ _key_pair k in (Just pk, keyToText pk)
      keys = ffor (model ^. wallet_keys) $ Map.fromList . fmap toPair . IntMap.elems
  value <$> uiDropdown Nothing keys (def & dropdownConfig_attributes .~ pure ("class" =: renderClass cls))

-- | Widget listing all available keys.
uiAvailableKeys
  :: forall t m model mConf key.
     ( MonadWidget t m
     , HasUiWalletModelCfg model mConf key m t
     )
  => model
  -> m mConf
uiAvailableKeys model = do
  divClass "wallet__keys-list" $ do
    uiKeyItems model

-- | Render a list of key items.
--
-- Does not include the surrounding `div` tag. Use uiAvailableKeys for the
-- complete `div`.
uiKeyItems
  :: forall t m model mConf key.
     ( MonadWidget t m
     , HasUiWalletModelCfg model mConf key m t
     )
  => model
  -> m mConf
uiKeyItems model = do
  let
    keyMap' = model ^. wallet_keys
    keyMap = Map.fromAscList . IntMap.toAscList <$> keyMap'
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
  td $ uiPublicKeyShrunk $ _keyPair_publicKey . _key_pair <$> key
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
