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

import Control.Monad.Logger (LogLevel (..))
------------------------------------------------------------------------------
import           Control.Applicative         (liftA2)
import           Control.Lens
import           Control.Monad               (when, (<=<), join)
import           Data.Dependent.Sum          ((==>))
import qualified Data.IntMap                 as IntMap
import           Data.Map (Map)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import qualified Pact.Types.ChainId          as Pact
import           Reflex
import           Reflex.Dom hiding (Key)
------------------------------------------------------------------------------
import           Frontend.Log (HasLogCfg, logCfg_logMessage)
import           Frontend.Crypto.Class
import           Frontend.Crypto.Ed25519     (keyToText)
import           Frontend.Wallet
import           Frontend.UI.Widgets
import           Frontend.Foundation
import           Frontend.JsonData (HasJsonData, HasJsonDataCfg)
import           Frontend.UI.Dialogs.AccountDetails (uiAccountDetails)
import           Frontend.UI.Dialogs.KeyDetails (uiKeyDetails)
import           Frontend.UI.Dialogs.Receive (uiReceiveModal)
import           Frontend.UI.Dialogs.Send (uiSendModal)
import           Frontend.UI.Modal
import           Frontend.Network
------------------------------------------------------------------------------
import Frontend.KadenaAddress

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
  , HasLogCfg mConf t
  )

-- | Possible actions from an account
data AccountDialog
  = AccountDialog_Details Account
  | AccountDialog_Receive AccountName AccountCreated ChainId
  | AccountDialog_Send Account

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

flattenKeys :: (Ord k1, Ord k2) => Map k1 (Map k2 a) -> Map (k1, k2) a
flattenKeys = Map.foldMapWithKey $ \k1 -> Map.foldMapWithKey $ \k2 -> Map.singleton (k1, k2)

uiAccountItems
  :: forall t m model mConf key.
  ( MonadWidget t m, HasUiWalletModelCfg model mConf key m t)
  => model -> m mConf
uiAccountItems model = do
  let net = model ^. network_selectedNetwork
      accounts = liftA2 (Map.findWithDefault mempty) net (unAccountStorage <$> model ^. wallet_accounts)
      accountsMap
        = Map.filter (not . _accountInfo_hidden . _vanityAccount_info)
        . flattenKeys . _accounts_vanity <$> accounts
      tableAttrs = mconcat
        [ "style" =: "table-layout: fixed; width: 98%"
        , "class" =: "wallet table"
        ]

  events <- elAttr "table" tableAttrs $ do
    el "colgroup" $ do
      elAttr "col" ("style" =: "width: 25%") blank
      elAttr "col" ("style" =: "width: 10%") blank
      elAttr "col" ("style" =: "width: 25%") blank
      elAttr "col" ("style" =: "width: 15%") blank
      elAttr "col" ("style" =: "width: 25%") blank

    el "thead" $ el "tr" $ do
      let mkHeading = elClass "th" "wallet__table-heading" . text
      traverse_ mkHeading $
        [ "Account Name"
        , "Chain ID"
        , "Notes"
        , "Balance (KDA)"
        , ""
        ]

    el "tbody" $ do
      events <- listWithKey accountsMap uiAccountItem
      dyn_ $ ffor accountsMap $ \accs ->
        when (null accs) $
          elClass "tr" "wallet__table-row" $ elAttr "td" ("colspan" =: "5" <> "class" =: "wallet__table-cell") $
            text "No accounts ..."
      pure events

  let
    onAccountModal = switchDyn $ leftmost . Map.elems <$> events

    accModal n = Just . \case
      AccountDialog_Details acc -> uiAccountDetails n acc
      AccountDialog_Receive name created chain -> uiReceiveModal model name created (Just chain)
      AccountDialog_Send acc -> uiSendModal model acc

  refresh <- delay 1 =<< getPostBuild

  pure $ mempty
    & modalCfg_setModal .~ attachWith accModal (current net) onAccountModal
    & walletCfg_refreshBalances .~ refresh
    & logCfg_logMessage .~ ((LevelDebug, "account modal opened") <$ onAccountModal)

uiAccountItem
  :: MonadWidget t m
  => (AccountName, ChainId)
  -> Dynamic t VanityAccount
  -> m (Event t AccountDialog)
uiAccountItem (name, chain) acc = do
  elClass "tr" "wallet__table-row" $ do
    let td = elClass "td" "wallet__table-cell"
        info = view accountInfo <$> acc

    td $ divClass "wallet__table-wallet-address" $ text $ unAccountName name
    td $ text $ Pact._chainId chain
    td $ dynText $ ffor acc $ unAccountNotes . _vanityAccount_notes
    td $ dynText $ ffor info $ uiAccountBalance' False

    td $ divClass "wallet__table-buttons" $ do
      let cfg = def
            & uiButtonCfg_class <>~ "wallet__table-button"

      recv <- receiveButton cfg
      send <- sendButton cfg
      onDetails <- detailsIconButton cfg

      pure $ leftmost
        [ AccountDialog_Details . (AccountRef_Vanity name chain ==>) <$> current acc <@ onDetails
        , AccountDialog_Receive name AccountCreated_Yes chain <$ recv
        , AccountDialog_Send . (AccountRef_Vanity name chain ==>) <$> current acc <@ send
        ]

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
      elAttr "col" ("style" =: "width: 5%") blank
      elAttr "col" ("style" =: "width: 35%") blank
      elAttr "col" ("style" =: "width: 20%") blank
      elAttr "col" ("style" =: "width: 20%") blank
      elAttr "col" ("style" =: "width: 20%") blank
    el "thead" $ el "tr" $ do
      let mkHeading = elClass "th" "wallet__table-heading" . text
      traverse_ mkHeading $
        [ ""
        , "Public Key"
        , "Notes"
        , "Balance (KDA)"
        , ""
        ]

    el "tbody" $ do
      events <- listWithKey keyMap (uiKeyItem model)
      dyn_ $ ffor keyMap $ \keys ->
        when (all _key_hidden keys) $
          elClass "tr" "wallet__table-row" $ elAttr "td" ("colspan" =: "5" <> "class" =: "wallet__table-cell") $
            text "No keys ..."
      pure events

  refresh <- delay 1 =<< getPostBuild

  let modalEvents = switch $ leftmost . Map.elems <$> current events

  pure $ mempty
    & modalCfg_setModal .~ attachWith keyModal (current $ model ^. network_selectedNetwork) modalEvents
    & walletCfg_refreshBalances .~ refresh
  where
    keyModal n = Just . \case
      KeyDialog_Receive chain name created -> uiReceiveModal model name created (Just chain)
      KeyDialog_Send acc -> uiSendModal model acc
      KeyDialog_Details i key -> uiKeyDetails i key
      KeyDialog_AccountDetails acc -> uiAccountDetails n acc

-- | Dialogs which can be launched from keys.
data KeyDialog key
  = KeyDialog_Receive ChainId AccountName AccountCreated
  | KeyDialog_Send Account
  | KeyDialog_Details IntMap.Key (Key key)
  | KeyDialog_AccountDetails Account

------------------------------------------------------------------------------
-- | Display a key as list item together with its name.
uiKeyItem
  :: forall model key t m. (MonadWidget t m, HasNetwork model t, HasWallet model key t)
  => model
  -> IntMap.Key
  -> Dynamic t (Key key)
  -> m (Event t (KeyDialog key))
uiKeyItem model keyIndex key = do
  hidden <- holdUniqDyn $ _key_hidden <$> key
  switchHold never <=< dyn $ ffor hidden $ \case
    True -> pure never
    False -> do
      let mAccounts = ffor3 key (model ^. network_selectedNetwork) (model ^. wallet_accounts) $ \k net (AccountStorage as) -> fromMaybe mempty $ do
            accounts <- Map.lookup net as
            nva <- Map.lookup (_keyPair_publicKey $ _key_pair k) (_accounts_nonVanity accounts)
            pure $ Map.filter (not . _accountInfo_hidden . _nonVanityAccount_info) nva
      rec
        (clk, dialog) <- keyRow visible $ sum . catMaybes <$> balances
        visible <- toggle False clk
        results <- listWithKey mAccounts $ accountRow visible
        let balances :: Dynamic t [Maybe AccountBalance]
            balances = join $ traverse fst . Map.elems <$> results
      let dialogs = switch $ leftmost . fmap snd . Map.elems <$> current results
      pure $ leftmost [dialog, dialogs]
     where
      trKey = elClass "tr" "wallet__table-row wallet__table-row-key"
      trAcc visible = elDynAttr "tr" $ ffor visible $ \v -> mconcat
        [ "class" =: "wallet__table-row"
        , if v then mempty else "style" =: "display:none"
        ]
      td = elClass "td" "wallet__table-cell"
      buttons = divClass "wallet__table-buttons"
      cfg = def
        & uiButtonCfg_class <>~ "wallet__table-button"

      keyRow open balance = trKey $ do
        let accordionCell o = "wallet__table-cell" <> if o then "" else " accordion-collapsed"
        clk <- elDynClass "td" (accordionCell <$> open) $ accordionButton def
        td $ dynText $ keyToText . _keyPair_publicKey . _key_pair <$> key
        td $ dynText $ unAccountNotes . _key_notes <$> key
        td $ dynText $ uiAccountBalance False . Just <$> balance
        dialog <- td $ buttons $ do
          onDetails <- detailsButton (cfg & uiButtonCfg_class <>~ "wallet__table-button--hamburger" <> "wallet__table-button-key")
          pure $ KeyDialog_Details keyIndex <$> current key <@ onDetails
        pure (clk, dialog)

      accountRow visible chain dAccount = trAcc visible $ do
        td blank -- Arrow column
        td $ text $ "Chain ID: " <> _chainId chain
        td blank -- Notes column
        td $ dynText $ fmap (uiAccountBalance' False) dAccount
        td $ buttons $ do
          recv <- receiveButton cfg
          send <- sendButton cfg
          onDetails <- detailsIconButton cfg
          let pk = _keyPair_publicKey . _key_pair <$> current key
              balance = _accountInfo_balance . _nonVanityAccount_info <$> dAccount
              created = maybe AccountCreated_No (const AccountCreated_Yes) <$> balance
          pure
            ( balance
            , leftmost
              [ (\k -> KeyDialog_Send . (AccountRef_NonVanity k chain ==>)) <$> pk <*> current dAccount <@ send
              , (\k -> KeyDialog_AccountDetails . (AccountRef_NonVanity k chain ==>)) <$> pk <*> current dAccount <@ onDetails
              -- TODO we need a way of adding these accounts when they already
              -- exist too, e.g. for users who are recovering wallets. An
              -- automatic account discovery thing would work well.
              , KeyDialog_Receive chain
                  <$> (AccountName . keyToText <$> pk)
                  <*> current created
                  <@ recv
              ]
            )

uiGenerateKeyButton
  :: (MonadWidget t m, Monoid mConf, HasWalletCfg mConf key t)
  => m mConf
uiGenerateKeyButton = do
  e <- uiButton (def & uiButtonCfg_class <>~ " main-header__add-account-button")  (text "+ Generate Key")
  pure $ mempty & walletCfg_genKey .~ e
