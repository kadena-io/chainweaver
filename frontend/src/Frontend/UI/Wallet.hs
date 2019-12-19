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
    -- ** Filters for keys
  , hasPrivateKey
  , HasUiWalletModelCfg
  ) where

------------------------------------------------------------------------------
import           Control.Applicative         (liftA2)
import           Control.Lens
import           Control.Monad               (when, (<=<), join)
import           Data.Dependent.Sum          (DSum (..), (==>))
import           Data.Some                   (Some(..))
import qualified Data.IntMap                 as IntMap
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import qualified Pact.Types.ChainId          as Pact
import           Reflex
import           Reflex.Dom hiding (Key)
------------------------------------------------------------------------------
import           Frontend.Crypto.Class
import           Frontend.Crypto.Ed25519     (keyToText)
import           Frontend.Wallet
import           Frontend.UI.Widgets
import           Frontend.Foundation
import           Frontend.JsonData (HasJsonData, HasJsonDataCfg)
import           Frontend.UI.Dialogs.AccountDetails (uiAccountDetails)
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
  )

-- | Possible actions from an account
data AccountDialog
  = AccountDialog_Details
  | AccountDialog_Receive
  | AccountDialog_Send
  deriving Eq

uiWalletRefreshButton
  :: forall m t key model mConf
  . ( MonadWidget t m
    , HasUiWalletModelCfg model mConf key m t
    )
  => model
  -> m mConf
uiWalletRefreshButton model = do
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
      accounts = liftA2 (Map.findWithDefault mempty) net (unAccountStorage <$> model ^. wallet_accounts)
      accountsMap = ffor accounts $ foldAccounts $ \a@(r :=> _) -> Map.singleton (Some r) a
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
      events <- listWithKey accountsMap (\_ -> uiAccountItem)
      dyn_ $ ffor accountsMap $ \accs ->
        when (null accs) $
          elClass "tr" "wallet__table-row" $ elAttr "td" ("colspan" =: "5" <> "class" =: "wallet__table-cell") $
            text "No accounts ..."
      pure events

  let
    onAccountModal = switchDyn $ leftmost . Map.elems <$> events

    accModal n (d,a) = Just $ case d of
      AccountDialog_Details -> uiAccountDetails n (accountToName a) a
      AccountDialog_Receive -> uiReceiveModal model (accountToName a) (accountIsCreated a) (Just $ accountChain a)
      --TODO: AccountDialog_Send -> uiSendModal model i a

  refresh <- delay 1 =<< getPostBuild

  pure $ mempty
    & modalCfg_setModal .~ (accModal <$> current net <@> onAccountModal)
    & walletCfg_refreshBalances .~ refresh

uiAccountItem
  :: MonadWidget t m
  => Dynamic t Account
  -> m (Event t (AccountDialog, Account))
uiAccountItem acc = do
  elClass "tr" "wallet__table-row" $ do
    let td = elClass "td" "wallet__table-cell"
        info = view accountInfo <$> acc
        notes = accountNotes <$> acc

    td $ divClass "wallet__table-wallet-address" $ dynText $ ffor acc $ unAccountName . accountToName
    td $ dynText $ ffor acc $ Pact._chainId . accountChain
    td $ dynText $ ffor acc $ maybe "" unAccountNotes . accountNotes
    td $ dynText $ ffor info $ \i -> case _accountInfo_balance i of
      Nothing -> "Unknown"
      Just b -> tshow (unAccountBalance b) <> " KDA" <> maybe "" (const "*") (_accountInfo_unfinishedCrossChainTransfer i)

    td $ divClass "wallet__table-buttons" $ do
      let cfg = def
            & uiButtonCfg_class <>~ "wallet__table-button"

      recv <- receiveButton cfg
      send <- sendButton cfg
      onDetails <- detailsButton (cfg & uiButtonCfg_class <>~ " wallet__table-button--hamburger")

      let mkDialog dia onE = (dia,) <$> current acc <@ onE

      pure $ leftmost
        [ mkDialog AccountDialog_Details onDetails
        , mkDialog AccountDialog_Receive recv
        , mkDialog AccountDialog_Send send
        ]


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
      "style" =: "table-layout: fixed; width: 98%"
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
        , "Balance"
        , ""
        ]

    el "tbody" $ do
      events <- listWithKey keyMap (uiKeyItem model)
      dyn_ $ ffor keyMap $ \keys ->
        when (all _key_hidden keys) $
          elClass "tr" "wallet__table-row" $ elAttr "td" ("colspan" =: "5" <> "class" =: "wallet__table-cell") $
            text "No accounts ..."
      pure events

  refresh <- delay 1 =<< getPostBuild

  let modalEvents = switch $ leftmost . Map.elems <$> current events

  pure $ mempty
    & modalCfg_setModal .~ fmap keyModal modalEvents
    & walletCfg_refreshBalances .~ refresh
  where
    keyModal = Just . \case
      KeyDialog_Receive name created -> uiReceiveModal model name created Nothing
      KeyDialog_Send acc -> uiSendModal model acc

-- | Dialogs which can be launched from keys.
data KeyDialog
  = KeyDialog_Receive AccountName AccountCreated
  | KeyDialog_Send Account

------------------------------------------------------------------------------
-- | Display a key as list item together with its name.
uiKeyItem
  :: forall model key t m. (MonadWidget t m, HasNetwork model t, HasWallet model key t)
  => model
  -> IntMap.Key
  -> Dynamic t (Key key)
  -> m (Event t KeyDialog)
uiKeyItem model _index key = do
  hidden <- holdUniqDyn $ _key_hidden <$> key
  switchHold never <=< dyn $ ffor hidden $ \case
    True -> pure never
    False -> mdo
      rec
        (clk, dialog) <- keyRow visible $ sum . catMaybes <$> balances
        visible <- toggle False clk
      let mAccounts = ffor3 key (model ^. network_selectedNetwork) (model ^. wallet_accounts) $ \k net (AccountStorage as) -> fromMaybe mempty $ do
            accounts <- Map.lookup net as
            nva <- Map.lookup (_keyPair_publicKey $ _key_pair k) (_accounts_nonVanity accounts)
            pure nva
      results <- listWithKey mAccounts $ accountRow visible
      let balances :: Dynamic t [Maybe AccountBalance]
          balances = join $ traverse fst . Map.elems <$> results
          dialogs = switch $ leftmost . fmap snd . Map.elems <$> current results
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
        td $ divClass "wallet__table-wallet-address" $ dynText $ keyToText . _keyPair_publicKey . _key_pair <$> key
        td $ dynText $ unAccountNotes . _key_notes <$> key
        td $ dynText $ uiAccountBalance . Just <$> balance
        dialog <- td $ buttons $ do
          recv <- receiveButton cfg
          onDetails <- detailsButton (cfg & uiButtonCfg_class <>~ " wallet__table-button--hamburger")

          let pk = AccountName . keyToText . _keyPair_publicKey . _key_pair <$> current key

          pure $ leftmost
            [ -- TODO details dialog
            -- TODO we need a way of adding these accounts when they already
            -- exist too, e.g. for users who are recovering wallets. An
            -- automatic account discovery thing would work well.
              (\x -> KeyDialog_Receive x AccountCreated_No) <$> pk <@ recv
            ]
        pure (clk, dialog)

      accountRow visible chain dAccount = trAcc visible $ do
        td blank -- Arrow column
        td $ text $ "Chain ID: " <> _chainId chain
        td blank -- Notes column
        td $ dynText $ uiAccountBalance' . _nonVanityAccount_info <$> dAccount
        td $ buttons $ do
          send <- sendButton cfg
          onDetails <- detailsButton (cfg & uiButtonCfg_class <>~ " wallet__table-button--hamburger")
          let mkSendDialog k acc = KeyDialog_Send $ AccountRef_NonVanity (_keyPair_publicKey $ _key_pair k) chain ==> acc
          pure
            ( _accountInfo_balance . _nonVanityAccount_info <$> dAccount
            , leftmost
              [ mkSendDialog <$> current key <*> current dAccount <@ send
              ]
            )
