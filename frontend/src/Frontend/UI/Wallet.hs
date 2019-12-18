{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
import           Control.Monad               (when, (<=<))
import           Data.Dependent.Sum          (DSum (..))
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
uiWalletRefreshButton _model = do
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
      AccountDialog_Receive -> uiReceiveModal model a (Just $ accountChain a)
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
        info = accountInfo <$> acc
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

  pure $ mempty
-- TODO   & modalCfg_setModal .~ (accModal <$> onAccountModal)
    & walletCfg_refreshBalances .~ refresh

------------------------------------------------------------------------------
-- | Display a key as list item together with its name.
uiKeyItem
  :: (MonadWidget t m, HasNetwork model t)
  => model
  -> IntMap.Key
  -> Dynamic t (Key key)
  -> m (Event t (AccountDialog, IntMap.Key, Key key))
uiKeyItem model i key = do
  hidden <- holdUniqDyn $ _key_hidden <$> key
  switchHold never <=< dyn $ ffor hidden $ \case
    True -> pure never
    False -> do
      clk <- keyRow
      visible <- toggle False clk
      --TODO listWithKey accs (\_ -> accountRow visible)
      pure never
     where
      trKey = elClass "tr" "wallet__table-row wallet__table-row-key"
      trAcc visible = elDynAttr "tr" $ ffor visible $ \v -> mconcat
        [ "class" =: "wallet__table-row"
        ,  bool mempty ("style" =: "display:none") v
        ]
      td = elClass "td" "wallet__table-cell"
      buttons = divClass "wallet__table-buttons"
      cfg = def
        & uiButtonCfg_class <>~ "wallet__table-button"

      keyRow = trKey $ do
        clk <- td $ accordionButton def
        td $ divClass "wallet__table-wallet-address" $ dynText $ keyToText . _keyPair_publicKey . _key_pair <$> key
        td $ dynText $ unAccountNotes . _key_notes <$> key
        td $ text "TODO"
        --TODO
        --dynText $ ffor key $ \a -> case _account_balance a of
        --  Nothing -> "Unknown"
        --  Just b -> tshow (unAccountBalance b) <> " KDA" <> maybe "" (const "*") (_account_unfinishedCrossChainTransfer a)
        td $ buttons $ do
          recv <- receiveButton cfg
          onDetails <- detailsButton (cfg & uiButtonCfg_class <>~ " wallet__table-button--hamburger")
          pure ()

          {- TODO
          let mkDialog dia onE = (\a -> (dia, i, a)) <$> current key <@ onE

          pure $ leftmost
            [ mkDialog AccountDialog_Details onDetails
            , mkDialog AccountDialog_Receive recv
            , mkDialog AccountDialog_Send send
            ] -}
        pure clk

      accountRow visible acc = trAcc visible $ do
        td blank
        td $ dynText $ ffor acc $ uiAccountChain
        td $ dynText $ ffor acc $ uiAccountNotes
        td $ dynText $ ffor acc $ uiAccountBalance
        td $ buttons $ do
          send <- sendButton cfg
          onDetails <- detailsButton (cfg & uiButtonCfg_class <>~ " wallet__table-button--hamburger")
          pure ()
