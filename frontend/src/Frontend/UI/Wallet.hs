{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Wallet management ui for handling private/public keys.
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.UI.Wallet
  ( -- * Key management widget
    uiWallet
  , uiAvailableKeys
    -- ** Filters for keys
  , hasPrivateKey
  , HasUiWalletModelCfg
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad               (when, (<=<))
import qualified Data.IntMap                 as IntMap
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import Obelisk.Route.Frontend
import qualified Pact.Types.PactValue as Pact
import qualified Pact.Types.Exp as Pact
import qualified Pact.Types.ChainId as Pact
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import Common.Route
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

-- | UI for managing the keys wallet.
uiWallet
  :: forall m t key model mConf
     . ( MonadWidget t m
       , HasUiWalletModelCfg model mConf key m t
       , HasCrypto key m
       , Routed t (R FrontendRoute) m
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

-- | Widget listing all available keys.
uiAvailableKeys
  :: forall t m model mConf key.
     ( MonadWidget t m
     , HasUiWalletModelCfg model mConf key m t
     , HasCrypto key m
     , Routed t (R FrontendRoute) m
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
     , HasCrypto key m
     , Routed t (R FrontendRoute) m
     )
  => model
  -> m mConf
uiKeyItems model = do
  let
    keyMap' = model ^. wallet_accounts
    keyMap = Map.fromAscList . IntMap.toAscList <$> keyMap'
    tableAttrs =
      "style" =: "table-layout: fixed; width: 98%"
      <> "class" =: "wallet table"
  events <- elAttr "table" tableAttrs $ do
    el "colgroup" $ do
      elAttr "col" ("style" =: "width: 19%") blank
      elAttr "col" ("style" =: "width: 19%") blank
      elAttr "col" ("style" =: "width: 10%") blank
      elAttr "col" ("style" =: "width: 19%") blank
      elAttr "col" ("style" =: "width: 15%") blank
      elAttr "col" ("style" =: "width: 20%") blank
    el "thead" $ el "tr" $ do
      let mkHeading = elClass "th" "wallet__table-heading" . text
      traverse_ mkHeading $
        [ "Account Name"
        , "Public Key"
        , "Chain ID"
        , "Notes"
        , "Balance"
        , ""
        ]

    el "tbody" $ listWithKey keyMap (uiKeyItem model)

  dyn_ $ ffor keyMap $ \keys -> when (Map.null keys) $ text "No accounts ..."

  let
    onAccountModal = switchDyn $ leftmost . Map.elems <$> events

    accModal (d,i,a) = Just $ case d of
      -- AccountDialog_Delete -> uiDeleteConfirmation i (_account_name a)
      AccountDialog_Details -> uiAccountDetails i a
      AccountDialog_Receive -> uiReceiveModal model a
      AccountDialog_Send -> uiSendModal model a

  -- TODO test if this works. Testnet is down at the time of writing.
  refresh <- getPostBuild

  pure $ mempty
    & modalCfg_setModal .~ (accModal <$> onAccountModal)
    & walletCfg_refreshBalances .~ refresh

------------------------------------------------------------------------------
-- | Display a key as list item together with it's name.
uiKeyItem
  :: (MonadWidget t m, HasNetwork model t, HasCrypto key (Performable m))
  => model
  -> IntMap.Key
  -> Dynamic t (SomeAccount key)
  -> m (Event t (AccountDialog, IntMap.Key, Account key))
uiKeyItem model i d = do
  let selectedNetwork = model ^. network_selectedNetwork
  md <- maybeDyn $ someAccount Nothing Just <$> d
  switchHold never <=< dyn $ ffor md $ \case
    Nothing -> pure never
    Just account -> do
      onSelectedNetwork <- holdUniqDyn $ (==) . _account_network <$> account <*> selectedNetwork
      switchHold never <=< dyn $ ffor onSelectedNetwork $ \case
        False -> pure never
        True -> do
          elClass "tr" "wallet__table-row" $ do
            let td = elClass "td" "wallet__table-cell"

            td $ divClass "wallet__table-wallet-address" $ dynText $ unAccountName . _account_name <$> account
            td $ divClass "wallet__table-wallet-address" $ dynText $ keyToText . _keyPair_publicKey . _account_key <$> account
            td $ dynText $ Pact._chainId . _account_chainId <$> account
            td $ dynText $ _account_notes <$> account
            td $ dynText $ ffor account $ \a -> case _account_balance a of
              Nothing -> "Unknown"
              Just b -> tshow (unAccountBalance b) <> " KDA"

            td $ divClass "wallet__table-buttons" $ do
              let cfg = def
                    & uiButtonCfg_class <>~ "wallet__table-button"

              recv <- receiveButton cfg
              send <- sendButton cfg
              onDetails <- detailsButton (cfg & uiButtonCfg_class <>~ " wallet__table-button--hamburger")

              let mkDialog dia onE = (\a -> (dia, i, a)) <$> current account <@ onE

              pure $ leftmost
                [ mkDialog AccountDialog_Details onDetails
                , mkDialog AccountDialog_Receive recv
                , mkDialog AccountDialog_Send send
                ]
