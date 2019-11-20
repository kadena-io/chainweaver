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
    -- * Keys related helper widgets
--  , uiSelectKey
  , getBalance
  , showBalance
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
import qualified Pact.Types.PactValue as Pact
import qualified Pact.Types.Exp as Pact
import qualified Pact.Types.ChainId as Pact
import           Reflex
import           Reflex.Dom
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
      elAttr "col" ("style" =: "width: 16%") blank
      elAttr "col" ("style" =: "width: 16%") blank
      elAttr "col" ("style" =: "width: 16%") blank
      elAttr "col" ("style" =: "width: 16%") blank
      elAttr "col" ("style" =: "width: 16%") blank
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

    el "tbody" $ listWithKey keyMap (uiKeyItem $ model ^. network_selectedNetwork)

  dyn_ $ ffor keyMap $ \keys -> when (Map.null keys) $ text "No accounts ..."

  let
    onAccountModal = switchDyn $ leftmost . Map.elems <$> events

    accModal (d,i,a) = Just $ case d of
      -- AccountDialog_Delete -> uiDeleteConfirmation i (_account_name a)
      AccountDialog_Details -> uiAccountDetails i a
      AccountDialog_Receive -> uiReceiveModal model a
      AccountDialog_Send -> uiSendModal model a

  pure $ mempty & modalCfg_setModal .~ (accModal <$> onAccountModal)

------------------------------------------------------------------------------
-- | Display a key as list item together with it's name.
uiKeyItem
  :: MonadWidget t m
  => Dynamic t NetworkName
  -> IntMap.Key
  -> Dynamic t (SomeAccount key)
  -> m (Event t (AccountDialog, IntMap.Key, Account key))
uiKeyItem selectedNetwork i d = do
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

            td $ dynText $ unAccountName . _account_name <$> account
            td $ dynText $ keyToText . _keyPair_publicKey . _account_key <$> account
            td $ dynText $ Pact._chainId . _account_chainId <$> account
            td $ dynText $ _account_notes <$> account
            -- TODO balance
            td $ text "Balance Placeholder"

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

-- | Get the balance of an account from the network. 'Nothing' indicates _some_
-- failure, either a missing account or connectivity failure. We have no need to
-- distinguish between the two at this point.
getBalance
  :: (MonadWidget t m, HasNetwork model t, HasCrypto key (Performable m))
  => model -> ChainId -> Event t AccountName -> m (Event t (Maybe AccountBalance))
getBalance model chain account = do
  networkRequest <- performEvent $ attachWith mkReq (current $ getNetworkNameAndMeta model) account
  response <- performLocalReadCustom (model ^. network) pure networkRequest
  let toBalance (_, [Right (_, Pact.PLiteral (Pact.LDecimal d))]) = Just $ AccountBalance d
      toBalance _ = Nothing
  pure $ toBalance <$> response
  where
    accountBalanceReq acc = "(coin.get-balance " <> tshow (unAccountName acc) <> ")"
    mkReq (netName, pm) acc = mkSimpleReadReq (accountBalanceReq acc) netName pm (ChainRef Nothing chain)

-- | Display the balance of an account after retrieving it from the network
showBalance
  :: (MonadWidget t m, HasNetwork model t, HasCrypto key (Performable m))
  => model -> Event t () -> ChainId -> AccountName -> m ()
showBalance model refresh chain acc = do
  -- This delay ensures we have the networks stuff set up by the time we do the
  -- requests, thus avoiding immediate failure.
  pb <- delay 2 =<< getPostBuild
  bal <- getBalance model chain $ acc <$ (pb <> refresh)
  _ <- runWithReplace (text "Loading...") $ ffor bal $ \case
    Nothing -> text "Unknown"
    Just b -> text $ tshow $ unAccountBalance b
  pure ()
