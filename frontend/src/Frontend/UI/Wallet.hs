{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ConstraintKinds       #-}

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
import           Control.Monad               (void, when, (<=<))
import qualified Data.IntMap                 as IntMap
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import qualified Pact.Types.PactValue as Pact
import qualified Pact.Types.Exp as Pact
import qualified Pact.Types.ChainId as Pact
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.Ide (_ide_wallet, _ide_network)
import           Frontend.Crypto.Class
import           Frontend.Crypto.Ed25519     (keyToText)
import           Frontend.Wallet
import           Frontend.UI.Widgets
import           Frontend.Foundation
import           Frontend.UI.Dialogs.DeleteConfirmation (uiDeleteConfirmation)
import           Frontend.UI.Dialogs.AccountDetails (uiAccountDetails)
import           Frontend.UI.Modal
import           Frontend.UI.Modal.Impl (ModalIde)
import           Frontend.Network
------------------------------------------------------------------------------

-- | Constraints on the model config we have for implementing this widget.
type HasUiWalletModelCfg mConf key m t =
  ( Monoid mConf, Flattenable mConf t
  , HasModalCfg mConf (Modal mConf m t) t
  , IsWalletCfg mConf key t
  , HasWalletCfg (ModalCfg mConf t) key t
  , Flattenable (ModalCfg mConf t) t
  , Monoid (ModalCfg mConf t)
  )

-- | Possible actions from an account
data AccountDialog
  = AccountDialog_Details
  | AccountDialog_Delete
  deriving Eq

-- | UI for managing the keys wallet.
uiWallet
  :: forall m t key mConf
     . ( MonadWidget t m
       , HasUiWalletModelCfg mConf key m t
       )
  => ModalIde m key t
  -> m mConf
uiWallet ideL =
  uiAvailableKeys (_ide_wallet ideL) (_ide_network ideL)

----------------------------------------------------------------------
-- Keys related helper widgets:
----------------------------------------------------------------------

-- | UI for letting the user select a particular key
-- from a filtered view of the available keys.
--uiSelectKey
--  :: MonadWidget t m
--  => Wallet t
--  -> ((Text, KeyPair) -> Bool)
--  -> m (Dynamic t (Maybe Text))
--uiSelectKey w kFilter = do
--  let keyNames = map fst . filter kFilter . Map.toList <$> _wallet_accounts w
--      mkPlaceholder ks = if null ks then "No keys available" else "Select key"
--      options = Map.fromList . (Nothing:"No keys") . (\a -> (Just a,a)) <$> keyNames
--  d <- dropdown Nothing options def
--  pure $ _dropdown_value d

-- | Check whether a given key does contain a private key.
hasPrivateKey :: (Text, KeyPair PrivateKey) -> Bool
hasPrivateKey = isJust . _keyPair_privateKey . snd

----------------------------------------------------------------------

-- | Widget listing all available keys.
uiAvailableKeys
  :: forall t m mConf key.
     ( MonadWidget t m
     , HasUiWalletModelCfg mConf key m t
     )
  => Wallet key t
  -> Network t
  -> m mConf
uiAvailableKeys aWallet aNetwork = do
  divClass "wallet__keys-list" $ do
    uiKeyItems aWallet aNetwork


-- | Render a list of key items.
--
-- Does not include the surrounding `div` tag. Use uiAvailableKeys for the
-- complete `div`.
uiKeyItems
  :: forall t m mConf key.
     ( MonadWidget t m
     , HasUiWalletModelCfg mConf key m t
     )
  => Wallet key t
  -> Network t
  -> m mConf
uiKeyItems aWallet aNetwork = do
  let
    keyMap' = aWallet ^. wallet_accounts
    keyMap = Map.fromAscList . IntMap.toAscList <$> keyMap'
    tableAttrs =
      "style" =: "table-layout: fixed; width: 100%"
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

    el "tbody" $ listWithKey keyMap uiKeyItem

  dyn_ $ ffor keyMap $ \keys -> when (Map.null keys) $ text "No accounts ..."

  let
    onAccountModal = switchDyn $ leftmost . Map.elems <$> events

    accModal (d,i,a) = Just $ case d of
      AccountDialog_Delete -> uiDeleteConfirmation i (_account_name a)
      AccountDialog_Details -> uiAccountDetails aNetwork a

  pure $ mempty & modalCfg_setModal .~ (accModal <$> onAccountModal)

------------------------------------------------------------------------------
-- | Display a key as list item together with it's name.
uiKeyItem
  :: MonadWidget t m
  => IntMap.Key
  -> Dynamic t (SomeAccount key)
  -> m (Event t (AccountDialog, IntMap.Key, Account key))
uiKeyItem i d = do
  md <- maybeDyn $ someAccount Nothing Just <$> d
  switchHold never <=< dyn $ ffor md $ \case
    Nothing -> pure never
    Just account -> elClass "tr" "wallet__table-row" $ do
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

        void $ receiveButton $ cfg & uiButtonCfg_disabled .~ True
        void $ sendButton $ cfg & uiButtonCfg_disabled .~ True
        onDetails <- detailsButton cfg

        onDel <- do
          deleteButton $
            def & uiButtonCfg_title .~ Just "Delete key permanently"
                & uiButtonCfg_class %~ (<> "wallet__add-delete-button")

        let mkDialog dia onE = (\a -> (dia, i, a)) <$> current account <@ onE

        pure $ leftmost
          [ mkDialog AccountDialog_Delete onDel
          , mkDialog AccountDialog_Details onDetails
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
