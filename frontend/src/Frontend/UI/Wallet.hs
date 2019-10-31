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
import           Frontend.UI.Dialogs.DeleteConfirmation (uiDeleteConfirmation)
import           Frontend.UI.Dialogs.AddAccount (uiCreateWalletOnlyAccount, uiWalletOnlyAccountCreated)
import           Frontend.UI.Modal
import           Frontend.UI.Modal.Impl (ModalImpl, ModalIde, ModalIdeCfg, showModal)
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

-- | UI for managing the keys wallet.
uiWallet
  :: forall m t key mConf
     . ( MonadWidget t m
       , HasUiWalletModelCfg mConf key m t
       )
  => Wallet key t
  -> ModalIde m key t
  -> m (mConf, ModalIdeCfg m key t)
uiWallet w m = divClass "keys group" $ do
  eOpenAddAccount <- confirmButton def "+ Add Account"

  onCreate <- uiCreateKey w
  keysCfg <- uiAvailableKeys w

  pure ( keysCfg & walletCfg_genKey .~ fmap (\a -> (a, "0", "")) onCreate -- TODO let user pick chain/notes
       , mempty & modalCfg_setModal .~ leftmost
         [ Just (uiCreateWalletOnlyAccount m) <$ eOpenAddAccount
         , Just . uiWalletOnlyAccountCreated m <$> (w ^. wallet_walletOnlyAccountCreated)
         ]
       )

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


-- | Line input with "Create" button for creating a new key.
uiCreateKey :: MonadWidget t m => Wallet key t -> m (Event t AccountName)
uiCreateKey w =
  validatedInputWithButton "group__header" (checkAccountNameValidity w) "Enter account name" "Generate"


-- | Widget listing all available keys.
uiAvailableKeys
  :: (MonadWidget t m, HasUiWalletModelCfg mConf key m t)
  => Wallet key t
  -> m mConf
uiAvailableKeys aWallet = do
  divClass "keys-list" $ do
    uiKeyItems aWallet


-- | Render a list of key items.
--
-- Does not include the surrounding `div` tag. Use uiAvailableKeys for the
-- complete `div`.
uiKeyItems
  :: (MonadWidget t m, HasUiWalletModelCfg mConf key m t)
  => Wallet key t
  -> m mConf
uiKeyItems aWallet = do
    let
      keyMap' = aWallet ^. wallet_accounts
      keyMap = Map.fromAscList . IntMap.toAscList <$> keyMap'
      tableAttrs =
        "style" =: "table-layout: fixed; width: 100%"
        <> "class" =: "wallet table"
    events <- elAttr "table" tableAttrs $ do
      el "colgroup" $ do
        elAttr "col" ("style" =: "width: 20%") blank
        elAttr "col" ("style" =: "width: 35%") blank
        elAttr "col" ("style" =: "width: 15%") blank
        elAttr "col" ("style" =: "width: 35%") blank
        elAttr "col" ("style" =: "width: 15%") blank
        elAttr "col" ("style" =: "width: 10%") blank
      el "thead" $ el "tr" $ do
        let mkHeading = elClass "th" "table__heading" . text
        traverse_ mkHeading $
          [ "Account Name"
          , "Public Key"
          , "Chain ID"
          , "Notes"
          , "Balance"
          ]

      el "tbody" $ listWithKey keyMap uiKeyItem

    dyn_ $ ffor keyMap $ \keys -> when (Map.null keys) $ text "No accounts ..."
    let onDelKey = switchDyn $ leftmost . Map.elems <$> events
    pure $ mempty
      & modalCfg_setModal .~ fmap (Just . uncurry uiDeleteConfirmation) onDelKey

------------------------------------------------------------------------------
-- | Display a key as list item together with it's name.
uiKeyItem
  :: MonadWidget t m
  => IntMap.Key
  -> Dynamic t (SomeAccount key)
  -> m (Event t (IntMap.Key, AccountName))
uiKeyItem i d = do
  md <- maybeDyn $ someAccount Nothing Just <$> d
  switchHold never <=< dyn $ ffor md $ \case
    Nothing -> pure never
    Just account -> elClass "tr" "table__row" $ do
      el "td" $ dynText $ unAccountName . _account_name <$> account

      let public = keyToText . _keyPair_publicKey . _account_key <$> account
      elClass "td" "wallet__key wallet__key_type_public" $ dynText public
      el "td" $ dynText $ Pact._chainId . _account_chainId <$> account
      el "td" $ dynText $ _account_notes <$> account
      -- TODO balance
      el "td" $ text "Balance Placeholder"

      onDel <- elClass "td" "wallet__delete" $
        deleteButton $
          def & uiButtonCfg_title .~ Just "Delete key permanently"
              & uiButtonCfg_class %~ (<> "wallet__add-delete-button")

      pure (attachWith (\a _ -> (i, _account_name a)) (current account) onDel)

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
    accountBalanceReq acc = "(coin.account-balance " <> tshow (unAccountName acc) <> ")"
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
