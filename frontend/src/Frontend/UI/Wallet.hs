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
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad               (when, void)
import           Data.Decimal                (Decimal)
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.Text                   (Text)
import qualified Pact.Types.PactValue as Pact
import qualified Pact.Types.Exp as Pact
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.Crypto.Ed25519     (keyToText)
import           Frontend.Wallet
import           Frontend.UI.Widgets
import           Frontend.Foundation
import           Frontend.UI.Dialogs.KeyImport (uiKeyImport, HasUiKeyImportModelCfg)
import           Frontend.UI.Dialogs.DeleteConfirmation (uiDeleteConfirmation)
import           Frontend.UI.Modal
import           Frontend.Network
------------------------------------------------------------------------------


-- | Constraints on the model config we have for implementing this widget.
type HasUiWalletModelCfg mConf m t =
  ( Monoid mConf, Flattenable mConf t
  , HasModalCfg mConf (Modal mConf m t) t
  , HasUiKeyImportModelCfg (ModalCfg mConf t) t
  , IsWalletCfg mConf t
  )

-- | UI for managing the keys wallet.
uiWallet
  :: (MonadWidget t m, HasUiWalletModelCfg mConf m t)
  => Wallet t
  -> m mConf
uiWallet w = divClass "keys group" $ do
    onCreate <- uiCreateKey w
    keysCfg <- uiAvailableKeys w

    pure $ keysCfg & walletCfg_genKey .~ onCreate

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
--  let keyNames = map fst . filter kFilter . Map.toList <$> _wallet_keys w
--      mkPlaceholder ks = if null ks then "No keys available" else "Select key"
--      options = Map.fromList . (Nothing:"No keys") . (\a -> (Just a,a)) <$> keyNames
--  d <- dropdown Nothing options def
--  pure $ _dropdown_value d

-- | Check whether a given key does contain a private key.
hasPrivateKey :: (Text, KeyPair) -> Bool
hasPrivateKey = isJust . _keyPair_privateKey . snd

----------------------------------------------------------------------


-- | Line input with "Create" button for creating a new key.
uiCreateKey :: MonadWidget t m => Wallet t -> m (Event t KeyName)
uiCreateKey w =
  validatedInputWithButton "group__header" (checkAccountNameUniqueness w) "Enter account name" "Generate"


-- | Widget listing all available keys.
uiAvailableKeys
  :: (MonadWidget t m, HasUiWalletModelCfg mConf m t)
  => Wallet t
  -> m mConf
uiAvailableKeys aWallet = do
  divClass "keys-list" $ do
    uiKeyItems aWallet


-- | Render a list of key items.
--
-- Does not include the surrounding `div` tag. Use uiAvailableKeys for the
-- complete `div`.
uiKeyItems
  :: (MonadWidget t m, HasUiWalletModelCfg mConf m t)
  => Wallet t
  -> m mConf
uiKeyItems aWallet = do
    let
      keyMap = aWallet ^. wallet_keys
      tableAttrs =
        "style" =: "table-layout: fixed; width: 100%"
        <> "class" =: "wallet table"
    (events, onAdd) <- elAttr "table" tableAttrs $ do
      el "colgroup" $ do
        elAttr "col" ("style" =: "width: 20%") blank
        elAttr "col" ("style" =: "width: 35%") blank
        elAttr "col" ("style" =: "width: 15%") blank
        elAttr "col" ("style" =: "width: 35%") blank
        elAttr "col" ("style" =: "width: 15%") blank
        elAttr "col" ("style" =: "width: 10%") blank
      onAdd <- el "thead" $ el "tr" $ do
        let mkHeading = elClass "th" "table__heading" . text
        traverse_ mkHeading $
          [ "Account Name"
          , "Public Key"
          , ""
          , "Private Key"
          , ""
          ]
        elClass "th" "table__heading wallet__add" $
          importButton

      el "tbody" $ do
        evs <- listWithKey keyMap $ \name key -> uiKeyItem (name, key)
        pure (evs, onAdd)

    dyn_ $ ffor keyMap $ \keys -> when (Map.null keys) $ text "No accounts ..."
    let onDelKey = switchDyn $ leftmost . Map.elems <$> events
    pure $ mempty
      & modalCfg_setModal .~ leftmost
        [ Just (uiKeyImport aWallet) <$ onAdd
        , Just . uiDeleteConfirmation <$> onDelKey
        ]
  where
    importButton =
      addButton $ def
        & uiButtonCfg_title .~ Just "Import existing key"
        & uiButtonCfg_class %~ (<> "wallet__add-delete-button")


------------------------------------------------------------------------------
-- | Display a key as list item together with it's name.
uiKeyItem
  :: MonadWidget t m
  => (Text, Dynamic t KeyPair)
  -> m (Event t KeyName)
uiKeyItem (n, k) = do
    elClass "tr" "table__row" $ do
      el "td" $ text n

      let public = keyToText . _keyPair_publicKey <$> k
      elClass "td" "wallet__key wallet__key_type_public" $ dynText public
      el "td" $
        void $ copyButton copyBtnCfg $ current public

      let private = maybe "No key" keyToText . _keyPair_privateKey <$> k
      isShown <- keyCopyWidget "td" "wallet__key wallet__key_type_private" private
      el "td" $ do
        let cfg = copyBtnCfg & uiButtonCfg_disabled .~ fmap not isShown
        void $ copyButton cfg $ current private

      onDel <- elClass "td" "wallet__delete" $
        deleteButton $
          def & uiButtonCfg_title .~ Just "Delete key permanently"
              & uiButtonCfg_class %~ (<> "wallet__add-delete-button")

      pure (const n <$> onDel)
  where
    copyBtnCfg =
      btnCfgTertiary
        & uiButtonCfg_class %~ (fmap (<> "button_size_tiny"))
        & uiButtonCfg_title .~ pure (Just "Copy key to clipboard")

-- | Widget showing/hiding a private key.
--
--   The returned Dynamic tells whether the key is currently hidden or shown.
keyCopyWidget
  :: MonadWidget t m
  => Text
  -> Text
  -> Dynamic t Text
  -> m (Dynamic t Bool)
keyCopyWidget t cls keyText = mdo
  isShown <- foldDyn (const not) False (domEvent Click e)
  let mkShownCls True = ""
      mkShownCls False = " wallet__key_hidden"

  (e, _) <- elDynClass t (pure cls <> fmap mkShownCls isShown) $ do
    let
      mkText True k = k
      mkText False _ = "****************************"

    elDynClass' "span" "key-content" $
      dynText (mkText <$> isShown <*> keyText)
  pure isShown

-- | Get the balance of an account from the network. 'Nothing' indicates _some_
-- failure, either a missing account or connectivity failure. We have no need to
-- distinguish between the two at this point.
getBalance :: (MonadWidget t m, HasNetwork model t) => model -> ChainId -> Event t AccountName -> m (Event t (Maybe Decimal))
getBalance model chain account = do
  networkRequest <- performEvent $ attachWith mkReq (current $ getNetworkNameAndMeta model) account
  response <- performLocalReadCustom (model ^. network) pure networkRequest
  let toBalance (_, [Right (_, Pact.PLiteral (Pact.LDecimal d))]) = Just d
      toBalance _ = Nothing
  pure $ toBalance <$> response
  where
    accountBalanceReq acc = "(coin.get-balance " <> tshow (unAccountName acc) <> ")"
    mkReq (netName, pm) acc = mkSimpleReadReq (accountBalanceReq acc) netName pm (ChainRef Nothing chain)

-- | Display the balance of an account after retrieving it from the network
showBalance
  :: (MonadWidget t m, HasNetwork model t)
  => model -> Event t () -> ChainId -> AccountName -> m ()
showBalance model refresh chain acc = do
  -- This delay ensures we have the networks stuff set up by the time we do the
  -- requests, thus avoiding immediate failure.
  pb <- delay 2 =<< getPostBuild
  bal <- getBalance model chain $ acc <$ (pb <> refresh)
  _ <- runWithReplace (text "Loading...") $ ffor bal $ \case
    Nothing -> text "Unknown"
    Just b -> text $ tshow b
  pure ()
