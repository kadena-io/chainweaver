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

    -- ** Filters for keys
  , hasPrivateKey
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad               (when, void)
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.Text                   (Text)
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.Crypto.Ed25519     (keyToText)
import           Frontend.Wallet
import           Frontend.UI.Widgets
import           Frontend.Foundation
------------------------------------------------------------------------------



-- | UI for managing the keys wallet.
uiWallet
  :: (MonadWidget t m, IsWalletCfg cfg t)
  => Wallet t
  -> m cfg
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
  validatedInputWithButton "group__header" check "Enter key name" "Generate"
    where
      check k = do
        keys <- sample $ current $ _wallet_keys w
        pure $ if Map.member k keys then Just "This key name is already in use." else Nothing

-- | Widget listing all available keys.
uiAvailableKeys
  :: (MonadWidget t m, IsWalletCfg cfg t)
  => Wallet t
  -> m cfg
uiAvailableKeys aWallet = do
  divClass "keys-list" $ do
    uiKeyItems aWallet


-- | Render a list of key items.
--
-- Does not include the surrounding `div` tag. Use uiAvailableKeys for the
-- complete `div`.
uiKeyItems
  :: (MonadWidget t m, IsWalletCfg cfg t)
  => Wallet t
  -> m cfg
uiKeyItems aWallet = do
  let
    keyMap = aWallet ^. wallet_keys
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
        [ "Key Name"
        , "Public Key"
        , ""
        , "Private Key"
        , ""
        , ""
        ]
    el "tbody" $ listWithKey keyMap $ \name key -> uiKeyItem (name, key)
  dyn_ $ ffor keyMap $ \keys -> when (Map.null keys) $ text "No keys ..."
  let delKey = switchDyn $ leftmost . Map.elems <$> events
  pure $ mempty & walletCfg_delKey .~ delKey


------------------------------------------------------------------------------
-- | Display a key as list item together with it's name.
uiKeyItem
  :: MonadWidget t m
  => (Text, Dynamic t KeyPair)
  -> m (Event t KeyName)
uiKeyItem (n, k) = do
    elClass "tr" "table__row" $ do
      el "td" $ text n

      (pEl, _) <- elClass' "td" "wallet__key wallet__key_type_public" $
        dynText (keyToText . _keyPair_publicKey <$> k)
      el "td" $
        void $ copyButton copyBtnCfg $ _element_raw pEl

      (privEl, isShown) <- keyCopyWidget "td" "wallet__key wallet__key_type_private" $
        maybe "No key" keyToText . _keyPair_privateKey <$> k
      el "td" $ do
        let cfg = copyBtnCfg & uiButtonCfg_disabled .~ fmap not isShown
        void $ copyButton cfg $ _element_raw privEl

      onDel <- elClass "td" "wallet__delete" deleteButton

      pure (const n <$> onDel)
  where
    copyBtnCfg =
      btnCfgTertiary & uiButtonCfg_class %~ (fmap (<> "button_size_tiny"))

-- | Widget showing/hiding a private key.
--
--   The returned Dynamic tells whether the key is currently hidden or shown.
keyCopyWidget
  :: MonadWidget t m
  => Text
  -> Text
  -> Dynamic t Text
  -> m (Element EventResult (DomBuilderSpace m) t, Dynamic t Bool)
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
  pure (e, isShown)
