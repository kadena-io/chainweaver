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
import           Control.Arrow               ((&&&))
import           Control.Lens
import           Control.Monad               (when)
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import           Reflex
import           Reflex.Dom
import           Obelisk.Generated.Static
------------------------------------------------------------------------------
import           Frontend.Crypto.Ed25519     (keyToText)
import           Frontend.Foundation
import           Frontend.UI.Button
import           Frontend.UI.Icon
import           Frontend.Wallet
import           Frontend.Widgets
------------------------------------------------------------------------------



-- | UI for managing the keys wallet.
uiWallet
  :: (MonadWidget t m, IsWalletCfg cfg t)
  => Wallet t
  -> m cfg
uiWallet w = divClass "keys" $ do
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
uiCreateKey w = validatedInputWithButton check "Enter key name" "Generate"
  where
    check k = do
      keys <- sample $ current $ _wallet_keys w
      pure $ if Map.member k keys then Left "This key name is already in use." else Right k

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
  let keyMap = aWallet ^. wallet_keys
      signingKeys = aWallet ^. wallet_signingKeys
  events <- elAttr "table" ("style" =: "table-layout: fixed; width: 100%") $ do
    el "colgroup" $ do
      elAttr "col" ("style" =: "width: 20%") blank
      elAttr "col" ("style" =: "width: 30%") blank
      elAttr "col" ("style" =: "width: 30%") blank
      elAttr "col" ("style" =: "width: 10%") blank
      elAttr "col" ("style" =: "width: 10%") blank
    el "thead" $ el "tr" $ do
      el "th" $ text "Key Name"
      el "th" $ text "Public Key"
      el "th" $ text "Private Key"
      el "th" $ text "Signing"
      el "th" $ text "Delete"
    el "tbody" $ listWithKey keyMap $ \name key -> uiKeyItem signingKeys (name, key)
  dyn_ $ ffor keyMap $ \keys -> when (Map.null keys) $ text "No keys ..."
  let setSigning = switchDyn $ leftmost . fmap fst . Map.elems <$> events
      delKey = switchDyn $ leftmost . fmap snd . Map.elems <$> events
  pure $ mempty
    & walletCfg_setSigning .~ setSigning
    & walletCfg_delKey .~ delKey


------------------------------------------------------------------------------
-- | Display a key as list item together with it's name.
uiKeyItem
  :: MonadWidget t m
  => Dynamic t (Set KeyName)
  -> (Text, Dynamic t KeyPair)
  -> m (Event t (KeyName, Bool), Event t KeyName)
uiKeyItem signingKeys (n, k) = do
    el "tr" $ do
      el "td" $ text n
      elClass "td" "public walletkey" $
        dynText (keyToText . _keyPair_publicKey <$> k)
      elClass "td" "private walletkey" $
        keyCopyWidget (maybe "No key" keyToText . _keyPair_privateKey <$> k)

      isSigning <- tagOnPostBuild $ Set.member n <$> signingKeys
      box <- elClass "td" "centercell" $ checkbox False $ def
        & checkboxConfig_setValue .~ isSigning
      onDel <- elClass "td" "centercell" $ uiIcon "fa-trash" $ def
        & iconConfig_size .~ Just IconLG
        & iconConfig_attrs .~ ("type" =: "button")

      pure ((fmap (n, ) . _checkbox_change $ box), fmap (const n) onDel)

    --elClass "div" "key" $ do
    --  (box, onDel) <- divClass "header" $ do

    --    el "h4" $ do
    --      text (" " <> n)
    --      -- TODO Later
    --      --elClass "span" "description" $ dynText $ keyDescription <$> k

    --    isSigning <- tagOnPostBuild $ Set.member n <$> signingKeys

    --    divClass "signing" $ do
    --      box <- checkbox False $ def
    --                & checkboxConfig_setValue .~ isSigning

    --      text "Signing"
    --      return box

    --    onDelI <- divClass "delete" $ uiIcon "fa-trash" $ def
    --      & iconConfig_size .~ Just IconLG
    --    pure (boxI, onDelI)

    --  dyn_ $ ffor viewKeys $ \case
    --    False -> pure ()
    --    True -> uiKeyDetails (keyToText . _keyPair_publicKey <$> k)
    --                         (maybe "No key" keyToText . _keyPair_privateKey <$> k)
    --  pure $ ((fmap (n, ) . _checkbox_change $ box), fmap (const n) onDel)
  --where
  --  keyDescription k1 =
  --    case _keyPair_privateKey k1 of
  --      Nothing -> "Public key only"
  --      Just _  -> "Full key pair"

keyCopyWidget :: MonadWidget t m => Dynamic t Text -> m ()
keyCopyWidget keyText = mdo
  isShown <- foldDyn (const not) False (domEvent Click e)
  let mkText True t = t
      mkText False _ = "****************************"
  (e,_) <- elAttr' "span" ("class" =: "key-content") $
    dynText (mkText <$> isShown <*> keyText)
  return ()

uiKeyDetails :: MonadWidget t m => Dynamic t Text -> Dynamic t Text -> m ()
uiKeyDetails public private = do
    divClass "pair" $ do
      keySection "Public" public
      keySection "Private" private
  where
    keySection nm val = divClass "keyval" $ do
      elClass "span" "label" $ text nm
      elClass "span" "value" $ dynText val
