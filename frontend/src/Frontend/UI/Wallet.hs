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

    -- * Keys related helper widgets
  , uiSelectKey

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
import           Reflex.Dom.SemanticUI       hiding (mainWidget)

import           Frontend.Foundation
import           Frontend.Wallet
import           Frontend.Widgets
import           Frontend.Crypto.Ed25519     (keyToText)



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
uiSelectKey
  :: MonadWidget t m
  => Wallet t
  -> ((Text, KeyPair) -> Bool)
  -> m (Dynamic t (Maybe Text))
uiSelectKey w kFilter = do
  let keyNames = map fst . filter kFilter . Map.toList <$> w ^. wallet_keys
      mkPlaceholder ks = if null ks then "No keys available" else "Select key"
  d <- dropdown
      (def & dropdownConfig_placeholder .~ fmap mkPlaceholder keyNames)
      Nothing
      $ TaggedDynamic (Map.fromList . fmap (id &&& text) <$> keyNames)
  pure $ _dropdown_value d

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
  elClass "div" "ui relaxed middle aligned divided list" $ do
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
  events <- listWithKey keyMap $ \name key -> uiKeyItem signingKeys (name, key)
  dyn_ $ ffor keyMap $ \keys -> when (Map.null keys) $ text "No keys ..."
  let setSigning = switchDyn $ leftmost . fmap fst . Map.elems <$> events
      delKey = switchDyn $ leftmost . fmap snd . Map.elems <$> events
  pure $ mempty
    & walletCfg_setSigning .~ setSigning
    & walletCfg_delKey .~ delKey


-- | Display a key as list item together with it's name.
uiKeyItem :: MonadWidget t m => Dynamic t (Set KeyName) -> (Text, Dynamic t KeyPair) -> m (Event t (KeyName, Bool), Event t KeyName)
uiKeyItem signingKeys (n, k) = do
    elClass "div" "item" $ do
      (box, onDel) <- elClass "div" "right floated content" $ do

        isSigning <- tagOnPostBuild $ Set.member n <$> signingKeys

        boxI <- checkbox (text "Signing")
          $ def & checkboxConfig_type .~ pure Nothing
                & checkboxConfig_setValue .~ SetValue False (Just isSigning)
        let
          buttonIcon = snd <$> elClass' "i" "large trash right aligned icon" blank
        onDelI <- flip button buttonIcon $ def
          & buttonConfig_emphasis .~ Static (Just Tertiary)
          & classes .~ Static "input-aligned-btn"
        pure (boxI, onDelI)

      viewKeys <- toggle False =<< domEvent Click <$> icon' "large link key middle aligned" def
      elClass "div" "content" $ do
        elClass "h4" "ui header" $ text n
        elClass "div" "description" $ dynText $ keyDescription <$> k
      dyn_ $ ffor viewKeys $ \case
        False -> pure ()
        True -> table (def & classes .~ "very basic") $ do
          let item lbl key = el "tr" $ do
                el "td" $ label (def & labelConfig_pointing .~ Static (Just RightPointing)) $ text lbl
                elAttr "td" ("style" =: "word-break: break-all") $ dynText key
          item "Public key" $ keyToText . _keyPair_publicKey <$> k
          item "Private key" $ maybe "No key" keyToText . _keyPair_privateKey <$> k
      pure $ ((fmap (n, ) . _checkbox_change $ box), fmap (const n) onDel)
  where
    keyDescription k1 =
      case _keyPair_privateKey k1 of
        Nothing -> "Public key only"
        Just _  -> "Full key pair"
