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

module UI.Wallet
  ( -- * Key management widget
    uiWallet

    -- * Keys related helper widgets
  , uiSelectKey

    -- ** Filters for keys
  , hasPrivateKey
  ) where

------------------------------------------------------------------------------
import           Control.Arrow              ((&&&))
import           Control.Lens
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Text                  (Text)
import           Reflex
import           Reflex.Dom.SemanticUI      hiding (mainWidget)

import           Wallet



-- | UI for managing the keys wallet.
uiWallet :: MonadWidget t m => Wallet t -> m (WalletCfg t)
uiWallet w = do
    elClass "h3" "ui header" $ text "Available Keys"
    onSetSig <- uiAvailableKeys w

    elClass "div" "ui fluid action input" $ mdo
      name <- textInput $ def
          & textInputConfig_value .~ SetValue "" (Just $ "" <$ clicked)
          & textInputConfig_placeholder .~ pure "Enter key name"

      clicked <- button (def & buttonConfig_emphasis .~ Static (Just Secondary)) $ text "Generate"

      let onReq = tag (current $ _textInput_value name) clicked

      pure $ WalletCfg { _walletCfg_onRequestNewKey = onReq
                       , _walletCfg_onSetSigning = onSetSig
                       }

----------------------------------------------------------------------
-- Keys related helper widgets:
----------------------------------------------------------------------

-- | UI for letting the user select a particular key
-- from a filtered view of the available keys.
uiSelectKey
  :: MonadWidget t m
  => Wallet t
  -> ((Text, KeyPair t) -> Bool)
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
hasPrivateKey :: (Text, KeyPair t) -> Bool
hasPrivateKey = isJust . _keyPair_privateKey . snd

----------------------------------------------------------------------

-- | Widget listing all available keys.
uiAvailableKeys :: MonadWidget t m => Wallet t -> m (Event t (Text, Bool))
uiAvailableKeys aWallet = do
  elClass "div" "ui relaxed middle aligned divided list" $ do
    {- elClass "div" "item" $ do -}
    {-   elClass "div" "right floated content" $ el "div" $ elClass "h3" "ui heading" $ text "Sign" -}
    {- elClass "div" "content" $ el "div" $ elClass "h3" "ui heading" $ text "Key" -}
    let itemsDyn = uiKeyItems <$> aWallet ^. wallet_keys
    evEv <- dyn itemsDyn
    switchHold never evEv


-- | Render a list of key items.
--
-- Does not include the surrounding `div` tag. Use `uiAvailableKeys` for the
-- complete `div`.
uiKeyItems :: MonadWidget t m => Keys t -> m (Event t (Text, Bool))
uiKeyItems keyMap =
  case Map.toList keyMap of
    []   -> do
      text "No keys ..."
      pure never
    keys -> do
      rs <- traverse uiKeyItem keys
      pure $ leftmost rs

-- | Display a key as list item together with it's name.
uiKeyItem :: MonadWidget t m => (Text, KeyPair t) -> m (Event t (Text, Bool))
uiKeyItem (n, k) = do
    elClass "div" "item" $ do
      box <- elClass "div" "right floated content" $ do
        onPostBuild <- getPostBuild
        let
          cSigned = current $ k ^. keyPair_forSigning
          onPbSigned = tag cSigned onPostBuild

        checkbox (text "Signing")
          $ def & checkboxConfig_type .~ pure Nothing
                & checkboxConfig_setValue .~ SetValue False (Just onPbSigned)

      elClass "i" "large key middle aligned icon" blank
      elClass "div" "content" $ do
        elClass "h4" "ui header" $ text n
        elClass "div" "description" $ text $ keyDescription k
      pure $ fmap (n, ) . _checkbox_change $ box
  where
    keyDescription k1 =
      case _keyPair_privateKey k1 of
        Nothing -> "Public key only"
        Just _  -> "Full key pair"

