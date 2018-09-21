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
import           Control.Monad
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.Text                   (Text)
import           Language.Javascript.JSaddle (js0, liftJSM, pToJSVal)
import           Reflex
import           Reflex.Dom.Core             (keypress, _textInput_element)
import           Reflex.Dom.SemanticUI       hiding (mainWidget)

import           Frontend.Foundation
import           Frontend.Wallet



-- | UI for managing the keys wallet.
uiWallet :: MonadWidget t m => Wallet t -> m (WalletCfg t)
uiWallet w = do
    onReq <- elClass "div" "ui fluid action input" $ mdo
      name <- textInput $ def
          & textInputConfig_value .~ SetValue "" (Just $ "" <$ confirmed)
          & textInputConfig_placeholder .~ pure "Enter key name"
      let
        onEnter = keypress Enter name
        nameEmpty = (== "") <$> value name
        duplicate = Map.member <$> value name <*> w ^. wallet_keys

      clicked <- flip button (text "Generate") $ def
        & buttonConfig_emphasis .~ Static (Just Secondary)
        & buttonConfig_disabled .~ Dyn ((||) <$> nameEmpty <*> duplicate)

      let
        confirmed = leftmost [ onEnter, clicked ]
      void $ performEvent (liftJSM (pToJSVal (_textInput_element name) ^. js0 ("focus" :: String)) <$ confirmed)
      pure $ tag (current $ _textInput_value name) confirmed

    keysCfg <- uiAvailableKeys w

    pure $ keysCfg & walletCfg_genKey .~ onReq

----------------------------------------------------------------------
-- Keys related helper widgets:
----------------------------------------------------------------------

-- | UI for letting the user select a particular key
-- from a filtered view of the available keys.
uiSelectKey
  :: MonadWidget t m
  => Wallet t
  -> ((Text, DynKeyPair t) -> Bool)
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
hasPrivateKey :: (Text, DynKeyPair t) -> Bool
hasPrivateKey = isJust . _keyPair_privateKey . snd

----------------------------------------------------------------------

-- | Widget listing all available keys.
uiAvailableKeys :: MonadWidget t m => Wallet t -> m (WalletCfg t)
uiAvailableKeys aWallet = do
  elClass "div" "ui relaxed middle aligned divided list" $ do
    let itemsDyn = uiKeyItems <$> aWallet ^. wallet_keys
    networkViewFlatten $ itemsDyn


-- | Render a list of key items.
--
-- Does not include the surrounding `div` tag. Use uiAvailableKeys for the
-- complete `div`.
uiKeyItems :: MonadWidget t m => DynKeyPairs t -> m (WalletCfg t)
uiKeyItems keyMap =
  case Map.toList keyMap of
    []   -> do
      text "No keys ..."
      pure mempty
    keys -> do
      rs <- traverse uiKeyItem keys
      pure $ mconcat rs

-- | Display a key as list item together with it's name.
uiKeyItem :: MonadWidget t m => (Text, DynKeyPair t) -> m (WalletCfg t)
uiKeyItem (n, k) = do
    elClass "div" "item" $ do
      (box, onDel) <- elClass "div" "right floated content" $ do
        onPostBuild <- getPostBuild
        let
          cSigned = current $ k ^. keyPair_forSigning
          onPbSigned = tag cSigned onPostBuild

        boxI <- checkbox (text "Signing")
          $ def & checkboxConfig_type .~ pure Nothing
                & checkboxConfig_setValue .~ SetValue False (Just onPbSigned)
        let
          buttonIcon = elClass "i" "large trash right aligned icon" blank
        onDelI <- flip button buttonIcon $ def
          & buttonConfig_emphasis .~ Static (Just Tertiary)
          & classes .~ Static "input-aligned-btn"
        pure (boxI, onDelI)

      elClass "i" "large key middle aligned icon" blank
      elClass "div" "content" $ do
        elClass "h4" "ui header" $ text n
        elClass "div" "description" $ text $ keyDescription k
      pure $ mempty
        & walletCfg_setSigning .~ (fmap (n, ) . _checkbox_change $ box)
        & walletCfg_delKey .~  fmap (const n) onDel
  where
    keyDescription k1 =
      case _keyPair_privateKey k1 of
        Nothing -> "Public key only"
        Just _  -> "Full key pair"

