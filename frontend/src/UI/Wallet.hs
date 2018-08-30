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
import           Control.Applicative
import           Control.Arrow               ((&&&))
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Aeson                  (Object, decodeStrict)
import           Data.Foldable
import qualified Data.List.Zipper            as Z
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.Semigroup
import           Data.Sequence               (Seq)
import qualified Data.Sequence               as S
import           Data.String.QQ
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Generics.Deriving.Monoid    (mappenddefault, memptydefault)
import           GHC.Generics                (Generic)
import           Language.Javascript.JSaddle hiding (Object)
import           Reflex
import           Reflex.Dom.ACE.Extended
import           Reflex.Dom.Core             (keypress, mainWidget, setValue)
import qualified Reflex.Dom.Core             as Core
import           Reflex.Dom.SemanticUI       hiding (mainWidget)
------------------------------------------------------------------------------
import           Pact.Repl
import           Pact.Repl.Types
import           Pact.Types.Lang
------------------------------------------------------------------------------
import           Static
import           Wallet
import           Widgets



-- | UI for managing the keys wallet.
uiWallet :: MonadWidget t m => Wallet t -> m (WalletConfig t)
uiWallet w = elClass "div" "" $ do
    elClass "div" "" $ do
      elClass "h3" "ui header" $ text "Available Keys"
      uiAvailableKeys w

    {- elClass "div" "ui hidden divider" blank -}

    {- elClass "div" "" $ do -}
    {-   elClass "h3" "ui header" $ text "Create New Key" -}
      elClass "div" "ui fluid action input" $ mdo
        name <- textInput $ def 
            & textInputConfig_value .~ SetValue "" (Just $ "" <$ clicked)
            & textInputConfig_placeholder .~ pure "Enter key name"

        clicked <- button (def & buttonConfig_emphasis |?~ Tertiary) $ text "Generate"

        let onReq = tag (current $ _textInput_value name) clicked

        pure $ WalletConfig { _walletConfig_onRequestNewKey = onReq }

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

-- | Widget listing all available keys.
uiAvailableKeys :: MonadWidget t m => Wallet t -> m ()
uiAvailableKeys aWallet = do
  elClass "div" "ui relaxed divided list" $ do
    let itemsDyn =  uiKeyItems <$> aWallet ^. wallet_keys
    dyn itemsDyn
    pure ()


-- | Render a list of key items.
--
-- Does not include the surrounding `div` tag. Use `uiAvailableKeys` for the
-- complete `div`.
uiKeyItems :: MonadWidget t m => Keys -> m ()
uiKeyItems keyMap =
  case Map.toList keyMap of
    []   -> text "No keys ..."
    keys -> traverse_ uiKeyItem keys

-- | Display a key as list item together with it's name.
uiKeyItem :: MonadWidget t m => (Text, KeyPair) -> m ()
uiKeyItem (n, k) = do
    elClass "div" "item" $ do
      elClass "i" "large key middle aligned icon" blank
      elClass "div" "content" $ do
        elClass "h4" "ui header" $ text n
        elClass "div" "description" $ text $ keyDescription k
  where
    keyDescription k =
      case _keyPair_privateKey k of
        Nothing -> "Public key only"
        Just _  -> "Full key pair"

