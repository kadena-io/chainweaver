{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Wallet setup screens
module Frontend.Setup.Widgets where

import Control.Monad (void)
import Data.Text (Text)

import Frontend.UI.Widgets
import Obelisk.Generated.Static
import Reflex.Dom.Core

setupClass :: Text -> Text
setupClass = mappend "setup__"

setupDiv :: DomBuilder t m => Text -> m a -> m a
setupDiv t = divClass (setupClass t)

setupCheckbox
  :: (DomBuilder t m, PostBuild t m)
  => Bool
  -> CheckboxConfig t
  -> m ()
  -> m (Checkbox t)
setupCheckbox initialValue cfg inner = elClass "div" (setupClass "checkbox")
  $ uiCheckbox def initialValue cfg inner

setupForm :: forall t m a. (DomBuilder t m, PostBuild t m) => Text -> Text -> Dynamic t Bool -> m a -> m (Event t (), a)
setupForm cls lbl disabled = uiForm cfg $ setupDiv cls $ void $ confirmButton (def & uiButtonCfg_disabled .~ disabled) lbl
  where cfg = def & elementConfig_initialAttributes .~ ("class" =: setupClass "form")

restoreForm :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> m a -> m (Event t (), a)
restoreForm = setupForm "recover-restore-button" "Restore"

continueForm :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> m a -> m (Event t (), a)
continueForm = setupForm "continue-button" "Continue"

-- | Wallet logo
kadenaWalletLogo :: DomBuilder t m => m ()
kadenaWalletLogo = divClass "logo" $ do
  elAttr "img" ("src" =: static @"img/kadena_blue_logo.png" <> "class" =: setupClass "kadena-logo") blank
  elClass "div" "chainweaver" $ text "Chainweaver"
  elClass "div" "by-kadena" $ text "by Kadena (BETA)"
