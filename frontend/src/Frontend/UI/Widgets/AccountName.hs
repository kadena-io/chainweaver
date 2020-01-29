{-# LANGUAGE RecursiveDo #-}
module Frontend.UI.Widgets.AccountName
  ( uiAccountNameInput
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Error (hush)
import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Reflex
import Reflex.Dom

import Frontend.UI.Widgets (mkLabeledInput, uiInputElement)
import Frontend.UI.Widgets.Helpers (inputIsDirty)
import Frontend.Network (HasNetwork)
import Frontend.Wallet (AccountName(..), checkAccountNameValidity, HasWallet)

uiAccountNameInput
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , HasWallet model key t
     , HasNetwork model t
     )
  => model
  -> Maybe AccountName
  -> m (Dynamic t (Maybe AccountName))
uiAccountNameInput w initval = mdo
  inputE <- elDynAttr "div" dContainerAttrs $ mkLabeledInput True "Account Name"
    uiInputElement $ def & inputElementConfig_initialValue .~ fold (fmap unAccountName initval)

  let
    showPopover True (Left e) = "class" =: "popover popover__error popover__display" <> "data-tip" =: e
    showPopover _    _        = "class" =: "popover popover__error"

    dEitherAccName = checkAccountNameValidity w <*> value inputE
    dContainerAttrs = showPopover <$> (inputIsDirty inputE) <*> dEitherAccName

  pure $ hush <$> dEitherAccName
