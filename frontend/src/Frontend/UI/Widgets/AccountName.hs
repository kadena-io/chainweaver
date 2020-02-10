module Frontend.UI.Widgets.AccountName
  ( uiAccountNameInput
  ) where

import Control.Error (hush)
import Data.Foldable (fold)
import qualified Data.Text as Text
import Reflex
import Reflex.Dom
import Language.Javascript.JSaddle (MonadJSM)
import Frontend.UI.Widgets (PopoverState (..), mkLabeledInput, uiInputElement, uiInputWithPopover)
import Frontend.Network (HasNetwork)
import Frontend.Wallet (AccountName(..), checkAccountNameValidity, HasWallet)

uiAccountNameInput
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , HasWallet model key t
     , HasNetwork model t
     , DomBuilderSpace m ~ GhcjsDomSpace
     , PerformEvent t m
     , MonadJSM (Performable m)
     )
  => model
  -> Maybe AccountName
  -> m (Dynamic t (Maybe AccountName))
uiAccountNameInput w initval = do
  let
    mkMsg True (Left e) = PopoverState_Error e
    mkMsg _    _ = PopoverState_Disabled

    showPopover (ie, _) = pure $ (\chk t -> mkMsg (not $ Text.null t) (chk t))
      <$> current (checkAccountNameValidity w)
      <@> fmap Text.strip (_inputElement_input ie)

    uiNameInput cfg = do
      inp <- uiInputElement cfg
      pure (inp, _inputElement_raw inp)

  (inputE, _) <- mkLabeledInput True "Account Name" (uiInputWithPopover uiNameInput snd showPopover)
    $ def & inputElementConfig_initialValue .~ fold (fmap unAccountName initval)

  pure $ hush <$> (checkAccountNameValidity w <*> fmap Text.strip (value inputE))
