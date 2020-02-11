module Frontend.UI.Widgets.AccountName
  ( uiAccountNameInput
  , noValidation
  ) where

import Control.Error (hush)
import Control.Monad ((<=<))
import Data.Foldable (fold)
import Data.Text (Text)
import qualified Data.Text as Text
import Reflex
import Reflex.Dom
import Language.Javascript.JSaddle (MonadJSM)
import Frontend.UI.Widgets (PopoverState (..), mkLabeledInput, uiInputElement, uiInputWithPopover)
import Frontend.Wallet (AccountName(..), mkAccountName)

noValidation :: Applicative f => f (a -> Either err a)
noValidation = pure Right

uiAccountNameInput
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , PerformEvent t m
     , MonadJSM (Performable m)
     )
  => Maybe AccountName
  -> Dynamic t (AccountName -> Either Text AccountName)
  -> m (Dynamic t (Maybe AccountName))
uiAccountNameInput initval validateName = do
  let
    mkMsg True (Left e) = PopoverState_Error e
    mkMsg _    _ = PopoverState_Disabled

    validate = ffor validateName $ (<=< mkAccountName)

    showPopover (ie, _) = pure $ (\v t -> mkMsg (not $ Text.null t) (v t))
      <$> current validate
      <@> fmap Text.strip (_inputElement_input ie)

    uiNameInput cfg = do
      inp <- uiInputElement cfg
      pure (inp, _inputElement_raw inp)

  (inputE, _) <- mkLabeledInput True "Account Name" (uiInputWithPopover uiNameInput snd showPopover)
    $ def & inputElementConfig_initialValue .~ fold (fmap unAccountName initval)

  pure $ hush <$> (validate <*> fmap Text.strip (value inputE))
