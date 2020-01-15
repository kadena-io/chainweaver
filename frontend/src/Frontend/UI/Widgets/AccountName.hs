module Frontend.UI.Widgets.AccountName
  ( uiAccountNameInput
  ) where

import Control.Error (hush)
import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Kadena.SigningApi (mkAccountName)
import Pact.Types.ChainId
import Reflex
import Reflex.Dom

import Frontend.UI.Widgets (mkLabeledInput, uiInputElement, uiInputWithInlineFeedback)
import Frontend.UI.Widgets.Helpers (inputIsDirty)
import Frontend.Network (HasNetwork)
import Frontend.Wallet (AccountName(..), checkAccountNameValidity, HasWallet)

uiAccountNameInput
  :: ( DomBuilder t m
     , MonadHold t m
     , PostBuild t m
     , MonadFix m
     , HasWallet model key t
     , HasNetwork model t
     )
  => model
  -> Maybe AccountName
  -> m (Dynamic t (Maybe AccountName))
uiAccountNameInput w initval = do
  let
    validateAccountName v = checkAccountNameValidity w <*> value v

    inputWithFeedback = uiInputWithInlineFeedback
      validateAccountName
      inputIsDirty
      id
      Nothing
      uiInputElement

  (_, dEitherAccName) <- mkLabeledInput True "Account Name" inputWithFeedback $ def
    & inputElementConfig_initialValue .~ fold (fmap unAccountName initval)

  pure $ hush <$> dEitherAccName
