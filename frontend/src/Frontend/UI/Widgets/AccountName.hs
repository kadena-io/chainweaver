module Frontend.UI.Widgets.AccountName
  ( uiAccountNameInput
  ) where

import Control.Error (hush)
import Control.Monad.Fix (MonadFix)
import Pact.Types.ChainId
import Data.Foldable (fold)
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
  -> Dynamic t (Maybe ChainId)
  -> Maybe AccountName
  -> m (Dynamic t (Maybe AccountName))
uiAccountNameInput w mChain initval = do
  let
    validateAccountName v = checkAccountNameValidity w
      <*> mChain
      <*> value v

    inputWithFeedback = uiInputWithInlineFeedback
      validateAccountName
      inputIsDirty
      id
      Nothing
      uiInputElement

  (_, dEitherAccName) <- mkLabeledInput True "Account Name" inputWithFeedback $ def
    & inputElementConfig_initialValue .~ fold (fmap unAccountName initval)

  pure $ hush <$> dEitherAccName
