module Frontend.UI.Widgets.AccountName
  ( uiAccountNameInput
  ) where

import Control.Error (hush)
import Control.Monad.Fix (MonadFix)
import qualified Data.Text as Text
import Pact.Types.ChainId
import Data.Foldable (fold)
import Reflex
import Reflex.Dom

import Frontend.UI.Widgets (mkLabeledInput, uiInputElement, uiInputWithInlineFeedback)
import Frontend.Network (HasNetwork)
import Frontend.Wallet (AccountName(..), checkAccountNameValidity, HasWallet)

uiAccountNameInput
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
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
      (fmap (not . Text.null) . value)
      id
      Nothing
      uiInputElement

  (_, dEitherAccName) <- mkLabeledInput True "Account Name" inputWithFeedback $ def
    & inputElementConfig_initialValue .~ fold (fmap unAccountName initval)

  pure $ hush <$> dEitherAccName
