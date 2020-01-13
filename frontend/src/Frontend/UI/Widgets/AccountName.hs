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

import Frontend.Foundation (renderClass)
import Frontend.UI.Widgets (mkLabeledClsInput, uiInputElement, mkLabeledView)
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
    validateAccountName = ($) <$> checkAccountNameValidity w <*> mChain

    inp lbl wrapperCls = divClass wrapperCls $ mkLabeledClsInput True lbl
      $ \cls -> uiInputElement $ def
        & initialAttributes .~ "class" =: (renderClass cls)
        & inputElementConfig_initialValue .~ fold (fmap unAccountName initval)

  divClass "vanity-account-create__account-name" $ do
    dValue <- value <$> inp "Account Name" "vanity-account-create__account-name-input"

    let dEitherAccName = validateAccountName <*> dValue

    dAccNameDirty <- holdUniqDyn $ not . Text.null <$> dValue

    divClass "vanity-account-create__account-name-error" $ mkLabeledView True Text.empty $
      dyn_ $ ffor2 dAccNameDirty dEitherAccName $ curry $ \case
        (True, Left e) -> text e
        _ -> blank

    pure $ hush <$> dEitherAccName
