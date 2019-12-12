module Frontend.UI.Widgets.AccountName
  ( uiAccountNameInput
  ) where

import Control.Error (hush)
import Control.Monad.Fix (MonadFix)
import Pact.Types.ChainId

import Reflex
import Reflex.Dom

import Frontend.Foundation (renderClass)
import Frontend.UI.Widgets (mkLabeledClsInput, uiInputElement)
import Frontend.Wallet (AccountName, checkAccountNameValidity, Wallet (..))

uiAccountNameInput
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => Wallet key t
  -> Dynamic t (Maybe ChainId)
  -> m (Dynamic t (Maybe AccountName))
uiAccountNameInput w mChain = do
  let
    validateAccountName = ($) <$> checkAccountNameValidity w <*> mChain

    inp lbl wrapperCls = divClass wrapperCls $ mkLabeledClsInput True lbl
      $ \cls -> uiInputElement $ def & initialAttributes .~ "class" =: (renderClass cls)

  divClass "vanity-account-create__account-name" $ do
    dEitherAccName <- (validateAccountName <*>) . value <$>
      inp "Account Name" "vanity-account-create__account-name-input"

    dAccNameDirty <- holdUniqDyn =<< holdDyn False (True <$ updated dEitherAccName)

    divClass "vanity-account-create__account-name-error" $
      dyn_ $ ffor2 dAccNameDirty dEitherAccName $ curry $ \case
        (True, Left e) -> text e
        _ -> blank

    pure $ hush <$> dEitherAccName
