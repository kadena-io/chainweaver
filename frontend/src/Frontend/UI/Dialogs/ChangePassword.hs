{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

-- | Dialog for changing the password which encrypts the private keys and guards
-- the wallet.
module Frontend.UI.Dialogs.ChangePassword
  ( uiChangePasswordDialog
  , minPasswordLength
  ) where

import Reflex.Dom.Core
import Data.Functor (void)
import Data.Text (Text)

import Frontend.AppCfg
import Frontend.Foundation
import Frontend.UI.Modal.Impl
import Frontend.UI.Widgets
import Frontend.UI.Widgets.Helpers (dialogSectionHeading)
import Frontend.Crypto.Password

minPasswordLength :: Int
minPasswordLength = 10

uiChangePasswordDialog
  :: ( DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m, Monoid mConf, Flattenable mConf t
     )
  => ChangePassword key t m
  -> Event t ()
  -> m (mConf, Event t ())
uiChangePasswordDialog changePassword _onCloseExternal = mdo
  onClose <- modalHeader $ dynText dTitle
  (dTitle, (conf, closes)) <- fmap splitDynPure . splitDynPure <$> workflow (uiChangePasswordScreen changePassword onClose)
  mConf <- flatten =<< tagOnPostBuild conf
  let close = switch $ current closes
  pure (mConf, close <> onClose)

uiChangePasswordScreen
  :: (DomBuilder t m, PostBuild t m, Monoid mConf, MonadFix m)
  => ChangePassword key t m -> Event t () -> Workflow t m (Text, (mConf, Event t ()))
uiChangePasswordScreen (ChangePassword changePassword _) onClose = Workflow $ mdo
  (currentPassword, newPassword, repeatPassword) <- modalMain $ do
    dialogSectionHeading mempty "Notice"
    divClass "group" $ text "Change the password which guards this wallet. \
      \ Your private keys will be re-encrypted using the new password, and the new password will take immediate effect. \
      \ Any wallet exports you already have will still require your current password to unlock. \
      \ Future wallet exports will be locked by your new password."
    let passwordInput style title = fmap (current . value) $ uiInputElement $ def
          & initialAttributes .~ ("type" =: "password" <> "placeholder" =: title <> "class" =: "input_width_full" <> "style" =: style)
    dialogSectionHeading mempty "Current Password"
    currentPassword' <- divClass "group" $ passwordInput "" "Current password"
    dialogSectionHeading mempty "New Password"
    out <- divClass "group" $ do
      newPassword' <- passwordInput "" $ "New password (" <> tshow minPasswordLength <> " character min.)"
      repeatPassword' <- passwordInput "margin-top: 1rem" "Confirm new password"
      pure (currentPassword', newPassword', repeatPassword')

    void $ runWithReplace blank $ leftmost
      [ (blank <$ eOk)
      , ffor eErr $ \e -> do
        dialogSectionHeading mempty "Error"
        divClass "group" $ text e
      ]

    pure out
  (eCancel, (eErr, eOk)) <- modalFooter $ do
    eCancel' <- cancelButton def "Cancel"
    eSubmit <- confirmButton def "Submit"
    eResponse <- changePassword $ (,,) 
      <$> (fmap Password currentPassword)
      <*> (fmap Password newPassword)
      <*> (fmap Password repeatPassword) <@ eSubmit
    pure $ (eCancel', fanEither eResponse)

  return (("Change Password", (mempty, onClose <> eCancel)), uiChangePasswordSuccess onClose <$ eOk)

uiChangePasswordSuccess
  :: (DomBuilder t m, PostBuild t m, Monoid mConf)
  => Event t () -> Workflow t m (Text, (mConf, Event t ()))
uiChangePasswordSuccess onClose = Workflow $ do
  modalMain $ do
    elClass "div" "modal__success_screen" $ do
      el "p" $ elClass "i" "fa fa-check-circle" $ blank
      el "p" $ text $ "Password successfully changed"

  done <- modalFooter $ do
    confirmButton def "Close"

  pure (("Change Password Success", (mempty, onClose <> done)), never)
