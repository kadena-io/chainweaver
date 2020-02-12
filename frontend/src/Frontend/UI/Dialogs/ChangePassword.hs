{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

-- | Dialog for changing the password which encrypts the private keys and guards
-- the wallet.
module Frontend.UI.Dialogs.ChangePassword
  ( uiChangePasswordDialog
  , minPasswordLength
  ) where

import Control.Monad (void)
import Reflex.Dom.Core

import Frontend.AppCfg
import Frontend.Foundation
import Frontend.UI.Modal.Impl
import Frontend.UI.Widgets
import Frontend.UI.Widgets.Helpers (dialogSectionHeading)

minPasswordLength :: Int
minPasswordLength = 10

uiChangePasswordDialog
  :: ( MonadWidget t m, Monoid mConf
     )
  => ChangePassword key t m
  -> Event t ()
  -> m (mConf, Event t ())
uiChangePasswordDialog (ChangePassword changePassword _) _onCloseExternal = mdo
  onClose <- modalHeader $ text "Change Password"
  modalMain $ do
    dialogSectionHeading mempty "Notice"
    divClass "group" $ text "Change the password which guards this wallet. \
      \ Your private keys will be re-encrypted using the new password, and the new password will take immediate effect. \
      \ Any wallet exports you already have will still require your current password to unlock. \
      \ Future wallet exports will be locked by your new password."
    let passwordInput style title = fmap (current . value) $ uiInputElement $ def
          & initialAttributes .~ ("type" =: "password" <> "placeholder" =: title <> "class" =: "input_width_full" <> "style" =: style)
    dialogSectionHeading mempty "Current Password"
    currentPassword <- divClass "group" $ passwordInput "" "Current password"
    dialogSectionHeading mempty "New Password"
    response <- divClass "group" $ do
      newPassword <- passwordInput "" $ "New password (" <> tshow minPasswordLength <> " character min.)"
      repeatPassword <- passwordInput "margin-top: 1rem" "Confirm new password"
      change <- confirmButton (def & uiButtonCfg_class .~ "button_right_floated") "Change Password"
      changePassword $ (,,) <$> currentPassword <*> newPassword <*> repeatPassword <@ change
    void $ runWithReplace blank $ ffor response $ \case
      Right () -> do
        dialogSectionHeading mempty "Success"
        divClass "group" $ text "Your password has successfully been changed."
      Left e -> do
        dialogSectionHeading mempty "Error"
        divClass "group" $ text e
  done <- modalFooter $ do
    cancelButton def "Done"
  return (mempty, onClose <> done)
