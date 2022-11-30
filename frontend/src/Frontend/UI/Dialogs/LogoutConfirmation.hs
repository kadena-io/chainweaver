{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Confirmation dialog for creating a GIST allowing setting of name and description.
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.LogoutConfirmation
  ( uiLogoutConfirmation
  , uiIdeLogoutConfirmation
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Obelisk.Generated.Static
------------------------------------------------------------------------------
import           Frontend.OAuth
import           Common.OAuth
import           Frontend.Foundation            hiding (Arg)
import           Frontend.UI.Modal
import           Frontend.UI.Widgets
import           Frontend.UI.Widgets.Helpers (imgWithAltCls, dialogSectionHeading)
------------------------------------------------------------------------------

type HasUILogoutConfirmationModelCfg mConf t =
  ( Monoid mConf, Flattenable mConf t , HasOAuthCfg mConf t
  )


-- | Ask user for confirmation before logging out from GitHub.
--
uiLogoutConfirmation
  :: forall t m mConf
  . (MonadWidget t m, HasUILogoutConfirmationModelCfg mConf t)
  => Event t () -> m (mConf, Event t ())
uiLogoutConfirmation _onClose = do
  onClose <- modalHeader $ text "Logout from GitHub?"
  modalMain $ do
      divClass "segment modal__filler" $ do
        divClass "modal__filler-horizontal-center-box" $
          imgWithAltCls "modal__filler-img" $(static "img/Octocat.jpg") "GitHub logo" blank

        dialogSectionHeading mempty "Really log this IDE out from GitHub?"

        divClass "group" $ do
          text "When creating a new Gist, you will be automatically logged in again."

        divClass "group" $ do
          text "If you want to switch to a different GitHub account, you will also have to "
          elAttr "a" ("href" =: "https://github.com/logout" <> "target" =: "_blank") $
            text "logout"
          text " "
          el "em" $ text "yourself"
          text " from GitHub."

  modalFooter $ do
    onCancel <- cancelButton def "Cancel"
    onConfirm <- confirmButton def "Logout"
    let
      cfg = mempty & oAuthCfg_logout .~ (OAuthProvider_GitHub <$ onConfirm)
    pure (cfg, leftmost [onClose, onConfirm, onCancel])

uiIdeLogoutConfirmation
  :: MonadWidget t m
  => Event t ()
  -> m (Event t (), Event t ())
uiIdeLogoutConfirmation _onClose = do
  onClose <- modalHeader $ text "Logout?"
  modalMain $ do
    divClass "segment modal__filler logout-confirm__modal-filler" $
      divClass ".modal__filler-horizontal-center-box-no-hide" $ do
        onCancel <- cancelButton btnCfg "Cancel"
        onConfirm <- confirmButton btnCfg "Yes, logout"
        pure (onConfirm, leftmost [onClose, onCancel])
  where
    btnCfg = def & uiButtonCfg_class <>~ "logout-confirm-modal__button"
