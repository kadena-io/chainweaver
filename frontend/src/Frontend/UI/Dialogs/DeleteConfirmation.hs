{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}

-- | Confirmation dialog for (key) deletion.
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.DeleteConfirmation
  ( uiDeleteConfirmation
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Obelisk.Generated.Static
------------------------------------------------------------------------------
import           Frontend.Foundation         hiding (Arg)
import           Frontend.UI.Modal
import           Frontend.UI.Widgets
import           Frontend.UI.Widgets.Helpers (imgWithAltCls)
import           Frontend.Wallet             (KeyName, HasWalletCfg(..))
------------------------------------------------------------------------------

type HasUIDeleteConfirmationModelCfg mConf t =
  ( Monoid mConf, Flattenable mConf t , HasWalletCfg mConf t
  )


-- | Ask user for confirmation before deleting "something".
--
-- At the moment "something" is only keys, so we skip making "something"
-- configurable for now.
--
uiDeleteConfirmation
  :: forall t m mConf
  . (MonadWidget t m, HasUIDeleteConfirmationModelCfg mConf t)
  => KeyName -> Event t () -> m (mConf, Event t ())
uiDeleteConfirmation keyName _onClose = do
  onClose <- modalHeader $ text "Delete Confirmation"
  modalMain $ do
    modalBody $ do
      divClass "segment modal__filler" $ do
        divClass "modal__filler-horizontal-center-box" $
          imgWithAltCls "modal__filler-img" (static @"img/bin-scalable.svg") "Bin" blank

        elClass "h2" "heading heading_type_h2" $ do
          text "Really delete key '"
          text keyName
          text "'?"

        divClass "group" $ do
          text "If this key is associated with any account holding money, you will lose that money!"

        divClass "group" $ do
          text "Double and triple check that you want to delete this key and make sure you have backups!"

    modalFooter $ do
      onCancel <- cancelButton def "Cancel"
      text " "
      onConfirm <- confirmButton def "Delete"
      let
        cfg = mempty & walletCfg_delKey .~ fmap (const keyName) onConfirm
      pure (cfg, leftmost [onClose, onConfirm, onCancel])
