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
import qualified Data.IntMap as IntMap
------------------------------------------------------------------------------
import           Obelisk.Generated.Static
------------------------------------------------------------------------------
import           Frontend.Foundation         hiding (Arg)
import           Frontend.UI.Modal
import           Frontend.UI.Widgets
import           Frontend.UI.Widgets.Helpers (imgWithAltCls)
import           Frontend.Wallet             (AccountName(..), HasWalletCfg(..))
------------------------------------------------------------------------------

type HasUIDeleteConfirmationModelCfg mConf key t =
  ( Monoid mConf, Flattenable mConf t , HasWalletCfg mConf key t
  )


-- | Ask user for confirmation before deleting "something".
--
-- At the moment "something" is only keys, so we skip making "something"
-- configurable for now.
--
uiDeleteConfirmation
  :: forall key t m mConf
  . (MonadWidget t m, HasUIDeleteConfirmationModelCfg mConf key t)
  => IntMap.Key -> AccountName -> Event t () -> m (mConf, Event t ())
uiDeleteConfirmation thisKey accountName _onClose = do
  onClose <- modalHeader $ text "Delete Confirmation"
  modalMain $ do
      divClass "segment modal__filler" $ do
        divClass "modal__filler-horizontal-center-box" $
          imgWithAltCls "modal__filler-img" (static @"img/bin-scalable.svg") "Bin" blank

        elClass "h2" "heading heading_type_h2" $ do
          text "Really delete account '"
          text $ unAccountName accountName
          text "'?"

        divClass "group" $ do
          text "This will delete the account and key from the local wallet."

        divClass "group" $ do
          text "If this account is holding money, without having access to the key you will lose that money!"

        divClass "group" $ do
          text "Double and triple check that you want to delete this key and make sure you have backups!"

  modalFooter $ do
    onCancel <- cancelButton def "Cancel"
    text " "
    onConfirm <- confirmButton def "Delete"
    let
      cfg = mempty & walletCfg_delKey .~ (thisKey <$ onConfirm)
    pure (cfg, leftmost [onClose, onConfirm, onCancel])
