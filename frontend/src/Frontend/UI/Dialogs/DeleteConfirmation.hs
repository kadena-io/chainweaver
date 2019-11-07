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
uiDeleteConfirmation thisKey _accountName _onClose = do
  onClose <- modalHeader $ text "Remove Confirmation"
  modalMain $ do
      divClass "segment modal__filler" $ do
        elClass "h2" "heading heading_type_h2" $ text "Warning"

        divClass "group" $
          text "You are about to remove this account from view in your wallet"

        divClass "group" $
          text "The only way to recover any balance in this account will be by restoring the complete wallet with your recovery phrase"
        divClass "group" $
          text "Ensure that you have a backup of account data before removing."

  modalFooter $ do
    -- onCancel <- cancelButton def "Cancel"
    onConfirm <- confirmButton (def & uiButtonCfg_class .~ "account-delete__confirm") "Permanently Remove Account"
    let
      cfg = mempty & walletCfg_delKey .~ (thisKey <$ onConfirm)
    pure (cfg, leftmost [onClose, onConfirm])
