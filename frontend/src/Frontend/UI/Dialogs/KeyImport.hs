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

-- | Dialog for importing a key to the wallet.
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.KeyImport
  ( uiKeyImport
  , HasUiKeyImportModel
  , HasUiKeyImportModelCfg
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Void                      (Void)
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Obelisk.Generated.Static
------------------------------------------------------------------------------
import           Frontend.Ide
import           Frontend.ModuleExplorer        (HasModuleExplorerCfg (..))
import           Frontend.Wallet
import           Frontend.UI.DeploymentSettings
import           Frontend.UI.Modal
import           Frontend.UI.Widgets
import           Frontend.Foundation
import           Frontend.UI.Widgets.Helpers (imgWithAltCls)
------------------------------------------------------------------------------


type HasUiKeyImportModel model t =
  (HasWallet model t)

type HasUiKeyImportModelCfg mConf t =
  ( Monoid mConf, Flattenable mConf t
  , HasWalletCfg mConf t
  )


-- | Dialog for entering a public/private key pair.
--
uiKeyImport
  :: forall t m model mConf.
    ( MonadWidget t m, HasUiKeyImportModel model t, HasUiKeyImportModelCfg mConf t
    )
  => model
  -> m (mConf, Event t ())
uiKeyImport m = do
  onClose <- modalHeader $ text "Key Import"
  modalMain $ do
    modalBody $ do
      divClass "segment modal__filler" $ do
        divClass "modal__filler-horizontal-center-box" $
          imgWithAltCls "modal__filler-img" (static @"img/keys-scalable.svg") "Keys" blank

        elClass "h2" "heading heading_type_h2" $ text "Import Existing Key"
        elKlass "div" "group segment" $ do
          nameInput <- mkLabeledInput uiInputElement "Key Name" def
          pubKeyInput <- mkLabeledInput uiTextAreaElement "Public Key" def
          privKeyInput <- mkLabeledInput uiTextAreaElement "Private Key" def
          pure ()
    modalFooter $ do
      onCancel <- cancelButton def "Cancel"
      text " "
      {- let isDisabled = maybe True (const False) <$> pure Nothing -}
      onConfirm <- confirmButton def "Import"

      pure (mempty, leftmost [onClose, onCancel, onConfirm])
