{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

-- | Dialog for exporting the entire storage to the user
module Frontend.UI.Dialogs.ExportWallet
  ( uiExportWalletDialog
  ) where

import Control.Error (hush)
import Reflex.Dom.Core

import Frontend.AppCfg
import Frontend.UI.Modal.Impl
import Frontend.UI.Widgets
import Frontend.UI.Widgets.Helpers (dialogSectionHeading)

uiExportWalletDialog
  :: ( MonadWidget t m, Monoid mConf
     )
  => ExportWallet t m
  -> Event t ()
  -> m (mConf, Event t ())
uiExportWalletDialog (ExportWallet _exportWallet_requestExport) _onCloseExternal = mdo
  onClose <- modalHeader $ text "Export Wallet"
  pw <- modalMain $ do
    dialogSectionHeading mempty "Notice"
    divClass "group" $ do
      el "p" $ text
        "Exporting wallet data will generate an encrypted file containing sensitive and \
        \non-sensitive information available within the wallet. The file will be protected\
        \by the wallet password that was in use at time of export. Store this file in a \
        \secure location and do not alter its contents."

      el "p" $ text "The wallet data to be exported within this file includes:"
      el "ul" $ do
        el "li" $ text "Generated public/private key pairs"
        el "li" $ text "Added accounts"
        el "li" $ text "Account notes"
        el "li" $ text "Network configuration"
        el "li" $ text "Transaction configuration"

    dialogSectionHeading mempty "Current Password"
    divClass "group" $ do
      uiInputElement $ def & initialAttributes .~
          ( "type" =: "password"
          <> "placeholder" =: "Current Password"
          <> "class" =: "input_width_full"
          )

  done <- modalFooter $ do
    eDone <- cancelButton def "Cancel"
    eExport <- confirmButton def "Export"
    eRes <- _exportWallet_requestExport $ (current $ value pw) <@ eExport
    pure $ eDone <> (fmapMaybe hush eRes)
  return (mempty, onClose <> done)
