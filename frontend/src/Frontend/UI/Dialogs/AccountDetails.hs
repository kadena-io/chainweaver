{-# LANGUAGE ConstraintKinds #-}
-- | Dialog for viewing the details of an account.
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.AccountDetails
  ( uiAccountDetails
  ) where
------------------------------------------------------------------------------
import           Control.Lens
import qualified Data.Text as T
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.KadenaAddress (mkKadenaAddress, textKadenaAddress)
------------------------------------------------------------------------------
import           Frontend.Wallet
import           Frontend.Network
import           Frontend.Crypto.Ed25519 (keyToText)
import           Frontend.UI.Modal
import           Frontend.UI.Widgets
import           Frontend.Foundation
------------------------------------------------------------------------------
type HasUiAccountDetailsModel model key t =
  ( HasNetwork model t
  )

type HasUiAccountDetailsModelCfg mConf key t =
  ( Monoid mConf, Flattenable mConf t
  )

uiAccountDetails
  :: ( HasUiAccountDetailsModel model key t
     , HasUiAccountDetailsModelCfg mConf key t
     , MonadWidget t m
     )
  => model
  -> Account key
  -> Event t ()
  -> m (mConf, Event t ())
uiAccountDetails m a _onCloseExternal = do
  let dKAddr = (\n -> textKadenaAddress $ mkKadenaAddress n (_account_chainId a) (_account_name a)) <$> m ^. network_selectedNetwork

  let displayText lbl v cls =
        let
          attrFn cfg = uiInputElement $ cfg
            & initialAttributes <>~ ("disabled" =: "true" <> "class" =: (" " <> cls))
        in
          mkLabeledInputView attrFn lbl v

  onClose <- modalHeader $ text "Account Details"
  modalMain $ divClass "modal__main account-details" $ do
    elClass "h2" "heading heading_type_h2" $ text "Info"
    divClass "group" $ do
      -- Account name
      _ <- displayText "Account Name" (constDyn $ unAccountName (_account_name a)) "account-details__name"
      -- Public key
      _ <- displayText "Public Key" (constDyn . keyToText . _keyPair_publicKey $ _account_key a) "account-details__pubkey"
      -- Chain id
      _ <- displayText "Chain ID" (constDyn . T.pack . show $ _account_chainId a) "account-details__chain-id"
      -- separator
      el "hr" blank
      -- Kadena Address
      _ <- displayText "Kadena Address" dKAddr "account-details__kadena-address"
      -- copy
      _ <- divClass "account-details__copy-btn-wrapper" $ copyButton (def
        & uiButtonCfg_class .~ constDyn "account-details__copy-btn button_type_confirm"
        & uiButtonCfg_title .~ constDyn (Just "Copy")
        ) $ current dKAddr

      pure ()

    elClass "h2" "heading heading_type_h2" $ text "History"
    divClass "group" $ text "Coming soon to a dialog near you!"

  modalFooter $ do
    _ <- cancelButton (def & uiButtonCfg_class <>~ " account-details__remove-account-btn") "Remove Account"
    onDone <- confirmButton def "Done"
    pure
      ( mempty
      , leftmost [onClose, onDone]
      )
