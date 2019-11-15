{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Dialog for viewing the details of an account.
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.AccountDetails
  ( uiAccountDetails
  ) where
------------------------------------------------------------------------------
import           Control.Lens
import           Data.Text (Text)
import qualified Data.IntMap as IntMap
import qualified Pact.Types.ChainId as Pact
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.KadenaAddress (mkKadenaAddress, textKadenaAddress)
------------------------------------------------------------------------------
import           Frontend.UI.Modal
import           Frontend.Wallet
import           Frontend.Crypto.Ed25519 (keyToText)
import           Frontend.UI.Widgets
import           Frontend.Foundation
------------------------------------------------------------------------------

type HasUiAccountDetailsModelCfg mConf key t =
  ( Monoid mConf
  , Flattenable mConf t
  , HasWalletCfg mConf key t
  )

uiAccountDetails
  :: ( HasUiAccountDetailsModelCfg mConf key t
     , MonadWidget t m
     )
  => IntMap.Key
  -> Account key
  -> Event t ()
  -> m (mConf, Event t ())
uiAccountDetails key a _onCloseExternal = mdo
  onClose <- modalHeader $ dynText title

  dwf <- workflow (uiAccountDetailsDetails key a onClose)

  let (title, (conf, dEvent)) = fmap splitDynPure $ splitDynPure dwf

  mConf <- flatten =<< tagOnPostBuild conf

  return ( mConf
         , leftmost [switch $ current dEvent, onClose]
         )

uiAccountDetailsDetails
  :: ( HasUiAccountDetailsModelCfg mConf key t
     , MonadWidget t m
     )
  => IntMap.Key
  -> Account key
  -> Event t ()
  -> Workflow t m (Text, (mConf, Event t ()))
uiAccountDetailsDetails key a onClose = Workflow $ do
  let kAddr = textKadenaAddress $ mkKadenaAddress (_account_network a) (_account_chainId a) (_account_name a)

  let displayText lbl v cls =
        let
          attrFn cfg = uiInputElement $ cfg
            & initialAttributes <>~ ("disabled" =: "true" <> "class" =: (" " <> cls))
        in
          mkLabeledInputView attrFn lbl $ pure v

  modalMain $ divClass "modal__main account-details" $ do
    elClass "h2" "heading heading_type_h2" $ text "Info"
    divClass "group" $ do
      -- Account name
      _ <- displayText "Account Name" (unAccountName (_account_name a)) "account-details__name"
      -- Public key
      _ <- displayText "Public Key" (keyToText . _keyPair_publicKey $ _account_key a) "account-details__pubkey"
      -- Chain id
      _ <- displayText "Chain ID" (Pact._chainId $ _account_chainId a) "account-details__chain-id"
      -- separator
      el "hr" blank
      -- Kadena Address
      _ <- displayText "Kadena Address" kAddr "account-details__kadena-address"
      -- copy
      _ <- divClass "account-details__copy-btn-wrapper" $ copyButton (def
        & uiButtonCfg_class .~ constDyn "account-details__copy-btn button_type_confirm"
        & uiButtonCfg_title .~ constDyn (Just "Copy")
        ) $ pure kAddr

      pure ()

    elClass "h2" "heading heading_type_h2" $ text "History"
    divClass "group" $ text "Coming soon to a dialog near you!"

  modalFooter $ do
    onRemove <- cancelButton (def & uiButtonCfg_class <>~ " account-details__remove-account-btn") "Remove Account"
    onDone <- confirmButton def "Done"

    pure ( ("Account Details", (mempty, leftmost [onClose, onDone]))
         , uiDeleteConfirmation key onClose <$ onRemove
         )

uiDeleteConfirmation
  :: forall key t m mConf
  . ( MonadWidget t m
    , Monoid mConf
    , HasWalletCfg mConf key t
    )
  => IntMap.Key
  -> Event t ()
  -> Workflow t m (Text, (mConf, Event t ()))
uiDeleteConfirmation thisKey onClose = Workflow $ do
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
    onConfirm <- confirmButton (def & uiButtonCfg_class .~ "account-delete__confirm") "Permanently Remove Account"
    let cfg = mempty & walletCfg_delKey .~ (thisKey <$ onConfirm)
    pure ( ("Remove Confirmation", (cfg, leftmost [onClose, onConfirm]))
         , never
         )
