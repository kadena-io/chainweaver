{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE TupleSections #-}

module Frontend.UI.Dialogs.AddAccount
  ( uiCreateWalletOnlyAccount
  , uiWalletOnlyAccountCreated
  ) where

import           Control.Lens
------------------------------------------------------------------------------
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.Wallet
import           Frontend.Network
import           Frontend.UI.DeploymentSettings (transactionDisplayNetwork, userChainIdSelect)

import           Frontend.UI.Modal
import           Frontend.UI.Widgets
import           Frontend.Foundation

type HasAddAccountModelCfg mConf key t =
  ( Monoid mConf, Flattenable mConf t
  , HasWalletCfg mConf key t
  )

uiCreateWalletOnlyAccount
  :: forall t m model key mConf
     . ( MonadWidget t m
       , HasAddAccountModelCfg mConf key t
       , HasNetwork model t
       )
  => model
  -> Event t ()
  -> m (mConf, Event t ())
uiCreateWalletOnlyAccount model _onClose = do
  onClose <- modalHeader $ text "Add Account"
  (dSelectedChain, dNotes) <- modalMain $ do
    divClass "segment modal__main transaction_details" $ do
      elClass "h2" "heading heading_type_h2" $ text "Destination"
      dChainId <- divClass "group segment" $ do
        transactionDisplayNetwork model
        userChainIdSelect model

      elClass "h2" "heading heading_type_h2" $ text "Reference Data"
      dNotes <- divClass "group segment" $
        value <$> mkLabeledClsInput inpElem "Notes"

      _ <- accordionItem' False "add-account__advanced-content" (text "Advanced") $
        confirmButton (def & uiButtonCfg_disabled .~ pure True) "Create Vanity Account"

      pure (dChainId, dNotes)

  modalFooter $ do
    onCancel <- cancelButton def "Cancel"
    text " "
    let isDisabled = isNothing <$> dSelectedChain

    onConfirm <- confirmButton (def & uiButtonCfg_disabled .~ isDisabled) "Add New Account"

    let eAddAcc = attachWithMaybe (\n -> fmap (,n)) (current dNotes) (current dSelectedChain <@ onConfirm)

    pure
      ( mempty & walletCfg_createWalletOnlyAccount .~ eAddAcc
      , leftmost [onClose, onCancel, onConfirm]
      )
  where
    inpElem cls = uiInputElement $ def
      & initialAttributes .~
        ( ("class" =: renderClass (cls <> "input")) <>
          ("placeholder" =: "Some personal notes")
        )

uiWalletOnlyAccountCreated
  :: forall t m mConf
     . ( MonadWidget t m
       , Monoid mConf
       )
  => AccountName
  -> Event t ()
  -> m (mConf, Event t ())
uiWalletOnlyAccountCreated newAccount _onClose = do
  onClose <- modalHeader $ text "All Set!"
  _ <- modalMain $ divClass "segment modal__main wallet_only__account-created-modal" $ do
    elClass "h2" "heading heading_type_h2" $
      text "[WALLET IMAGE PLACEHOLDER]"
    divClass "wallet-only__account-created-details" $ do
      divClass "wallet-only__account-heading" $ text "Account Created:"
      divClass "wallet-only__account-name" $ text (unAccountName newAccount)

  modalFooter $ do
    onConfirm <- confirmButton def "Done"
    pure $ (mempty, leftmost [onClose, onConfirm])
