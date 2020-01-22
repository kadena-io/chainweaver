{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecursiveDo #-}
-- | Dialog for viewing the details of an account.
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.AccountDetails
  ( uiAccountDetailsOnChain
  , uiAccountDetails
  ) where

import Control.Lens
import qualified Data.Map as Map
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core hiding (Key)

import qualified Pact.Types.ChainId as Pact

import Frontend.Crypto.Ed25519 (keyToText)
import Frontend.Foundation
import Frontend.KadenaAddress
import Frontend.Network
import Frontend.UI.Modal
import Frontend.UI.Widgets
import Frontend.UI.Widgets.Helpers (dialogSectionHeading)
import Frontend.Wallet

type HasUiAccountDetailsModelCfg mConf key t =
  ( Monoid mConf
  , Flattenable mConf t
  , HasWalletCfg mConf key t
  )

uiAccountDetailsOnChain
  :: ( HasUiAccountDetailsModelCfg mConf key t
     , MonadWidget t m
     )
  => NetworkName
  -> (AccountName, ChainId, Account)
  -> Event t ()
  -> m (mConf, Event t ())
uiAccountDetailsOnChain netname a onCloseExternal = mdo
  onClose <- modalHeader $ dynText title

  dwf <- workflow (uiAccountDetailsOnChainImpl netname a (onClose <> onCloseExternal))

  let (title, (conf, dEvent)) = fmap splitDynPure $ splitDynPure dwf

  mConf <- flatten =<< tagOnPostBuild conf

  return ( mConf
         , leftmost [switch $ current dEvent, onClose]
         )

uiAccountDetailsOnChainImpl
  :: forall mConf key t m.
     ( HasUiAccountDetailsModelCfg mConf key t
     , MonadWidget t m
     )
  => NetworkName
  -> (AccountName, ChainId, Account)
  -> Event t ()
  -> Workflow t m (Text, (mConf, Event t ()))
uiAccountDetailsOnChainImpl netname (name, chain, account) onClose = Workflow $ do
  let kAddr = KadenaAddress name chain Nothing

      displayText lbl v cls =
        let
          attrFn cfg = uiInputElement $ cfg
            & initialAttributes <>~ ("disabled" =: "true" <> "class" =: (" " <> cls))
        in
          mkLabeledInputView False lbl attrFn $ pure v

  notesEdit <- divClass "modal__main account-details" $ do
    dialogSectionHeading mempty "Basic Info"
    notesEdit <- divClass "group" $ do
      -- Account name
      _ <- displayText "Account Name" (unAccountName name) "account-details__name"
      -- Chain id
      _ <- displayText "Chain ID" (Pact._chainId chain) "account-details__chain-id"
      -- Notes edit
      notesEdit :: Maybe (Dynamic t Text) <- do
        fmap (Just . value) $ mkLabeledClsInput False "Notes" $ \cls -> uiInputElement $ def
          & inputElementConfig_initialValue .~ case _vanityAccount_notes $ _account_storage account of
            Nothing -> ""
            Just n -> unAccountNotes n
          & initialAttributes . at "class" %~ pure . maybe (renderClass cls) (mappend (" " <> renderClass cls))
          & initialAttributes <>~ "maxlength" =: "70"
      -- separator
      horizontalDashedSeparator
      -- Kadena Address
      _ <- uiDisplayKadenaAddressWithCopy kAddr
      pure notesEdit

    dialogSectionHeading mempty "Keyset Info"
    divClass "group" $ do
      -- Public key
      case _account_status account of
        AccountStatus_Unknown -> text "Unknown"
        AccountStatus_DoesNotExist -> text "Does not exist"
        AccountStatus_Exists d -> do
          _ <- displayText "Predicate" (_addressKeyset_pred $ _accountDetails_keyset d) ""
          elClass "div" "segment segment_type_tertiary labeled-input" $ do
            divClass "label labeled-input__label" $ text "Public Keys Associated to Account"
            for_ (_addressKeyset_keys $ _accountDetails_keyset d) $ \key -> uiInputElement $ def
              & initialAttributes %~ Map.insert "disabled" "disabled" . addToClassAttr "labeled-input__input labeled-input__multiple"
              & inputElementConfig_initialValue .~ keyToText key

    pure notesEdit

  modalFooter $ do
    onDone <- confirmButton def "Done"

    let
      onNotesUpdate = case notesEdit of
        Nothing -> never
        Just notes -> (netname, name, Just chain,) . mkAccountNotes <$> current notes <@ (onDone <> onClose)
      conf = mempty & walletCfg_updateAccountNotes .~ onNotesUpdate

    pure ( ("Account Details", (conf, onDone))
         , never
         )

uiAccountDetails
  :: ( Monoid mConf, Flattenable mConf t
     , HasWalletCfg mConf key t
     , MonadWidget t m
     )
  => NetworkName
  -> AccountName
  -> Maybe AccountNotes
  -> Event t ()
  -> m (mConf, Event t ())
uiAccountDetails net account notes _onCloseExternal = mdo
  onClose <- modalHeader $ dynText title
  dwf <- workflow (uiAccountDetailsImpl net account notes onClose)
  let (title, (dConf, dEvent)) = fmap splitDynPure $ splitDynPure dwf
  conf <- flatten =<< tagOnPostBuild dConf
  return ( conf
         , leftmost [switch $ current dEvent, onClose]
         )

uiAccountDetailsImpl
  :: ( Monoid mConf
     , HasWalletCfg mConf key t
     , MonadWidget t m
     )
  => NetworkName
  -> AccountName
  -> Maybe AccountNotes
  -> Event t ()
  -> Workflow t m (Text, (mConf, Event t ()))
uiAccountDetailsImpl net account notes onClose = Workflow $ do
  let displayText lbl v cls =
        let
          attrFn cfg = uiInputElement $ cfg
            & initialAttributes <>~ ("disabled" =: "true" <> "class" =: (" " <> cls))
        in
          mkLabeledInputView False lbl attrFn $ pure v

  divClass "modal__main key-details" $ do
    divClass "group" $ do
      _ <- displayText "Account Name" (unAccountName account) "account-details__name"
      _ <- displayText "Notes" (maybe "" unAccountNotes notes) "account-details__notes"
      pure ()

  modalFooter $ do
    onRemove <- cancelButton (def & uiButtonCfg_class <>~ " account-details__remove-account-btn") "Remove Account"
    onDone <- confirmButton def "Done"

    let done = leftmost [onClose, onDone]

    pure ( ("Key Details", (mempty, done))
         , uiDeleteConfirmation net account onClose <$ onRemove
         )

uiDeleteConfirmation
  :: forall key t m mConf
  . ( MonadWidget t m
    , Monoid mConf
    , HasWalletCfg mConf key t
    )
  => NetworkName
  -> AccountName
  -> Event t ()
  -> Workflow t m (Text, (mConf, Event t ()))
uiDeleteConfirmation net name onClose = Workflow $ do
  modalMain $ do
    divClass "segment modal__filler" $ do
      dialogSectionHeading mempty "Warning"
      divClass "group" $
        text "You are about to remove this account from your wallet"
  modalFooter $ do
    onConfirm <- confirmButton (def & uiButtonCfg_class .~ "account-delete__confirm") "Remove Account"
    let cfg = mempty & walletCfg_delAccount .~ ((net, name) <$ onConfirm)
    pure ( ("Remove Confirmation", (cfg, leftmost [onClose, onConfirm]))
         , never
         )
