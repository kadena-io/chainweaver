{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecursiveDo #-}
-- | Dialog for viewing the details of an account.
-- Copyright   :  (C) 2020-2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.AccountDetails
  ( uiAccountDetailsOnChain
  , uiAccountDetails
  , uiDisplayKeyset
  ) where

import Control.Lens
import Control.Monad (void)
import qualified Data.Map as Map
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core hiding (Key)

import Pact.Types.Pretty (renderCompactText)
import qualified Pact.Types.ChainId as Pact
import qualified Pact.Types.Term as Pact

import Frontend.Crypto.Ed25519 (keyToText)
import Frontend.Foundation
import Frontend.TxBuilder
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
  -> (AccountName, ChainId, AccountDetails, Account)
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

notesEditor :: MonadWidget t m => Maybe AccountNotes -> m (Dynamic t (Maybe AccountNotes))
notesEditor mNotes = do
  fmap (fmap mkAccountNotes . value) $ mkLabeledClsInput False "Notes" $ \cls -> uiInputElement $ def
    & inputElementConfig_initialValue .~ case mNotes of
      Nothing -> ""
      Just n -> unAccountNotes n
    & initialAttributes . at "class" %~ pure . maybe (renderClass cls) (mappend (" " <> renderClass cls))
    & initialAttributes <>~ "maxlength" =: "70"

uiAccountDetailsOnChainImpl
  :: forall mConf key t m.
     ( HasUiAccountDetailsModelCfg mConf key t
     , MonadWidget t m
     )
  => NetworkName
  -> (AccountName, ChainId, AccountDetails, Account)
  -> Event t ()
  -> Workflow t m (Text, (mConf, Event t ()))
uiAccountDetailsOnChainImpl netname (name, chain, details, account) onClose = Workflow $ do
  let kAddr = TxBuilder name chain $ details
        ^? accountDetails_guard
        . _AccountGuard_KeySetLike
        . to toPactKeyset

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
      notesEdit <- notesEditor $ _vanityAccount_notes $ _account_storage account
      pure notesEdit

    let keysetTitle = \case
          Pact.GKeySetRef (Pact.KeySetName _ _) -> "Keyset Reference"
          _ -> "Guard"
        guardTitle = maybe "Keyset" keysetTitle $ account ^? account_status
          . _AccountStatus_Exists
          . accountDetails_guard
          . _AccountGuard_Other

    dialogSectionHeading mempty (guardTitle <> " Info")
    divClass "group" $ do
      -- Public key
      case _account_status account of
        AccountStatus_Unknown -> text "Unknown"
        AccountStatus_DoesNotExist -> text "Does not exist"
        AccountStatus_Exists d -> uiDisplayKeyset $ _accountDetails_guard d
    pure notesEdit
  modalFooter $ do
    onDone <- confirmButton def "Done"
    let
      onNotesUpdate = (netname, name, Just chain,) <$> current notesEdit <@ (onDone <> onClose)
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
uiAccountDetails net account notes onCloseExternal = mdo
  onClose <- modalHeader $ dynText title
  dwf <- workflow (uiAccountDetailsImpl net account notes (onClose <> onCloseExternal))
  let (title, (dConf, dEvent)) = fmap splitDynPure $ splitDynPure dwf
  conf <- flatten =<< tagOnPostBuild dConf
  return ( conf
         , leftmost [switch $ current dEvent, onClose]
         )

uiDisplayKeyset :: MonadWidget t m => AccountGuard -> m ()
uiDisplayKeyset guard =
  let displayText lbl v cls =
        let
          attrFn cfg = uiInputElement $ cfg
            & initialAttributes <>~ ("disabled" =: "true" <> "class" =: (" " <> cls))
        in
          mkLabeledInputView False lbl attrFn $ pure v
  in case guard of
    AccountGuard_KeySetLike (KeySetHeritage ksKeys ksPred _ksRef) -> do
      _ <- displayText "Predicate" ksPred ""
      elClass "div" "segment segment_type_tertiary labeled-input" $ do
        divClass "label labeled-input__label" $ text "Public Keys Controlling Account"
        for_ ksKeys $ \key -> uiInputElement $ def
          & initialAttributes %~ Map.insert "disabled" "disabled" . addToClassAttr "labeled-input__input labeled-input__multiple"
          & inputElementConfig_initialValue .~ keyToText key
    AccountGuard_Other g -> case g of
      (Pact.GKeySetRef ksN@(Pact.KeySetName _ _)) -> do
        void $ displayText "Name" (renderCompactText ksN) ""
      _ -> void $ displayText (pactGuardTypeText $ Pact.guardTypeOf g) (renderCompactText g) ""

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

  notesEdit <- divClass "modal__main key-details" $ do
    dialogSectionHeading mempty "Basic Info"
    divClass "group" $ do
      _ <- displayText "Account Name" (unAccountName account) "account-details__name"
      notesEditor notes

  modalFooter $ do
    onRemove <- cancelButton (def & uiButtonCfg_class <>~ " account-details__remove-account-btn") "Remove Account"
    onDone <- confirmButton def "Done"

    let onNotesUpdate = (net, account, Nothing,) <$> current notesEdit <@ (onDone <> onClose)
        conf = mempty & walletCfg_updateAccountNotes .~ onNotesUpdate

    pure ( ("Account Details", (conf, onDone))
         , uiDeleteConfirmation net account <$ onRemove
         )

uiDeleteConfirmation
  :: forall key t m mConf
  . ( MonadWidget t m
    , Monoid mConf
    , HasWalletCfg mConf key t
    )
  => NetworkName
  -> AccountName
  -> Workflow t m (Text, (mConf, Event t ()))
uiDeleteConfirmation net name = Workflow $ do
  modalMain $ do
    divClass "segment modal__filler" $ do
      dialogSectionHeading mempty "Warning"
      let line = divClass "group" . text
      line "You are about to remove this account from view in your wallet"
      line "Note that removing an account from your wallet does not remove any existing accounts from the blockchain."
      line "To restore this account back into view, simply enter the account's name within the \"Add Account\" dialog."
  modalFooter $ do
    onConfirm <- confirmButton (def & uiButtonCfg_class .~ "account-delete__confirm") "Remove Account"
    let cfg = mempty & walletCfg_delAccount .~ ((net, name) <$ onConfirm)
    pure ( ("Remove Confirmation", (cfg, onConfirm))
         , never
         )
