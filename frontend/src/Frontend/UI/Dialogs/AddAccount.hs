{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Frontend.UI.Dialogs.AddAccount
  ( uiAddWalletOnlyAccountDialogButton
  ) where

import           Control.Lens
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Text (Text)
------------------------------------------------------------------------------
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.Wallet
import           Frontend.Network (HasNetworkCfg, network_selectedNetwork)
import           Frontend.JsonData (HasJsonDataCfg)
import           Frontend.Crypto.Class (HasCrypto)
import           Frontend.UI.DeploymentSettings (transactionDisplayNetwork, userChainIdSelect)
import           Frontend.UI.Dialogs.AddVanityAccount (uiAddVanityAccountSettings)

import           Frontend.Ide (ide_wallet)
import           Frontend.UI.Modal
import           Frontend.UI.Modal.Impl (ModalIde, ModalImpl)
import           Frontend.UI.Widgets
import           Frontend.Foundation

import Obelisk.Generated.Static

type HasAddAccountModelCfg model mConf key m t =
  ( Monoid mConf, Flattenable mConf t
  , HasWalletCfg mConf key t
  , HasCrypto key (Performable m)
  , HasNetworkCfg mConf t
  , HasJsonDataCfg mConf t
  )

uiAddWalletOnlyAccountDialogButton
  :: forall t m model key mConf
     . ( MonadWidget t m
       , HasAddAccountModelCfg model mConf key m t
       , HasModalCfg mConf (ModalImpl m key t) t
       )
  => ModalIde m key t
  -> m mConf
uiAddWalletOnlyAccountDialogButton m = do
  eOpenAddAccount <- divClass "wallet__add-wallet-only-account-btn" $ confirmButton def "+ Add Account"
  pure $ mempty & modalCfg_setModal .~ (Just (uiCreateWalletOnlyAccount m) <$ eOpenAddAccount)

uiCreateWalletOnlyAccount
  :: forall t m model key mConf
     . ( MonadWidget t m
       , HasAddAccountModelCfg model mConf key m t
       )
  => ModalIde m key t
  -> Event t ()
  -> m (mConf, Event t ())
uiCreateWalletOnlyAccount model _onCloseExternal = mdo
  onClose <- modalHeader $ dynText title

  dwf <- workflow (uiCreateWalletStepOne model onClose)

  let (title, (conf, dEvent)) = fmap splitDynPure $ splitDynPure dwf

  mConf <- flatten =<< tagOnPostBuild conf

  return (mConf, leftmost [switch $ current dEvent, onClose])

uiCreateWalletStepOne
  :: forall t m model key mConf
     . ( MonadWidget t m
       , HasAddAccountModelCfg model mConf key m t
       )
  => ModalIde m key t
  -> Event t ()
  -> Workflow t m (Text, (mConf, Event t ()))
uiCreateWalletStepOne model onClose = Workflow $ do
  (dSelectedChain, dNotes, onAddVanityAcc) <- modalMain $ do
    divClass "segment modal__main transaction_details" $ do
      elClass "h2" "heading heading_type_h2" $ text "Destination"
      dChainId <- divClass "group segment" $ do
        transactionDisplayNetwork model
        userChainIdSelect model

      elClass "h2" "heading heading_type_h2" $ text "Reference Data"
      dNotes <- divClass "group segment" $
        value <$> mkLabeledClsInput inpElem "Notes"

      onAddVanityAcc <- fmap snd $ accordionItem' False "add-account__advanced-content" (text "Advanced") $
        confirmButton def "Create Vanity Account"

      pure (dChainId, dNotes, onAddVanityAcc)

  modalFooter $ do
    onCancel <- cancelButton def "Cancel"
    let isDisabled = isNothing <$> dSelectedChain

    onConfirm <- confirmButton (def & uiButtonCfg_disabled .~ isDisabled) "Add New Account"

    let eAddAcc = flip push onConfirm $ \() -> runMaybeT $ do
          chain <- MaybeT $ sample $ current dSelectedChain
          net <- lift $ sample $ current $ model ^. network_selectedNetwork
          notes <- lift $ sample $ current dNotes
          pure (net, chain, notes)
        newConf = mempty & walletCfg_createWalletOnlyAccount .~ eAddAcc

    pure
      ( ("Add Account", (newConf, leftmost [onClose, onCancel]))
      , leftmost
        [ uiWalletOnlyAccountCreated newConf onClose <$> (model ^. ide_wallet . wallet_walletOnlyAccountCreated)
        , uiAddVanityAccountSettings model dSelectedChain dNotes <$ onAddVanityAcc
        ]
      )
  where
    inpElem cls = uiInputElement $ def
      & initialAttributes .~
        ( ("class" =: renderClass (cls <> "input")) <>
          ("placeholder" =: "Some personal notes")
        )

uiWalletOnlyAccountCreated
  :: forall t m mConf.
     MonadWidget t m
  => mConf
  -> Event t ()
  -> AccountName
  -> Workflow t m (Text, (mConf, Event t ()))
uiWalletOnlyAccountCreated newConf onClose newAccount = Workflow $ do
  _ <- modalMain $ divClass "segment modal__main wallet_only__account-created-modal" $ do
    elClass "h2" "heading heading_type_h2" $ do
      elAttr "img" (
        "src" =: static @"img/Wallet_Graphic_1.png" <>
        "class" =: "wallet_only__account-created-splash-bg wallet_only__account-created-done-splash-bg"
        ) blank

      elAttr "img" (
        "src" =: static @"img/Wallet_Icon_Highlighted_Blue.png" <>
        "class" =: "wallet_only__account-created-wallet-blue-icon"
        ) blank

    divClass "wallet-only__account-created-details" $ do
      divClass "wallet-only__account-heading" $ text "Account Created:"
      divClass "wallet-only__account-name" $ text (unAccountName newAccount)

  modalFooter $ do
    onConfirm <- confirmButton def "Done"
    pure (("All Set!", (newConf, leftmost [onClose, onConfirm])), never)
