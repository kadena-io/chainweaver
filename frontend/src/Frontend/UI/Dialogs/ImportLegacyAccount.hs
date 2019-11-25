{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.UI.Dialogs.ImportLegacyAccount
  ( uiImportLegacyAccountSettings
  ) where

import Control.Error (hush, note)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT, lift)
import Control.Lens ((^.))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex
import Reflex.Dom.Contrib.CssClass (renderClass)
import Reflex.Dom.Core
import Reflex.Network.Extended (Flattenable)
import Pact.Types.Scheme (PPKScheme(ETH, ED25519))

import Frontend.UI.Modal.Impl (ModalIde, modalFooter)
import Frontend.Crypto.Class (HasCrypto, cryptoVerifyPactKey, _pactKey_publicKey)
import Frontend.Network (ChainId, network_selectedNetwork)
import Frontend.UI.Widgets
import Frontend.Wallet (HasWalletCfg (..), AccountName, unAccountName, wallet_pactAccountCreated)
import Frontend.UI.Modal
import Common.Wallet (keyToText)
import Obelisk.Generated.Static

type HasUIImportLegacyAccountCfg mConf key t =
  ( Monoid mConf
  , Flattenable mConf t
  , HasWalletCfg mConf key t
  )

uiImportLegacyAccountSettings
  :: forall key t m mConf
  . ( MonadWidget t m
    , HasUIImportLegacyAccountCfg mConf key t
    , HasCrypto key (Performable m)
    )
  => ModalIde m key t
  -> Dynamic t (Maybe ChainId)
  -> Dynamic t Text
  -> Workflow t m (Text, (mConf, Event t ()))
uiImportLegacyAccountSettings ideL mChainId initialNotes = Workflow $ do
  (schemeInput, secretInput) <- elClass "div" "modal__main transaction_details" $ do
    (schemeInput,_) <- divClass "legacy-import-account__key-scheme"  $ mkLabeledClsInput True "Key Scheme" $ \cls ->
      uiSelectElement @t @m
        (def
          & initialAttributes .~ "class" =: (renderClass cls)
          & selectElementConfig_initialValue .~ "ED25519"
          ) $ do
          elAttr "option" ("value" =: "ED25519") $ text "ED25519"
          elAttr "option" ("value" =: "ETH") $ text "ETH"

    secretInput <- divClass "legacy-import-account__secret" $ mkLabeledClsInput True "Secret Key"
        $ \cls -> uiInputElement $ def
                  & initialAttributes .~ "class" =: (renderClass cls)

    pure (schemeInput, secretInput)

  ePactKey <- performEvent $ cryptoVerifyPactKey
    <$> (schemeFromText <$> current (_selectElement_value schemeInput))
    <@> _inputElement_input secretInput

  dPactKey <- holdDyn (Left "Enter your private key to continue") ePactKey
  let dCreateInput = runExceptT $
        (\pk netw chainId notes -> (pk, netw, chainId, notes))
        <$> (ExceptT dPactKey)
        <*> lift (ideL ^. network_selectedNetwork)
        <*> (ExceptT $ note "Chain id is not selected" <$> mChainId)
        <*> lift initialNotes

  dyn_ $ ffor dCreateInput $ \case
    Left e -> el "p" $ text $ T.pack $ e
    Right (pk,_,_,_) -> el "p" $ text $ "Public Key: " <> keyToText (_pactKey_publicKey $ pk)

  let isDisabled = either (const True) (const False) <$> dCreateInput

  (eOnCancel, eOnImport) <- modalFooter $ do
    onCancel <- cancelButton def "Cancel"
    onImport <- confirmButton (def & uiButtonCfg_disabled .~ isDisabled) "Import"
    pure (onCancel, onImport)

  let eNewAccountOk = fmapMaybe hush $ current dCreateInput <@ eOnImport

  pure
    ( ( "Import Legacy Account"
      , (mempty & walletCfg_importPactKeypair .~ eNewAccountOk
        , eOnCancel
        )
      )
    , uiImportLegacyAccountCreated <$> (ideL ^. wallet_pactAccountCreated)
    )
  where
    schemeFromText "ED25519" = ED25519
    schemeFromText "ETH" = ETH
    schemeFromText _ = ED25519


-- TODO: Dedupe this from AddAccount
uiImportLegacyAccountCreated
  :: forall t m mConf.
     (MonadWidget t m, Monoid mConf)
  => AccountName
  -> Workflow t m (Text, (mConf, Event t ()))
uiImportLegacyAccountCreated newAccount = Workflow $ do
  _ <- modalMain $ divClass "segment modal__main wallet_only__account-created-modal" $ do
    elClass "h2" "heading heading_type_h2" $ do
      elAttr "div"
        (  "class" =: "wallet_only__account-created-done-splash-bg"
        <> "style" =: ("background-image: url(" <> (static @"img/Wallet_Graphic_1.png") <> ")")
        ) $
        elAttr "img" (
          "src" =: static @"img/Wallet_Icon_Highlighted_Blue.png" <>
          "class" =: "wallet_only__account-created-wallet-blue-icon"
          ) blank

    divClass "wallet-only__account-created-details" $ do
      divClass "wallet-only__account-heading" $ text "Account Created:"
      divClass "wallet-only__account-name" $ text (unAccountName newAccount)

  modalFooter $ do
    onConfirm <- confirmButton def "Done"
    pure (("All Set!", (mempty, onConfirm)), never)
