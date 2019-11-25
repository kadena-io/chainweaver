{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
module Frontend.UI.Dialogs.ImportLegacyAccount
  ( uiImportLegacyAccountSettings
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Reflex
import Reflex.Dom.Contrib.CssClass (renderClass)
import Reflex.Dom.Core
import Reflex.Network.Extended (Flattenable)
import Pact.Types.Scheme (PPKScheme(ETH, ED25519))

import Frontend.UI.Modal.Impl (ModalIde, modalFooter)
import Frontend.Crypto.Class (HasCrypto, GenKeyArg(GenWalletIndex), cryptoGenKey, cryptoVerifyPactKey, _pactKey_publicKey)
import Frontend.JsonData
import Frontend.Network (ChainId, HasNetworkCfg, NodeInfo)
import Frontend.UI.Widgets
import Frontend.Wallet (HasWalletCfg (..), KeyPair (..), checkAccountNameValidity, findNextKey, unAccountName)
import Common.Wallet (keyToText)

-- Allow the user to create a 'vanity' account, which is an account with a custom name
-- that lives on the chain. Requires GAS to create.

type HasUISigningModelCfg mConf key t =
  ( Monoid mConf
  , Flattenable mConf t
  , HasWalletCfg mConf key t
  , HasJsonDataCfg mConf t
  )

uiImportLegacyAccountSettings
  :: forall key t m mConf
  . ( MonadWidget t m
    , HasUISigningModelCfg mConf key t
    , HasCrypto key (Performable m)
    )
  => ModalIde m key t
  -> Dynamic t (Maybe ChainId)
  -> Dynamic t Text
  -> Workflow t m (Text, (mConf, Event t ()))
uiImportLegacyAccountSettings ideL mChainId initialNotes = Workflow $ do
  pb <- getPostBuild
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

  dyn $ ffor dPactKey $ \case
    Left e -> el "p" $ text $ T.pack $ e
    Right pk -> el "p" $ text $ "Public Key: " <> keyToText (_pactKey_publicKey $ pk)

  let isDisabled = either (const True) (const False) <$> dPactKey

  eNewAccount <- modalFooter $ uiButtonDyn
    (def & uiButtonCfg_class .~ "button_type_confirm" & uiButtonCfg_disabled .~ isDisabled)
    (text "Import")

  pure
    ( ("Import Legacy Account", (mempty, never))
    , never
    )
  where
    schemeFromText "ED25519" = ED25519
    schemeFromText "ETH" = ETH
