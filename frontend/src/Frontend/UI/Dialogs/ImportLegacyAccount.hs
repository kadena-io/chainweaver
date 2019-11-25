{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
module Frontend.UI.Dialogs.ImportLegacyAccount
  ( uiImportLegacyAccountSettings
  ) where

import Data.Text (Text)
import Reflex
import Reflex.Dom.Contrib.CssClass (renderClass)
import Reflex.Dom.Core
import Reflex.Network.Extended (Flattenable)

import Frontend.UI.Modal.Impl (ModalIde, modalFooter)
import Frontend.Crypto.Class (HasCrypto, GenKeyArg(GenWalletIndex), cryptoGenKey)
import Frontend.JsonData
import Frontend.Network (ChainId, HasNetworkCfg, NodeInfo)
import Frontend.UI.Widgets
import Frontend.Wallet (HasWalletCfg (..), KeyPair (..), checkAccountNameValidity, findNextKey, unAccountName)

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
  elClass "div" "modal__main transaction_details" $ do
    pkInput <- divClass "legacy-import-account__public-key"  $ mkLabeledClsInput True "Public Key"
        $ \cls -> uiInputElement $ def
                  & initialAttributes .~ "class" =: (renderClass cls)
    secretInput <- divClass "legacy-import-account__secret" $ mkLabeledClsInput True "Secret Key"
        $ \cls -> uiInputElement $ def
                  & initialAttributes .~ "class" =: (renderClass cls)

    pure (pkInput, secretInput)

  let isDisabled = constDyn False

  eNewAccount <- modalFooter $ uiButtonDyn
    (def & uiButtonCfg_class .~ "button_type_confirm" & uiButtonCfg_disabled .~ isDisabled)
    (text "Import")

  pure
    ( ("Import Legacy Account", (mempty, never))
    , never
    )

