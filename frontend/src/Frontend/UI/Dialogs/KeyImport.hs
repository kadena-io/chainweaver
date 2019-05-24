{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

-- | Dialog for importing a key to the wallet.
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.KeyImport
  ( uiKeyImport
  , HasUiKeyImportModel
  , HasUiKeyImportModelCfg
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Text (Text)
import           Data.Void                      (Void)
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Obelisk.Generated.Static
------------------------------------------------------------------------------
import           Frontend.Ide
import           Frontend.ModuleExplorer        (HasModuleExplorerCfg (..))
import           Frontend.Wallet
import           Frontend.UI.DeploymentSettings
import           Frontend.UI.Modal
import           Frontend.UI.Widgets
import           Frontend.Foundation
import           Frontend.UI.Widgets.Helpers (imgWithAltCls)
------------------------------------------------------------------------------


type HasUiKeyImportModel model t =
  (HasWallet model t)

type HasUiKeyImportModelCfg mConf t =
  ( Monoid mConf, Flattenable mConf t
  , HasWalletCfg mConf t
  )


-- | Dialog for entering a public/private key pair.
--
uiKeyImport
  :: forall t m model mConf.
    ( MonadWidget t m, HasUiKeyImportModel model t, HasUiKeyImportModelCfg mConf t
    )
  => model
  -> m (mConf, Event t ())
uiKeyImport m = do
    onClose <- modalHeader $ text "Key Import"
    modalMain $ do
      (name, errKeyPair) :: (Dynamic t Text, Dynamic t (Either ParseKeyPairError KeyPair)) <- modalBody $ do
        divClass "segment modal__filler" $ do
          divClass "modal__filler-horizontal-center-box" $
            imgWithAltCls "modal__filler-img" (static @"img/keys-scalable.svg") "Keys" blank

          elClass "h2" "heading heading_type_h2" $ text "Import Existing Key"
          elKlass "div" "group segment" $ mdo
            nameInput <- mkLabeledInput uiInputElement "Key Name" def
            pubKeyInput <- mkLabeledInput (mkPubKeyInput err) "Public Key" def
            privKeyInput <- mkLabeledInput (mkPrivKeyInput err) "Private Key" def

            let
              errKeyPair = parseTextKeyPair <$> value pubKeyInput <*> value privKeyInput
              err = fmap (^? _Left) errKeyPair

            pure (value nameInput, errKeyPair)

      modalFooter $ do
        onCancel <- cancelButton def "Cancel"
        text " "
        let
          isDisabled = either (const True) (const False) <$> errKeyPair
          namedKeyPair = do
            n <- name
            errP <- errKeyPair
            pure $ (n,) <$> errP
        onConfirm <- confirmButton (def & uiButtonCfg_disabled .~ isDisabled) "Import"

        let onConfirmKeyPair = fmapMaybe (^? _Right) $ tag (current namedKeyPair) onConfirm

        pure
          ( mempty & walletCfg_importKey .~ onConfirmKeyPair
          , leftmost [onClose, onCancel, onConfirm]
          )
  where
    mkPubKeyInput err cfg = do
      let
        errTxt = ffor err $ \case
          Just ParseKeyPairError_InvalidPublicKey -> "Key not valid."
          _ -> ""
      inputWithError uiTextAreaElement errTxt cfg

    mkPrivKeyInput err cfg = do
      let
        errTxt = ffor err $ \case
          Just ParseKeyPairError_InvalidPrivateKey -> "Key not valid."
          Just ParseKeyPairError_PrivatePublicMismatch -> "Private key is not compatible with public key."
          _ -> ""
      inputWithError uiTextAreaElement errTxt cfg


inputWithError
  :: (Monad m, DomBuilder t m, PostBuild t m)
  => (cfg -> m input)
  -> Dynamic t Text
  -> cfg
  -> m input
inputWithError mkInput dynErr cfg = do
  i <- mkInput cfg
  el "div" $
    elClass "span" "error_inline" $ dynText dynErr
  pure i
