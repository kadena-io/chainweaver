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
import           Control.Applicative ((<|>))
import           Control.Monad.Except (runExcept, runExceptT, ExceptT(..))
import           Data.Either (isLeft)
import           Control.Arrow (left)
import           Data.Text (Text)
import           Reflex
import           Reflex.Dom
import qualified Data.Text as T
------------------------------------------------------------------------------
import           Obelisk.Generated.Static
------------------------------------------------------------------------------
import           Frontend.Wallet
import           Frontend.Crypto.Ed25519 (parsePublicKey)
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
      (errName, errKeyPair) :: (Dynamic t (Either Text Text), Dynamic t (Either Text KeyPair))
        <- modalBody $ do
        divClass "segment modal__filler" $ do
          divClass "modal__filler-horizontal-center-box" $
            imgWithAltCls "modal__filler-img" (static @"img/keys-scalable.svg") "Keys" blank

          elClass "h2" "heading heading_type_h2" $ text "Import Existing Key"
          elKlass "div" "group segment" $ do
            (_, name) <- mkLabeledInput mkNameInput "Key Name" def
            (_, errPub) <- mkLabeledInput mkPubKeyInput  "Public Key" def
            (_, parsedPair) <- mkLabeledInput (mkPrivKeyInput errPub) "Private Key" def

            pure (name, parsedPair)

      modalFooter $ do
        onCancel <- cancelButton def "Cancel"
        text " "
        let
          isInvalidName = isLeft <$> errName
          isInvalidKeys = either (const True) (const False) <$> errKeyPair
          isDisabled = (||) <$> isInvalidKeys <*> isInvalidName

          namedKeyPair = runExceptT $
            (,) <$> ExceptT errName <*> ExceptT errKeyPair

        onConfirm <- confirmButton (def & uiButtonCfg_disabled .~ isDisabled) "Import"

        let onConfirmKeyPair = fmapMaybe (^? _Right) $ tag (current namedKeyPair) onConfirm

        pure
          ( mempty & walletCfg_importKey .~ onConfirmKeyPair
          , leftmost [onClose, onCancel, onConfirm]
          )
  where

    mkNameInput cfg = do
      let
        nameParser = do
          mkNotValidMsg <- checkKeyNameValidityStr m
          pure $ \v -> maybe (Right v) Left $ mkNotValidMsg v <|> mkEmptyNameMessage v
      inputWithError uiInputElement nameParser cfg

    mkEmptyNameMessage v =
      if (T.null . T.strip) v then Just "Key name must not be empty" else Nothing

    mkPubKeyInput =
      inputWithError uiTextAreaElement (pure $ runExcept . parsePublicKey)

    mkPrivKeyInput dynErrPub =
      inputWithError uiTextAreaElement $ parseWalletKeyPair . left (const "") <$> dynErrPub




inputWithError
  :: (Monad m, DomBuilder t m, PostBuild t m, HasValue input, Value input ~ Dynamic t Text)
  => (cfg -> m input)
  -> (Dynamic t (Text -> Either Text a))
  -> cfg
  -> m (input, Dynamic t (Either Text a))
inputWithError mkInput parseVal cfg = do
    i <- mkInput cfg
    el "div" $ do
      let
        r = parseVal <*> value i
        errorMsg = either id (const "") <$> r
      elClass "span" "error_inline" $ dynText errorMsg
      pure (i, r)

