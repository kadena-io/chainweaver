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
{-# LANGUAGE TypeApplications      #-}

-- | Confirmation dialog for (key) deletion.
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.Signing
  ( uiSigning
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.Foundation         hiding (Arg)
import           Frontend.UI.Modal
import           Frontend.UI.Widgets
import           Frontend.Wallet             (HasWalletCfg(..))
------------------------------------------------------------------------------
import Data.ByteString (ByteString)
import Frontend.Crypto.Ed25519
import Frontend.Wallet
import Frontend.Network
import Frontend.UI.DeploymentSettings
import Control.Monad (void)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Pact.Types.Hash (hash, hashToText, toUntypedHash, PactHash)
import Frontend.AppCfg
import Frontend.UI.Modal.Impl

type HasUISigningModelCfg mConf t =
  ( Monoid mConf, Flattenable mConf t , HasWalletCfg mConf t
  )

-- | Ask user for confirmation before deleting "something".
--
-- At the moment "something" is only keys, so we skip making "something"
-- configurable for now.
--
uiSigning
  :: forall t m mConf
  . (MonadWidget t m, HasUISigningModelCfg mConf t)
  => AppCfg t m -> ModalIde m t -> ByteString -> m (mConf, Event t ())
uiSigning appCfg ideL obj = do
  onClose <- modalHeader $ text "Signing Request"
  modalMain $ do
    chosenKeys <- modalBody $ do
      divClass "group" $ do
        el "strong" $ text "Hash: "
        text $ hashToText $ toUntypedHash (hash obj :: PactHash)
      elAttr "pre" ("style" =: "white-space: pre-wrap;") $ do
        text $ T.decodeUtf8With T.lenientDecode obj
      elClass "h2" "heading heading_type_h2" $ text "Select signing keys below:"
      current <$> uiSigningKeys mempty ideL

    modalFooter $ do
      cancel <- cancelButton def "Cancel"
      text " "
      sign <- confirmButton def "Sign"
      let doSign (keySet, allKeys) () = do
            let toTuple k = (_keyPair_publicKey k,) <$> _keyPair_privateKey k
                (public, private) = unzip $ fmapMaybe toTuple $ getSigningPairs keySet allKeys
            sigs <- traverse (mkSignature obj) private
            pure $ Right $ SigningResult
              { _signingResult_publicKeys = public
              , _signingResult_signatures = sigs
              }
          walletKeys = current $ ideL ^. wallet_keys
      signed <- performEvent $ attachWith doSign ((,) <$> chosenKeys <*> walletKeys) sign
      let done = leftmost [signed, Left "Cancelled" <$ (cancel <> onClose)]
      performEvent_ $ liftJSM . _appCfg_signingResponse appCfg <$> done
      pure (mempty, void done)
