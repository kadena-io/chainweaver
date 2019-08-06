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

-- | Dialog presented for generating responses to signing API requests.
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.Signing
  ( uiSigning
  ) where

import Control.Lens
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Pact.Types.ChainMeta (PublicMeta(..))
import Pact.Types.Hash (hash, hashToText, toUntypedHash, PactHash)
import Reflex
import Reflex.Dom
import Safe (readMay)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Pact.Parse as Pact
import qualified Pact.Types.ChainMeta as Pact
import qualified Pact.Types.Gas as Pact

import Common.Network (ChainId(..))
import Frontend.AppCfg
import Frontend.Crypto.Ed25519
import Frontend.Foundation hiding (Arg)
import Frontend.Network
import Frontend.UI.DeploymentSettings
import Frontend.UI.Modal
import Frontend.UI.Modal.Impl
import Frontend.UI.TabBar
import Frontend.UI.Widgets
import Frontend.Wallet

type HasUISigningModelCfg mConf t =
  ( Monoid mConf, Flattenable mConf t , HasWalletCfg mConf t
  )

data SigningTab
  = SigningTab_Preview -- ^ Preview of object to be signed
  | SigningTab_Cfg -- ^ Actual settings like gas price/limit, ...
  | SigningTab_Keys -- ^ Select keys for signing the transaction.
  deriving (Eq, Ord, Show)

textSigningTab :: SigningTab -> Text
textSigningTab = \case
  SigningTab_Preview -> "Preview"
  SigningTab_Cfg -> "Configuration"
  SigningTab_Keys -> "Sign"

-- | Ask user for confirmation before deleting "something".
--
-- At the moment "something" is only keys, so we skip making "something"
-- configurable for now.
--
uiSigning
  :: forall t m mConf
  . (MonadWidget t m, HasUISigningModelCfg mConf t)
  => AppCfg t m -> ModalIde m t -> SigningRequest -> m (mConf, Event t ())
uiSigning appCfg ideL signingRequest = do
  let code = _signingRequest_code signingRequest
  onClose <- modalHeader $ text "Signing Request"
  modalMain $ do
    results <- modalBody $ uiSegment mempty $ mdo
      tabSelection <- holdDyn SigningTab_Preview tabChoice
      (TabBar tabChoice) <- makeTabBar $ TabBarCfg
        { _tabBarCfg_tabs = [SigningTab_Preview, SigningTab_Cfg, SigningTab_Keys]
        , _tabBarCfg_mkLabel = const $ text . textSigningTab
        , _tabBarCfg_selectedTab = Just <$> tabSelection
        , _tabBarCfg_classes = mempty
        , _tabBarCfg_type = TabBarType_Secondary
        }

      _ <- tabPane mempty tabSelection SigningTab_Preview $ do
        divClass "group" $ do
          el "strong" $ text "Hash: "
          text $ hashToText $ toUntypedHash (hash $ T.encodeUtf8 code :: PactHash)
        elAttr "pre" ("style" =: "white-space: pre-wrap;") $ do
          text code

      let walletKeys = ideL ^. wallet_keys
      (chainId, sender, price, limit) <- tabPane mempty tabSelection SigningTab_Cfg $ do
        chainId <- case _signingRequest_chainId signingRequest of
          Nothing -> divClass "group segment" $ current <$> userChainIdSelect ideL
          Just cid -> pure $ pure $ Just cid
        (sender, price, limit) <- divClass "group segment" $ do
          let s = senderDropdown (ideL ^. network_meta) walletKeys
          sender <- case _signingRequest_sender signingRequest of
            Nothing -> _selectElement_value <$> mkLabeledInput s "Sender" def
            Just s -> pure $ maybe "" fst . M.lookupMin
              . M.filter (\kp -> _keyPair_publicKey kp == s) <$> walletKeys

          initGasPrice <- sample $ _pmGasPrice <$> current (ideL ^. network_meta)
          gp <- mkLabeledInput uiRealInputElement "Gas price" $ def
            & inputElementConfig_initialValue .~ tshow initGasPrice

          gl <- mkLabeledInput uiIntInputElement "Gas limit" $ def
            & inputElementConfig_initialValue .~
              maybe "100" (T.pack . show) (_signingRequest_gasLimit signingRequest)
          let f g x = fmap g . readMay . T.unpack <$> current (value x)
              price = f (Pact.GasPrice . Pact.ParsedDecimal) gp
              limit = f (Pact.GasLimit . Pact.ParsedInteger) gl
          pure (sender, price, limit)
        pure (chainId, sender, price, limit)

      keys <- tabPane mempty tabSelection SigningTab_Keys $ do
        let preselected = S.singleton <$> sender
        current <$> uiSigningKeys preselected ideL

      pure $ (,,,,,) <$>
        keys <*> current walletKeys <*> chainId <*> current sender <*> price <*> limit

    modalFooter $ do
      cancel <- cancelButton def "Cancel"
      text " "
      sign <- confirmButton def "Sign"
      let doSign (keySet, allKeys, Just chainId', sender, Just price, Just limit) () = Just $ do
            let networkRequest = NetworkRequest
                  { _networkRequest_code = code
                  , _networkRequest_data = fromMaybe mempty $ _signingRequest_data signingRequest
                  -- Nothing below here seems to be used
                  , _networkRequest_chainRef = error "chainRef"
                  , _networkRequest_endpoint = error "endpoint"
                  , _networkRequest_signing = error "signing"
                  }
                publicMeta = PublicMeta
                  { _pmChainId = Pact.ChainId $ T.pack $ show $ unChainId chainId'
                  , _pmSender = sender
                  , _pmGasLimit = fromMaybe limit $ _signingRequest_gasLimit signingRequest
                  , _pmGasPrice = price
                  }
                nonce = _signingRequest_nonce signingRequest
            command <- buildCmd nonce publicMeta allKeys keySet networkRequest
            pure $ Right $ SigningResponse
              { _signingResponse_body = command
              , _signingResponse_chainId = chainId'
              }
          doSign _ _ = Nothing
      signed <- performEvent $ attachWithMaybe doSign results sign
      let done = leftmost [signed, Left "Cancelled" <$ (cancel <> onClose)]
      performEvent_ $ liftJSM . _appCfg_signingResponse appCfg <$> done
      pure (mempty, void done)
