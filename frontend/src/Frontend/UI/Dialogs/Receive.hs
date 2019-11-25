{-# LANGUAGE RecursiveDo #-}
-- | Dialog for displaying account information required for receiving transfers
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.Receive
  ( uiReceiveModal
  ) where

import Control.Lens ((^.), (<>~))
import Control.Monad (void)
import Data.Text (Text)

import Reflex
import Reflex.Dom

import Pact.Types.ChainId (ChainId(..))

import Language.Javascript.JSaddle.Types (MonadJSM)

import Frontend.Foundation
import Frontend.KadenaAddress
import Frontend.Network
import Frontend.UI.Dialogs.NetworkEdit
import Frontend.UI.DeploymentSettings (userChainIdSelect)
import Frontend.UI.Modal
import Frontend.UI.Widgets
import Frontend.Wallet

uiDisplayAddress
  :: ( MonadJSM (Performable m)
     , DomBuilder t m
     , PostBuild t m
     , PerformEvent t m
     )
  => Text
  -> m ()
uiDisplayAddress address = do
  elClass "h2" "heading heading_type_h2" $ text "Kadena Address"
  divClass "group" $ do
    -- Kadena Address
    divClass "segment segment_type_tertiary labeled-input account-details__kadena-address-wrapper" $ do
      void $ uiInputElement $ def
        & initialAttributes <>~ ("disabled" =: "true" <> "class" =: "account-details__kadena-address labeled-input__input")
        & inputElementConfig_initialValue .~ address
      void $ copyButton (def
        & uiButtonCfg_class .~ constDyn "account-details__copy-btn button_type_confirm"
        & uiButtonCfg_title .~ constDyn (Just "Copy")
        ) $ pure address
      pure ()

uiReceiveModal
  :: ( MonadWidget t m
     , Monoid mConf
     , HasNetwork model t
     , HasWallet model key t
     )
  => model
  -> Account key
  -> Event t ()
  -> m (mConf, Event t ())
uiReceiveModal model account _onClose = do
  let address = textKadenaAddress $ accountToKadenaAddress account
  close <- modalHeader $ text "Receive"

  let displayText lbl v cls =
        let
          attrFn cfg = uiInputElement $ cfg
            & initialAttributes <>~ ("disabled" =: "true" <> "class" =: (" " <> cls))
        in
          mkLabeledInputView True lbl attrFn $ pure v

  divClass "modal__main account-details" $ do
    elClass "h2" "heading heading_type_h2" $ text "Destination"
    divClass "group" $ do
      -- Network
      void $ mkLabeledClsInput True "Network" $ \_ -> do
        stat <- queryNetworkStatus (model ^. network_networks) $ pure $ _account_network account
        uiNetworkStatus "signal__left-floated" stat
        text $ textNetworkName $ _account_network account
      -- Chain id
      _ <- displayText "Chain ID" (_chainId $ _account_chainId account) "account-details__chain-id"
      pure ()

    rec
      showingKadenaAddress <- holdDyn True $
        not <$> current showingKadenaAddress <@ onAddrClick <> onReceiClick

      (onAddrClick, _) <- controlledAccordionItem showingKadenaAddress mempty
        (text "To this address")
        $ uiDisplayAddress address

      (onReceiClick, _) <- controlledAccordionItem (not <$> showingKadenaAddress) mempty
        (text "From this key")
        $ uiReceiveFromLegacyAccount model account

    pure ()

  done <- modalFooter $ confirmButton def "Close"

  pure (mempty, close <> done)

uiReceiveFromLegacyAccount
  :: ( MonadWidget t m
     , HasWallet model key t
     , HasNetwork model t
     )
  => model
  -> Account key
  -> m ()
uiReceiveFromLegacyAccount model account = do
  elClass "h2" "heading heading_type_h2" $ text "Payer Details"
  divClass "group" $ do

    let dPrivKeyCheck = constDyn Right

    mAccountName <- uiAccountNameInput (model ^. wallet)
    mPrivKey <- validatedInput "account-details__private-key" dPrivKeyCheck $
      mkLabeledInput True "Private Key" (\c -> uiInputElement c) def
    chain <- divClass "account-details__receive-from-chain" $ userChainIdSelect model

    pure ()
