{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
-- | Dialog for displaying account information required for receiving transfers
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.Receive
  ( uiReceiveModal
  ) where

import Control.Applicative (liftA2)
import Control.Lens ((^.), (<>~), (^?), to)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Text (Text)

import Data.Text (Text)

import Reflex
import Reflex.Dom

import Frontend.Crypto.Class (HasCrypto)

import Frontend.Foundation
import Frontend.TxBuilder
import Frontend.Network
import Frontend.Log

import Frontend.UI.Dialogs.Receive.Legacy
import Frontend.UI.DeploymentSettings (transactionDisplayNetwork)

import Frontend.UI.Modal
import Frontend.UI.Widgets
import Frontend.UI.Widgets.Helpers (dialogSectionHeading)
import Frontend.Wallet

uiReceiveModal
  :: ( MonadWidget t m
     , Monoid mConf
     , HasNetwork model t
     , HasNetworkCfg mConf t
     , Flattenable mConf t
     , HasCrypto key m
     , HasCrypto key (Performable m)
     , HasLogger model t
     , HasTransactionLogger m
     )
  => Text
  -> model
  -> AccountName
  -> AccountDetails
  -> Maybe ChainId
  -> Event t ()
  -> m (mConf, Event t ())
uiReceiveModal modalTitle model account details mchain _onClose = do
  onClose <- modalHeader $ text modalTitle
  (conf, closes) <- fmap splitDynPure $ workflow $
    uiReceiveModal0 model account details mchain onClose
  mConf <- flatten =<< tagOnPostBuild conf
  let close = switch $ current closes
  pure (mConf, close)

uiReceiveModal0
  :: ( MonadWidget t m
     , Monoid mConf
     , HasNetwork model t
     , HasNetworkCfg mConf t
     , HasCrypto key (Performable m)
     , HasCrypto key m
     , HasLogger model t
     , HasTransactionLogger m
     )
  => model
  -> AccountName
  -> AccountDetails
  -> Maybe ChainId
  -> Event t ()
  -> Workflow t m (mConf, Event t ())
uiReceiveModal0 model account details mchain onClose = Workflow $ do
  let
    netInfo = getNetworkInfoTriple $ model ^. network

    displayText lbl v cls =
      let
        attrFn cfg = uiInputElement $ cfg
          & initialAttributes <>~ ("disabled" =: "true" <> "class" =: (" " <> cls))
      in
        mkLabeledInputView True lbl attrFn $ pure v

  (showingAddr, chain, (conf, ttl, gaslimit, transferInfo)) <- divClass "modal__main receive" $ do
    rec
      showingTxBuilder <- toggle True $ onAddrClick <> onReceiClick

      dialogSectionHeading mempty "Destination"
      chain <- divClass "group" $ do
        -- Network
        transactionDisplayNetwork model
        -- Chain id
        case mchain of
          Nothing -> value <$> userChainIdSelect model
          Just cid -> (pure $ Just cid) <$ displayText "Chain ID" (_chainId cid) mempty

      (onAddrClick, ((), ())) <- controlledAccordionItem showingTxBuilder mempty
        (accordionHeaderBtn "Option 1: Copy and share Tx Builder") $ do
        dyn_ $ ffor chain $ divClass "group" . \case
          Nothing -> text "Please select a chain"
          Just cid -> uiDisplayTxBuilderWithCopy True
            $ TxBuilder account cid
            $ details ^? accountDetails_guard . _AccountGuard_KeySet . to (uncurry toPactKeyset)

      (onReceiClick, results) <- controlledAccordionItem (not <$> showingTxBuilder) mempty
        (accordionHeaderBtn "Option 2: Transfer from non-Chainweaver Account") $ do
        legacyReceive <- uiReceiveFromLegacy model
        pure ( _receiveFromLegacy_conf legacyReceive
             , _receiveFromLegacy_ttl legacyReceive
             , _receiveFromLegacy_gasLimit legacyReceive
             , _receiveFromLegacy_transferInfo legacyReceive
             )

    pure (showingTxBuilder, chain, snd results)

  let needsSender = liftA2 (&&) (isNothing <$> transferInfo) (not <$> showingAddr)
      isDisabled = liftA2 (||) (isNothing <$> chain) needsSender

  doneNext <- modalFooter $ uiButtonDyn
    (def
     & uiButtonCfg_class <>~ "button_type_confirm"
     & uiButtonCfg_disabled .~ isDisabled
    )
    $ dynText (bool "Submit Transfer" "Close" <$> showingAddr)

  let
    done = gate (current showingAddr) doneNext
    deploy = gate (not <$> current showingAddr) doneNext

    submit = flip push deploy $ \() -> runMaybeT $ do
      c <- MaybeT $ sample $ current chain
      t <- lift $ sample $ current ttl
      g <- lift $ sample $ current gaslimit
      ni <- MaybeT $ sample $ current netInfo
      ti <- MaybeT $ sample $ current transferInfo
      pure $ receiveFromLegacySubmitTransfer model onClose account c t g ni ti

  pure ( (conf, onClose <> done)
       , submit
       )
