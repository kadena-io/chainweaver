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

import Control.Applicative (liftA2, liftA3)
import Control.Lens ((^.), (^?), (<>~), _1, _2, _3, view, to)
import Control.Monad ((<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Error (hush, headMay)
import Data.Bifunctor (first)
import Data.Either (isLeft,rights)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map

import Reflex
import Reflex.Dom

import Kadena.SigningApi (DappCap (..))
import Pact.Types.Capability (SigCapability (..))
import Pact.Types.Term (QualifiedName (..))
import Pact.Types.ChainMeta (PublicMeta (..), TTLSeconds)
import Pact.Types.PactValue (PactValue (..))
import Pact.Types.Exp (Literal (LString, LDecimal))
import Pact.Types.Runtime (GasLimit, GasPrice (..))
import Pact.Parse (ParsedDecimal (..))
import qualified Pact.Types.Scheme as PactScheme

import Language.Javascript.JSaddle.Types (MonadJSM)

import Frontend.Crypto.Class (PactKey (..), HasCrypto, cryptoGenPubKeyFromPrivate)

import Frontend.Foundation
import Frontend.TxBuilder
import Frontend.Network
import Frontend.Log

import Frontend.UI.Dialogs.DeployConfirmation (CanSubmitTransaction, TransactionSubmitFeedback (..), submitTransactionWithFeedback)

import Frontend.UI.DeploymentSettings (uiMetaData, defaultGASCapability, transactionDisplayNetwork)

import Frontend.UI.Modal
import Frontend.UI.Widgets
import Frontend.UI.Widgets.Helpers (dialogSectionHeading)
import Frontend.Wallet

data NonBIP32TransferInfo = NonBIP32TransferInfo
  { _legacyTransferInfo_account :: AccountName
  , _legacyTransferInfo_amount :: GasPrice
  , _legacyTransferInfo_pactKey :: PactKey
  }

uiReceiveFromLegacyAccount
  :: ( MonadWidget t m
     , HasCrypto key (Performable m)
     )
  => m (Dynamic t (Maybe NonBIP32TransferInfo))
uiReceiveFromLegacyAccount = do
  (_, mAccountName) <- uiAccountNameInput True Nothing noValidation

  let
    onDeriveKey (_, onKey) =
      pure $ ffor onKey $ \case
        Left e -> PopoverState_Error $ T.takeWhile (/= ':') e
        Right _ -> PopoverState_Disabled

    uiInputPrivKey cfg = do
      inputE <- uiInputElement cfg
      onKey <- performEvent $ deriveKeyPair <$> _inputElement_input inputE
      pure (inputE, onKey)

  (_, onKeyPair) <- mkLabeledInput True "Private Key"
    ( uiInputWithPopover
        uiInputPrivKey
        (_inputElement_raw . fst)
        onDeriveKey
    ) def

  keyPair <- holdDyn Nothing $ hush <$> onKeyPair

  amount <- view _2 <$> mkLabeledInput True "Amount" (uiGasPriceInputField never) def

  pure $ (liftA3 . liftA3) NonBIP32TransferInfo mAccountName amount keyPair
  where
    deriveKeyPair :: (HasCrypto key m, MonadJSM m) => Text -> m (Either Text PactKey)
    deriveKeyPair = fmap (first T.pack) . cryptoGenPubKeyFromPrivate PactScheme.ED25519

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
  => Maybe Text
  -> model
  -> AccountName
  -> AccountDetails
  -> Maybe ChainId
  -> Event t ()
  -> m (mConf, Event t ())
uiReceiveModal titleOverride model account details mchain _onClose = do
  onClose <- modalHeader $ text $ fromMaybe "Receive" titleOverride
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
    netInfo = do
      nodes <- model ^. network_selectedNodes
      meta <- model ^. network_meta
      let networkId = hush . mkNetworkName . nodeVersion <=< headMay $ rights nodes
      pure $ (nodes, meta, ) <$> networkId

    displayText lbl v cls =
      let
        attrFn cfg = uiInputElement $ cfg
          & initialAttributes <>~ ("disabled" =: "true" <> "class" =: (" " <> cls))
      in
        mkLabeledInputView True lbl attrFn $ pure v

  (showingAddr, chain, (conf, ttl, gaslimit, _, transferInfo)) <- divClass "modal__main receive" $ do
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
        dialogSectionHeading mempty "Sender Details"
        transferInfo0 <- divClass "group" uiReceiveFromLegacyAccount
        dialogSectionHeading mempty "Transaction Settings"
        (conf0, ttl0, gaslimit0, gasPrice) <- divClass "group" $ uiMetaData model Nothing Nothing
        pure (conf0, ttl0, gaslimit0, gasPrice, transferInfo0)

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
      pure $ receiveFromLegacySubmit model onClose account c t g ni ti

  pure ( (conf, onClose <> done)
       , submit
       )

receiveFromLegacySubmit
  :: ( Monoid mConf
     , CanSubmitTransaction t m
     , HasCrypto key m
     , HasLogger model t
     , HasTransactionLogger m
     )
  => model
  -> Event t ()
  -> AccountName
  -> ChainId
  -> TTLSeconds
  -> GasLimit
  -> ([Either a NodeInfo], PublicMeta, NetworkName)
  -> NonBIP32TransferInfo
  -> Workflow t m (mConf, Event t ())
receiveFromLegacySubmit model onClose account chain ttl gasLimit netInfo transferInfo = Workflow $ do
  let
    sender = _legacyTransferInfo_account transferInfo
    senderKey = _legacyTransferInfo_pactKey transferInfo
    senderPubKey = _pactKey_publicKey senderKey
    amount = _legacyTransferInfo_amount transferInfo

    unpackGasPrice (GasPrice (ParsedDecimal d)) = d

    code = T.unwords $
      [ "(coin.transfer"
      , tshow $ unAccountName $ sender
      , tshow $ unAccountName account
      , tshow amount
      , ")"
      ]

    transferSigCap = SigCapability
      { _scName = QualifiedName
        { _qnQual = "coin"
        , _qnName = "TRANSFER"
        , _qnInfo = def
        }
      , _scArgs =
        [ PLiteral $ LString $ unAccountName sender
        , PLiteral $ LString $ unAccountName account
        , PLiteral $ LDecimal (unpackGasPrice amount)
        ]
      }

    dat = mempty

    pkCaps = Map.singleton senderPubKey [_dappCap_cap defaultGASCapability, transferSigCap]

    pm = (netInfo ^. _2)
      { _pmChainId = chain
      , _pmSender = unAccountName sender
      , _pmGasLimit = gasLimit
      , _pmTTL = ttl
      }

  cmd <- buildCmdWithPactKey
    senderKey
    Nothing
    (netInfo ^. _3)
    pm
    [KeyPair (_pactKey_publicKey senderKey) Nothing]
    []
    code
    dat
    pkCaps

  txnSubFeedback <- elClass "div" "modal__main transaction_details" $
    submitTransactionWithFeedback model cmd sender chain (netInfo ^. _1)

  let isDisabled = maybe True isLeft <$> _transactionSubmitFeedback_message txnSubFeedback

  done <- modalFooter $ uiButtonDyn
    (def & uiButtonCfg_class .~ "button_type_confirm" & uiButtonCfg_disabled .~ isDisabled)
    (text "Done")

  pure
    ( (mempty, done <> onClose)
    , never
    )
