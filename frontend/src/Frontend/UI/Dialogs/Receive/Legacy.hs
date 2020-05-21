{-# LANGUAGE TemplateHaskell #-}
module Frontend.UI.Dialogs.Receive.Legacy
  ( ReceiveFromLegacy (..)
  , HasReceiveFromLegacy (..)
  , NonBIP32TransferInfo (..)
  , HasNonBIP32TransferInfo (..)
  , uiReceiveFromLegacy
  , receiveFromLegacySubmitTransfer
  , receiveFromLegacySubmitTransferCreate
  ) where

import Control.Applicative (liftA3)
import Control.Lens (view, _1, _2, _3, (^.))
import Control.Error (hush)
import Data.Decimal
import Data.Either (isLeft)
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.HashMap.Lazy as HM

import Language.Javascript.JSaddle.Types (MonadJSM)

import Reflex
import Reflex.Dom.Core

import Kadena.SigningApi (DappCap (..))
import Pact.Types.PactValue (PactValue (..))
import Pact.Types.Exp (Literal (LString, LDecimal))
import Pact.Types.Capability (SigCapability (..))
import Pact.Types.Term (QualifiedName (..))
import Pact.Types.ChainMeta (PublicMeta (..), TTLSeconds)
import Pact.Types.Runtime (GasLimit, GasPrice (..))
import qualified Pact.Types.Scheme as PactScheme

import Common.Foundation

import Frontend.UI.Widgets
import Frontend.UI.Modal
import Frontend.UI.DeploymentSettings (uiMetaData, defaultGASCapability)
import Frontend.UI.Widgets.Helpers (dialogSectionHeading)
import Frontend.UI.Dialogs.DeployConfirmation (CanSubmitTransaction, TransactionSubmitFeedback (..), submitTransactionWithFeedback)

import Frontend.Crypto.Class (PactKey (..), HasCrypto, cryptoGenPubKeyFromPrivate)
import Frontend.Wallet
import Frontend.Log
import Frontend.Network

data NonBIP32TransferInfo = NonBIP32TransferInfo
  { _legacyTransferInfo_account :: AccountName
  , _legacyTransferInfo_amount :: Decimal
  , _legacyTransferInfo_pactKey :: PactKey
  }
makePactLenses ''NonBIP32TransferInfo

data ReceiveFromLegacy t conf = ReceiveFromLegacy
  { _receiveFromLegacy_conf :: conf
  , _receiveFromLegacy_ttl :: Dynamic t TTLSeconds
  , _receiveFromLegacy_gasLimit :: Dynamic t GasLimit
  , _receiveFromLegacy_gasPrice :: Dynamic t GasPrice
  , _receiveFromLegacy_transferInfo :: Dynamic t (Maybe NonBIP32TransferInfo)
  }

makePactLenses ''ReceiveFromLegacy

uiReceiveFromLegacyAccount
  :: ( MonadWidget t m
     , HasCrypto key (Performable m)
     )
  => m (Dynamic t (Maybe NonBIP32TransferInfo))
uiReceiveFromLegacyAccount = do
  (_, mAccountName) <- uiAccountNameInput "Account Name" True Nothing never noValidation
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

  amount <- fmap hush . view _2 <$> mkLabeledInput True "Amount" uiAmountInput def

  pure $ (liftA3 . liftA3) NonBIP32TransferInfo mAccountName amount keyPair
  where
    deriveKeyPair :: (HasCrypto key m, MonadJSM m) => Text -> m (Either Text PactKey)
    deriveKeyPair = fmap (first T.pack) . cryptoGenPubKeyFromPrivate PactScheme.ED25519

uiReceiveFromLegacy
  :: ( MonadWidget t m
     , HasCrypto key (Performable m)
     , HasNetwork model t
     , HasNetworkCfg mConf t
     , Monoid mConf
     )
  => model
  -> m (ReceiveFromLegacy t mConf)
uiReceiveFromLegacy model = do
  dialogSectionHeading mempty "Sender Details"
  transferInfo0 <- divClass "group" uiReceiveFromLegacyAccount

  dialogSectionHeading mempty "Transaction Settings"
  (conf0, ttl0, gaslimit0, gasPrice) <- divClass "group" $ uiMetaData model Nothing Nothing

  pure $ ReceiveFromLegacy
    conf0
    ttl0
    gaslimit0
    gasPrice
    transferInfo0

receiveFromLegacySubmitTransfer
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
receiveFromLegacySubmitTransfer m onClose account chain ttl gasLimit netInfo transferInfo =
  let
    sender = _legacyTransferInfo_account transferInfo
    amount = _legacyTransferInfo_amount transferInfo

    code = T.unwords $
      [ "(coin.transfer"
      , tshow $ unAccountName $ sender
      , tshow $ unAccountName account
      , tshow amount
      , ")"
      ]
  in
    receiveFromLegacySubmit m onClose account chain ttl gasLimit netInfo transferInfo code mempty

receiveFromLegacySubmitTransferCreate
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
  -> AccountGuard
  -> Workflow t m (mConf, Event t ())
receiveFromLegacySubmitTransferCreate m onClose account chain ttl gasLimit netInfo transferInfo keyset =
  let
    sender = _legacyTransferInfo_account transferInfo
    amount = _legacyTransferInfo_amount transferInfo
    tempkeyset = "tempkeyset"

    code = T.unwords $
      [ "(coin.transfer-create"
      , tshow $ unAccountName $ sender
      , tshow $ unAccountName account
      , "(read-keyset \""<> tempkeyset <> "\")"
      , tshow $ addDecimalPoint amount
      , ")"
      ]

    payload = HM.singleton tempkeyset $ Aeson.toJSON keyset
  in
    receiveFromLegacySubmit m onClose account chain ttl gasLimit netInfo transferInfo code payload

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
  -> Text
  -> HM.HashMap Text Aeson.Value
  -> Workflow t m (mConf, Event t ())
receiveFromLegacySubmit m onClose account chain ttl gasLimit netInfo transferInfo code payload = Workflow $ do
  let
    sender = _legacyTransferInfo_account transferInfo
    senderKey = _legacyTransferInfo_pactKey transferInfo
    senderPubKey = _pactKey_publicKey senderKey
    amount = _legacyTransferInfo_amount transferInfo

    transferSigCap = SigCapability
      { _scName = QualifiedName
        { _qnQual = "coin"
        , _qnName = "TRANSFER"
        , _qnInfo = def
        }
      , _scArgs =
        [ PLiteral $ LString $ unAccountName sender
        , PLiteral $ LString $ unAccountName account
        , PLiteral $ LDecimal amount
        ]
      }

    pkCaps = Map.singleton senderPubKey
      [ _dappCap_cap defaultGASCapability
      , transferSigCap
      ]

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
    payload
    pkCaps

  txnSubFeedback <- elClass "div" "modal__main transaction_details" $
    submitTransactionWithFeedback m cmd sender chain (netInfo ^. _1)

  let isDisabled = maybe True isLeft <$> _transactionSubmitFeedback_message txnSubFeedback

  done <- modalFooter $ uiButtonDyn
    (def & uiButtonCfg_class .~ "button_type_confirm" & uiButtonCfg_disabled .~ isDisabled)
    (text "Done")

  pure
    ( (mempty, done <> onClose)
    , never
    )
