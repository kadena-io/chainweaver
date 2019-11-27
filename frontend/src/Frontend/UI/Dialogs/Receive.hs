{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Dialog for displaying account information required for receiving transfers
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.Receive
  ( uiReceiveModal
  ) where

import Debug.Trace (traceShowM)
import Control.Applicative (liftA2,liftA3)
import Control.Lens ((^.), (<>~), (^?), _1, _3)
import Control.Monad (void, (<=<))
import Control.Error (hush, headMay)

import Control.Monad.Trans.Maybe (runMaybeT)

import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import Data.Either (isLeft,rights)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Decimal (Decimal)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HM
import Reflex
import Reflex.Dom

import Kadena.SigningApi (DappCap (..))
import Pact.Types.Capability (SigCapability (..))
import Pact.Types.Term (QualifiedName (..), KeySet (..), Name (..), BareName (..))
import Pact.Types.ChainId (ChainId(..))
import Pact.Types.ChainMeta (PublicMeta (..), TTLSeconds)
import Pact.Types.PactValue (PactValue (..))
import Pact.Types.Exp (Literal (LString, LDecimal))
import Pact.Types.Command (Command)
import Pact.Types.Runtime (GasLimit)

import Language.Javascript.JSaddle.Types (MonadJSM)

import Frontend.Crypto.Class (HasCrypto, cryptoGenPubKeyFromPrivate)
import Frontend.Crypto.Ed25519 (parsePrivateKey, keyToText)
import Common.Wallet (parsePublicKey,toPactPublicKey, decodeBase16M)

import Frontend.Foundation
import Frontend.KadenaAddress
import Frontend.Network
import Frontend.JsonData
import Frontend.UI.Dialogs.NetworkEdit

import Frontend.UI.Dialogs.DeployConfirmation (CanSubmitTransaction,
                                               TransactionSubmitFeedback (..),
                                               submitTransactionWithFeedback
                                              )

import Frontend.UI.DeploymentSettings (DeploymentSettingsView (..),
                                       DeploymentSettingsConfig (..),
                                       uiMetaData,
                                       uiDeployPreview,
                                       buildDeployTabs,
                                       buildDeployTabFooterControls,
                                       buildDeploymentSettingsResult,
                                       defaultTabViewProgressButtonLabel,
                                       userChainIdSelect,
                                       defaultGASCapability,
                                       predefinedChainIdSelect,
                                       uiSenderFixed
                                      )
import Frontend.UI.Modal
import Frontend.UI.Widgets
import Frontend.Wallet

data LegacyTransferInfo key = LegacyTransferInfo
  { _legacyTransferInfo_account :: AccountName
  , _legacyTransferInfo_chainId :: ChainId
  , _legacyTransferInfo_amount :: Decimal
  , _legacyTransferInfo_keyPair :: (key, PublicKey)
  }

data ReceiveBuildCommandInfo key = ReceiveBuildCommandInfo
  { _receiveBuildCommandInfo_code :: Text
  , _receiveBuildCommandInfo_transferInfo :: LegacyTransferInfo key
  , _receiveBuildCommandInfo_command :: Command Text
  , _receiveBuildCommandInfo_signingPairs :: [KeyPair key]
  , _receiveBuildCommandInfo_publicMeta :: PublicMeta
  , _receiveBuildCommandInfo_capabilities :: [DappCap]
  , _receiveBuildCommandInfo_assignedCaps :: Map PublicKey [SigCapability]
  , _receiveBuildCommandInfo_payload :: Aeson.Object
  }

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

-- uiReceiveModal
--   :: ( MonadWidget t m
--      , Monoid mConf
--      , HasNetwork model t
--      , HasWallet model key t
--      )
--   => model
--   -> Account key
--   -> Event t ()
--   -> m (mConf, Event t ())
-- uiReceiveModal model account _onClose = do
--   let address = textKadenaAddress $ accountToKadenaAddress account
--   close <- modalHeader $ text "Receive"

--   let displayText lbl v cls =
--         let
--           attrFn cfg = uiInputElement $ cfg
--             & initialAttributes <>~ ("disabled" =: "true" <> "class" =: (" " <> cls))
--         in
--           mkLabeledInputView True lbl attrFn $ pure v

--   divClass "modal__main account-details" $ do
--     elClass "h2" "heading heading_type_h2" $ text "Destination"
--     divClass "group" $ do
--       -- Network
--       void $ mkLabeledClsInput True "Network" $ \_ -> do
--         stat <- queryNetworkStatus (model ^. network_networks) $ pure $ _account_network account
--         uiNetworkStatus "signal__left-floated" stat
--         text $ textNetworkName $ _account_network account
--       -- Chain id
--       _ <- displayText "Chain ID" (_chainId $ _account_chainId account) "account-details__chain-id"
--       pure ()

--     rec
--       showingKadenaAddress <- holdDyn True $
--         not <$> current showingKadenaAddress <@ onAddrClick <> onReceiClick

--       (onAddrClick, _) <- controlledAccordionItem showingKadenaAddress mempty
--         (text "To this address")
--         $ uiDisplayAddress address

--       (onReceiClick, _) <- controlledAccordionItem (not <$> showingKadenaAddress) mempty
--         (text "From this key")
--         $ uiReceiveFromLegacyAccount model account

--     pure ()

--   done <- modalFooter $ confirmButton def "Close"

--   pure (mempty, close <> done)

uiReceiveFromLegacyAccount
  :: ( MonadWidget t m
     , HasWallet model key t
     , HasNetwork model t
     , HasCrypto key (Performable m)
     )
  => model
  -> Account key
  -> m (Dynamic t (Maybe (LegacyTransferInfo key)))
uiReceiveFromLegacyAccount model account = do
  divClass "group" $ do
    mAccountName <- uiAccountNameInput (model ^. wallet)

    rec
      onKeyPair <- divClass "account-details__private-key" $
        _inputElement_input <$> mkLabeledInput True "Private Key" uiInputElement def

      onDerivePair <- performEvent $ deriveKeyPair <$> onKeyPair
      keyPair <- holdDyn Nothing onDerivePair

    chain <- divClass "account-details__receive-from-chain" $ userChainIdSelect model

    amount <- fst . snd <$> mkLabeledInput True "Amount"
      (uiRealWithPrecisionInputElement maxCoinPrecision id) def

    pure $ (\macc mc mamnt mkeypair ->
              LegacyTransferInfo <$> macc <*> mc <*> mamnt <*> mkeypair
           )
      <$> mAccountName
      <*> chain
      <*> amount
      <*> keyPair
  where
    deriveKeyPair :: (HasCrypto key m, MonadJSM m) => Text -> m (Maybe (key, PublicKey))
    deriveKeyPair inp = do
      let x = decodeBase16M $ TE.encodeUtf8 inp
      case x of
        Nothing -> pure Nothing
        Just i -> Just <$> cryptoGenPubKeyFromPrivate i

uiReceiveModal
  :: ( MonadWidget t m
     , Monoid mConf
     , HasNetwork model t
     , HasNetworkCfg mConf t
     , HasJsonDataCfg mConf t
     , HasJsonData model t
     , HasWallet model key t
     , Flattenable mConf t
     , HasCrypto key m
     , HasCrypto key (Performable m)
     )
  => model
  -> Account key
  -> Event t ()
  -> m (mConf, Event t ())
uiReceiveModal model account _onClose = do
  onClose <- modalHeader $ text "Receive"
  (conf, closes) <- fmap splitDynPure $ workflow $ uiReceiveModal0 model account onClose
  mConf <- flatten =<< tagOnPostBuild conf
  let close = switch $ current closes
  pure (mConf, close)

uiReceiveModal0
  :: ( MonadWidget t m
     , Monoid mConf
     , Flattenable mConf t
     , HasNetwork model t
     , HasNetworkCfg mConf t
     , HasJsonDataCfg mConf t
     , HasJsonData model t
     , HasWallet model key t
     , HasCrypto key (Performable m)
     , HasCrypto key m
     )
  => model
  -> Account key
  -> Event t ()
  -> Workflow t m (mConf, Event t ())
uiReceiveModal0 model account onClose = Workflow $ do
  let address = textKadenaAddress $ accountToKadenaAddress account

  let displayText lbl v cls =
        let
          attrFn cfg = uiInputElement $ cfg
            & initialAttributes <>~ ("disabled" =: "true" <> "class" =: (" " <> cls))
        in
          mkLabeledInputView True lbl attrFn $ pure v

  (showingAddr, onTransferStart) <- divClass "modal__main account-details" $ do
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
        (text "To this address") $ uiDisplayAddress address

      (onReceiClick, onStartTransfer) <- controlledAccordionItem (not <$> showingKadenaAddress) mempty
        (text "From this key") $ button "Start Transfer"

    pure (showingKadenaAddress, snd onStartTransfer)

  let isDisabled = (not <$> showingAddr)

  doneNext <- modalFooter $ uiButtonDyn
    (def
     & uiButtonCfg_class <>~ "button_type_confirm"
     & uiButtonCfg_disabled .~ isDisabled
    )
    $ dynText (bool "Next" "Close" <$> showingAddr)

  let
    done = gate (current showingAddr) doneNext

  pure ( (mempty, onClose <> done)
       , uiReceiveDeployLegacyTransfer onClose model account <$ onTransferStart
       )

uiReceiveDeployLegacyTransfer
  :: forall t m model mConf key.
     ( MonadWidget t m
     , HasNetwork model t
     , HasNetworkCfg mConf t
     , HasWallet model key t
     , HasJsonDataCfg mConf t
     , Flattenable mConf t
     , Monoid mConf
     , HasJsonData model t
     , HasCrypto key m
     , HasCrypto key (Performable m)
     )
  => Event t ()
  -> model
  -> Account key
  -> Workflow t m (mConf, Event t ())
uiReceiveDeployLegacyTransfer onClose model account = Workflow $ do
  let
    accounts = model ^. wallet_accounts
    chain = _account_chainId account

    includePreviewTab = True
    customConfigTab = Nothing

    netInfo = do
      nodes <- model ^. network_selectedNodes
      meta <- model ^. network_meta
      let networkId = hush . mkNetworkName . nodeVersion <=< headMay $ rights nodes
      pure $ (nodes, meta, ) <$> networkId

  rec
    (curSelection, done, _) <- buildDeployTabs customConfigTab includePreviewTab controls

    (conf, mTransferInfo) <- elClass "div" "modal__main transaction_details" $ do
      (cfg, ttl, gaslim, mTfrInfo) <- tabPane mempty curSelection DeploymentSettingsView_Cfg $ do
        elClass "h2" "heading heading_type_h2" $ text "Transfer Details"
        transferInfo <- divClass "group" $ uiReceiveFromLegacyAccount model account
        elClass "h2" "heading heading_type_h2" $ text "Transaction Settings"
        (conf, ttl, gaslim) <- divClass "group" $ uiMetaData model Nothing Nothing
        pure (conf, ttl, gaslim, transferInfo)

      _ <- tabPane mempty curSelection DeploymentSettingsView_Keys $
        text "erm..."

      _ <- tabPane mempty curSelection DeploymentSettingsView_Preview $ do

        dyn_ $ receiveFromLegacyPreview model chain account
          <$> (model ^. wallet_accounts)
          <*> ttl
          <*> gaslim
          <*> netInfo
          <*> mTransferInfo

      pure (cfg, mTfrInfo)

    let preventProgress = isNothing <$> mTransferInfo

    controls <- modalFooter $ buildDeployTabFooterControls
      customConfigTab
      includePreviewTab
      curSelection
      defaultTabViewProgressButtonLabel
      preventProgress

  pure
    ( (conf, onClose)
    , attachWithMaybe
        (liftA2 $ receiveFromLegacySubmit account chain)
        (current netInfo)
        (current mTransferInfo <@ done)
    )
receiveFromLegacyPreview
  :: ( MonadWidget t m
     , HasCrypto key m
     , HasCrypto key (Performable m)
     , HasNetwork model t
     )
  => model
  -> ChainId
  -> Account key
  -> Accounts key
  -> TTLSeconds
  -> GasLimit
  -> Maybe ([Either a NodeInfo], PublicMeta, NetworkName)
  -> Maybe (LegacyTransferInfo key)
  -> m ()
receiveFromLegacyPreview _ _ _ _ _ _ Nothing _ =
  text "Insufficient information provided for Preview."
receiveFromLegacyPreview _ _ _ _ _ _ _ Nothing =
  text "Insufficient information provided for Preview."
receiveFromLegacyPreview model chainId account accounts ttl gasLimit (Just netInfo) (Just transferInfo) = do
  cmdInfo <- receiveBuildCommand account chainId netInfo transferInfo

  let
    dat = _receiveBuildCommandInfo_payload cmdInfo
    code = _receiveBuildCommandInfo_code cmdInfo
    caps = _receiveBuildCommandInfo_capabilities cmdInfo
    pm = _receiveBuildCommandInfo_publicMeta cmdInfo
    signers = _keyPair_publicKey <$> _receiveBuildCommandInfo_signingPairs cmdInfo

    previewCaps = Map.mapKeys (AccountName . keyToText) $ _receiveBuildCommandInfo_assignedCaps cmdInfo

    settings = DeploymentSettingsConfig
      { _deploymentSettingsConfig_userTab = Nothing
      , _deploymentSettingsConfig_userSections = []
      , _deploymentSettingsConfig_chainId = predefinedChainIdSelect chainId
      , _deploymentSettingsConfig_sender = \_ _ -> uiSenderFixed (_account_name account)
      , _deploymentSettingsConfig_data = Just dat
      , _deploymentSettingsConfig_code = constDyn code
      , _deploymentSettingsConfig_nonce = Nothing
      , _deploymentSettingsConfig_ttl = Just ttl
      , _deploymentSettingsConfig_gasLimit = Just gasLimit
      , _deploymentSettingsConfig_caps = Just caps
      , _deploymentSettingsConfig_extraSigners = signers
      , _deploymentSettingsConfig_includePreviewTab = True
      }

  uiDeployPreview
    model
    settings
    accounts
    gasLimit
    ttl
    code
    pm
    previewCaps
    (Right dat)
    (netInfo ^? _3)
    (Just chainId)
    (Just $ _account_name account)


receiveFromLegacySubmit
  :: ( Monoid mConf
     , CanSubmitTransaction t m
     , HasCrypto key m
     )
  => Account key
  -> ChainId
  -> ([Either a NodeInfo], PublicMeta, NetworkName)
  -> LegacyTransferInfo key
  -> Workflow t m (mConf, Event t ())
receiveFromLegacySubmit account chainId netInfo transferInfo = Workflow $ do
  recInfo <- receiveBuildCommand account chainId netInfo transferInfo
  txnSubFeedback <- elClass "div" "modal__main transaction_details" $
    submitTransactionWithFeedback (_receiveBuildCommandInfo_command recInfo) chainId (netInfo ^. _1)

  let isDisabled = maybe True isLeft <$> _transactionSubmitFeedback_message txnSubFeedback

  done <- modalFooter $ uiButtonDyn
    (def & uiButtonCfg_class .~ "button_type_confirm" & uiButtonCfg_disabled .~ isDisabled)
    (text "Done")

  pure
    ( (mempty, done)
    , never
    )

receiveBuildCommand
  :: ( MonadJSM m
     , HasCrypto key m
     )
  => Account key
  -> ChainId
  -> ([Either a NodeInfo], PublicMeta, NetworkName)
  -> LegacyTransferInfo key
  -> m (ReceiveBuildCommandInfo key)
receiveBuildCommand account chainId (nodeInfos, publicMeta, networkId) transferInfo = do
  let
    chain = _legacyTransferInfo_chainId transferInfo
    sender = _legacyTransferInfo_account transferInfo
    senderKeys = _legacyTransferInfo_keyPair transferInfo
    amount = _legacyTransferInfo_amount transferInfo

    code = T.unwords $
      [ "(coin." <> accountCreatedYesNo account "transfer" "transfer-create"
      , tshow $ unAccountName $ sender
      , tshow $ unAccountName $ _account_name account
      , accountCreatedYesNo account mempty "(read-keyset 'key)"
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
        , PLiteral $ LString $ unAccountName $ _account_name account
        , PLiteral $ LDecimal amount
        ]
      }

    transferCapability = DappCap
      { _dappCap_role = "TRANFER"
      , _dappCap_description = T.empty
      , _dappCap_cap = transferSigCap
      }

    senderPubKey = toPactPublicKey $ snd senderKeys
    dat = HM.singleton "key" $ Aeson.toJSON $ KeySet [senderPubKey] (Name $ BareName "keys-all" def)

    signingPairs =
      [ KeyPair (snd senderKeys) (Just $ fst senderKeys)
      , _account_key account
      ]

    caps = [transferCapability, defaultGASCapability]

    pkCaps = Map.singleton (snd senderKeys) [transferSigCap, _dappCap_cap defaultGASCapability]

    pm = publicMeta
      { _pmChainId = chain
      , _pmSender = unAccountName sender
      }

  cmd <- buildCmd Nothing networkId pm signingPairs [] code dat pkCaps

  traceShowM cmd
  pure $ ReceiveBuildCommandInfo code transferInfo cmd signingPairs pm caps pkCaps dat
