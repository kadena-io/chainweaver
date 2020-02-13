{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Dialogs for sending money between accounts
-- Copyright   :  (C) 2019 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.Send
  ( uiSendModal
  ) where

import Control.Applicative (liftA2)
import Control.Error.Util (hush)
import Control.Lens hiding (failover)
import Control.Monad (guard, join, when, void, (<=<))
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import Data.Decimal (Decimal)
import Data.Either (isLeft, rights)
import Kadena.SigningApi
import Pact.Parse (ParsedDecimal (..))
import Pact.Types.Capability
import Pact.Types.ChainMeta
import Pact.Types.Exp
import Pact.Types.Names
import Pact.Types.PactValue
import Pact.Types.Runtime (GasPrice (..))
import Reflex
import Reflex.Dom
import Safe (headMay)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Common.Wallet
import Frontend.Crypto.Class (HasCrypto)
import Frontend.Foundation hiding (Arg)
import Frontend.TxBuilder
import Frontend.Network
import Frontend.Log
import Frontend.UI.DeploymentSettings
import Frontend.UI.Dialogs.DeployConfirmation (submitTransactionWithFeedback)
import Frontend.UI.Modal
import Frontend.UI.TabBar
import Frontend.UI.Widgets
import Frontend.UI.Widgets.Helpers (dialogSectionHeading)
import Frontend.Wallet

-- | A modal for handling sending coin
uiSendModal
  :: ( SendConstraints model mConf key t m
     , Flattenable mConf t
     , HasTransactionLogger m
     )
  => model -> (AccountName, ChainId, Account) -> Event t () -> m (mConf, Event t ())
uiSendModal model (name, chain, acc) _onCloseExternal = do
  (conf, closes) <- fmap splitDynPure $ workflow $ sendConfig model (InitialTransferData_Account (name, chain, acc))
  mConf <- flatten =<< tagOnPostBuild conf
  let close = switch $ current closes
  pure (mConf, close)

type SendConstraints model mConf key t m
  = ( Monoid mConf, HasNetwork model t, HasNetworkCfg mConf t, HasWallet model key t, HasWalletCfg mConf key t
    , MonadWidget t m, PostBuild t m, HasCrypto key m
    , HasCrypto key (Performable m)
    , HasLogger model t
    )

-- | This stuff was being passed around so much it became a new type
data SharedNetInfo a = SharedNetInfo
  { _sharedNetInfo_network :: NetworkName
  -- ^ The actual network name as reported by the node
  , _sharedNetInfo_selectedNetwork :: NetworkName
  -- ^ The network name according to the user
  , _sharedNetInfo_meta :: PublicMeta
  , _sharedNetInfo_nodes :: [a]
  }

data TransferData = TransferData
  { _transferData_fromAccount :: (AccountName, ChainId, Account)
  , _transferData_fromGasPayer :: (AccountName, Account)
  , _transferData_toTxBuilder :: TxBuilder
  , _transferData_amount :: Decimal
  }

data InitialTransferData
  = InitialTransferData_Account (AccountName, ChainId, Account)
  | InitialTransferData_Transfer TransferData

initialTransferDataCata :: ((AccountName, ChainId, Account) -> b) -> (TransferData -> b) -> InitialTransferData -> b
initialTransferDataCata fa _ (InitialTransferData_Account a) = fa a
initialTransferDataCata _ ft (InitialTransferData_Transfer t) = ft t

-- should probably make lenses, but we'd have to move the data types out to another file because this
-- file can't deal with the mutually recursive functions if TH was on. So lets just avoid the lenses

withInitialTransfer :: (TransferData -> a) -> InitialTransferData -> Maybe a
withInitialTransfer f (InitialTransferData_Transfer t)  = Just $ f t
withInitialTransfer _ (InitialTransferData_Account _)  = Nothing

-- | Preview the transfer. Doesn't actually send any requests.
previewTransfer
  :: forall model mConf key t m.
    ( SendConstraints model mConf key t m
    , HasTransactionLogger m
    )
  => model
  -> TransferData
  -> Workflow t m (mConf, Event t ())
previewTransfer model transfer = Workflow $ do
  let
    toTxBuilder = _transferData_toTxBuilder transfer
    fromAccount@(fromName,_,_) = _transferData_fromAccount transfer
    fromGasPayer = _transferData_fromGasPayer transfer

    amount = _transferData_amount transfer
  close <- modalHeader $ text "Transaction Preview"
  elClass "div" "modal__main" $ do
    dialogSectionHeading mempty "Destination"
    divClass "group" $ transactionDisplayNetwork model
    dialogSectionHeading mempty "Participants"
    divClass "group" $ do
      mkLabeledInput True "Sender Account" uiInputElement $ def
        & initialAttributes .~ "disabled" =: "disabled"
        & inputElementConfig_initialValue .~ unAccountName fromName
      mkLabeledInput True "Gas Payer" uiInputElement $ def
        & initialAttributes .~ "disabled" =: "disabled"
        & inputElementConfig_initialValue .~ unAccountName (fst fromGasPayer)
      mkLabeledInput True "Recipient Account" uiInputElement $ def
        & initialAttributes .~ "disabled" =: "disabled"
        & inputElementConfig_initialValue .~ unAccountName (_txBuilder_accountName toTxBuilder)
    dialogSectionHeading mempty "Transaction Details"
    divClass "group" $ do
      void $ mkLabeledInput True "Amount" uiGasPriceInputField $ def
        & initialAttributes .~ "disabled" =: "disabled"
        & inputElementConfig_initialValue .~ tshow amount
      -- TODO The designs show gas fees here, but we can't get that information yet.
  (back, next) <- modalFooter $ do
    back <- cancelButton def "Back"
    next <- confirmButton def "Create Transaction"
    pure (back, next)
  let nextScreen = leftmost
        [ sendConfig model (InitialTransferData_Transfer transfer) <$ back

        , flip push next $ \() -> do
          mNetInfo <- sampleNetInfo model
          keys <- sample $ current $ model ^. wallet_keys

          pure $ ffor mNetInfo $ \n ->
            sameChainTransfer (model ^. logger) n keys fromAccount fromGasPayer toTxBuilder amount
        ]
  pure ((mempty, close), nextScreen)

-- | Perform a same chain transfer or transfer-create
sameChainTransfer
  :: (MonadWidget t m, HasCrypto key m, Monoid mConf, HasLogger model t, HasTransactionLogger m)
  => model
  -> SharedNetInfo NodeInfo
  -> KeyStorage key
  -> (AccountName, ChainId, Account)
  -- ^ From account
  -> (AccountName, Account)
  -- ^ Gas payer
  -> TxBuilder
  -- ^ Recipient account
  -> Decimal
  -- ^ Amount to transfer
  -> Workflow t m (mConf, Event t ())
sameChainTransfer model netInfo keys (fromName, fromChain, fromAcc) (gasPayer, gasPayerAcc) toAccount amount = Workflow $ do
  let mKeyset = _txBuilder_keyset toAccount
  let code = T.unwords $
        [ "(coin." <> case mKeyset of
          Nothing -> "transfer"
          Just _ -> "transfer-create"
        , tshow $ unAccountName fromName
        , tshow $ unAccountName $ _txBuilder_accountName toAccount
        , case mKeyset of
          Nothing -> mempty
          Just _ -> "(read-keyset 'key)"
        , tshow amount
        , ")"
        ]
      signingSet = Set.unions [accountKeys fromAcc, accountKeys gasPayerAcc]
      signingPairs = filterKeyPairs signingSet keys
      transferCap = SigCapability
        { _scName = QualifiedName { _qnQual = "coin", _qnName = "TRANSFER", _qnInfo = def }
        , _scArgs =
          [ PLiteral $ LString $ unAccountName fromName
          , PLiteral $ LString $ unAccountName $ _txBuilder_accountName toAccount
          , PLiteral $ LDecimal amount
          ]
        }
      dat = case mKeyset of
        -- TODO check against chain (this data may be outdated unless refreshed recently)
        Just keyset -> HM.singleton "key" $ Aeson.toJSON keyset
        _ -> mempty
      pkCaps = Map.unionsWith (<>)
        [ Map.fromSet (\_ -> [_dappCap_cap defaultGASCapability]) (accountKeys gasPayerAcc)
        , Map.fromSet (\_ -> [transferCap]) (accountKeys fromAcc)
        ]
      pm = (_sharedNetInfo_meta netInfo)
        { _pmChainId = fromChain
        , _pmSender = unAccountName gasPayer
        }
      nodeInfos = _sharedNetInfo_nodes netInfo
      networkName = _sharedNetInfo_network netInfo
  close <- modalHeader $ text "Transaction Status"
  cmd <- buildCmd Nothing networkName pm signingPairs [] code dat pkCaps
  _ <- elClass "div" "modal__main transaction_details" $
    submitTransactionWithFeedback model cmd fromChain (fmap Right nodeInfos)
  done <- modalFooter $ confirmButton def "Done"
  pure
    ( (mempty, close <> done)
    , never
    )

-- | General transfer workflow. This is the initial configuration screen.
sendConfig
  :: (SendConstraints model mConf key t m, HasTransactionLogger m)
  => model -> InitialTransferData -> Workflow t m (mConf, Event t ())
sendConfig model initData = Workflow $ do
  close <- modalHeader $ text "Send"
  rec
    (currentTab, _done) <- makeTabs initData $ leftmost [prevTab, fmapMaybe id nextTab]
    (conf, mCaps, recipient) <- mainSection currentTab
    (cancel, prevTab, nextTab) <- footerSection currentTab recipient mCaps
  let nextScreen = flip push nextTab $ \case
        Just _ -> pure Nothing
        Nothing -> runMaybeT $ do
          fromGasPayer <- MaybeT $ sample $ current mCaps
          (toAccount, amount) <- MaybeT $ fmap hush $ sample $ current recipient
          let transfer = TransferData
                { _transferData_amount = amount
                , _transferData_fromAccount = fromAccount
                , _transferData_fromGasPayer = fromGasPayer
                , _transferData_toTxBuilder = toAccount
                }
          pure $ previewTransfer model transfer
  pure ((conf, close <> cancel), nextScreen)
  where
    fromAccount@(fromName, fromChain, fromAcc) = initialTransferDataCata id _transferData_fromAccount initData

    mInitToAddress = withInitialTransfer _transferData_toTxBuilder initData

    mInitFromGasPayer = withInitialTransfer (_transferData_fromGasPayer) initData

    mInitAmount = withInitialTransfer _transferData_amount initData

    mainSection currentTab = elClass "div" "modal__main" $ do
      (conf, useEntireBalance, txBuilder, amount) <- tabPane mempty currentTab SendModalTab_Configuration $ do
        dialogSectionHeading mempty  "Destination"
        divClass "group" $ transactionDisplayNetwork model

        dialogSectionHeading mempty  "Recipient"
        (txBuilder, amount, useEntireBalance) <- divClass "group" $ do

          let insufficientFundsMsg = "Sender has insufficient funds."
              cannotBeReceiverMsg = "Sender cannot be the receiver of a transfer"
              mustBeSameChainMsg = "Recipient must be on chain " <> _chainId fromChain

              renderTxBuilder = T.decodeUtf8 . LBS.toStrict . Aeson.encode
              validateTxBuilder = Aeson.eitherDecodeStrict . T.encodeUtf8

              uiTxBuilderInput cfg = do
                ie <- uiTxBuilder Nothing cfg
                pure (ie
                     , ( validateTxBuilder <$> _textAreaElement_input ie
                       , validateTxBuilder <$> value ie
                       )
                     )

              showTxBuilderPopover (_, (onInput, _)) = pure $ ffor onInput $ \case
                Left _ ->
                  PopoverState_Error "Invalid Tx Builder"
                Right txb ->
                  if _txBuilder_accountName txb == fromName && _txBuilder_chainId txb == fromChain then
                    PopoverState_Error cannotBeReceiverMsg
                  else if _txBuilder_chainId txb /= fromChain then
                    PopoverState_Error mustBeSameChainMsg
                  else
                    PopoverState_Disabled

          decoded <- fmap (snd . snd) $ mkLabeledInput True "Tx Builder"
            (uiInputWithPopover uiTxBuilderInput (_textAreaElement_raw . fst) showTxBuilderPopover)
            (def & textAreaElementConfig_initialValue .~ (maybe "" renderTxBuilder mInitToAddress))

          let balance = _account_status fromAcc ^? _AccountStatus_Exists . accountDetails_balance . to unAccountBalance

              showGasPriceInsuffPopover (_, (_, onInput)) = pure $ ffor onInput $ \(GasPrice (ParsedDecimal gp)) ->
                if maybe True (gp >) balance then
                  PopoverState_Error insufficientFundsMsg
                else
                  PopoverState_Disabled

              gasInputWithMaxButton cfg = mdo
                let attrs = ffor useEntireBalance $ \u -> "disabled" =: ("disabled" <$ u)
                    nestTuple (a,b,c) = (a,(b,c))
                    field = fmap nestTuple . uiGasPriceInputField
                (_, (amountValue, _)) <- uiInputWithPopover field (_inputElement_raw . fst) showGasPriceInsuffPopover $ cfg
                  & inputElementConfig_setValue .~ fmap tshow (mapMaybe id $ updated useEntireBalance)
                  & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ updated attrs
                useEntireBalance <- case balance of
                  Nothing -> pure $ pure Nothing
                  Just b -> fmap (\v -> b <$ guard v) . _checkbox_value <$> uiCheckbox "input-max-toggle" False def (text "Max")
                pure ( isJust <$> useEntireBalance
                     , amountValue & mapped . mapped %~ view (_Wrapped' . _Wrapped')
                     )

          (useEntireBalance, amount) <- mkLabeledInput True "Amount" gasInputWithMaxButton
            (def & inputElementConfig_initialValue .~ (maybe "" tshow mInitAmount))

          let validatedTxBuilder = runExceptT $ do
                r <- ExceptT $ first (\_ -> "Invalid Tx Builder") <$> decoded
                when (r == TxBuilder fromName fromChain Nothing) $
                  throwError cannotBeReceiverMsg
                when (_txBuilder_chainId r /= fromChain ) $
                  throwError mustBeSameChainMsg
                pure r

              validatedAmount = runExceptT $ do
                a <- ExceptT $ maybe (Left "Invalid amount") Right <$> amount
                when (maybe True (a >) $ fromAcc ^? account_status . _AccountStatus_Exists . accountDetails_balance . _AccountBalance) $
                  throwError insufficientFundsMsg
                pure a

          pure (validatedTxBuilder, validatedAmount, useEntireBalance)

        dialogSectionHeading mempty  "Transaction Settings"
        (conf, _, _, _) <- divClass "group" $ uiMetaData model Nothing Nothing
        pure (conf, useEntireBalance, txBuilder, amount)
      mCaps <- tabPane mempty currentTab SendModalTab_Sign $ do
        dedrecipient <- eitherDyn txBuilder
        fmap join . holdDyn (pure Nothing) <=< dyn $ ffor dedrecipient $ \case
          Left de -> do
            divClass "group" $ dynText $ ffor de (<> ": please go back and check the configuration.")
            pure $ pure Nothing
          Right _ -> do
            let allowAccount0 = ffor useEntireBalance $ \useAll an _ -> not $ useAll && fromName == an
                mkPlaceholder = ffor useEntireBalance $ bool id (<> " (sender excluded due to maximum transfer)")

            _ <- dialogSectionHeading mempty "Gas Payer"
            divClass "group" $ elClass "div" "segment segment_type_tertiary labeled-input" $ do
              divClass "label labeled-input__label" $ text "Account Name"
              let cfg = def & dropdownConfig_attributes .~
                    pure ("class" =: "labeled-input__input select select_mandatory_missing")
                  allowAccount = ffor allowAccount0 $ \p an acc ->
                    p an acc && fromMaybe False (accountHasFunds acc)

              uiAccountDropdown' cfg allowAccount mkPlaceholder model
                (mInitFromGasPayer ^? _Just . _1)
                (constDyn $ Just fromChain)
                never

      pure (conf, mCaps, (liftA2 . liftA2) (,) txBuilder amount)

    footerSection currentTab recipient mCaps = modalFooter $ do
      let (lbl, fanTag) = splitDynPure $ ffor currentTab $ \case
            SendModalTab_Configuration -> ("Cancel", Left ())
            SendModalTab_Sign -> ("Back", Right SendModalTab_Configuration)
      ev <- cancelButton def lbl
      let (cancel, back) = fanEither $ current fanTag <@ ev
          (name, disabled) = splitDynPure $ ffor currentTab $ \case
            SendModalTab_Configuration -> ("Next", fmap isLeft recipient)
            SendModalTab_Sign -> ("Preview", fmap isNothing mCaps)
          cfg = def
            & uiButtonCfg_class <>~ "button_type_confirm"
            & uiButtonCfg_disabled .~ join disabled
      next <- uiButtonDyn cfg $ dynText name
      let nextTab = ffor (current currentTab <@ next) $ \case
            SendModalTab_Configuration -> Just SendModalTab_Sign
            SendModalTab_Sign -> Nothing
      pure (cancel, back, nextTab)

-- | Handy function for getting network / meta information in 'PushM'. Type
-- monomorphised to prevent accidentally sampling outside of 'push'
sampleNetInfo
  :: (Reflex t, HasNetwork model t)
  => model -> PushM t (Maybe (SharedNetInfo NodeInfo))
sampleNetInfo model = do
  net <- sample $ current $ model ^. network_selectedNetwork
  nodes <- fmap rights $ sample $ current $ model ^. network_selectedNodes
  meta <- sample $ current $ model ^. network_meta
  let networkName = hush . mkNetworkName . nodeVersion =<< headMay nodes
  pure $ ffor networkName $ \name -> SharedNetInfo
    { _sharedNetInfo_network = name
    , _sharedNetInfo_nodes = nodes
    , _sharedNetInfo_meta = meta
    , _sharedNetInfo_selectedNetwork = net
    }

data SendModalTab
  = SendModalTab_Configuration
  | SendModalTab_Sign
  deriving (Eq, Ord, Show, Enum, Bounded)

displaySendModalTab :: DomBuilder t m => SendModalTab -> m ()
displaySendModalTab = text . \case
  SendModalTab_Configuration -> "Configuration"
  SendModalTab_Sign -> "Sign"

makeTabs
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => InitialTransferData
  -> Event t SendModalTab
  -> m (Dynamic t SendModalTab, Event t ())
makeTabs initData tabEv = do
  -- We assume that if there is a full transfer that we should head to the sign tab
  let initTab = initialTransferDataCata (const SendModalTab_Configuration) (const SendModalTab_Sign) initData
      f t0 g = case g t0 of
        Nothing -> (Just t0, Just ())
        Just t -> (Just t, Nothing)
  rec
    (curSelection, done) <- mapAccumMaybeDyn f initTab $ leftmost
      [ const . Just <$> onTabClick
      , const . Just <$> tabEv
      ]
    (TabBar onTabClick) <- makeTabBar $ TabBarCfg
      { _tabBarCfg_tabs = [SendModalTab_Configuration, SendModalTab_Sign]
      , _tabBarCfg_mkLabel = \_ -> displaySendModalTab
      , _tabBarCfg_selectedTab = Just <$> curSelection
      , _tabBarCfg_classes = mempty
      , _tabBarCfg_type = TabBarType_Secondary
      }
  pure (curSelection, done)
