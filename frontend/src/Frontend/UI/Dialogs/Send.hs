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
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.Send where

import Control.Applicative (liftA2)
import Control.Concurrent
import Control.Error.Util (hush, failWithM)
import Control.Lens hiding (failover)
import Control.Monad (guard, join, when, void, (<=<))
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import Data.Decimal
import Data.Either (isLeft, rights)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Text (Text)
import Kadena.SigningApi
import Pact.Types.Capability
import Pact.Types.ChainMeta
import Pact.Types.Exp
import Pact.Types.Hash
import Pact.Types.PactError
import Pact.Types.Names
import Pact.Types.PactValue
import Pact.Types.RPC
import Pact.Types.Term
import Reflex
import Reflex.Dom
import Safe (headMay)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Pact.Server.ApiClient as Api
import qualified Pact.Types.API as Api
import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Continuation as Pact
import qualified Pact.Types.SPV as Pact
import qualified Pact.Types.Term as Pact
import qualified Servant.Client.JSaddle as S
import qualified Text.URI as URI

import Common.Wallet
import Frontend.Crypto.Class (HasCrypto)
import Frontend.Foundation hiding (Arg)
import Frontend.JsonData
import Frontend.Log
import Frontend.Network
import Frontend.PactQueries
import Frontend.TxBuilder
import Frontend.UI.DeploymentSettings
import Frontend.UI.Dialogs.DeployConfirmation (statusText, Status(..))
import Frontend.UI.Dialogs.DeployConfirmation (submitTransactionWithFeedback)
import Frontend.UI.Modal
import Frontend.UI.TabBar
import Frontend.UI.Widgets
import Frontend.UI.Widgets.Helpers (dialogSectionHeading)
import Frontend.Wallet

import Frontend.UI.Dialogs.Send.ManualTxBuilder (uiExplodedTxBuilder, recipientMatchesSenderTxBuilder)

type SendConstraints model mConf key t m
  = ( Monoid mConf, HasNetwork model t, HasNetworkCfg mConf t, HasWallet model key t, HasWalletCfg mConf key t
    , MonadWidget t m, PostBuild t m, HasCrypto key m
    , HasCrypto key (Performable m)
    , HasLogger model t
    , HasJsonData model t
    )

-- | Data which is only required for cross chain transfers
data CrossChainData = CrossChainData
  { _crossChainData_recipientChainGasPayer :: (AccountName, Account)
  -- ^ The account which will be paying the gas on the recipient chain
  }

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
  { _transferData_fromAccount :: (AccountName, ChainId, AccountDetails)
  , _transferData_fromGasPayer :: (AccountName, Account)
  , _transferData_toTxBuilder :: TxBuilder
  , _transferData_crossChainData :: Maybe CrossChainData
  , _transferData_amount :: Decimal
  }

data InitialTransferData
  = InitialTransferData_Account (AccountName, ChainId, AccountDetails) (Maybe UnfinishedCrossChainTransfer)
  | InitialTransferData_Transfer TransferData

-- -- | A modal for handling sending coin
-- uiSendModal
--   :: ( SendConstraints model mConf key t m
--      , Flattenable mConf t
--      , HasTransactionLogger m
--      )
--   => model
--   -> (AccountName, ChainId, AccountDetails)
--   -> Maybe UnfinishedCrossChainTransfer
--   -> Event t ()
--   -> m (mConf, Event t ())
-- uiSendModal model (name, chain, acc) mucct _onCloseExternal = do
--   (conf, closes) <- fmap splitDynPure $ workflow $ sendConfig model
--     $ InitialTransferData_Account (name, chain, acc) mucct

--   mConf <- flatten =<< tagOnPostBuild conf
--   let close = switch $ current closes
--   pure (mConf, close)

-- uiFinishCrossChainTransferModal
--   :: ( SendConstraints model mConf key t m
--      , Flattenable mConf t
--      , HasTransactionLogger m
--      )
--   => model
--   -> AccountName
--   -> ChainId
--   -> UnfinishedCrossChainTransfer
--   -> Event t ()
--   -> m (mConf, Event t ())
-- uiFinishCrossChainTransferModal model name chain ucct _onCloseExternal = do
--   (conf, closes) <- fmap splitDynPure $ workflow $ finishCrossChainTransferConfig model (name, chain) ucct
--   mConf <- flatten =<< tagOnPostBuild conf
--   let close = switch $ current closes
--   pure (mConf, close)

initialTransferDataCata
  :: ((AccountName, ChainId, AccountDetails) -> Maybe UnfinishedCrossChainTransfer -> b)
  -> (TransferData -> b)
  -> InitialTransferData
  -> b
initialTransferDataCata fa _ (InitialTransferData_Account a b) = fa a b
initialTransferDataCata _ ft (InitialTransferData_Transfer t) = ft t

-- should probably make lenses, but we'd have to move the data types out to another file because this
-- file can't deal with the mutually recursive functions if TH was on. So lets just avoid the lenses

withInitialTransfer :: (TransferData -> a) -> InitialTransferData -> Maybe a
withInitialTransfer f (InitialTransferData_Transfer t) = Just $ f t
withInitialTransfer _ (InitialTransferData_Account _ _) = Nothing

-- -- | Preview the transfer. Doesn't actually send any requests.
-- previewTransfer
--   :: forall model mConf key t m.
--     ( SendConstraints model mConf key t m
--     , HasTransactionLogger m
--     )
--   => model
--   -> TransferData
--   -> Workflow t m (mConf, Event t ())
-- previewTransfer model transfer = Workflow $ do
--   let
--     cidDisplay lbl cid = mkLabeledInput True lbl uiInputElement $ def
--       & initialAttributes %~ Map.insert "disabled" ""
--       & inputElementConfig_initialValue .~ _chainId cid

--     toTxBuilder = _transferData_toTxBuilder transfer
--     fromAccount@(fromName,fromChain,_) = _transferData_fromAccount transfer
--     fromGasPayer = _transferData_fromGasPayer transfer
--     crossChainData = _transferData_crossChainData transfer
--     amount = _transferData_amount transfer
--   close <- modalHeader $ text "Transaction Preview"
--   elClass "div" "modal__main" $ do
--     dialogSectionHeading mempty "Destination"
--     divClass "group" $ do
--       transactionDisplayNetwork model
--       case crossChainData of
--         Nothing ->
--           void $ predefinedChainIdDisplayed fromChain
--         Just _ -> do
--           _ <- cidDisplay "From Chain ID" fromChain
--           _ <- cidDisplay "To Chain ID" (_txBuilder_chainId toTxBuilder)
--           pure ()

--     dialogSectionHeading mempty "Participants"
--     display $ model ^. network_meta
--     divClass "group" $ do
--       mkLabeledInput True "Sender Account" uiInputElement $ def
--         & initialAttributes .~ "disabled" =: "disabled"
--         & inputElementConfig_initialValue .~ unAccountName fromName
--       let gasLabel = "Gas Payer" <> case crossChainData of
--             Nothing -> ""
--             Just _ -> " (Chain " <> _chainId fromChain <> ")"
--       mkLabeledInput True gasLabel uiInputElement $ def
--         & initialAttributes .~ "disabled" =: "disabled"
--         & inputElementConfig_initialValue .~ unAccountName (fst fromGasPayer)
--       mkLabeledInput True "Recipient Account" uiInputElement $ def
--         & initialAttributes .~ "disabled" =: "disabled"
--         & inputElementConfig_initialValue .~ unAccountName (_txBuilder_accountName toTxBuilder)
--       for_ crossChainData $ \ccd -> do
--         let toChain = _txBuilder_chainId toTxBuilder
--         mkLabeledInput True ("Gas Payer (Chain " <> _chainId toChain <> ")") uiInputElement $ def
--           & initialAttributes .~ "disabled" =: "disabled"
--           & inputElementConfig_initialValue .~ unAccountName (fst $ _crossChainData_recipientChainGasPayer ccd)
--     dialogSectionHeading mempty "Transaction Details"
--     divClass "group" $ do
--       void $ mkLabeledInput True "Amount" (uiGasPriceInputField never) $ def
--         & initialAttributes .~ "disabled" =: "disabled"
--         & inputElementConfig_initialValue .~ tshow amount
--       -- TODO The designs show gas fees here, but we can't get that information yet.
--   (back, next) <- modalFooter $ do
--     back <- cancelButton def "Back"
--     next <- confirmButton def "Create Transaction"
--     pure (back, next)
--   let nextScreen = leftmost
--         [ sendConfig model (InitialTransferData_Transfer transfer) <$ back
--         , flip push next $ \() -> do
--           mNetInfo <- sampleNetInfo model
--           keys <- sample $ current $ model ^. wallet_keys
--           AccountData accounts <- sample $ current $ model ^. wallet_accounts
--           let toAccount n = maybe (Left toTxBuilder) Right $ lookupAccountByTxBuilder toTxBuilder =<< Map.lookup n accounts
--           pure $ ffor mNetInfo $ \n -> case crossChainData of
--             Nothing -> sameChainTransfer (model ^. logger) n keys fromAccount fromGasPayer toTxBuilder amount
--             Just ccd -> crossChainTransfer (model ^. logger) n keys fromAccount (toAccount $ _sharedNetInfo_network n) fromGasPayer ccd amount
--         ]
--   pure ((mempty, close), nextScreen)

lookupAccountByTxBuilder :: TxBuilder -> Map AccountName (AccountInfo Account) -> Maybe (AccountName, ChainId, Account)
lookupAccountByTxBuilder ka accounts = (name, chain,) <$> accounts ^? ix name . accountInfo_chains . ix chain
  where
    name = _txBuilder_accountName ka
    chain = _txBuilder_chainId ka

-- | Perform a same chain transfer or transfer-create
sameChainTransfer
  :: (MonadWidget t m, HasCrypto key m, Monoid mConf, HasLogger model t, HasTransactionLogger m)
  => model
  -> SharedNetInfo NodeInfo
  -> KeyStorage key
  -> (AccountName, ChainId, AccountDetails)
  -- ^ From account
  -> (AccountName, Account)
  -- ^ Gas payer
  -> TxBuilder
  -- ^ Recipient account
  -> Decimal
  -- ^ Amount to transfer
  -> Workflow t m (mConf, Event t ())
sameChainTransfer model netInfo keys (fromName, fromChain, fromAcc) (gasPayer, gasPayerAcc) toAccount amount = Workflow $ do
  -- TODO check against chain - this data may be outdated (key rotations) unless refreshed recently
  let (functionName, readKeyset, keyData) = case _txBuilder_keyset toAccount of
        Just ks | not (null $ _ksKeys ks) -> ("transfer-create", "(read-keyset 'key)", HM.singleton "key" $ Aeson.toJSON ks)
        _ -> ("transfer", "", mempty)
      code = T.unwords $
        [ "(coin." <> functionName
        , tshow $ unAccountName fromName
        , tshow $ unAccountName $ _txBuilder_accountName toAccount
        , readKeyset
        , tshow $ addDecimalPoint amount
        , ")"
        ]
      fromAccKeys = fromAcc ^. accountDetails_guard . _AccountGuard_KeySetLike . ksh_keys
      signingSet = Set.unions [fromAccKeys, accountKeys gasPayerAcc]
      signingPairs = filterKeyPairs signingSet keys
      transferCap = SigCapability
        { _scName = QualifiedName { _qnQual = "coin", _qnName = "TRANSFER", _qnInfo = def }
        , _scArgs =
          [ PLiteral $ LString $ unAccountName fromName
          , PLiteral $ LString $ unAccountName $ _txBuilder_accountName toAccount
          , PLiteral $ LDecimal amount
          ]
        }
      pkCaps = Map.unionsWith (<>)
        [ Map.fromSet (\_ -> [_dappCap_cap defaultGASCapability]) (accountKeys gasPayerAcc)
        , Map.fromSet (\_ -> [transferCap]) fromAccKeys
        ]
      pm = (_sharedNetInfo_meta netInfo)
        { _pmChainId = fromChain
        , _pmSender = unAccountName gasPayer
        }
      nodeInfos = _sharedNetInfo_nodes netInfo
      networkName = _sharedNetInfo_network netInfo
  close <- modalHeader $ text "Transaction Status"
  cmd <- buildCmd Nothing networkName pm signingPairs [] code keyData pkCaps
  _ <- elClass "div" "modal__main transaction_details" $
    submitTransactionWithFeedback model cmd fromName fromChain (fmap Right nodeInfos)
  done <- modalFooter $ confirmButton def "Done"
  pure
    ( (mempty, close <> done)
    , never
    )

-- -- | General transfer workflow. This is the initial configuration screen.
-- sendConfig
--   :: ( SendConstraints model mConf key t m
--      , HasTransactionLogger m
--      )
--   => model
--   -> InitialTransferData
--   -> Workflow t m (mConf, Event t ())
-- sendConfig model initData = Workflow $ do
--   close <- modalHeader $ text "Send"
--   rec
--     (currentTab, _done) <- makeTabs initData $ leftmost [prevTab, fmapMaybe id nextTab]
--     (conf, mCaps, recipient) <- mainSection currentTab
--     (cancel, prevTab, nextTab, onFinishXChain) <- footerSection currentTab recipient mCaps
--   let onToPreviewTransfer = flip push nextTab $ \case
--         Just _ -> pure Nothing
--         Nothing -> runMaybeT $ do
--           (fromGasPayer, mToGasPayer) <- MaybeT $ sample $ current mCaps
--           (toAccount, amount) <- MaybeT $ fmap hush $ sample $ current recipient
--           let mCCD = case mToGasPayer of
--                 Just toGasPayer | toGasPayer /= fromGasPayer -> Just $ CrossChainData
--                   { _crossChainData_recipientChainGasPayer = toGasPayer
--                   }
--                 _ -> Nothing
--           let transfer = TransferData
--                 { _transferData_amount = amount
--                 , _transferData_crossChainData = mCCD
--                 , _transferData_fromAccount = fromAccount
--                 , _transferData_fromGasPayer = fromGasPayer
--                 , _transferData_toTxBuilder = toAccount
--                 }
--           pure $ previewTransfer model transfer
--   pure ( (conf, close <> cancel)
--        , leftmost
--          [ onToPreviewTransfer
--          , finishCrossChainTransferConfig model (fromName, fromChain) <$> onFinishXChain
--          ]
--        )
--   where
--     (fromAccount@(fromName, fromChain, fromAcc), mUcct) = initialTransferDataCata
--       (,)
--       ((,Nothing) . _transferData_fromAccount)
--       initData

--     mInitToAddress = withInitialTransfer _transferData_toTxBuilder initData
--     mInitFromGasPayer = withInitialTransfer (_transferData_fromGasPayer) initData
--     mInitCrossChainGasPayer = join $ withInitialTransfer (fmap (_crossChainData_recipientChainGasPayer) . _transferData_crossChainData) initData
--     mInitAmount = withInitialTransfer _transferData_amount initData

--     insufficientFundsMsg = "Sender has insufficient funds."
--     cannotBeReceiverMsg = "Sender cannot be the receiver of a transfer"

--     mainSection currentTab = elClass "div" "modal__main" $ do
--       (conf, useEntireBalance, txBuilder, amount) <- tabPane mempty currentTab SendModalTab_Configuration $ do

--         let balance = fromAcc ^. accountDetails_balance . to unAccountBalance

--         dialogSectionHeading mempty "Destination"
--         divClass "group" $ transactionDisplayNetwork model

--         dialogSectionHeading mempty "Amount"
--         (useEntireBalance, validatedAmount) <- divClass "group" $ do
--           let checkFunds amt =
--                 if amt > balance then
--                   PopoverState_Error insufficientFundsMsg
--                 else
--                   PopoverState_Disabled

--               -- NOTE If this results in an event loop, might need to switch to
--               -- using the change event rather than the value
--               showGasPriceInsuffPopover (ie, v) = pure $ leftmost
--                 [ PopoverState_Disabled <$ ffilter T.null (_inputElement_input ie)
--                 , ffor (ffilter isLeft $ updated v) $ either (PopoverState_Error . T.pack) (const PopoverState_Disabled)
--                 , ffor (fmapMaybe hush $ updated v) checkFunds
--                 ]

--               gasInputWithMaxButton cfg = mdo

--                 let attrs = ffor useEntireBalance $ \u ->
--                       "disabled" =: ("disabled" <$ u)

--                 (_, amountValue) <- uiInputWithPopover uiAmountInput
--                   (_inputElement_raw . fst)
--                   showGasPriceInsuffPopover $ cfg
--                     & inputElementConfig_setValue .~ fmap tshow (mapMaybe id $ updated useEntireBalance)
--                     & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ updated attrs

--                 useEntireBalance <- do
--                   cb <- uiCheckbox "input-max-toggle" False def (text "Max")
--                   pure $ fmap (\v -> balance <$ guard v) $ _checkbox_value cb

--                 pure ( isJust <$> useEntireBalance
--                      , amountValue
--                      )

--           (useEntireBalance, amount) <- mkLabeledInput True "Amount" gasInputWithMaxButton $ def
--             & inputElementConfig_initialValue .~ maybe "" tshow mInitAmount

--           let validatedAmount = runExceptT $ do
--                 a <- ExceptT $ either (\_ -> Left "Invalid amount") Right <$> amount
--                 when (a > balance) $
--                   throwError insufficientFundsMsg
--                 pure a

--           pure (useEntireBalance, validatedAmount)

--         dialogSectionHeading mempty "Recipient Info"
--         validatedTxBuilder <- divClass "group" $ do
--           dTxBuilder <- uiExplodedTxBuilder model fromName fromChain mUcct mInitToAddress
--           pure $ runExceptT $ do
--             r <- failWithM "Invalid Tx Builder" dTxBuilder
--             when (recipientMatchesSenderTxBuilder (fromName, fromChain) r) $
--               throwError cannotBeReceiverMsg
--             pure r

--         dialogSectionHeading mempty  "Transaction Settings"
--         (conf, _, _, _) <- divClass "group" $ uiMetaData model Nothing Nothing

--         pure (conf, useEntireBalance, validatedTxBuilder, validatedAmount)

--       mCaps <- tabPane mempty currentTab SendModalTab_Sign $ do
--         dedrecipient <- eitherDyn txBuilder
--         fmap join . holdDyn (pure Nothing) <=< dyn $ ffor dedrecipient $ \case
--           Left de -> do
--             divClass "group" $ dynText $ ffor de (<> ": please go back and check the configuration.")
--             pure $ pure Nothing
--           Right dka -> do
--             toChain <- holdUniqDyn $ _txBuilder_chainId <$> dka
--             let isCross = ffor toChain (fromChain /=)
--                 gasPayerSection forChain mpayer accountPredicate mkPlaceholder = do
--                   dyn_ $ ffor2 isCross forChain $ \x c ->
--                     dialogSectionHeading mempty $ "Gas Payer" <> if x then " (Chain " <> _chainId c <> ")" else ""
--                   divClass "group" $ elClass "div" "segment segment_type_tertiary labeled-input" $ do
--                     divClass "label labeled-input__label" $ text "Account Name"
--                     let cfg = def & dropdownConfig_attributes .~ pure ("class" =: "labeled-input__input select select_mandatory_missing")
--                         chain = fmap Just forChain
--                         allowAccount = ffor accountPredicate $ \p an acc -> p an acc && fromMaybe False (accountHasFunds acc)
--                     uiAccountDropdown' cfg allowAccount mkPlaceholder model (mpayer ^? _Just . _1) chain never

--             dyn_ $ ffor2 isCross toChain $ \x c -> when x $ do
--               elClass "h3" ("heading heading_type_h3") $ text "This is a cross chain transfer."
--               el "p" $ text $ T.concat
--                 [ "Coin will be transferred from chain "
--                 , _chainId fromChain
--                 , " to chain "
--                 , _chainId c
--                 , "."
--                 ]
--               el "p" $ text $ T.unwords
--                 [ "This is a multi step operation."
--                 , "The coin will leave the sender account immediately, and gas must be paid on the recipient chain in order to redeem the coin."
--                 ]
--             fromGasPayer <- mdo
--               let allowAccount = ffor useEntireBalance $ \useAll an _ -> not $ useAll && fromName == an
--                   mkPlaceholder = ffor useEntireBalance $ bool id (<> " (sender excluded due to maximum transfer)")
--               gasPayerSection (pure fromChain) mInitFromGasPayer allowAccount mkPlaceholder
--             toGasPayer <- fmap join . holdDyn (pure Nothing) <=< dyn $ ffor isCross $ \x ->
--               if not x
--               then pure $ pure $ Just Nothing
--               else (fmap . fmap . fmap) Just $ do
--                 -- TODO this bit should have an option with a generic input for Ed25519 keys
--                 -- and perhaps be skippable entirely in favour of a blob the user
--                 -- can send to someone else to continue the tx on the recipient chain
--                 gasPayerSection toChain mInitCrossChainGasPayer (pure $ \_ _ -> True) (pure id)
--             pure $ (liftA2 . liftA2) (,) fromGasPayer toGasPayer
--       pure (conf, mCaps, (liftA2 . liftA2) (,) txBuilder amount)

--     footerSection currentTab recipient mCaps = modalFooter $ do
--       let (lbl, fanTag) = splitDynPure $ ffor currentTab $ \case
--             SendModalTab_Configuration -> ("Cancel", Left ())
--             SendModalTab_Sign -> ("Back", Right SendModalTab_Configuration)

--       onFinXChain <- case mUcct of
--         Nothing -> pure never
--         Just ucct -> fmap (ucct <$) $ confirmButton def "Complete Crosschain"

--       ev <- cancelButton def lbl
--       let (cancel, back) = fanEither $ current fanTag <@ ev
--           (name, disabled) = splitDynPure $ ffor currentTab $ \case
--             SendModalTab_Configuration -> ("Next", fmap isLeft recipient)
--             SendModalTab_Sign -> ("Preview", fmap isNothing mCaps)
--           cfg = def
--             & uiButtonCfg_class <>~ "button_type_confirm"
--             & uiButtonCfg_disabled .~ join disabled
--       next <- uiButtonDyn cfg $ dynText name
--       let nextTab = ffor (current currentTab <@ next) $ \case
--             SendModalTab_Configuration -> Just SendModalTab_Sign
--             SendModalTab_Sign -> Nothing
--       pure (cancel, back, nextTab, onFinXChain)

-- | This function finishes cross chain transfers. The return event signals that
-- the transfer is complete.
runUnfinishedCrossChainTransfer
  :: ( PerformEvent t m, PostBuild t m, TriggerEvent t m, MonadHold t m, DomBuilder t m
     , MonadJSM (Performable m), MonadFix m
     , HasCrypto key (Performable m)
     , HasTransactionLogger m
     )
  => Logger t
  -> SharedNetInfo NodeInfo
  -- ^ Network info
  -> KeyStorage key
  -- ^ Keys
  -> ChainId
  -- ^ From chain
  -> ChainId
  -- ^ To chain
  -> Maybe (AccountName, AccountStatus AccountDetails)
  -- ^ Gas payer on "To" chain
  -> Event t Pact.RequestKey
  -- ^ The request key to follow up on
  -> PublicMeta
  -> m (Event t (), Event t Text, Event t ())
runUnfinishedCrossChainTransfer logL netInfo keys fromChain toChain mtoGasPayer requestKey publicMeta = mdo
  let nodeInfos = _sharedNetInfo_nodes netInfo
      networkName = _sharedNetInfo_network netInfo
  -- Client envs for making requests to each chain
  let envFromChain = mkClientEnvs nodeInfos fromChain
      envToChain = mkClientEnvs nodeInfos toChain
  -- Wait for result
  mRequestKey <- hold Nothing $ Just <$> requestKey
  let initCont = leftmost [requestKey, tagMaybe mRequestKey retry]
  contResponse <- pollNodesForCont logL envFromChain initCont
  let (contError, contOk) = fanEither contResponse
  contStatus <- holdDyn Status_Waiting $ leftmost
    [ Status_Working <$ initCont
    , Status_Failed <$ contError
    , Status_Done <$ contOk
    ]

  es@(resultOk, resultErr, retry) <- case mtoGasPayer of
    Nothing -> do
      dialogSectionHeading mempty "Notice: Cannot finish cross-chain transfer"
      divClass "group" $ text "No gas payer specified on destination chain"
      return (() <$ contResponse, never, never)
    Just toGasPayer -> do
      -- Get the proof
      spvResponse <- getSPVProof logL nodeInfos fromChain toChain contOk
      let (spvError, spvOk) = fanEither spvResponse
      -- Run continuation on target chain
      continueResponse <- continueCrossChainTransfer logL networkName envToChain publicMeta keys toChain toGasPayer spvOk
      let (continueError, continueOk) = fanEither continueResponse
      -- Wait for result
      resultResponse <- pollNodesForSuccess logL envToChain continueOk
      let (resultError, resultOk) = fanEither resultResponse

      spvStatus <- holdDyn Status_Waiting $ leftmost
        [ Status_Waiting <$ initCont
        , Status_Working <$ contOk
        , Status_Failed <$ spvError
        , Status_Done <$ spvOk
        ]
      continueStatus <- holdDyn Status_Waiting $ leftmost
        [ Status_Waiting <$ (void contOk <> void initCont)
        , Status_Working <$ spvOk
        , Status_Failed <$ continueError
        , Status_Done <$ continueOk
        ]
      resultStatus <- holdDyn Status_Waiting $ leftmost
        [ Status_Working <$ continueOk
        , Status_Failed <$ resultError
        , Status_Done <$ resultOk
        ]

      let item ds txt = elDynAttr "li" (ffor ds $ \s -> "class" =: statusText s)
                          $ el "p" $ text txt

      item contStatus "Got continuation response"
      item spvStatus "SPV proof retrieved"
      item continueStatus $ "Initiate claiming coins on chain " <> _chainId toChain
      item resultStatus "Coins retrieved on target chain"

      anyError <- holdUniqDyn $ any (== Status_Failed) <$> sequence
        [ contStatus
        , spvStatus
        , continueStatus
        , resultStatus
        ]

      retry <- switchHold never <=< dyn $ ffor anyError $ \case
        False -> pure never
        True -> uiButton (btnCfgPrimary & uiButtonCfg_class .~ "cross-chain-transfer-retry")  $ text "Retry"

      pure ( resultOk
           , leftmost [contError, spvError, continueError, resultError]
           , retry
           )
  pure es

-- -- | Configuration step before finishing a previously unfinished cross chain
-- -- transfer.
-- finishCrossChainTransferConfig
--   :: ( PerformEvent t m, PostBuild t m, TriggerEvent t m, MonadHold t m, DomBuilder t m
--      , GhcjsDomSpace ~ DomBuilderSpace m, MonadJSM m
--      , MonadJSM (Performable m), MonadFix m
--      , HasCrypto key (Performable m)
--      , Monoid mConf, HasWalletCfg mConf key t
--      , HasNetworkCfg mConf t
--      , HasNetwork model t
--      , HasWallet model key t
--      , HasLogger model t
--      , HasTransactionLogger m
--      )
--   => model
--   -> (AccountName, ChainId)
--   -- ^ From account
--   -> UnfinishedCrossChainTransfer
--   -- ^ The unfinished transfer
--   -> Workflow t m (mConf, Event t ())
-- finishCrossChainTransferConfig model fromAccount ucct = Workflow $ do
--   let requestKey = _unfinishedCrossChainTransfer_requestKey ucct
--       toChain = _unfinishedCrossChainTransfer_recipientChain ucct
--       fromChain = snd fromAccount
--   close <- modalHeader $ text "Cross chain transfer: unfinished transfer"
--   (sender, conf) <- divClass "modal__main" $ do
--     el "p" $ text "It looks like you started a cross chain transfer which did not complete correctly."
--     dialogSectionHeading mempty  "Transaction Details"
--     divClass "group" $ do
--       mkLabeledInput True "Request Key" uiInputElement $ def
--         & initialAttributes .~ "disabled" =: "disabled"
--         & inputElementConfig_initialValue .~ Pact.requestKeyToB16Text requestKey
--       mkLabeledInput True "Recipient Chain" uiInputElement $ def
--         & initialAttributes .~ "disabled" =: "disabled"
--         & inputElementConfig_initialValue .~ _chainId toChain
--       mkLabeledInput True "Recipient Account" uiInputElement $ def
--         & initialAttributes .~ "disabled" =: "disabled"
--         & inputElementConfig_initialValue .~ unAccountName (_unfinishedCrossChainTransfer_recipientAccount ucct)
--       mkLabeledInput True "Amount" (uiGasPriceInputField never) $ def
--         & initialAttributes .~ "disabled" =: "disabled"
--         & inputElementConfig_initialValue .~ tshow (_unfinishedCrossChainTransfer_amount ucct)
--     el "p" $ text "The coin has been debited from your account but hasn't been redeemed by the recipient."
--     dialogSectionHeading mempty "Transaction Settings"
--     (conf, _, _, _) <- divClass "group" $ uiMetaData model Nothing Nothing
--     dialogSectionHeading mempty "Gas Payer (recipient chain)"
--     sender <- divClass "group" $ elClass "div" "segment segment_type_tertiary labeled-input" $ do
--       divClass "label labeled-input__label" $ text "Account Name"
--       let cfg = def & dropdownConfig_attributes .~ pure ("class" =: "labeled-input__input select select_mandatory_missing")
--           chain = pure $ Just toChain
--       gasAcc <- uiAccountDropdown cfg (pure $ \_ a -> fromMaybe False (accountHasFunds a)) (pure id) model chain never
--       pure $ ffor3 (model ^. wallet_accounts) (model ^. network_selectedNetwork) gasAcc $ \netToAccount net ma -> do
--         accounts <- Map.lookup net $ unAccountData netToAccount
--         n <- ma
--         AccountInfo _ chains <- Map.lookup (fst n) accounts
--         guard $ Map.member fromChain chains
--         pure n
--     pure (sender, conf)
--   next <- modalFooter $ confirmButton (def & uiButtonCfg_disabled .~ fmap isNothing sender) "Next"
--   let nextScreen = flip push next $ \() -> do
--         mNetInfo <- sampleNetInfo model
--         mToGasPayer <- sample $ current sender
--         keys <- sample $ current $ model ^. wallet_keys
--         pure $ ffor2 mNetInfo mToGasPayer $ \ni gp -> finishCrossChainTransfer (model ^. logger) ni keys fromAccount ucct (second _account_status gp)
--   pure ((conf, close), nextScreen)

-- | Handy function for getting network / meta information in 'PushM'. Type
-- monomorphised to prevent accidentally sampling outside of 'push'
sampleNetInfo
  :: (Reflex t, HasNetwork model t)
  => model -> PushM t (Maybe (SharedNetInfo NodeInfo))
sampleNetInfo model = do
  net <- sample $ current $ model ^. network_selectedNetwork
  nodes <- fmap rights $ sample $ current $ model ^. network_selectedNodes
  meta <- sample $ current $ model ^. network_meta
  let networkName = mkNetworkName . nodeVersion <$> headMay nodes
  pure $ ffor networkName $ \name -> SharedNetInfo
    { _sharedNetInfo_network = name
    , _sharedNetInfo_nodes = nodes
    , _sharedNetInfo_meta = meta
    , _sharedNetInfo_selectedNetwork = net
    }

-- | Modal for finishing previously unfinished cross chain transfers
-- finishCrossChainTransfer
--   :: ( PerformEvent t m, PostBuild t m, TriggerEvent t m, MonadHold t m, DomBuilder t m
--      , MonadJSM (Performable m), MonadFix m
--      , HasCrypto key (Performable m)
--      , Monoid mConf, HasWalletCfg mConf key t
--      , HasTransactionLogger m
--      )
--   => Logger t
--   -> SharedNetInfo NodeInfo
--   -> KeyStorage key
--   -> (AccountName, ChainId)
--   -- ^ From account
--   -> UnfinishedCrossChainTransfer
--   -- ^ The unfinished transfer
--   -> (AccountName, AccountStatus AccountDetails)
--   -- ^ The account which pays the gas on the recipient chain
--   -> Workflow t m (mConf, Event t ())
-- finishCrossChainTransfer logL netInfo keys (fromName, fromChain) ucct toGasPayer = Workflow $ do
--   close <- modalHeader $ text "Cross chain transfer"
--   let toChain = _unfinishedCrossChainTransfer_recipientChain ucct
--       requestKey = _unfinishedCrossChainTransfer_requestKey ucct
--   resultOk <- divClass "modal__main" $ do
--     dialogSectionHeading mempty "Transaction Status"

--     (resultOk0, errMsg, retry) <- divClass "group" $ do
--       elClass "ol" "transaction_status" $ do
--         let item ds = elDynAttr "li" (ffor ds $ \s -> "class" =: statusText s)
--         item (pure Status_Done) $ el "p" $
--           text $ "Cross chain transfer initiated on chain " <> _chainId fromChain
--         pb <- getPostBuild
--         runUnfinishedCrossChainTransfer logL netInfo keys fromChain toChain (Just toGasPayer) $ requestKey <$ pb

--     dialogSectionHeading mempty "Transaction Result"
--     divClass "group" $ do
--       void $ runWithReplace (text . ("Request Key " <>) $ Pact.requestKeyToB16Text requestKey) $ leftmost
--         [ text <$> errMsg
--         , blank <$ retry
--         ]

--     pure resultOk0

--   (abandon, done) <- modalFooter $ do
--     (abandon, _) <- runWithReplace (uiButton btnCfgSecondary $ text "Abandon Transfer") (blank <$ resultOk)
--     done <- confirmButton def "Done"
--     pure (abandon, done)

--   let
--     cctDetails = (_sharedNetInfo_selectedNetwork netInfo, fromName, fromChain, Nothing)
--     conf = mempty & walletCfg_setCrossChainTransfer .~ (cctDetails <$ (resultOk <> abandon))

--   pure ( ( conf
--          , close <> done <> abandon
--          )
--        , never
--        )

-- | Workflow for doing cross chain transfers from scratch. Steps are roughly:
--
-- 1. Initiate a cross chain transfer on the sender chain.
--  a. *where we know the guard*, i.e. we own the recipient account
--  b. *where we don't know the guard* i.e. it's someone elses account
--     In this case, look up the guard using `coin.details` on the recipient chain.
-- 2. Send off for an SPV proof. Should be done after the network has caught up
-- with the initiation.
-- 3. Send a continuation to the recipient chain.
--
-- It's important to treat 1a and 1b separately, because the recipient account
-- might not exist. In the second case, this will fail the entire transfer.
-- However, if we know the guard, we can skip the lookup and initiate the
-- crosschain transfer - and the account will be created for us automatically.
-- crossChainTransfer
--   :: ( PerformEvent t m, PostBuild t m, TriggerEvent t m, MonadHold t m, DomBuilder t m
--      , MonadJSM (Performable m), MonadJSM m, MonadFix m
--      , HasCrypto key m, HasCrypto key (Performable m)
--      , Monoid mConf, HasWalletCfg mConf key t
--      , HasTransactionLogger m
--      )
--   => Logger t
--   -> SharedNetInfo NodeInfo
--   -> KeyStorage key
--   -> (AccountName, ChainId, AccountDetails)
--   -- ^ From account
--   -> Either TxBuilder (AccountName, ChainId, Account)
--   -- ^ To address/account. We pass in a full 'Account' if we have one, such that
--   -- we can avoid looking up the keyset.
--   -> (AccountName, Account)
--   -- ^ Gas payer for 'from' chain
--   -> CrossChainData
--   -- ^ Cross chain specific info
--   -> Decimal
--   -- ^ Amount to transfer
--   -> Workflow t m (mConf, Event t ())
-- crossChainTransfer logL netInfo keys fromAccount toAccount fromGasPayer crossChainData amount = Workflow $ do
--   let nodeInfos = _sharedNetInfo_nodes netInfo
--       networkName = _sharedNetInfo_network netInfo
--       publicMeta = _sharedNetInfo_meta netInfo
--   close <- modalHeader $ text "Cross chain transfer"
--   pb <- getPostBuild
--   let (fromName, fromChain, _) = fromAccount
--       toChain = either _txBuilder_chainId (view _2) toAccount
--       toTxBuilder = either id (\(n, c, _) -> TxBuilder n c Nothing) toAccount
--   -- Client envs for making requests to each chain
--   let envFromChain = mkClientEnvs nodeInfos fromChain
--   -- TODO *always* look up the guard and check the validity
--   -- Lookup the guard if we don't already have it
--   keySetResponse <- case toAccount of
--     Right (_name, _chain, acc)
--       | Just ks <- acc ^? account_status
--           . _AccountStatus_Exists
--           . accountDetails_guard
--           . _AccountGuard_KeySetLike
--           . to toPactKeyset

--       -> pure $ Right ks <$ pb
--       | otherwise -> lookupKeySet logL networkName nodeInfos
--                        (_txBuilder_chainId toTxBuilder)
--                        (_txBuilder_accountName toTxBuilder)
--     Left ka -> case _txBuilder_keyset ka of
--       Nothing -> lookupKeySet logL networkName nodeInfos
--                    (_txBuilder_chainId ka)
--                    (_txBuilder_accountName ka)
--       -- If the account hasn't been created, don't try to lookup the guard. Just
--       -- assume the account name _is_ the public key (since it must be a
--       -- non-vanity account).
--       Just ks -> pure $ Right ks <$ pb -- TODO verify against chain
--   let (keySetError, keySetOk) = fanEither keySetResponse
--   -- Start the transfer
--   initiated <- initiateCrossChainTransfer logL networkName envFromChain publicMeta keys fromAccount fromGasPayer toTxBuilder amount keySetOk
--   let (initiatedError, initiatedOk) = fanEither initiated
--   initiateStatus <- holdDyn Status_Waiting $ leftmost
--     [ Status_Working <$ keySetOk
--     , Status_Failed <$ initiatedError
--     , Status_Done <$ initiatedOk
--     ]
--   resultOk <- divClass "modal__main" $ do
--     dialogSectionHeading mempty "Transaction Status"

--     (resultOk0, errMsg0, retry0) <- divClass "group" $ do
--       elClass "ol" "transaction_status" $ do
--         let item ds = elDynAttr "li" (ffor ds $ \s -> "class" =: statusText s)
--         item initiateStatus $
--           el "p" $ text $ "Cross chain transfer initiated on chain " <> _chainId fromChain

--         let toGasPayer = second _account_status $ _crossChainData_recipientChainGasPayer crossChainData
--         runUnfinishedCrossChainTransfer logL netInfo keys fromChain toChain (Just toGasPayer) initiatedOk

--     let errMsg = leftmost [keySetError, initiatedError, errMsg0]
--     dialogSectionHeading mempty "Transaction Result"
--     divClass "group" $ do
--       void $ runWithReplace (text "Waiting for response...") $ leftmost
--         [ text . ("Request Key " <>) . Pact.requestKeyToB16Text <$> initiatedOk
--         , text <$> errMsg
--         , blank <$ retry0
--         ]

--     pure resultOk0
--   done <- modalFooter $ confirmButton def "Done"
--   let mkUCCT requestKey = (_sharedNetInfo_selectedNetwork netInfo, fromName, fromChain, Just UnfinishedCrossChainTransfer
--         { _unfinishedCrossChainTransfer_requestKey = requestKey
--         , _unfinishedCrossChainTransfer_recipientChain = toChain
--         , _unfinishedCrossChainTransfer_recipientAccount = _txBuilder_accountName toTxBuilder
--         , _unfinishedCrossChainTransfer_amount = amount
--         })
--   let conf = mempty
--         & walletCfg_setCrossChainTransfer .~ leftmost
--           [ fmap mkUCCT initiatedOk
--           , (_sharedNetInfo_selectedNetwork netInfo, fromName, fromChain, Nothing) <$ resultOk
--           ]
--   pure ((conf, close <> done), never)

-- | Continue the transfer on the target chain
continueCrossChainTransfer
  :: ( TriggerEvent t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , HasCrypto key (Performable m)
     , HasTransactionLogger m
     )
  => Logger t
  -> NetworkName
  -- ^ Which network we are on
  -> [S.ClientEnv]
  -- ^ Envs pointing at the target chain
  -> PublicMeta
  -- ^ Meta info (gas settings)
  -> KeyStorage key
  -- ^ Keys
  -> ChainId
  -- ^ To chain
  -> (AccountName, AccountStatus AccountDetails)
  -- ^ Gas payer on target chain
  -> Event t (Pact.PactExec, Pact.ContProof)
  -- ^ The previous continuation step and associated proof
  -> m (Event t (Either Text (Pact.PactId, Pact.RequestKey)))
continueCrossChainTransfer logL networkName envs publicMeta keys toChain gasPayer spvOk = do
  transactionLogger <- askTransactionLogger
  performEventAsync $ ffor spvOk $ \(pe, proof) cb -> do
    let
      sender = unAccountName $ fst gasPayer

      pm = publicMeta
        { _pmChainId = toChain
        , _pmSender = sender
        }
      signingSet = snd gasPayer ^. _AccountStatus_Exists . accountDetails_guard . _AccountGuard_KeySetLike . ksh_keys
      signingPairs = filterKeyPairs signingSet keys
    payload <- buildContPayload networkName pm signingPairs $ ContMsg
      { _cmPactId = Pact._pePactId pe
      , _cmStep = succ $ Pact._peStep pe
      , _cmRollback = False
      , _cmData = Aeson.Object mempty
      , _cmProof = Just proof
      }
    cont <- buildCmdWithPayload payload signingPairs
    putLog logL LevelWarn "transfer-crosschain: running continuation on target chain"
    putLog logL LevelWarn $ tshow cont
    -- We can't do a /local with continuations :(
    liftJSM $ forkJSM $ do
      r <- doReqFailover envs (Api.send Api.apiV1Client transactionLogger sender toChain $ Api.SubmitBatch $ pure cont) >>= \case
        Left es -> packHttpErrors logL es
        Right (Api.RequestKeys (requestKey :| _)) -> pure $ Right (Pact._pePactId pe, requestKey)
      liftIO $ cb r
    pure ()

-- | Lookup the keyset of an account
lookupKeySet
  :: (HasCrypto key m, TriggerEvent t m, MonadJSM m)
  => Logger t
  -> NetworkName
  -- ^ Which network we are on
  -> [NodeInfo]
  -- ^ Envs which point to the appropriate chain
  -> ChainId
  -> AccountName
  -- ^ Account on said chain to find
  -> m (Event t (Either Text Pact.KeySet))
lookupKeySet logL networkName nodes chainId accountName = do
  cmd <- mkCoinDetailsCmd networkName chainId accountName
  (result, trigger) <- newTriggerEvent
  let envs = mkClientEnvs nodes chainId
  liftJSM $ forkJSM $ do
    r <- doReqFailover envs (Api.local Api.apiV1Client cmd) >>= \case
      Left es -> packHttpErrors logL es
      Right cr -> case Pact._crResult cr of
        Pact.PactResult (Right (PObject (Pact.ObjectMap m)))
          | Just (PGuard (Pact.GKeySet keySet)) <- Map.lookup "guard" m
          -> pure $ Right keySet
        Pact.PactResult (Right v) -> do
          putLog logL LevelWarn $ "lookupKeySet: Failed to retrieve the recipient's account guard: " <> tshow v
          pure $ Left "Failed to retrieve the recipient's account guard"
        Pact.PactResult (Left e) -> do
          putLog logL LevelWarn $ "Received error for details: " <> tshow e
          pure $ Left "Failed to retrieve the recipient's account guard"
    liftIO $ trigger r
  pure result

-- | Lookup the keyset of an account
-- | Initiate a cross chain transfer on the sender chain.
-- initiateCrossChainTransfer
--   :: ( MonadJSM (Performable m)
--      , TriggerEvent t m
--      , PerformEvent t m
--      , HasCrypto key (Performable m)
--      , HasLogger model t
--      , HasTransactionLogger m
--      )
--   => model
--   -> NetworkName
--   -- ^ Network name
--   -> [S.ClientEnv]
--   -- ^ Where to send requests
--   -> PublicMeta
--   -- ^ Meta info - really only interested in gas settings
--   -> KeyStorage key
--   -- ^ Keys
--   -> (AccountName, ChainId, AccountDetails)
--   -- ^ From account
--   -> (AccountName, Account)
--   -- ^ Gas payer ("from" chain)
--   -> TxBuilder
--   -- ^ Recipient address
--   -> Decimal
--   -- ^ Amount to transfer
--   -> Event t Pact.KeySet
--   -- ^ Recipient keyset
--   -> m (Event t (Either Text Pact.RequestKey))
-- initiateCrossChainTransfer model networkName envs publicMeta keys fromAccount fromGasPayer toAccount amount eks = do
--   transactionLogger <- askTransactionLogger
--   performEventAsync $ ffor eks $ \rg cb -> do
--     cmd <- buildCmd Nothing networkName pm signingPairs [] code (mkDat rg) capabilities
--     liftJSM $ forkJSM $ do
--       r <- doReqFailover envs (Api.local Api.apiV1Client cmd) >>= \case
--         Left es -> packHttpErrors (model ^. logger) es
--         Right cr -> case Pact._crResult cr of
--           Pact.PactResult (Left e) -> do
--             putLog model LevelError (tshow e)
--             pure $ Left $ tshow e
--           Pact.PactResult (Right _) -> do
--             r <- doReqFailover envs $ Api.send Api.apiV1Client transactionLogger senderText toChain
--               $ Api.SubmitBatch $ pure cmd
--             case r of
--               Left es -> packHttpErrors (model ^. logger) es
--               Right (Api.RequestKeys (requestKey :| _)) -> pure $ Right requestKey

--       liftIO $ cb r
--   where
--     fromAccKeys = fromAccount ^. _3 . accountDetails_guard . _AccountGuard_KeySetLike . ksh_keys
--     keysetName = "receiverKey"
--     code = T.unwords
--       [ "(coin.transfer-crosschain"
--       , tshow $ unAccountName $ view _1 fromAccount
--       , tshow $ unAccountName $ _txBuilder_accountName toAccount
--       , "(read-keyset '" <> keysetName <> ")"
--       , tshow $ _chainId $ _txBuilder_chainId toAccount
--       , tshow $ addDecimalPoint amount
--       , ")"
--       ]
--     mkDat rg = HM.singleton keysetName $ Aeson.toJSON rg
--     signingSet = Set.unions [fromAccKeys, accountKeys $ snd fromGasPayer]
--     signingPairs = filterKeyPairs signingSet keys
--     -- This capability is required for `transfer-crosschain`.
--     debitCap = SigCapability
--       { _scName = QualifiedName { _qnQual = "coin", _qnName = "DEBIT", _qnInfo = def }
--       , _scArgs = [PLiteral $ LString $ unAccountName $ view _1 fromAccount]
--       }
--     capabilities = Map.unionsWith (<>)
--       [ Map.fromSet (\_ -> [_dappCap_cap defaultGASCapability]) (accountKeys $ snd fromGasPayer)
--       , Map.fromSet (\_ -> [debitCap]) fromAccKeys
--       ]

--     senderText = unAccountName $ fst fromGasPayer
--     toChain = view _2 fromAccount

--     pm = publicMeta
--       { _pmChainId = toChain
--       , _pmSender = senderText
--       }

packHttpErrors :: MonadIO m => Logger t -> [S.ClientError] -> m (Either Text a)
packHttpErrors logL es = do
  let prettyErr = T.unlines $ fmap (tshow . packHttpErr) es
  putLog logL LevelWarn prettyErr
  pure $ Left prettyErr

-- |Poll a request key for the second step (redeeming coin on target chain)
pollNodesForSuccess
  :: ( TriggerEvent t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , HasLogger model t
     )
  => model
  -> [S.ClientEnv]
  -> Event t (Pact.PactId, Pact.RequestKey)
  -> m (Event t (Either Text ()))
pollNodesForSuccess model envs reqEv = pollNodesForReq model envs (first Just <$> reqEv) 24 15 $ \pr mpid->
  case Pact._crResult pr of
    Pact.PactResult (Left pe)
      -- There doesn't seem to be a nicer way to do this check
      | (completedMsg mpid) == tshow (peDoc pe) -> Right ()
      | otherwise -> Left $ tshow $ peDoc pe
    Pact.PactResult (Right _) -> Right ()
   where
     completedMsg :: Maybe Pact.PactId -> Text
     completedMsg Nothing = ""
     completedMsg (Just (PactId pactId)) = "resumePact: pact completed: " <> pactId

-- |Poll a request key for some continuation
pollNodesForCont
  :: ( TriggerEvent t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , HasLogger model t
     )
  => model
  -> [S.ClientEnv]
  -> Event t Pact.RequestKey
  -> m (Event t (Either Text (Pact.RequestKey, Pact.PactExec)))
pollNodesForCont model envs reqEv = pollNodesForReq model envs ((Nothing,) <$> reqEv) 24 15 $ \cr _->
  case Pact._crContinuation cr of
    Nothing ->
      Left $ T.unlines ["Result was not a continuation", tshow (Pact._crResult cr)]
    Just pe ->
      Right (Pact._crReqKey cr, pe)

data PollingAttempt a =
    PollingAttempt_Error Text  -- Servant.ClientError
  | PollingAttempt_NotFound
  | PollingAttempt_Success a
  deriving (Eq)

pollNodesForReq
  :: ( TriggerEvent t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , HasLogger model t
     )
  => model
  -> [S.ClientEnv]
  -> Event t (Maybe Pact.PactId, Pact.RequestKey)
  -> Int --max polls
  -> Int -- delay between polls (seconds)
  -> (Pact.CommandResult Hash -> Maybe Pact.PactId -> Either Text a) -- callback for handling poll result
  -> m (Event t (Either Text a))
pollNodesForReq model envs requestPair maxPolls waitTime handler =
  performEventAsync $ ffor requestPair $ \(mPid, rk) cb -> liftJSM $ forkJSM $
    let
      getHttpErrors es = T.unlines $ fmap (tshow . packHttpErr) es

      pollReq = Api.poll Api.apiV1Client $ Api.Poll (rk :| [])

      pollNodesSingleRound = ffor (doReqFailover envs pollReq) $ \case
        Left es -> PollingAttempt_Error $ getHttpErrors es
        Right (Api.PollResponses m) -> case HM.toList m of
          -- Not Found, poll again
          [] -> PollingAttempt_NotFound
          (_,cr):_ -> PollingAttempt_Success $ handler cr mPid

      pollNodes iterations
        | iterations <= 0 = liftIO $ cb $ Left "Polling timeout"
        | otherwise = pollNodesSingleRound >>= \case
            PollingAttempt_Success r -> do
              putLog (model^.logger) LevelInfo "Successful polling attempt"
              liftIO $ cb r
            --Requests failed on all nodes
            PollingAttempt_Error e -> do
              putLog (model^.logger) LevelWarn e
              liftIO $ cb $ Left e
            --Polling came back empty
            PollingAttempt_NotFound -> do
              liftIO $ threadDelay $ waitTime * 1000 * 1000
              putLog (model^.logger) LevelWarn $ "Polling iteration " <> tshow (maxPolls - iterations) <> " / "<> tshow maxPolls <> " unsuccessful. Trying again in " <> tshow waitTime <> " seconds"
              pollNodes $ iterations - 1
     in pollNodes maxPolls

-- | Using the given nodes, attempt to get an SPV proof for
-- the request key. Polls every X seconds for the proof, repeatedly going
-- through the nodes until we get a response.
getSPVProof
  :: ( TriggerEvent t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , HasLogger model t
     )
  => model
  -> [NodeInfo]
  -- ^ Nodes to try
  -> ChainId
  -- ^ This chain
  -> ChainId
  -- ^ Target chain
  -> Event t (Pact.RequestKey, Pact.PactExec)
  -- ^ Request key of continuation. 'PactExec' is passed through.
  -> m (Event t (Either Text (Pact.PactExec, Pact.ContProof)))
getSPVProof model nodeInfos thisChain targetChain e = performEventAsync $ ffor e $ \(requestKey, pe) cb -> do
  liftJSM $ forkJSM $ do
    proof <- failover $ take maxPolls $ cycle $ spvRequests requestKey
    liftIO . cb $ (,) pe <$> proof
  where
    maxPolls = 12 -- Wait up to 6 blocks for the SPV proof
    waitTime = 15
    chainUrls = getChainBaseUrl thisChain <$> nodeInfos
    -- Can't use chainweb-node's definition to generate a client, it won't build on GHCJS.
    -- Maybe we can split the API bits out later.
    spvRequests requestKey = ffor chainUrls $ \u -> postJson (URI.render u <> "/spv") $ Aeson.object
      -- Essentially 'SpvRequest' from chainweb
      [ "requestKey" Aeson..= requestKey
      , "targetChainId" Aeson..= targetChain
      ]
    failover [] = do
      putLog model LevelWarn $ "No proof found after " <> tshow (maxPolls * waitTime) <> " seconds"
      pure $ Left "Failed to get SPV proof.  Save the request key and try to finish the cross-chain transfer later."
    failover (req : rs) = do
      putLog model LevelWarn $ "Sending SPV Request: " <> tshow req
      m <- liftIO newEmptyMVar
      void $ newXMLHttpRequest req (liftIO . putMVar m)
      resp <- liftIO $ takeMVar m
      let s = _xhrResponse_status resp
      putLog model LevelWarn $ "Got SPV Response " <> tshow s <> ": " <> tshow (_xhrResponse_responseText resp)
      case s of
        200 -> pure $ maybe (Left "Couldn't decode response") Right $ decodeXhrResponse resp
        _ -> do
          -- Wait 15 seconds. We should improve this to wait instead of poll,
          -- but this is simple and it works
          liftIO $ threadDelay $ waitTime * 1000 * 1000
          case s of
            400 -- If the TX isn't reachable yet. This is fragile if chainweb changes the response message under us.
              | Just "SPV target not reachable: Target of SPV proof can't be reached from the source transaction" <- _xhrResponse_responseText resp
              -> failover (rs ++ [req]) -- In this case we just need to wait a little longer. Push this request back to the end of the list
            _ -> failover rs

data SendModalTab
  = SendModalTab_Configuration
  | SendModalTab_Sign
  deriving (Eq, Ord, Show, Enum, Bounded)

displaySendModalTab :: DomBuilder t m => SendModalTab -> m ()
displaySendModalTab = text . \case
  SendModalTab_Configuration -> "Configuration"
  SendModalTab_Sign -> "Sign"

makeTabs
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => InitialTransferData -> Event t SendModalTab -> m (Dynamic t SendModalTab, Event t ())
makeTabs initData tabEv = do
  -- We assume that if there is a full transfer that we should head to the sign tab
  let initTab = initialTransferDataCata (const $ const SendModalTab_Configuration) (const SendModalTab_Sign) initData
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
