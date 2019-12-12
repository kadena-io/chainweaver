{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Dialogs for sending money between accounts
-- Copyright   :  (C) 2019 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.Send
  ( uiSendModal
  ) where

import Control.Monad.Trans.Maybe
import Control.Applicative (liftA2)
import Control.Concurrent
import Control.Error.Util (hush)
import Control.Lens hiding (failover)
import Control.Monad (join, when, void, (<=<))
import Control.Monad.Trans.Except
import Data.Bifunctor (first)
import Data.Decimal (Decimal)
import Data.Either (isLeft, rights)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (First(..))
import Data.Text (Text)
import Kadena.SigningApi
import Pact.Types.Capability
import Pact.Types.ChainMeta
import Pact.Types.Exp
import Pact.Types.PactError
import Pact.Types.Names
import Pact.Types.PactValue
import Pact.Types.Runtime (GasPrice (..))
import Pact.Parse (ParsedDecimal (..))
import Pact.Types.RPC
import Pact.Types.Term
import Reflex
import Reflex.Dom
import Safe (succMay, headMay)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HM
import qualified Data.IntMap as IntMap
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Pact.Server.ApiV1Client as Api
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
import Frontend.KadenaAddress
import Frontend.Network
import Frontend.UI.DeploymentSettings
import Frontend.UI.Dialogs.DeployConfirmation (statusText, Status(..))
import Frontend.UI.Dialogs.DeployConfirmation (submitTransactionWithFeedback)
import Frontend.UI.Modal
import Frontend.UI.TabBar
import Frontend.UI.Widgets
import Frontend.Wallet

-- | A modal for handling sending coin
uiSendModal
  :: (SendConstraints model mConf key t m, Flattenable mConf t)
  => model -> IntMap.Key -> Account key -> Event t () -> m (mConf, Event t ())
uiSendModal model fromIndex sender _onCloseExternal = do
  (conf, closes) <- fmap splitDynPure $ workflow $ case _account_unfinishedCrossChainTransfer sender of
    Nothing -> sendConfig model fromIndex sender
    -- If we have unfinished business, force the user to finish it first
    Just ucct -> finishCrossChainTransferConfig model fromIndex sender ucct
  mConf <- flatten =<< tagOnPostBuild conf
  let close = switch $ current closes
  pure (mConf, close)

type SendConstraints model mConf key t m
  = ( Monoid mConf, HasNetwork model t, HasNetworkCfg mConf t, HasWallet model key t, HasWalletCfg mConf key t
    , MonadWidget t m, PostBuild t m, HasCrypto key m
    , HasCrypto key (Performable m)
    )

-- | Data which is only required for cross chain transfers
data CrossChainData key = CrossChainData
  { _crossChainData_recipientChainGasPayer :: Account key
  -- ^ The account which will be paying the gas on the recipient chain
  }

-- | This stuff was being passed around so much it became a new type
data SharedNetInfo a = SharedNetInfo
  { _sharedNetInfo_network :: NetworkName
  , _sharedNetInfo_meta :: PublicMeta
  , _sharedNetInfo_nodes :: [a]
  }

-- | Preview the transfer. Doesn't actually send any requests.
previewTransfer
  :: forall model mConf key t m.
     SendConstraints model mConf key t m
  => model
  -> IntMap.Key
  -- ^ Key of the "from" account
  -> Account key
  -- ^ From account
  -> Account key
  -- ^ Gas payer on "from" chain
  -> KadenaAddress
  -- ^ To address
  -> Maybe (CrossChainData key)
  -- ^ Cross chain data, if applicable
  -> Decimal
  -- ^ Amount to transfer
  -> Workflow t m (mConf, Event t ())
previewTransfer model fromIndex fromAccount fromGasPayer toAddress crossChainData amount = Workflow $ do
  close <- modalHeader $ text "Transaction Preview"
  elClass "div" "modal__main" $ do
    elClass "h2" "heading heading_type_h2" $ text "Destination"
    divClass "group" $ transactionDisplayNetwork model
    elClass "h2" "heading heading_type_h2" $ text "Participants"
    divClass "group" $ do
      mkLabeledInput True "Sender Account" uiInputElement $ def
        & initialAttributes .~ "disabled" =: "disabled"
        & inputElementConfig_initialValue .~ unAccountName (_account_name fromAccount)
      let gasLabel = "Gas Payer" <> case crossChainData of
            Nothing -> ""
            Just _ -> " (Chain " <> _chainId (_account_chainId fromAccount) <> ")"
      mkLabeledInput True gasLabel uiInputElement $ def
        & initialAttributes .~ "disabled" =: "disabled"
        & inputElementConfig_initialValue .~ unAccountName (_account_name fromGasPayer)
      mkLabeledInput True "Recipient Account" uiInputElement $ def
        & initialAttributes .~ "disabled" =: "disabled"
        & inputElementConfig_initialValue .~ unAccountName (_kadenaAddress_accountName toAddress)
      for_ crossChainData $ \ccd -> do
        let toChain = _kadenaAddress_chainId toAddress
        mkLabeledInput True ("Gas Payer (Chain " <> _chainId toChain <> ")") uiInputElement $ def
          & initialAttributes .~ "disabled" =: "disabled"
          & inputElementConfig_initialValue .~ unAccountName (_account_name $ _crossChainData_recipientChainGasPayer ccd)
    elClass "h2" "heading heading_type_h2" $ text "Transaction Details"
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
        [ sendConfig model fromIndex fromAccount <$ back
        , flip push next $ \() -> do
          mNetInfo <- sampleNetInfo model
          accounts <- sample $ current $ model ^. wallet_accounts
          let toAccount = maybe (Left toAddress) Right $ lookupAccountByKadenaAddress toAddress accounts
          pure $ ffor mNetInfo $ \n -> case crossChainData of
            Nothing -> sameChainTransfer n fromAccount fromGasPayer toAddress amount
            Just ccd -> crossChainTransfer n fromIndex fromAccount toAccount fromGasPayer ccd amount
        ]
  pure ((mempty, close), nextScreen)

lookupAccountByKadenaAddress :: KadenaAddress -> Accounts key -> Maybe (Account key)
lookupAccountByKadenaAddress ka = fmap getFirst . foldMap f
  where f = \case
          SomeAccount_Account a | accountToKadenaAddress a (_account_chainId a) == ka -> Just $ First a
          _ -> Nothing

-- | Perform a same chain transfer or transfer-create
sameChainTransfer
  :: (MonadWidget t m, HasCrypto key m, Monoid mConf)
  => SharedNetInfo NodeInfo
  -> Account key
  -- ^ From account
  -> Account key
  -- ^ Gas payer
  -> KadenaAddress
  -- ^ Recipient account
  -> Decimal
  -- ^ Amount to transfer
  -> Workflow t m (mConf, Event t ())
sameChainTransfer netInfo fromAccount gasPayer toAccount amount = Workflow $ do
  let recipientCreated = _kadenaAddress_accountCreated toAccount
  let code = T.unwords $
        [ "(coin." <> case recipientCreated of
          AccountCreated_Yes -> "transfer"
          AccountCreated_No -> "transfer-create"
        , tshow $ unAccountName $ _account_name fromAccount
        , tshow $ unAccountName $ _kadenaAddress_accountName toAccount
        , case recipientCreated of
          AccountCreated_Yes -> mempty
          AccountCreated_No -> "(read-keyset 'key)"
        , tshow amount
        , ")"
        ]
      signingPairs = L.nubBy (\x y -> _keyPair_publicKey x == _keyPair_publicKey y) [_account_key fromAccount, _account_key gasPayer]
      transferCap = SigCapability
        { _scName = QualifiedName { _qnQual = "coin", _qnName = "TRANSFER", _qnInfo = def }
        , _scArgs =
          [ PLiteral $ LString $ unAccountName $ _account_name fromAccount
          , PLiteral $ LString $ unAccountName $ _kadenaAddress_accountName toAccount
          , PLiteral $ LDecimal amount
          ]
        }
      dat = case _kadenaAddress_accountCreated toAccount of
        AccountCreated_No
          -- This makes the assumption that the only non-created accounts are
          -- non-vanity: that is, the name is the public key.
          | Right pk <- parsePublicKey (unAccountName $ _kadenaAddress_accountName toAccount)
          -> HM.singleton "key" $ Aeson.toJSON $ KeySet [toPactPublicKey pk] (Name $ BareName "keys-all" def)
        _ -> mempty
      pkCaps = Map.unionsWith (<>)
        [ Map.singleton (_keyPair_publicKey $ _account_key gasPayer) [_dappCap_cap defaultGASCapability]
        , Map.singleton (_keyPair_publicKey $ _account_key fromAccount) [transferCap]
        ]
      pm = (_sharedNetInfo_meta netInfo)
        { _pmChainId = _account_chainId fromAccount
        , _pmSender = unAccountName $ _account_name gasPayer
        }
      nodeInfos = _sharedNetInfo_nodes netInfo
      networkName = _sharedNetInfo_network netInfo
  close <- modalHeader $ text "Transaction Status"
  cmd <- buildCmd Nothing networkName pm signingPairs [] code dat pkCaps
  _ <- elClass "div" "modal__main transaction_details" $
    submitTransactionWithFeedback cmd (_account_chainId fromAccount) (fmap Right nodeInfos)
  done <- modalFooter $ confirmButton def "Done"
  pure
    ( (mempty, close <> done)
    , never
    )

-- | General transfer workflow. This is the initial configuration screen.
sendConfig
  :: SendConstraints model mConf key t m
  => model -> IntMap.Key -> Account key -> Workflow t m (mConf, Event t ())
sendConfig model fromIndex fromAccount = Workflow $ do
  close <- modalHeader $ text "Send"
  rec
    (currentTab, _done) <- makeTabs $ attachWithMaybe (const . void . hush) (current recipient) next
    (conf, mCaps, recipient) <- mainSection currentTab
    (cancel, next) <- footerSection currentTab recipient mCaps
  let nextScreen = flip push next $ \() -> runMaybeT $ do
        (fromGasPayer, mToGasPayer) <- MaybeT $ sample $ current mCaps
        (toAccount, amount) <- MaybeT $ fmap hush $ sample $ current recipient
        let mCrossChain = ffor mToGasPayer $ \a -> CrossChainData
              { _crossChainData_recipientChainGasPayer = a }
        pure $ previewTransfer model fromIndex fromAccount fromGasPayer toAccount mCrossChain amount
  pure ((conf, close <> cancel), nextScreen)
  where
    mainSection currentTab = elClass "div" "modal__main" $ do
      (conf, recipient) <- tabPane mempty currentTab SendModalTab_Configuration $ do
        elClass "h2" "heading heading_type_h2" $ text "Destination"
        divClass "group" $ transactionDisplayNetwork model
        elClass "h2" "heading heading_type_h2" $ text "Recipient"
        recipient <- divClass "group" $ do
          kad <- mkLabeledInput True "Kadena Address" uiInputElement def
          let decoded = decodeKadenaAddressText <$> value kad
          (_, amount, _) <- mkLabeledInput True "Amount" uiGasPriceInputField def
            -- We're only interested in the decimal of the gas price
            <&> over (_2 . mapped . mapped) (\(GasPrice (ParsedDecimal d)) -> d)

          pure $ runExceptT $ do
            r <- ExceptT $ first (\_ -> "Invalid kadena address") <$> decoded
            a <- ExceptT $ maybe (Left "Invalid amount") Right <$> amount
            pure (r, a)
        elClass "h2" "heading heading_type_h2" $ text "Transaction Settings"
        (conf, _, _) <- divClass "group" $ uiMetaData model Nothing Nothing
        pure (conf, recipient)
      mCaps <- tabPane mempty currentTab SendModalTab_Sign $ do
        fmap join . holdDyn (pure Nothing) <=< dyn $ ffor recipient $ \case
          Left e -> do
            divClass "group" $ text $ e <> ": please go back and check the recipient."
            pure $ pure Nothing
          Right (ka, _amount) -> do
            let toChain = _kadenaAddress_chainId ka
                fromChain = _account_chainId fromAccount
            when (toChain /= fromChain) $ do
              elClass "h3" ("heading heading_type_h3") $ text "This is a cross chain transfer."
              el "p" $ text $ T.concat
                [ "Coin will be transferred from chain "
                , _chainId fromChain
                , " to chain "
                , _chainId toChain
                , "."
                ]
              el "p" $ text $ T.unwords
                [ "This is a multi step operation."
                , "The coin will leave the sender account immediately, and gas must be paid on the recipient chain in order to redeem the coin."
                ]
            elClass "h2" "heading heading_type_h2" $ do
              text "Gas Payer"
              when (toChain /= fromChain) $ text $ " (Chain " <> _chainId fromChain <> ")"
            fromGasPayer <- divClass "group" $ elClass "div" "segment segment_type_tertiary labeled-input" $ do
              divClass "label labeled-input__label" $ text "Account Name"
              let cfg = def & dropdownConfig_attributes .~ pure ("class" =: "labeled-input__input")
                  chain = pure $ Just fromChain
              gasAcc <- uiSenderDropdown cfg never model chain
              let toAccount ma m = find ((ma ==) . Just . _account_name) $ fmapMaybe (\case SomeAccount_Account a -> Just a; _ -> Nothing) m
              pure $ toAccount <$> gasAcc <*> model ^. wallet_accounts
            toGasPayer <- if toChain == fromChain
              then pure $ pure $ Just Nothing
              else do
                -- TODO this bit should have an option with a generic input for Ed25519 keys
                -- and perhaps be skippable entirely in favour of a blob the user
                -- can send to someone else to continue the tx on the recipient chain
                elClass "h2" "heading heading_type_h2" $ text $ "Gas Payer (Chain " <> _chainId toChain <> ")"
                divClass "group" $ elClass "div" "segment segment_type_tertiary labeled-input" $ do
                  divClass "label labeled-input__label" $ text "Account Name"
                  let cfg = def & dropdownConfig_attributes .~ pure ("class" =: "labeled-input__input")
                      chain = pure $ Just toChain
                  gasAcc <- uiSenderDropdown cfg never model chain
                  let toAccount ma m = find ((ma ==) . Just . _account_name) $ fmapMaybe (\case SomeAccount_Account a -> Just a; _ -> Nothing) m
                  pure $ fmap Just $ toAccount <$> gasAcc <*> model ^. wallet_accounts
            pure $ (liftA2 . liftA2) (,) fromGasPayer toGasPayer
      pure (conf, mCaps, recipient)
    footerSection currentTab recipient mCaps = modalFooter $ do
      cancel <- cancelButton def "Cancel"
      let (name, disabled) = splitDynPure $ ffor currentTab $ \case
            SendModalTab_Configuration -> ("Next", fmap isLeft recipient)
            SendModalTab_Sign -> ("Preview", fmap isNothing mCaps)
      let cfg = def
            & uiButtonCfg_class <>~ "button_type_confirm"
            & uiButtonCfg_disabled .~ join disabled
      next <- uiButtonDyn cfg $ dynText name
      pure (cancel, next)

-- | This function finishes cross chain transfers. The return event signals that
-- the transfer is complete.
runUnfinishedCrossChainTransfer
  :: ( PerformEvent t m, PostBuild t m, TriggerEvent t m, MonadHold t m, DomBuilder t m
     , MonadJSM (Performable m), MonadFix m
     , HasCrypto key (Performable m)
     )
  => SharedNetInfo NodeInfo
  -- ^ Network info
  -> ChainId
  -- ^ From chain
  -> ChainId
  -- ^ To chain
  -> Account key
  -- ^ Gas payer on "To" chain
  -> Event t Pact.RequestKey
  -- ^ The request key to follow up on
  -> m (Event t (), Event t Text, Event t ())
runUnfinishedCrossChainTransfer netInfo fromChain toChain toGasPayer requestKey = mdo
  let nodeInfos = _sharedNetInfo_nodes netInfo
      networkName = _sharedNetInfo_network netInfo
      publicMeta = _sharedNetInfo_meta netInfo
  -- Client envs for making requests to each chain
  let envFromChain = mkClientEnvs nodeInfos fromChain
      envToChain = mkClientEnvs nodeInfos toChain
  -- Wait for result
  mRequestKey <- hold Nothing $ Just <$> requestKey
  let initCont = leftmost [requestKey, tagMaybe mRequestKey retry]
  contResponse <- listenForContinuation envFromChain initCont
  let (contError, contOk) = fanEither contResponse
  -- Get the proof
  spvResponse <- getSPVProof nodeInfos fromChain toChain contOk
  let (spvError, spvOk) = fanEither spvResponse
  -- Run continuation on target chain
  continueResponse <- continueCrossChainTransfer networkName envToChain publicMeta toGasPayer spvOk
  let (continueError, continueOk) = fanEither continueResponse
  -- Wait for result
  resultResponse <- listenForSuccess envToChain continueOk
  let (resultError, resultOk) = fanEither resultResponse

  contStatus <- holdDyn Status_Waiting $ leftmost
    [ Status_Working <$ initCont
    , Status_Failed <$ contError
    , Status_Done <$ contOk
    ]
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

  let item ds txt = elDynAttr "li" (ffor ds $ \s -> "class" =: statusText s) $ el "p" $ text txt

  item contStatus "Got continuation response"
  item spvStatus "SPV proof retrieved"
  item continueStatus $ "Initiate claiming coin on chain " <> _chainId toChain
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

-- | Configuration step before finishing a previously unfinished cross chain
-- transfer.
finishCrossChainTransferConfig
  :: ( PerformEvent t m, PostBuild t m, TriggerEvent t m, MonadHold t m, DomBuilder t m
     , MonadJSM (Performable m), MonadFix m
     , HasCrypto key (Performable m)
     , Monoid mConf, HasWalletCfg mConf key t
     , HasNetworkCfg mConf t
     , HasNetwork model t
     , HasWallet model key t
     )
  => model
  -> IntMap.Key
  -- ^ Key of the "from" account
  -> Account key
  -- ^ From account
  -> UnfinishedCrossChainTransfer
  -- ^ The unfinished transfer
  -> Workflow t m (mConf, Event t ())
finishCrossChainTransferConfig model fromIndex fromAccount ucct = Workflow $ do
  let requestKey = _unfinishedCrossChainTransfer_requestKey ucct
      toChain = _unfinishedCrossChainTransfer_recipientChain ucct
  close <- modalHeader $ text "Cross chain transfer: unfinished transfer"
  (sender, conf) <- divClass "modal__main" $ do
    el "p" $ text "It looks like you started a cross chain transfer which did not complete correctly."
    elClass "h2" "heading heading_type_h2" $ text "Transaction Details"
    divClass "group" $ do
      mkLabeledInput True "Request Key" uiInputElement $ def
        & initialAttributes .~ "disabled" =: "disabled"
        & inputElementConfig_initialValue .~ Pact.requestKeyToB16Text requestKey
      mkLabeledInput True "Recipient Chain" uiInputElement $ def
        & initialAttributes .~ "disabled" =: "disabled"
        & inputElementConfig_initialValue .~ _chainId toChain
      mkLabeledInput True "Recipient Account" uiInputElement $ def
        & initialAttributes .~ "disabled" =: "disabled"
        & inputElementConfig_initialValue .~ unAccountName (_unfinishedCrossChainTransfer_recipientAccount ucct)
      mkLabeledInput True "Amount" uiGasPriceInputField $ def
        & initialAttributes .~ "disabled" =: "disabled"
        & inputElementConfig_initialValue .~ tshow (_unfinishedCrossChainTransfer_amount ucct)
    el "p" $ text "The coin has been debited from your account but hasn't been redeemed by the recipient."
    elClass "h2" "heading heading_type_h2" $ text "Transaction Settings"
    (conf, _, _) <- divClass "group" $ uiMetaData model Nothing Nothing
    elClass "h2" "heading heading_type_h2" $ text "Gas Payer (recipient chain)"
    sender <- divClass "group" $ elClass "div" "segment segment_type_tertiary labeled-input" $ do
      divClass "label labeled-input__label" $ text "Account Name"
      let cfg = def & dropdownConfig_attributes .~ pure ("class" =: "labeled-input__input")
          chain = pure $ Just toChain
      gasAcc <- uiSenderDropdown cfg never model chain
      let toAccount ma m = find ((ma ==) . Just . _account_name) $ fmapMaybe (\case SomeAccount_Account a -> Just a; _ -> Nothing) m
      pure $ toAccount <$> gasAcc <*> model ^. wallet_accounts
    pure (sender, conf)
  next <- modalFooter $ confirmButton def "Next"
  let nextScreen = flip push next $ \() -> do
        mNetInfo <- sampleNetInfo model
        mToGasPayer <- sample $ current sender
        pure $ ffor2 mNetInfo mToGasPayer $ \ni gp -> finishCrossChainTransfer ni fromIndex fromAccount ucct gp
  pure ((conf, close), nextScreen)

-- | Handy function for getting network / meta information in 'PushM'. Type
-- monomorphised to prevent accidentally sampling outside of 'push'
sampleNetInfo
  :: (Reflex t, HasNetwork model t)
  => model -> PushM t (Maybe (SharedNetInfo NodeInfo))
sampleNetInfo model = do
  nodes <- fmap rights $ sample $ current $ model ^. network_selectedNodes
  meta <- sample $ current $ model ^. network_meta
  let networkName = hush . mkNetworkName . nodeVersion =<< headMay nodes
  pure $ ffor networkName $ \name -> SharedNetInfo
    { _sharedNetInfo_network = name
    , _sharedNetInfo_nodes = nodes
    , _sharedNetInfo_meta = meta
    }

-- | Modal for finishing previously unfinished cross chain transfers
finishCrossChainTransfer
  :: ( PerformEvent t m, PostBuild t m, TriggerEvent t m, MonadHold t m, DomBuilder t m
     , MonadJSM (Performable m), MonadFix m
     , HasCrypto key (Performable m)
     , Monoid mConf, HasWalletCfg mConf key t
     )
  => SharedNetInfo NodeInfo
  -> IntMap.Key
  -- ^ Key for "from" account
  -> Account key
  -- ^ From account
  -> UnfinishedCrossChainTransfer
  -- ^ The unfinished transfer
  -> Account key
  -- ^ The account which pays the gas on the recipient chain
  -> Workflow t m (mConf, Event t ())
finishCrossChainTransfer netInfo fromIndex fromAccount ucct toGasPayer = Workflow $ do
  close <- modalHeader $ text "Cross chain transfer"
  let fromChain = _account_chainId fromAccount
      toChain = _unfinishedCrossChainTransfer_recipientChain ucct
      requestKey = _unfinishedCrossChainTransfer_requestKey ucct
  resultOk <- divClass "modal__main" $ do
    elClass "h2" "heading heading_type_h2" $ text "Transaction Status"

    (resultOk0, errMsg, retry) <- divClass "group" $ do
      elClass "ol" "transaction_status" $ do
        let item ds = elDynAttr "li" (ffor ds $ \s -> "class" =: statusText s)
        item (pure Status_Done) $ el "p" $
          text $ "Cross chain transfer initiated on chain " <> _chainId fromChain
        pb <- getPostBuild
        runUnfinishedCrossChainTransfer netInfo fromChain toChain toGasPayer $ requestKey <$ pb

    elClass "h2" "heading heading_type_h2" $ text "Request Key"
    divClass "group" $ text $ Pact.requestKeyToB16Text requestKey

    void $ runWithReplace blank $ ffor (leftmost [Just <$> errMsg, Nothing <$ retry]) $ \case
      Just e -> do
        elClass "h2" "heading heading_type_h2" $ text "Failure"
        divClass "group" $ text e
      Nothing ->
        blank

    pure resultOk0

  (abandon, done) <- modalFooter $ do
    abandon <- uiButton btnCfgSecondary $ text "Abandon Transfer"
    done <- confirmButton def "Done"
    pure (abandon, done)
  let conf = mempty & walletCfg_setCrossChainTransfer .~ ((fromIndex, Nothing) <$ (resultOk <> abandon))
  pure ((conf, close <> done <> abandon), never)

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
crossChainTransfer
  :: ( PerformEvent t m, PostBuild t m, TriggerEvent t m, MonadHold t m, DomBuilder t m
     , MonadJSM (Performable m), MonadJSM m, MonadFix m
     , HasCrypto key m, HasCrypto key (Performable m)
     , Monoid mConf, HasWalletCfg mConf key t
     )
  => SharedNetInfo NodeInfo
  -> IntMap.Key
  -- ^ Key of "from" account
  -> Account key
  -- ^ From account
  -> Either KadenaAddress (Account key)
  -- ^ To address/account. We pass in a full 'Account' if we have one, such that
  -- we can avoid looking up the keyset.
  -> Account key
  -- ^ Gas payer for 'from' chain
  -> CrossChainData key
  -- ^ Cross chain specific info
  -> Decimal
  -- ^ Amount to transfer
  -> Workflow t m (mConf, Event t ())
crossChainTransfer netInfo fromIndex fromAccount toAccount fromGasPayer crossChainData amount = Workflow $ do
  let nodeInfos = _sharedNetInfo_nodes netInfo
      networkName = _sharedNetInfo_network netInfo
      publicMeta = _sharedNetInfo_meta netInfo
  close <- modalHeader $ text "Cross chain transfer"
  pb <- getPostBuild
  let fromChain = _account_chainId fromAccount
      toChain = either _kadenaAddress_chainId _account_chainId toAccount
      toAddress = either id (\a -> accountToKadenaAddress a (_account_chainId a)) toAccount
  -- Client envs for making requests to each chain
  let envFromChain = mkClientEnvs nodeInfos fromChain
  -- Lookup the guard if we don't already have it
  keySetResponse <- case toAccount of
    Left ka -> case _kadenaAddress_accountCreated ka of
      AccountCreated_Yes -> lookupKeySet networkName envFromChain ka
      -- If the account hasn't been created, don't try to lookup the guard. Just
      -- assume the account name _is_ the public key (since it must be a
      -- non-vanity account).
      AccountCreated_No -> pure $ ffor pb $ \() -> case textToKey $ unAccountName $ _kadenaAddress_accountName ka of
        Nothing -> Left "Couldn't make public key from name"
        Just k -> Right $ toKS $ toPactPublicKey k
    Right ours -> pure $ Right (toKS $ toPactPublicKey $ _keyPair_publicKey $ _account_key ours) <$ pb
  let (keySetError, keySetOk) = fanEither keySetResponse
  -- Start the transfer
  initiated <- initiateCrossChainTransfer networkName envFromChain publicMeta fromAccount fromGasPayer toAddress amount keySetOk
  let (initiatedError, initiatedOk) = fanEither initiated
  initiateStatus <- holdDyn Status_Waiting $ leftmost
    [ Status_Working <$ keySetOk
    , Status_Failed <$ initiatedError
    , Status_Done <$ initiatedOk
    ]
  resultOk <- divClass "modal__main" $ do
    elClass "h2" "heading heading_type_h2" $ text "Transaction Status"

    (resultOk0, errMsg0, retry0) <- divClass "group" $ do
      elClass "ol" "transaction_status" $ do
        let item ds = elDynAttr "li" (ffor ds $ \s -> "class" =: statusText s)
        item initiateStatus $
          el "p" $ text $ "Cross chain transfer initiated on chain " <> _chainId fromChain

        let toGasPayer = _crossChainData_recipientChainGasPayer crossChainData
        runUnfinishedCrossChainTransfer netInfo fromChain toChain toGasPayer initiatedOk

    elClass "h2" "heading heading_type_h2" $ text "Request Key"
    divClass "group" $ void $ runWithReplace (text "Loading...") $ ffor initiatedOk $ \rk ->
      text $ Pact.requestKeyToB16Text rk

    let errMsg = leftmost [keySetError, initiatedError, errMsg0]
    void $ runWithReplace blank $ ffor (leftmost [Just <$> errMsg, Nothing <$ retry0]) $ \case
      Just e -> do
        elClass "h2" "heading heading_type_h2" $ text "Failure"
        divClass "group" $ text e
      Nothing -> do
        blank

    pure resultOk0
  done <- modalFooter $ confirmButton def "Done"
  let mkUCCT requestKey = (fromIndex, Just UnfinishedCrossChainTransfer
        { _unfinishedCrossChainTransfer_requestKey = requestKey
        , _unfinishedCrossChainTransfer_recipientChain = toChain
        , _unfinishedCrossChainTransfer_recipientAccount = _kadenaAddress_accountName toAddress
        , _unfinishedCrossChainTransfer_amount = amount
        })
  let conf = mempty
        & walletCfg_setCrossChainTransfer .~ leftmost
          [ fmap mkUCCT initiatedOk
          , (fromIndex, Nothing) <$ resultOk
          ]
  pure ((conf, close <> done), never)
  where
    toKS k = Pact.KeySet
      { Pact._ksKeys = [k]
      , Pact._ksPredFun = Pact.Name $ Pact.BareName "keys-all" def
      }

-- | Continue the transfer on the target chain
continueCrossChainTransfer
  :: (TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m), HasCrypto key (Performable m))
  => NetworkName
  -- ^ Which network we are on
  -> [S.ClientEnv]
  -- ^ Envs pointing at the target chain
  -> PublicMeta
  -- ^ Meta info (gas settings)
  -> Account key
  -- ^ Gas payer on target chain
  -> Event t (Pact.PactExec, Pact.ContProof)
  -- ^ The previous continuation step and associated proof
  -> m (Event t (Either Text (Pact.PactId, Pact.RequestKey)))
continueCrossChainTransfer networkName envs publicMeta gasPayer spvOk = performEventAsync $ ffor spvOk $ \(pe, proof) cb -> do
  let pm = publicMeta
        { _pmChainId = _account_chainId gasPayer
        , _pmSender = unAccountName $ _account_name gasPayer
        }
      signingPairs = [_account_key gasPayer]
  payload <- buildContPayload networkName pm signingPairs $ ContMsg
    { _cmPactId = Pact._pePactId pe
    , _cmStep = succ $ Pact._peStep pe
    , _cmRollback = False
    , _cmData = Aeson.Object mempty
    , _cmProof = Just proof
    }
  cont <- buildCmdWithPayload payload signingPairs
  liftIO $ putStrLn "transfer-crosschain: running continuation on target chain"
  -- We can't do a /local with continuations :(
  liftJSM $ forkJSM $ do
    r <- doReqFailover envs (Api.send Api.apiV1Client $ Api.SubmitBatch $ pure cont) >>= \case
      Left es -> packHttpErrors es
      Right (Api.RequestKeys (requestKey :| _)) -> pure $ Right (Pact._pePactId pe, requestKey)
    liftIO $ cb r
  pure ()

-- | Lookup the keyset of an account
lookupKeySet
  :: (HasCrypto key m, TriggerEvent t m, MonadJSM m)
  => NetworkName
  -- ^ Which network we are on
  -> [S.ClientEnv]
  -- ^ Envs which point to the appropriate chain
  -> KadenaAddress
  -- ^ Account on said chain to find
  -> m (Event t (Either Text Pact.KeySet))
lookupKeySet networkName envs addr = do
  now <- getCreationTime
  let code = T.unwords
        [ "(coin.details"
        , tshow $ unAccountName $ _kadenaAddress_accountName addr
        , ")"
        ]
      pm = PublicMeta
        { _pmChainId = _kadenaAddress_chainId addr
        , _pmSender = "chainweaver"
        , _pmGasLimit = 0
        , _pmGasPrice = 0
        , _pmTTL = 60
        , _pmCreationTime = now
        }
  cmd <- buildCmd Nothing networkName pm [] [] code mempty mempty
  (result, trigger) <- newTriggerEvent
  liftJSM $ forkJSM $ do
    r <- doReqFailover envs (Api.local Api.apiV1Client cmd) >>= \case
      Left es -> packHttpErrors es
      Right cr -> case Pact._crResult cr of
        Pact.PactResult (Right (PObject (Pact.ObjectMap m)))
          | Just (PGuard (Pact.GKeySet keySet)) <- Map.lookup "guard" m
          -> pure $ Right keySet
        Pact.PactResult (Right v) -> do
          liftIO $ putStrLn $ "lookupKeySet: Failed to retrieve the recipient's account guard: " <> show v
          pure $ Left "Failed to retrieve the recipient's account guard"
        Pact.PactResult (Left e) -> do
          liftIO $ putStrLn $ "Received error for details: " <> show e
          pure $ Left "Failed to retrieve the recipient's account guard"
    liftIO $ trigger r
  pure result

-- | Turn some node URLs into chain specific servant envs
mkClientEnvs :: [NodeInfo] -> ChainId -> [S.ClientEnv]
mkClientEnvs nodeInfos chain = fforMaybe nodeInfos $ \nodeInfo ->
  let chainUrl = getChainBaseUrl chain nodeInfo
  in S.mkClientEnv <$> S.parseBaseUrl (URI.renderStr chainUrl)

-- | Perform some servant request by stepping through the given envs
-- TODO a bunch of uses of this are pretty similar and should be factored out.
doReqFailover :: MonadJSM m => [S.ClientEnv] -> S.ClientM a -> m (Either [S.ClientError] a)
doReqFailover [] _ = pure $ Left []
doReqFailover (c:cs) request = liftJSM $ S.runClientM request c >>= \case
  Left e -> first (e:) <$> doReqFailover cs request
  Right r -> pure $ Right r

-- | Initiate a cross chain transfer on the sender chain.
initiateCrossChainTransfer
  :: (MonadJSM (Performable m), TriggerEvent t m, PerformEvent t m, HasCrypto key (Performable m))
  => NetworkName
  -- ^ Network name
  -> [S.ClientEnv]
  -- ^ Where to send requests
  -> PublicMeta
  -- ^ Meta info - really only interested in gas settings
  -> Account key
  -- ^ From account
  -> Account key
  -- ^ Gas payer ("from" chain)
  -> KadenaAddress
  -- ^ Recipient address
  -> Decimal
  -- ^ Amount to transfer
  -> Event t Pact.KeySet
  -- ^ Recipient keyset
  -> m (Event t (Either Text Pact.RequestKey))
initiateCrossChainTransfer networkName envs publicMeta fromAccount fromGasPayer toAccount amount eks = performEventAsync $ ffor eks $ \rg cb -> do
  cmd <- buildCmd Nothing networkName pm signingPairs [] code (mkDat rg) capabilities
  liftJSM $ forkJSM $ do
    r <- doReqFailover envs (Api.local Api.apiV1Client cmd) >>= \case
      Left es -> packHttpErrors es
      Right cr -> case Pact._crResult cr of
        Pact.PactResult (Left e) -> do
          liftIO $ print e
          pure $ Left $ tshow e
        Pact.PactResult (Right _) -> doReqFailover envs (Api.send Api.apiV1Client $ Api.SubmitBatch $ pure cmd) >>= \case
          Left es -> packHttpErrors es
          Right (Api.RequestKeys (requestKey :| _)) -> pure $ Right requestKey
    liftIO $ cb r
  where
    keysetName = "receiverKey"
    code = T.unwords
      [ "(coin.transfer-crosschain"
      , tshow $ unAccountName $ _account_name fromAccount
      , tshow $ unAccountName $ _kadenaAddress_accountName toAccount
      , "(read-keyset '" <> keysetName <> ")"
      , tshow $ _chainId $ _kadenaAddress_chainId toAccount
      , tshow amount
      , ")"
      ]
    mkDat rg = HM.singleton keysetName $ Aeson.toJSON rg
    signingPairs = L.nubBy (\x y -> _keyPair_publicKey x == _keyPair_publicKey y) [_account_key fromAccount, _account_key fromGasPayer]
    -- This capability is required for `transfer-crosschain`.
    debitCap = SigCapability
      { _scName = QualifiedName { _qnQual = "coin", _qnName = "DEBIT", _qnInfo = def }
      , _scArgs = [PLiteral $ LString $ unAccountName $ _account_name fromAccount]
      }
    capabilities = Map.unionsWith (<>)
      [ Map.singleton (_keyPair_publicKey $ _account_key fromGasPayer) [_dappCap_cap defaultGASCapability]
      , Map.singleton (_keyPair_publicKey $ _account_key fromAccount) [debitCap]
      ]
    pm = publicMeta
      { _pmChainId = _account_chainId fromAccount
      , _pmSender = unAccountName $ _account_name fromGasPayer
      }

-- | Listen to a request key for some continuation
listenForContinuation
  :: (TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m))
  => [S.ClientEnv] -> Event t Pact.RequestKey -> m (Event t (Either Text (Pact.RequestKey, Pact.PactExec)))
listenForContinuation envs requestKey = performEventAsync $ ffor requestKey $ \rk cb -> liftJSM $ forkJSM $ do
  r <- doReqFailover envs (Api.listen Api.apiV1Client $ Api.ListenerRequest rk) >>= \case
    Left es -> packHttpErrors es
    Right (Api.ListenResponse cr) -> case Pact._crContinuation cr of
      Nothing ->
        pure $ Left $ T.unlines ["Result was not a continuation", tshow (Pact._crResult cr)]
      Just pe ->
        pure $ Right (Pact._crReqKey cr, pe)

    Right (Api.ListenTimeout _) -> pure $ Left "Listen timeout"
  liftIO $ cb r

packHttpErrors :: MonadIO m => [S.ClientError] -> m (Either Text a)
packHttpErrors es = do
  let prettyErr = T.unlines $ fmap (tshow . packHttpErr) es
  liftIO $ T.putStrLn prettyErr
  pure $ Left prettyErr

-- | Listen to a request key for the second step (redeeming coin on the target
-- chain).
listenForSuccess
  :: (TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m))
  => [S.ClientEnv] -> Event t (Pact.PactId, Pact.RequestKey) -> m (Event t (Either Text ()))
listenForSuccess envs e = performEventAsync $ ffor e $ \(PactId pactId, requestKey) cb -> liftJSM $ forkJSM $ do
  -- This string appears in the error if the pact was previously completed
  let completedMsg = "resumePact: pact completed: " <> pactId
  r <- doReqFailover envs (Api.listen Api.apiV1Client $ Api.ListenerRequest requestKey) >>= \case
    Left es -> packHttpErrors es
    Right (Api.ListenResponse cr) -> case Pact._crResult cr of
      Pact.PactResult (Left pe)
        -- There doesn't seem to be a nicer way to do this check
        | completedMsg == tshow (peDoc pe) -> pure $ Right ()
        | otherwise -> pure $ Left $ tshow $ peDoc pe
      Pact.PactResult (Right _) -> pure $ Right ()
    Right (Api.ListenTimeout _) -> pure $ Left "Listen timeout"
  liftIO $ cb r

-- | Using the given nodes, attempt to get an SPV proof for
-- the request key. Polls every X seconds for the proof, repeatedly going
-- through the nodes until we get a response.
getSPVProof
  :: (TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m))
  => [NodeInfo]
  -- ^ Nodes to try
  -> ChainId
  -- ^ This chain
  -> ChainId
  -- ^ Target chain
  -> Event t (Pact.RequestKey, Pact.PactExec)
  -- ^ Request key of continuation. 'PactExec' is passed through.
  -> m (Event t (Either Text (Pact.PactExec, Pact.ContProof)))
getSPVProof nodeInfos thisChain targetChain e = performEventAsync $ ffor e $ \(requestKey, pe) cb -> do
  liftJSM $ forkJSM $ do
    proof <- failover $ spvRequests requestKey
    liftIO . cb $ (,) pe <$> proof
  where
    chainUrls = getChainBaseUrl thisChain <$> nodeInfos
    -- Can't use chainweb-node's definition to generate a client, it won't build on GHCJS.
    -- Maybe we can split the API bits out later.
    spvRequests requestKey = ffor chainUrls $ \u -> postJson (URI.render u <> "/spv") $ Aeson.object
      -- Essentially 'SpvRequest' from chainweb
      [ "requestKey" Aeson..= requestKey
      , "targetChainId" Aeson..= targetChain
      ]
    failover [] = do
      liftIO $ putStrLn $ "Ran out of nodes to try for SPV proof"
      pure $ Left "Failed to get SPV proof"
    failover (req : rs) = do
      liftIO $ putStrLn $ "Sending SPV Request: " <> show req
      m <- liftIO newEmptyMVar
      void $ newXMLHttpRequest req (liftIO . putMVar m)
      resp <- liftIO $ takeMVar m
      let s = _xhrResponse_status resp
      liftIO $ putStrLn $ "Got SPV Response " <> show s <> ": " <> show (_xhrResponse_responseText resp)
      case s of
        200 -> pure $ maybe (Left "Couldn't decode response") Right $ decodeXhrResponse resp
        _ -> do
          -- Wait 15 seconds. We should improve this to wait instead of poll,
          -- but this is simple and it works
          liftIO $ threadDelay $ 15 * 1000 * 1000
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
  => Event t () -> m (Dynamic t SendModalTab, Event t ())
makeTabs next = do
  let initTab = SendModalTab_Configuration
      f t0 g = case g t0 of
        Nothing -> (Just t0, Just ())
        Just t -> (Just t, Nothing)
  rec
    (curSelection, done) <- mapAccumMaybeDyn f initTab $ leftmost
      [ const . Just <$> onTabClick
      , succMay <$ next
      ]
    (TabBar onTabClick) <- makeTabBar $ TabBarCfg
      { _tabBarCfg_tabs = [SendModalTab_Configuration, SendModalTab_Sign]
      , _tabBarCfg_mkLabel = \_ -> displaySendModalTab
      , _tabBarCfg_selectedTab = Just <$> curSelection
      , _tabBarCfg_classes = mempty
      , _tabBarCfg_type = TabBarType_Secondary
      }
  pure (curSelection, done)
