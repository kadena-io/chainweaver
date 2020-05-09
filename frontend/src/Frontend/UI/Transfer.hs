{-|

Copyright   :  (C) 2020 Kadena
License     :  BSD-style (see the file LICENSE)

Design Requirements:

* MUST support both Chainweaver, non-Chainweaver, and external cold wallet keys
* MUST support cross chain transfers
* MUST allow cross chain transfer target gas to be paid by a gas station
* MUST support multi-sig
* MUST support both transfer and transfer-create AND allow transfer-create to be
  done even if the receiving account already exists

* SHOULD be able to support safe bi-directional transfers at some point in the future

* Does not need to support fully offline transfers (i.e. ok to require internet
  access to query keysets for the relevant accounts)

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend.UI.Transfer where

import           Control.Error hiding (bool, note)
import           Control.Lens hiding ((.=))
import           Control.Monad.State.Strict
import           Data.Aeson
import qualified Data.ByteString.Lazy as LB
import           Data.Decimal
import           Data.Default (Default (..))
import qualified Data.IntMap as IntMap
import           Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NEL
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Traversable
import           Data.Time.Clock.POSIX
import qualified Data.Yaml as Y
import           Kadena.SigningApi (AccountName(..))
import           Pact.Parse
import qualified Pact.Server.ApiClient as Api
import qualified Pact.Types.API as Api
import           Pact.Types.Capability
import           Pact.Types.ChainId
import           Pact.Types.ChainMeta
import qualified Pact.Types.Command as Pact
import           Pact.Types.Command
import           Pact.Types.Exp
import           Pact.Types.Gas
import           Pact.Types.Hash
import           Pact.Types.Info
import           Pact.Types.Names
import           Pact.Types.PactValue
import           Pact.Types.Pretty
import           Pact.Types.RPC
import           Pact.Types.Scheme
import           Pact.Types.Term (KeySet (..))
import qualified Pact.Types.Term as Pact
import           Reflex
import           Reflex.Dom.Core
import qualified Servant.Client.JSaddle as S
import           Text.Printf
import           Text.Read (readMaybe)
import qualified Text.URI as URI

import           Common.Foundation
import           Common.Wallet
import           Frontend.Crypto.Class
import           Frontend.Crypto.Ed25519
import           Frontend.Foundation
import           Frontend.JsonData
import           Frontend.Log
import           Frontend.Network
import           Frontend.PactQueries
import           Frontend.TxBuilder
import           Frontend.UI.Button
import           Frontend.UI.DeploymentSettings
import           Frontend.UI.Dialogs.DeployConfirmation
import           Frontend.UI.Dialogs.Send
import           Frontend.UI.Form.Common
import           Frontend.UI.FormWidget
import           Frontend.UI.KeysetWidget
import           Frontend.UI.Modal
import           Frontend.UI.TabBar
import           Frontend.UI.Widgets
import           Frontend.UI.Widgets.Helpers
import           Frontend.Wallet

data KeysetAction
  = KeysetAmbiguous (ChainId, AccountName, UserKeyset)
  | KeysetUnambiguous (ChainId, AccountName, UserKeyset)
  | KeysetError Text
  | KeysetNoAction
  deriving (Eq,Ord,Show)

makePrisms ''KeysetAction

data TransferCfg t = TransferCfg
  { _transferCfg_isVisible :: Dynamic t Bool
  , _transferCfg_setFrom :: Event t ChainAccount
  , _transferCfg_setTo :: Event t ReceivingAccount
  }

instance Reflex t => Default (TransferCfg t) where
  def = TransferCfg (constDyn False) never never

data ChainAccount = ChainAccount
  { _ca_chain :: ChainId
  , _ca_account :: AccountName
  } deriving (Eq,Ord,Show)

data ReceivingAccount = ReceivingAccount
  { _ra_chainAccount :: ChainAccount
  , _ra_keyset :: UserKeyset
  } deriving (Eq,Ord,Show)

labeledChainAccount
  :: (MonadWidget t m, HasNetwork model t)
  => model
  -> PrimFormWidgetConfig t (Maybe ChainAccount)
  -> m (FormWidget t (Maybe ChainAccount))
labeledChainAccount model cfg = do
  elClass "div" ("segment segment_type_tertiary labeled-input-inline") $ do
    divClass ("label labeled-input__label-inline") $ text "Account"
    divClass "labeled-input__input account-chain-input" $
      fst <$> uiChainAccount model cfg

uiChainAccount
  :: (MonadWidget t m, HasNetwork model t)
  => model
  -> PrimFormWidgetConfig t (Maybe ChainAccount)
  -> m (FormWidget t (Maybe ChainAccount), Event t (Maybe Text))
uiChainAccount model cfg = do
  (a,onPaste) <- accountNameFormWidget noValidation $ _ca_account <$$> cfg
  cd <- uiMandatoryChainSelection (getChainsFromHomogenousNetwork model) mempty
                                  (maybe (ChainId "0") _ca_chain <$> _primFormWidgetConfig_fwc cfg)
  return (runMaybeT $ ChainAccount <$> lift cd <*> MaybeT a, onPaste)

toFormWidget
  :: (MonadWidget t m, HasNetwork model t)
  => model
  -> FormWidgetConfig t (Maybe ChainAccount, Maybe UserKeyset)
  -> m (FormWidget t (Maybe ChainAccount), Dynamic t (Maybe UserKeyset))
toFormWidget model cfg = mdo
  let pastedBuilder = fmapMaybe ((decode' . LB.fromStrict . T.encodeUtf8) =<<) $ onPaste
      mkChainAccount b = ChainAccount (_txBuilder_chainId b) (_txBuilder_accountName b)
      modSetValue (Just e1) (Just e2) = Just $ leftmost [e1, e2]
      modSetValue Nothing a = a
      modSetValue a Nothing = a
  (tca,onPaste) <- elClass "div" ("segment segment_type_tertiary labeled-input-inline") $ do
    divClass ("label labeled-input__label-inline") $ text "Account"
    divClass "labeled-input__input account-chain-input" $ uiChainAccount model $ mkPfwc (fst <$> cfg)
      & initialAttributes %~ (<> "placeholder" =: "Account Name or Tx Builder")
      & setValue %~ modSetValue (Just (Just . mkChainAccount <$> pastedBuilder))

  let keysetStartsOpen = case snd (_initialValue cfg) of
                           Nothing -> False
                           Just uk -> not $ Set.null $ _userKeyset_keys uk
  keysetOpen <- foldDyn ($) keysetStartsOpen $ leftmost
    [ const not <$> clk
    , const True <$ pastedBuilder
    ]
  (clk,(_,k)) <- controlledAccordionItem keysetOpen mempty (accordionHeaderBtn "Keyset") $ do
    keysetFormWidget $ (snd <$> cfg)
      & setValue %~ modSetValue (Just (fmap userFromPactKeyset . _txBuilder_keyset <$> pastedBuilder))

  return (tca,k)

data TransferInfo = TransferInfo
  { _ti_fromAccount :: ChainAccount
  , _ti_amount :: Decimal -- Possibly use ParsedDecimal
  , _ti_toAccount :: ChainAccount
  , _ti_toKeyset :: Maybe UserKeyset
  } deriving (Eq,Ord,Show)

uiGenericTransfer
  :: ( MonadWidget t m
     , HasLogger model t
     , HasNetwork model t
     , HasNetworkCfg (ModalCfg mConf t) t
     , HasJsonData model t
     , Monoid mConf
     , Monoid (ModalCfg mConf t)
     , Flattenable (ModalCfg mConf t) t
     , HasModalCfg mConf (Modal mConf m t) t
     , HasCrypto key m
     , HasCrypto key (Performable m)
     , HasWallet model key t
     , HasTransactionLogger m
     )
  => model
  -> TransferCfg t
  -> m mConf
uiGenericTransfer model cfg = do
  let attrs = do
        visible <- _transferCfg_isVisible cfg
        pure $ if visible
          then ("class" =: "main transfer transfer__expanded")
          else ("class" =: "main transfer transfer__collapsed")
  elDynAttr "main" attrs $ mdo
    transferInfo <- divClass "transfer-fields" $ do
      (fromAcct,amount) <- divClass "transfer__left-pane" $ do
        el "h4" $ text "From"
        fca <- labeledChainAccount model $ mkCfg Nothing
          & initialAttributes .~ "placeholder" =: "Account Name"
          & setValue .~ (Just $ Nothing <$ clear)
        amt <- mkLabeledInput True "Amount (KDA)" amountFormWidget $ mkCfg (Right 2)
          & setValue .~ (Just $ Left "" <$ clear)
        return (fca,amt)
      (toAcct,ks) <- divClass "transfer__right-pane" $ do
        el "h4" $ text "To"
        toFormWidget model $ mkCfg (Nothing, Nothing)
          & setValue .~ (Just $ (Nothing, Nothing) <$ clear)
      return $ runMaybeT $ TransferInfo <$>
        MaybeT (value fromAcct) <*>
        MaybeT (hush <$> value amount) <*>
        MaybeT (value toAcct) <*>
        lift ks
    (clear, signTransfer) <- divClass "transfer-fields submit" $ do
      clr <- el "div" $ uiButton btnCfgTertiary $ text "Clear"
      st <- confirmButton (def { _uiButtonCfg_disabled = (isNothing <$> transferInfo) }) "Sign & Transfer"
      _ <- confirmButton (def { _uiButtonCfg_disabled = (isNothing <$> transferInfo) }) "Quick Transfer"
      return (clr, st)
    let netInfo = flip push signTransfer $ \() -> sampleNetInfo model
    let mkModal (Just ti) ni = Just $ lookupAndTransfer model ni ti
        mkModal Nothing _ = Nothing
    pure $ mempty & modalCfg_setModal .~ (attachWith mkModal (current transferInfo) netInfo)

lookupAndTransfer
  :: ( MonadWidget t m, Monoid mConf, Flattenable mConf t
     , HasLogger model t
     , HasCrypto key m
     , HasCrypto key (Performable m)
     , HasNetwork model t
     , HasNetworkCfg mConf t
     , HasWallet model key t
     , HasTransactionLogger m
     )
  => model
  -> SharedNetInfo NodeInfo
  -> TransferInfo
  -> Event t ()
  -> m (mConf, Event t ())
lookupAndTransfer model netInfo ti onCloseExternal = do
    let nodes = _sharedNetInfo_nodes netInfo
        fromAccount = _ca_account $ _ti_fromAccount ti
        fromChain = _ca_chain $ _ti_fromAccount ti
        toAccount = _ca_account $ _ti_toAccount ti
        toChain = _ca_chain $ _ti_toAccount ti
        accountNames = setify [fromAccount, toAccount]
        accounts = unAccountName <$> accountNames
        chains = setify [fromChain, toChain]
        code = renderCompactText $ accountDetailsObject accounts
    efks <- lookupKeySets (model ^. logger) (_sharedNetInfo_network netInfo)
                 nodes fromChain accountNames
    etks <- if fromChain == toChain
              then pure efks
              else lookupKeySets (model ^. logger) (_sharedNetInfo_network netInfo)
                                 nodes toChain accountNames
    allKeys <- foldDyn ($) (Nothing, Nothing) $ mergeWith (.)
      [ (\ks (_,b) -> (Just ks, b)) <$> efks
      , (\ks (a,_) -> (a, Just ks)) <$> etks
      ]
    let eWrapper (Just f, Just t) = do
          let fks = fromMaybe mempty f
          let tks = fromMaybe mempty t
          (conf, closes) <- fmap splitDynPure $ workflow $
            checkSendingAccountExists model netInfo ti fks tks
          mConf <- flatten =<< tagOnPostBuild conf
          let close = switch $ current closes
          pure (mConf, close)
        eWrapper (_, Nothing) = msgModal "Sign Transfer" $ text "Loading..."
        eWrapper (Nothing, _) = msgModal "Sign Transfer" $ text "Loading..."
    (conf, closes) <- splitDynPure <$> networkHold (msgModal "Sign Transfer" $ text "Querying keysets...")
                        (eWrapper <$> ffilter (\(a,b) -> isJust a && isJust b) (updated allKeys))
    mConf <- flatten =<< tagOnPostBuild conf
    return (mConf, switch $ current closes)
  where
    setify :: Ord a => [a] -> [a]
    setify = Set.toList . Set.fromList

msgModal :: (DomBuilder t m, PostBuild t m, Monoid mConf) => Text -> m a -> m (mConf, Event t ())
msgModal headerMsg body = do
  close <- modalHeader $ text headerMsg
  _ <- modalMain body
  done <- modalFooter $ do
    confirmButton def "Ok"

  pure (mempty, done <> close)

-- | Lookup the keyset of some accounts
lookupKeySets
  :: ( TriggerEvent t m, MonadJSM m
     , HasCrypto key m
     )
  => Logger t
  -> NetworkName
  -- ^ Which network we are on
  -> [NodeInfo]
  -- ^ Envs which point to the appropriate chain
  -> ChainId
  -> [AccountName]
  -- ^ Account on said chain to find
  -> m (Event t (Maybe (Map AccountName (AccountStatus AccountDetails))))
lookupKeySets logL networkName nodes chain accounts = do
  let code = renderCompactText $ accountDetailsObject (map unAccountName accounts)
  pm <- mkPublicMeta chain
  cmd <- buildCmd Nothing networkName pm [] [] code mempty mempty
  (result, trigger) <- newTriggerEvent
  let envs = mkClientEnvs nodes chain
  liftJSM $ forkJSM $ do
    r <- doReqFailover envs (Api.local Api.apiV1Client cmd) >>= \case
      Left _ -> pure Nothing
      Right cr -> case Pact._crResult cr of
        Pact.PactResult (Right pv) -> case parseAccountDetails pv of
          Left _ -> pure Nothing
          Right balances -> liftIO $ do
            putLog logL LevelInfo $ "lookupKeysets: success:"
            putLog logL LevelInfo $ tshow balances
            pure $ Just balances
        Pact.PactResult (Left e) -> do
          putLog logL LevelInfo $ "lookupKeysets failed:" <> tshow e
          pure Nothing

    liftIO $ trigger r
  pure result

uiTransferButton
  :: ( DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => m (Dynamic t Bool)
uiTransferButton = mdo
  let buttonText = bool "Show Transfer" "Hide Transfer" <$> isVisible
  click <- uiButton (def & uiButtonCfg_class <>~ " main-header__account-button") $ do
    dynText buttonText
  -- TODO Change this back to False
  isVisible <- toggle True click
  return isVisible

checkSendingAccountExists
  :: ( MonadWidget t m, Monoid mConf
     , HasNetwork model t
     , HasNetworkCfg mConf t
     , HasLogger model t
     , HasWallet model key t
     , HasCrypto key m
     , HasCrypto key (Performable m)
     , HasTransactionLogger m
     )
  => model
  -> SharedNetInfo NodeInfo
  -> TransferInfo
  -> Map AccountName (AccountStatus AccountDetails)
  -> Map AccountName (AccountStatus AccountDetails)
  -> Workflow t m (mConf, Event t ())
checkSendingAccountExists model netInfo ti fks tks = Workflow $ do
    let fromAccount = _ca_account $ _ti_fromAccount ti
    case Map.lookup fromAccount fks of
      Just (AccountStatus_Exists ad) -> do
        checkReceivingAccount model netInfo ti fks tks (fromAccount, ad)
      _ -> do
        cancel <- fatalTransferError $
          text $ "Sending account " <> unAccountName fromAccount <> " does not exist."
        return ((mempty, cancel), never)

checkReceivingAccount
  :: ( MonadWidget t m, Monoid mConf
     , HasNetwork model t
     , HasNetworkCfg mConf t
     , HasLogger model t
     , HasWallet model key t
     , HasCrypto key m
     , HasCrypto key (Performable m)
     , HasTransactionLogger m
     )
  => model
  -> SharedNetInfo NodeInfo
  -> TransferInfo
  -> Map AccountName (AccountStatus AccountDetails)
  -> Map AccountName (AccountStatus AccountDetails)
  -> (AccountName, AccountDetails)
  -> m ((mConf, Event t ()), Event t (Workflow t m (mConf, Event t ())))
checkReceivingAccount model netInfo ti fks tks fromPair = do
    let toAccount = _ca_account $ _ti_toAccount ti
    pb <- getPostBuild
    case (Map.lookup toAccount tks, _ti_toKeyset ti) of
      -- TODO Might need more checks for cross-chain error cases
      (Just (AccountStatus_Exists (AccountDetails _ g)), Just userKeyset) -> do
        -- Use transfer-create, but check first to see whether it will fail
        let AccountGuard_KeySet ks p = g
        let onChainKeyset = UserKeyset ks (parseKeysetPred p)
        if onChainKeyset /= userKeyset
          then do
            cancel <- fatalTransferError $ do
              el "div" $ text "Your keyset does not match the on-chain keyset.  Your transfer would fail."
              el "hr" blank
              el "div" $ do
                mkLabeledView False "Keyset You Entered" $ divClass "group" $
                  keysetWidget userKeyset
                mkLabeledView False "On-chain Keyset" $ divClass "group" $
                  keysetWidget onChainKeyset
            return ((mempty, cancel), never)
          else
            transferDialog model netInfo ti fks tks fromPair
      (Just (AccountStatus_Exists (AccountDetails _ g)), Nothing) -> do
        if (_ca_chain $ _ti_fromAccount ti) /= (_ca_chain $ _ti_toAccount ti)
          then do
            let AccountGuard_KeySet ks p = g
            let onChainKeyset = UserKeyset ks (parseKeysetPred p)
            cancel <- fatalTransferError $ do
              el "div" $ text "Your keyset does not match the on-chain keyset.  Your transfer would fail."
              el "hr" blank
              el "div" $ do
                mkLabeledView False "Keyset You Entered" $ divClass "group" $
                  text "You didn't enter any keys"
                mkLabeledView False "On-chain Keyset" $ divClass "group" $
                  keysetWidget onChainKeyset
            return ((mempty, cancel), never)
          else
            -- Use transfer, probably show the guard at some point
            -- TODO check well-formedness of all keys in the keyset
            transferDialog model netInfo ti fks tks fromPair
      (_, Just userKeyset) -> do
        -- Use transfer-create
        transferDialog model netInfo ti fks tks fromPair
      (_, Nothing) -> do
        -- If the account name looks like a public key, ask about making a keyset
        -- Otherwise throw an error
        handleMissingKeyset model netInfo ti fks tks fromPair

handleMissingKeyset
  :: ( MonadWidget t m, Monoid mConf
     , HasNetwork model t
     , HasNetworkCfg mConf t
     , HasLogger model t
     , HasWallet model key t
     , HasCrypto key m
     , HasCrypto key (Performable m)
     , HasTransactionLogger m
     )
  => model
  -> SharedNetInfo NodeInfo
  -> TransferInfo
  -> Map AccountName (AccountStatus AccountDetails)
  -> Map AccountName (AccountStatus AccountDetails)
  -> (AccountName, AccountDetails)
  -> m ((mConf, Event t ()), Event t (Workflow t m (mConf, Event t ())))
handleMissingKeyset model netInfo ti fks tks fromPair = do
    let toAccountText = unAccountName $ _ca_account $ _ti_toAccount ti
    case parsePublicKey toAccountText of
      Left _ -> do
        cancel <- fatalTransferError $
          el "div" $ text "Account does not exist, you must specify a keyset."
        return ((mempty, cancel), never)
      Right pk -> do
        close <- modalHeader $ text "Account Keyset"
        _ <- elClass "div" "modal__main" $ do
          el "div" $ text $ "The receiving account name looks like a public key and you did not specify a keyset.  Would you like to use it as the keyset to send to?"
          el "hr" blank
          el "div" $
            mkLabeledInput False "Public Key" uiInputElement $ def
              & initialAttributes .~ "disabled" =: "disabled"
              & inputElementConfig_initialValue .~ toAccountText
        modalFooter $ do
          cancel <- cancelButton def "No, take me back"
          let cfg = def & uiButtonCfg_class <>~ "button_type_confirm"
          next <- uiButtonDyn cfg $ text "Yes, proceed to transfer"
          let ti2 = ti { _ti_toKeyset = Just $ UserKeyset (Set.singleton pk) KeysAll }
          return ((mempty, close <> cancel),
                  Workflow (transferDialog model netInfo ti2 fks tks fromPair) <$ next)

transferDialog
  :: ( MonadWidget t m, Monoid mConf
     , HasNetwork model t
     , HasNetworkCfg mConf t
     , HasLogger model t
     , HasWallet model key t
     , HasCrypto key m
     , HasCrypto key (Performable m)
     , HasTransactionLogger m
     )
  => model
  -> SharedNetInfo NodeInfo
  -> TransferInfo
  -> Map AccountName (AccountStatus AccountDetails)
  -> Map AccountName (AccountStatus AccountDetails)
  -> (AccountName, AccountDetails)
  -> m ((mConf, Event t ()), Event t (Workflow t m (mConf, Event t ())))
transferDialog model netInfo ti fks tks _ = do
    -- TODO Clean up the unused parameter
    -- It contains the from account name and details retrieved from blockchain
    close <- modalHeader $ text "Sign Transfer"
    rec
      (currentTab, _done) <- transferTabs newTab
      (conf, meta, payload, dSignedCmd, destChainInfo) <- mainSection currentTab
      (cancel, newTab, next) <- footerSection currentTab meta dSignedCmd

    case destChainInfo of
      Nothing -> do
        let nextScreen = ffor (tag (current dSignedCmd) next) $ \case
              Nothing -> Workflow $ pure (mempty, never)
              Just sc -> Workflow $ do
                res <- sameChainTransferAndStatus model netInfo ti sc
                pure (res, never)
        pure ((conf, close <> cancel), nextScreen)

      Just (dgp, dss) -> do
        let allDyns = (,,,) <$> current payload <*> current dSignedCmd <*> current dgp <*> current dss
        let nextScreen = ffor (tag allDyns next) $ \case
              (_,Nothing,_,_) -> Workflow $ pure (mempty, never)
              (p,Just sc,gp,ss) -> Workflow $ do
                res <- crossChainTransferAndStatus model netInfo ti sc gp ss
                pure (res, never)
        pure ((conf, close <> cancel), nextScreen)
  where
    mainSection currentTab = elClass "div" "modal__main" $ do
      (conf, meta, destChainInfo) <- tabPane mempty currentTab TransferTab_Metadata $
        transferMetadata model netInfo fks tks ti
      let payload = buildUnsignedCmd netInfo ti <$> meta
      edSigned <- tabPane mempty currentTab TransferTab_Signatures $ do
        networkView $ transferSigs
          <$> (model ^. wallet_keys)
          <*> payload
          <*> (_transferMeta_sourceChainSigners <$> meta)
      sc <- holdUniqDyn . join =<< holdDyn (constDyn Nothing) (Just <$$> edSigned)
      return (conf, meta, payload, sc, destChainInfo)
    footerSection currentTab meta sc = modalFooter $ do
      let (lbl, fanTag) = splitDynPure $ ffor currentTab $ \case
            TransferTab_Metadata -> ("Cancel", Left ())
            TransferTab_Signatures -> ("Back", Right TransferTab_Metadata)
      ev <- cancelButton def lbl
      let (cancel, back) = fanEither $ current fanTag <@ ev

      let isDisabled m s = length (_transferMeta_sourceChainSigners m) /= maybe (-1) (length . _cmdSigs) s
      let mkNextButton ct m s = case ct of
            TransferTab_Metadata -> ("Next", constDyn False) -- TODO Properly enable/disable Next button
            TransferTab_Signatures -> ("Preview", isDisabled <$> meta <*> sc)
          (name, disabled) = splitDynPure $ mkNextButton <$> currentTab <*> meta <*> sc
          cfg = def
            & uiButtonCfg_class <>~ "button_type_confirm"
            & uiButtonCfg_disabled .~ join disabled
      nextClick <- uiButtonDyn cfg $ dynText name
      let newTab = ffor (tag (current currentTab) nextClick) $ \case
            TransferTab_Metadata -> Just TransferTab_Signatures
            TransferTab_Signatures -> Nothing

      let tabChange = leftmost [back, fmapMaybe id newTab]
      let screenChange = () <$ ffilter isNothing newTab

      pure (cancel, tabChange, screenChange)

sameChainTransferAndStatus
  :: (MonadWidget t m, Monoid mConf, HasLogger model t, HasTransactionLogger m)
  => model
  -> SharedNetInfo NodeInfo
  -> TransferInfo -- TODO Not principle of least context, but quick and dirty for now
  -> Command Text
  -> m (mConf, Event t ())
sameChainTransferAndStatus model netInfo ti cmd = do
    let nodeInfos = _sharedNetInfo_nodes netInfo
    close <- modalHeader $ text "Transfer Status"
    _ <- elClass "div" "modal__main transaction_details" $
      submitTransactionWithFeedback model cmd fromAccount fromChain (fmap Right nodeInfos)
    done <- modalFooter $ confirmButton def "Done"
    pure (mempty, close <> done)
  where
    fromChain = _ca_chain $ _ti_fromAccount ti
    fromAccount = _ca_account $ _ti_fromAccount ti

crossChainTransferAndStatus
  :: ( MonadWidget t m
     , Monoid mConf
     , HasLogger model t
     , HasTransactionLogger m
     , HasWallet model key t
     , HasCrypto key (Performable m)
     )
  => model
  -> SharedNetInfo NodeInfo
  -> TransferInfo -- TODO Not principle of least context, but quick and dirty for now
  -> Command Text
  -> Maybe (AccountName, AccountStatus AccountDetails)
  -> [Signer]
  -> m (mConf, Event t ())
crossChainTransferAndStatus model netInfo ti cmd Nothing destSigners = pure (mempty, never)
crossChainTransferAndStatus model netInfo ti cmd (Just destGP) destSigners = do
    let logL = model ^. logger
    let nodeInfos = _sharedNetInfo_nodes netInfo
    close <- modalHeader $ text "Cross Chain Transfer"
    _ <- elClass "div" "modal__main" $ do
      transactionHashSection cmd
      fbk <- submitTransactionAndListen model cmd fromAccount fromChain (fmap Right nodeInfos)
      let listenDone = ffilter (==Status_Done) $ updated $ _transactionSubmitFeedback_listenStatus fbk
          -- Not sure whether this should be when the listen is done or when the send is done
          rk = RequestKey (toUntypedHash $ _cmdHash cmd) <$ listenDone
      (resultOk0, errMsg0, retry0) <- divClass "group" $ do
        elClass "ol" "transaction_status" $ do
          let item ds = elDynAttr "li" (ffor ds $ \s -> "class" =: statusText s)
          item (_transactionSubmitFeedback_sendStatus fbk) $
            el "p" $ text $ "Cross chain transfer initiated on chain " <> _chainId fromChain

          keys <- sample $ current $ model ^. wallet_keys
          runUnfinishedCrossChainTransfer logL netInfo keys fromChain toChain destGP rk

      let isError = \case
            Just (Left _) -> True
            _ -> False
      let initiatedError = ffilter isError $ updated $ _transactionSubmitFeedback_message fbk
          toErrMsg = maybe "" (either prettyPrintNetworkError (const ""))
      let errMsg = leftmost [toErrMsg <$> initiatedError, errMsg0]
      dialogSectionHeading mempty "Transaction Result"
      divClass "group" $ do
        void $ runWithReplace (text "Waiting for response...") $ leftmost
          [ text . ("Request Key " <>) . Pact.requestKeyToB16Text <$> rk
          , text <$> errMsg
          , blank <$ retry0
          ]

      pure resultOk0
    done <- modalFooter $ confirmButton def "Done"
    pure (mempty, close <> done)
  where
    fromChain = _ca_chain $ _ti_fromAccount ti
    toChain = _ca_chain $ _ti_toAccount ti
    fromAccount = _ca_account $ _ti_fromAccount ti

submitTransactionAndListen
  :: ( CanSubmitTransaction t m
     , HasLogger model t
     , HasTransactionLogger m
     )
  => model
  -> Pact.Command Text
  -> AccountName
  -> ChainId
  -> [Either a NodeInfo]
  -> m (TransactionSubmitFeedback t)
submitTransactionAndListen model cmd sender chain nodeInfos = do
  -- Shove the node infos into servant client envs
  clientEnvs <- fmap catMaybes $ for (rights nodeInfos) $ \nodeInfo -> do
    getChainRefBaseUrl (ChainRef Nothing chain) (Just nodeInfo) >>= \case
      Left e -> do
        putLog model LevelWarn $ "deploySubmit: Couldn't get chainUrl: " <> e
        pure Nothing
      Right chainUrl -> case S.parseBaseUrl $ URI.renderStr chainUrl of
        Nothing -> do
          putLog model LevelWarn $ T.pack $ "deploySubmit: Failed to parse chainUrl: " <> URI.renderStr chainUrl
          pure Nothing

        Just baseUrl -> pure $ Just $ S.mkClientEnv baseUrl

  -- These maintain the UI state for each step and are updated as responses come in
  (sendStatus, send) <- newTriggerHold Status_Waiting

  -- Send the transaction

  pb <- getPostBuild
  transactionLogger <- askTransactionLogger
  rec
    onRequestKey <- performEventAsync $ ffor pb $ \() cb -> liftJSM $ do
      send Status_Working
      doReqFailover clientEnvs (Api.send Api.apiV1Client transactionLogger (unAccountName sender) chain $ Api.SubmitBatch $ pure cmd) >>= \case
        Left errs -> do
          send Status_Failed
          for_ (nonEmpty errs) $ setMessage . Just . Left . packHttpErr . NEL.last
        Right (Api.RequestKeys (key :| _)) -> do
          send Status_Done
          liftIO $ cb key
    (listenStatus, message, setMessage) <- listenToRequestKey clientEnvs $ Just <$> onRequestKey
    requestKey <- holdDyn Nothing $ Just <$> onRequestKey
  pure $ TransactionSubmitFeedback sendStatus listenStatus message

payloadToCommand :: Payload PublicMeta Text -> Command Text
payloadToCommand p =
    Pact.Command payloadText [] (hash $ T.encodeUtf8 payloadText)
  where
    payloadText = encodeAsText $ encode p

buildUnsignedCmd
  :: SharedNetInfo NodeInfo
  -> TransferInfo
  -> TransferMeta
  -> Payload PublicMeta Text
buildUnsignedCmd netInfo ti tmeta = payload
  where
    network = _sharedNetInfo_network netInfo
    fromAccount = unAccountName $ _ca_account $ _ti_fromAccount ti
    fromChain = _ca_chain $ _ti_fromAccount ti
    toAccount = unAccountName $ _ca_account $ _ti_toAccount ti
    toChain = _ca_chain $ _ti_toAccount ti
    amount = _ti_amount ti
    amountString = if not ('.' `elem` s) then s ++ ".0" else s
      where
        s = show amount
    dataKey = "ks" :: Text
    (mDataKey, code) = if fromChain == toChain
             then case _ti_toKeyset ti of
                    Nothing ->
                      (Nothing, printf "(coin.transfer %s %s %s)"
                                  (show fromAccount) (show toAccount) amountString)
                    Just ks ->
                      (Just dataKey, printf "(coin.transfer-create %s %s (read-keyset '%s) %s)"
                                    (show fromAccount) (show toAccount) dataKey amountString)
             else -- cross-chain transfer
               (Just dataKey, printf "(coin.transfer-crosschain %s %s (read-keyset '%s) %s %s)"
                             (show fromAccount) (show toAccount) (T.unpack dataKey) (show toChain) amountString)
    tdata = maybe Null (\a -> object [ dataKey .= toJSON (userToPactKeyset a) ]) $ _ti_toKeyset ti
    lim = _transferMeta_gasLimit tmeta
    price = _transferMeta_gasPrice tmeta
    ttl = _transferMeta_ttl tmeta
    ct = _transferMeta_creationTime tmeta
    meta = PublicMeta fromChain (fromAccount) lim price ttl ct

    signers = _transferMeta_sourceChainSigners tmeta

    payload = Payload
      { _pPayload = Exec (ExecMsg (T.pack code) tdata)
      , _pNonce = "chainweaver"
      , _pMeta = meta
      , _pSigners = signers
      , _pNetworkId = pure $ NetworkId $ textNetworkName network
      }


-- | An unrecoverable error.  Display an error message and only allow cancel.
fatalTransferError
  :: ( DomBuilder t m, PostBuild t m
     )
  => m ()
  -> m (Event t ())
fatalTransferError msg = do
  close <- modalHeader $ text "Transfer Error"
  elClass "div" "modal__main" msg
  cancel <- modalFooter $ do
    cancelButton def "Cancel"
  return $ leftmost [close, cancel]

isMissingGasPayer
  :: Map ChainId (Map AccountName (AccountStatus AccountDetails))
  -> (ChainId, Maybe AccountName)
  -> Bool
isMissingGasPayer _ (_, Nothing) = False
isMissingGasPayer keysets (cid, Just a) = isNothing $ Map.lookup a =<< Map.lookup cid keysets

data GasPayerDetails = GasPayerDetails
  { gpdChain :: ChainId
  , gpdAccount :: Maybe AccountName
  , gpdDetails :: Maybe (AccountStatus AccountDetails)
  }
-- Fields in this structure are Dynamic because they get consumed independently
data GasPayers t = GasPayers
  { srcChainGasPayer :: Dynamic t GasPayerDetails
    -- Outer Maybe indicates cross-chain transfer (redundantly with fromChain == toChain, but oh well)
    -- Inner Maybe indicates whether there is a gas payer.  Private blockchains allow this.
  , destChainGasPayer :: Maybe (Dynamic t GasPayerDetails)
  }

gasPayersSection
  :: ( MonadWidget t m, HasLogger model t
     , HasCrypto key m
     )
  => model
  -> SharedNetInfo NodeInfo
  -> TransferInfo
  -> m (GasPayers t)
gasPayersSection model netInfo ti = do
    let fromChain = _ca_chain (_ti_fromAccount ti)
        fromAccount = _ca_account (_ti_fromAccount ti)
        toChain = _ca_chain (_ti_toAccount ti)
    (dgp1, mdgp2) <- if fromChain == toChain
      then do
        (_,dgp1) <- uiAccountNameInput "Gas Paying Account" True (Just $ _ca_account $ _ti_fromAccount ti) never noValidation
        pure $ (dgp1, Nothing)
      else do
        let mkLabel c = T.pack $ printf "Gas Paying Account (Chain %s)" (T.unpack $ _chainId c)
        (_,dgp1) <- uiAccountNameInput (mkLabel fromChain) True (Just $ _ca_account $ _ti_fromAccount ti) never noValidation
        (_,dgp2) <- uiAccountNameInput (mkLabel toChain) True Nothing never noValidation
        pure $ (dgp1, Just dgp2)
    let getGasPayerKeys chain mgp = do
          case mgp of
            Nothing -> return never
            Just gp -> do
              -- I think lookupKeySets can't be done in the PushM monad
              evt <- lookupKeySets (model ^. logger) (_sharedNetInfo_network netInfo)
                            (_sharedNetInfo_nodes netInfo) chain [gp]
              return $ Map.lookup gp <$> fmapMaybe id evt

    -- Event t (Maybe AccountName)
    gp1Debounced <- debounce 1.0 $ updated dgp1
    -- Maybe (Event t (Maybe AccountName))
    mgp2Debounced <- maybe (return Nothing) (fmap Just . debounce 1.0 . updated) mdgp2
    -- also not sure what to do if the source chain gas payer is the same as dest chain gas payer
    -- should we let the user choose different signers for the source and dest chain?
    gp1Keys <- networkHold (return never)
                           (getGasPayerKeys fromChain <$> gp1Debounced)
    gp1 <- foldDyn ($) (GasPayerDetails fromChain (Just fromAccount) Nothing) $ leftmost
      [ (\a gp -> gp { gpdAccount = a }) <$> updated dgp1
      , (\keys gp -> gp { gpdDetails = keys }) <$> switch (current gp1Keys)
      ]
    case mgp2Debounced of
      Nothing -> return $ GasPayers gp1 Nothing
      Just dgp2 -> do
        gp2Keys <- networkHold (return never) (getGasPayerKeys toChain <$> dgp2)
        gp2 <- foldDyn ($) (GasPayerDetails toChain Nothing Nothing) $ leftmost
          [ (\a gp -> gp { gpdAccount = a }) <$> maybe never updated mdgp2
          , (\keys gp -> gp { gpdDetails = keys }) <$> switch (current gp2Keys)
          ]
        return $ GasPayers gp1 (Just gp2)


data TransferMeta = TransferMeta
  { _transferMeta_gasPrice :: GasPrice
  , _transferMeta_gasLimit :: GasLimit
  , _transferMeta_ttl :: TTLSeconds
  , _transferMeta_creationTime :: TxCreationTime
  , _transferMeta_sourceChainSigners :: [Signer]
  } deriving (Eq,Ord,Show)

transferCapability :: AccountName -> AccountName -> Decimal -> SigCapability
transferCapability from to amount = SigCapability
  { _scName = QualifiedName { _qnQual = "coin", _qnName = "TRANSFER", _qnInfo = def }
  , _scArgs =
    [ PLiteral $ LString $ unAccountName from
    , PLiteral $ LString $ unAccountName to
    , PLiteral $ LDecimal amount
    ]
  }

crosschainCapability :: AccountName -> SigCapability
crosschainCapability from = SigCapability
  { _scName = QualifiedName { _qnQual = "coin", _qnName = "DEBIT", _qnInfo = def }
  , _scArgs = [PLiteral $ LString $ unAccountName from]
  }

gasCapability :: SigCapability
gasCapability = SigCapability
  { _scName = QualifiedName
    { _qnQual = ModuleName
      { _mnName = "coin"
      , _mnNamespace = Nothing
      }
    , _qnName = "GAS"
    , _qnInfo = mkInfo "coin.GAS"
    }
  , _scArgs = []
  }

flashyStr label a = unlines [dashes, label, a, dashes]
  where
    dashes = "-----------------"

transferMetadata
  :: forall model mConf key t m.
     ( MonadWidget t m, HasNetwork model t, HasNetworkCfg mConf t, Monoid mConf
     , HasLogger model t
     , HasCrypto key m
     )
  => model
  -> SharedNetInfo NodeInfo
  -> Map AccountName (AccountStatus AccountDetails)
  -> Map AccountName (AccountStatus AccountDetails)
  -> TransferInfo
  -> m (mConf,
        Dynamic t TransferMeta,
        -- Destination chain gas payer and signer information when there is a cross-chain transfer
        Maybe (Dynamic t (Maybe (AccountName, AccountStatus AccountDetails)), Dynamic t [Signer]))
transferMetadata model netInfo fks tks ti = do
  (GasPayers srcPayer mdestPayer) <- gasPayersSection model netInfo ti
  let fromChain = _ca_chain $ _ti_fromAccount ti
      fromAccount = _ca_account $ _ti_fromAccount ti
      toChain = _ca_chain $ _ti_toAccount ti
      toAccount = _ca_account $ _ti_toAccount ti
      amount = _ti_amount ti
      ks = Map.fromList [(fromChain, fks), (toChain, tks)]

  let senderAction = getKeysetActionSingle fromChain fks (Just fromAccount)
      dgasAction = getKeysetActionSingle fromChain fks . gpdAccount <$> srcPayer
      dDestGasAction = getKeysetActionSingle toChain tks . gpdAccount <$$> mdestPayer
      actions = do
        ga <- dgasAction
        case dDestGasAction of
          Nothing -> pure $ Set.toList $ Set.fromList [senderAction, ga]
          Just ddga -> do
            dga <- ddga
            pure $ Set.toList $ Set.fromList [senderAction, ga, dga]

  -- TODO Filter out ambiguous accounts that Chainweaver knows how to sign for
  esigners <- networkView (signersSection <$> actions)
  signTuples <- fmap join $ holdDyn (constDyn mempty) esigners

  -- To get the final list of signers we need to take all the public keys from
  -- each of the unambiguous keysets. Then we combine that with the signers
  -- specified by the user which came from the ambiguous keysets. For now we just
  -- ignore the KeysetError and KeysetNoAction constructors because not doing so
  -- should only ever result in failed transactions. We can come back later and
  -- try to prevent them if it makes sense.
  let mkSigner (k, caps) = Signer Nothing (keyToText k) Nothing caps

      -- Get just the keys for coin.TRANSFER
      fromTxKeys = fromMaybe [] . Map.lookup (fromChain, fromAccount) <$> signTuples

      -- Get just the keys for coin.GAS
      lookupGas chain Nothing = mempty
      lookupGas chain (Just gp) = Map.lookup (chain, gp)
      fromGasKeys = fmap (fromMaybe mempty) $ lookupGas fromChain <$> fmap gpdAccount srcPayer <*> signTuples
      toGasKeys = case mdestPayer of
                    Nothing -> constDyn []
                    Just dp -> fmap (fromMaybe mempty) $ lookupGas toChain <$> fmap gpdAccount dp <*> signTuples

      -- Build up a Map PublicKey [SigCapability]
      addCap cap pk = Map.insertWith (<>) pk [cap]
      gasCaps = foldr (addCap gasCapability) mempty <$> fromGasKeys
      transferCap = if fromChain == toChain
                      then transferCapability fromAccount toAccount amount
                      else crosschainCapability fromAccount
      allFromCaps = foldr (addCap transferCap) <$> gasCaps <*> fromTxKeys
      toCaps = foldr (addCap gasCapability) mempty <$> toGasKeys

      -- Convert that map into a [Signer]
      fromSigners = map mkSigner . Map.toList <$> allFromCaps
      toSigners = map mkSigner . Map.toList <$> toCaps

      lookupDetails mp = (\p -> (p,) <$> Map.lookup p tks) =<< mp
      destChainSigners = case mdestPayer of
        Nothing -> Nothing
        Just dp -> Just $ ( lookupDetails . gpdAccount <$> dp
                          , traceDynWith (flashyStr "toSigners" . unlines . map show) toSigners )

  performEvent_ (liftIO . putStrLn . ("toSigners changed: "<>) . show <$> updated toSigners)

  dialogSectionHeading mempty "Transaction Settings"
  divClass "group" $ do
    (conf, ttl, lim, price) <- uiMetaData model Nothing Nothing
    elAttr "div" ("style" =: "margin-top: 10px") $ do
      now <- fmap round $ liftIO $ getPOSIXTime
      let timeParser = maybe (Left "Not a valid creation time") Right . readMaybe . T.unpack
      ect <- mkLabeledInput True "Creation Time" (fmap snd . uiParsingInputElement timeParser)
        (def { _inputElementConfig_initialValue = tshow now})
      ct <- holdDyn now $ fmapMaybe hush $ updated ect
      let meta = TransferMeta
             <$> price
             <*> lim
             <*> ttl
             <*> (TxCreationTime . ParsedInteger <$> ct)
             <*> traceDynWith (flashyStr "fromSigners" . unlines . map show) fromSigners
      return (conf, meta, destChainSigners)

combineStatus :: AccountStatus a -> AccountStatus a -> AccountStatus a
combineStatus a@(AccountStatus_Exists _) _ = a
combineStatus _ a@(AccountStatus_Exists _) = a
combineStatus a _ = a

getKeysetActions
  :: TransferInfo
  -> Map ChainId (Map AccountName (AccountStatus AccountDetails))
  -> [(ChainId, Maybe AccountName)]
  -> [KeysetAction]
getKeysetActions ti ks gps = map (getKeysetAction ks) signers
  where
    fromAccount = _ca_account $ _ti_fromAccount ti
    fromChain = _ca_chain $ _ti_fromAccount ti
    fromPair = (fromChain, Just fromAccount)
    signers = if fromPair `elem` gps then gps else (fromPair : gps)

-- | Returns a list of all the keys that should be signed with and their
-- associated chains and account names. These are either there because they were
-- unambiguous (keys-all or a single-key keyset) or the user selected them.
signersSection
  :: (MonadWidget t m)
  => [KeysetAction]
  -> m (Dynamic t (Map (ChainId, AccountName) [PublicKey]))
signersSection actions = do
    let ambiguous = actions ^.. each . _KeysetAmbiguous
        unamb = Map.fromList $ map (\(c,a,uk) -> ((c,a), Set.toList $ _userKeyset_keys uk)) $
                  actions ^.. each . _KeysetUnambiguous
        ukToKeys (c,a,uk) = (c,a,) <$> Set.toList (_userKeyset_keys uk)
    ambig <- if (null ambiguous)
      then return $ constDyn mempty
      else do
        dialogSectionHeading mempty "Public Keys to Sign With"
        dynList <- divClass "group signing-ui-signers" $ do
          mapM singleSigner ambiguous
        return $ Map.unions <$> distributeListOverDyn dynList
    return $ Map.union unamb <$> ambig

singleSigner
  :: (DomBuilder t m, PostBuild t m)
  => (ChainId, AccountName, UserKeyset)
  -> m (Dynamic t (Map (ChainId, AccountName) [PublicKey]))
singleSigner (c, a, UserKeyset keys p) = do
    elClass "h3" ("heading heading_type_h3") $
      text $ unAccountName a <> " (" <> T.toLower (prettyPred $ renderKeysetPred p) <> ")"
    res <- forM (Set.toList keys) $ \key -> do
      c <- uiCheckbox "signing-ui-signers__signer" False def $ text $ keyToText key
      return $ bool Nothing (Just key) <$> value c
    pure $ Map.singleton (c, a) . catMaybes <$> distributeListOverDyn res

getKeysetAction
  :: Map ChainId (Map AccountName (AccountStatus AccountDetails))
  -> (ChainId, Maybe AccountName)
  -> KeysetAction
getKeysetAction _ (_,Nothing) = KeysetNoAction
getKeysetAction allKeys (c,Just a) =
  case Map.lookup a =<< Map.lookup c allKeys of
    Nothing -> KeysetError $ T.pack $
      printf "Couldn't find account %s on chain %s" (unAccountName a) (show c)
    Just AccountStatus_DoesNotExist -> KeysetError $ T.pack $
      printf "Account %s does not exist on chain %s" (unAccountName a) (show c)
    Just AccountStatus_Unknown -> KeysetNoAction -- TODO not sure about this
    Just (AccountStatus_Exists (AccountDetails _ g)) -> do
      case g of
        AccountGuard_Other _ -> KeysetNoAction
        AccountGuard_KeySet keys p -> do
          let ks = UserKeyset keys (parseKeysetPred p)
          if unambiguousKeyset keys p
            then KeysetUnambiguous (c, a, ks)
            else KeysetAmbiguous (c, a, ks)

getKeysetActionSingle
  :: ChainId
  -> Map AccountName (AccountStatus AccountDetails)
  -> Maybe AccountName
  -> KeysetAction
getKeysetActionSingle _ _ (Nothing) = KeysetNoAction
getKeysetActionSingle c keyDetails (Just a) =
  case Map.lookup a keyDetails of
    Nothing -> KeysetError $ T.pack $
      printf "Couldn't find account %s on chain %s" (unAccountName a) (show c)
    Just AccountStatus_DoesNotExist -> KeysetError $ T.pack $
      printf "Account %s does not exist on chain %s" (unAccountName a) (show c)
    Just AccountStatus_Unknown -> KeysetNoAction -- TODO not sure about this
    Just (AccountStatus_Exists (AccountDetails _ g)) -> do
      case g of
        AccountGuard_Other _ -> KeysetNoAction
        AccountGuard_KeySet keys p -> do
          let ks = UserKeyset keys (parseKeysetPred p)
          if unambiguousKeyset keys p
            then KeysetUnambiguous (c, a, ks)
            else KeysetAmbiguous (c, a, ks)

unambiguousKeyset :: Set PublicKey -> Text -> Bool
unambiguousKeyset keys p =
  p == "keys-all" || length keys == 1 || (length keys == 2 && p == "keys-2")

data AdvancedDetails
  = DetailsJson
  | DetailsYaml
  deriving (Eq,Ord,Show,Read,Enum)

transferSigs
  :: ( MonadWidget t m
     , HasCrypto key m
     )
  => (KeyStorage key)
  -> Payload PublicMeta Text
  -> [Signer]
  -> m (Dynamic t (Command Text))
transferSigs keyStorage payload signers = do
  let cmd = payloadToCommand payload
  let hash = toUntypedHash $ _cmdHash cmd
  _ <- divClass "group" $ do
    mkLabeledInput True "Request Key" uiInputElement $ def
      & initialAttributes .~ "disabled" =: "disabled"
      & inputElementConfig_initialValue .~ (hashToText hash)
      & inputElementConfig_setValue .~ never

  let mkKeyTuple (KeyPair pub priv) = (pub, priv)
  let cwKeyMap = Map.fromList . map (mkKeyTuple . _key_pair) . IntMap.elems $ keyStorage
      pkt2pk = fromPactPublicKey . Pact.PublicKey . T.encodeUtf8
      sigsNeeded = length $ filter (\s -> not $ isJust $ join $ Map.lookup (pkt2pk $ _siPubKey s) cwKeyMap) signers

  dialogSectionHeading mempty $ if sigsNeeded > 0 then "External Signatures" else "No external signatures needed"

  let sig s = do
        let pubKeyText = _siPubKey s
            pubKey = pkt2pk pubKeyText
        case Map.lookup pubKey cwKeyMap of
          Just (Just priv) -> do
            s <- cryptoSign (unHash hash) priv
            pb <- getPostBuild
            let ju = Just $ UserSig $ keyToText s
            holdDyn ju (ju <$ pb)
          _ -> do
            el "div" $ text pubKeyText
            uiSigningInput hash pubKey
      wrapper sigSection = if sigsNeeded > 0
        then divClass "group signing-ui-signers" sigSection
        else sigSection
  ddsigs <- wrapper $ do
    forM signers $ \s -> do
      sig s

  let sigs = distributeListOverDynPure ddsigs
  let addSigs ss = cmd { _cmdSigs = catMaybes ss }
  let signedCmd = addSigs <$> sigs

  rec
    keysetOpen <- toggle False clk
    (clk,(_,k)) <- controlledAccordionItem keysetOpen mempty
      (accordionHeaderBtn "Advanced Details") $ do
        transferDetails signedCmd

  return signedCmd

data TransferDetails
  = TransferDetails_Yaml
  | TransferDetails_Json
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

showTransferDetailsTabName :: TransferDetails -> Text
showTransferDetailsTabName = \case
  TransferDetails_Yaml -> "YAML"
  TransferDetails_Json -> "JSON"

transferDetails
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t (Command Text)
  -> m ()
transferDetails signedCmd = do
    divClass "tabset" $ mdo
      curSelection <- holdDyn TransferDetails_Yaml onTabClick
      (TabBar onTabClick) <- makeTabBar $ TabBarCfg
        { _tabBarCfg_tabs = [minBound .. maxBound]
        , _tabBarCfg_mkLabel = const $ text . showTransferDetailsTabName
        , _tabBarCfg_selectedTab = Just <$> curSelection
        , _tabBarCfg_classes = mempty
        , _tabBarCfg_type = TabBarType_Primary
        }

      tabPane mempty curSelection TransferDetails_Yaml $ do
        let preview = T.decodeUtf8 . Y.encodeWith yamlOptions <$> signedCmd
        iv <- sample (current preview)
        uiTextAreaElement $ def
          & textAreaElementConfig_initialValue .~ iv
          & initialAttributes %~ (<> "disabled" =: "" <> "style" =: "width: 100%; height: 18em;")
          & textAreaElementConfig_setValue .~ updated preview

      tabPane mempty curSelection TransferDetails_Json $ do
        let preview = T.decodeUtf8 . LB.toStrict . encode . toJSON <$> signedCmd
        iv <- sample (current preview)
        uiTextAreaElement $ def
          & textAreaElementConfig_initialValue .~ iv
          & initialAttributes %~ (<> "disabled" =: "" <> "style" =: "width: 100%; height: 18em;")
          & textAreaElementConfig_setValue .~ updated preview

      pure ()

yamlOptions :: Y.EncodeOptions
yamlOptions = Y.setFormat (Y.setWidth Nothing Y.defaultFormatOptions) Y.defaultEncodeOptions

uiSigningInput
  :: ( MonadWidget t m
     , HasCrypto key m
--     , HasCrypto key (Performable m)
     )
  => Hash
  -> PublicKey
  -> m (Dynamic t (Maybe UserSig))
uiSigningInput hash pubKey = do
  let hashBytes = unHash hash
  let
    inp cfg = do
      ie <- mkLabeledInput False mempty uiInputElement cfg

      sigDyn <- networkHold (pure $ Left "") (checkSigOrKey hash pubKey <$> updated (value ie))
      pure (ie
           , ( sigDyn
             , updated sigDyn
             )
           )

    inputCfg = def
      & initialAttributes .~ ("placeholder" =: "Signature or Private Key" <>
                              "class" =: "signature-input" <>
                              "type" =: "password")
                              --"rows" =: "2")

    -- Don't show the error popover if nothing has been entered
    -- See checkSigOrKey for where this is generated.
    mkErr e = if T.null e
                then PopoverState_Disabled
                else PopoverState_Error e -- "Must be a signature or private key"
    showPopover (_, (_, onInput)) = pure $
      either mkErr (const PopoverState_Disabled) <$> onInput

  (inputE, (dE, onE)) <- uiInputWithPopover
    inp
    (_inputElement_raw . fst)
    showPopover
    inputCfg

  pure $ hush <$> dE

checkSigOrKey
  :: forall key m t.
     ( MonadJSM m
     , HasCrypto key m
     )
  => Hash
  -> PublicKey
  -> Text
  -> m (Either Text UserSig)
checkSigOrKey hash pubKey userInput = do
    let hashBytes = unHash hash
    let tryAsKey t = do
          -- Expects input to be 64 hex characters
          case textToKey t of
            Nothing -> do
              pure $ Left "Couldn't parse as key"
            Just (PrivateKey privKey) -> do
              let pactKey = PactKey ED25519 pubKey privKey
              cryptoSignWithPactKeyEither hashBytes pactKey >>= \case
                Left e -> pure $ Left $ "Wrong key"
                Right sig -> do
                  valid <- cryptoVerify hashBytes sig pubKey
                  if valid
                    then pure $ Right sig
                    else pure $ Left $ "Key does not generate a valid signature"
        tryAsSig t = do
          case parseSignature t of
            Left e -> do
              pure $ Left $ "Error parsing signature"
            Right sig -> do
              --pure $ Right sig
              valid <- cryptoVerify hashBytes sig pubKey
              pure $ if valid
                then Right sig
                else Left "Signature not valid"

    res <- case T.length userInput of
      128 -> if T.drop 64 userInput == keyToText pubKey
               then tryAsKey $ T.take 64 userInput
               else tryAsSig userInput
      64 -> tryAsKey userInput

      -- This is kind of a janky way to prevent the error popup from displaying
      -- when the form first opens but it's good enough for now.
      0 -> pure $ Left ""
      _ -> pure $ Left genericErr
    pure $ UserSig . keyToText <$> res
  where
    genericErr = "Must specify a key or signature"

data TransferTab
  = TransferTab_Metadata
  | TransferTab_Signatures
  deriving (Eq, Ord, Show, Enum, Bounded)

displayTransferTab :: DomBuilder t m => TransferTab -> m ()
displayTransferTab = text . \case
  TransferTab_Signatures -> "Signatures"
  TransferTab_Metadata -> "Metadata"

transferTabs
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Event t TransferTab
  -> m (Dynamic t TransferTab, Event t ())
transferTabs tabEv = do
  let f t0 g = case g t0 of
        Nothing -> (Just t0, Just ())
        Just t -> (Just t, Nothing)
  rec
    (curSelection, done) <- mapAccumMaybeDyn f TransferTab_Metadata $ leftmost
      [ const . Just <$> onTabClick
      , const . Just <$> tabEv
      ]
    (TabBar onTabClick) <- makeTabBar $ TabBarCfg
      { _tabBarCfg_tabs = [TransferTab_Metadata, TransferTab_Signatures]
      , _tabBarCfg_mkLabel = \_ -> displayTransferTab
      , _tabBarCfg_selectedTab = Just <$> curSelection
      , _tabBarCfg_classes = mempty
      , _tabBarCfg_type = TabBarType_Secondary
      }
  pure (curSelection, done)
