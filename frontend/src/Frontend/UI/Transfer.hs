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

{-# LANGUAGE CPP #-}
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

import           Control.Applicative
import           Control.Error hiding (bool, note)
import           Control.Lens hiding ((.=))
import           Control.Monad.State.Strict
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as LB
import           Data.Decimal
import           Data.Default (Default (..))
import qualified Data.IntMap as IntMap
import           Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NEL
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import           Data.These (These(This))
import           Data.Traversable
import           Data.Time.Clock.POSIX

#if !defined(ghcjs_HOST_OS)
import qualified Codec.QRCode as QR
import qualified Codec.QRCode.JuicyPixels as QR
#endif
import qualified Data.YAML.Aeson as Y

import           Pact.Types.SigData
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
import qualified Pact.Types.Term as Pact
import           Reflex.Dom.Core
import qualified Servant.Client.JSaddle as S
import           Text.Printf
import           Text.Read (readMaybe)
import qualified Text.URI as URI

import           Common.Foundation
import           Common.Wallet
import           Frontend.Crypto.Class
import           Frontend.Crypto.Ed25519
import           Frontend.Crypto.Signature
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
import           Frontend.UI.Dialogs.AccountDetails
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
  cd <- uiMandatoryChainSelection (getChainsFromHomogenousNetwork model)
                                  (maybe (ChainId "0") _ca_chain <$> cfg)
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
      & initialAttributes %~ (<> "placeholder" =: "Account Name or Paste Tx Builder")
      & setValue %~ modSetValue (Just (Just . mkChainAccount <$> pastedBuilder))

  let keysetStartsOpen = case snd (_initialValue cfg) of
                           Nothing -> False
                           Just uk -> not $ Set.null $ _userKeyset_keys uk
  keysetOpen <- foldDyn ($) keysetStartsOpen $ leftmost
    [ const not <$> clk
    , const True <$ pastedBuilder
    ]
  (clk,(_,k)) <- controlledAccordionItem keysetOpen mempty (accordionHeaderBtn "Owner Keyset") $ do
    keysetFormWidget $ (snd <$> cfg)
      & setValue %~ modSetValue (Just (fmap userFromPactKeyset . _txBuilder_keyset <$> pastedBuilder))

  return (tca,k)

data TransferInfo = TransferInfo
  { _ti_fromAccount :: ChainAccount
  , _ti_amount :: Decimal
  , _ti_maxAmount :: Bool
  , _ti_toAccount :: ChainAccount
  , _ti_toKeyset :: Maybe UserKeyset
  } deriving (Eq,Ord,Show)

data TransferType = NormalTransfer | SafeTransfer
  deriving (Eq,Ord,Show,Read,Enum)

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
        rec
          amt <- amountFormWithMaxButton model fca $ mkCfg (Left "", False)
            & setValue .~ (Just $ (Left "", False) <$ leftmost
                [ clear

                -- If Max is checked, clear the amount and max checkbox when the
                -- ChainAccount is updated.  Otherwise leave it alone.
                , () <$ gate (fmap snd <$> current $ value amt) (updated (value fca))
                ])
        return (fca,amt)
      (toAcct,ks) <- divClass "transfer__right-pane" $ do
        el "h4" $ text "To"
        toFormWidget model $ mkCfg (Nothing, Nothing)
          & setValue .~ (Just $ (Nothing, Nothing) <$ clear)
      return $ runMaybeT $ TransferInfo <$>
        MaybeT (value fromAcct) <*>
        MaybeT (hush . fst <$> value amount) <*>
        lift (snd <$> value amount) <*>
        MaybeT (value toAcct) <*>
        lift ks
    (clear, signTransfer) <- divClass "transfer-fields submit" $ do
      clr <- el "div" $ uiButton btnCfgTertiary $ text "Clear"
      normal <- confirmButton (def { _uiButtonCfg_disabled = (isNothing <$> transferInfo) }) "Sign & Transfer"
      let safeDisabled Nothing = True
          safeDisabled (Just i) = _ca_chain (_ti_fromAccount i) /= _ca_chain (_ti_toAccount i)
                               || _ti_maxAmount i
          -- It doesn't make sense to do safe transfer with max amount because
          -- the receiver sends coins back to the sender. If someone wants to do
          -- this, they can first do a safe transfer to create the new account
          -- and then do a second transfer for the max balance. The convenience
          -- of doing it in one transaction is not worth the additional code
          -- complexity.

          safeBtnCfg = def
            { _uiButtonCfg_disabled = (safeDisabled <$> transferInfo)
            , _uiButtonCfg_title = constDyn $ Just "Safe transfers make it impossible to lose coins by sending to the wrong public key when transferring to the same chain.  They require a little extra work because the receiving account also has to sign the transaction."
            }
      safe <- confirmButton safeBtnCfg "Safe Transfer"
      -- _ <- confirmButton (def { _uiButtonCfg_disabled = (isNothing <$> transferInfo) }) "Quick Transfer"
      let txEvt = leftmost
            [ NormalTransfer <$ normal
            , SafeTransfer <$ safe
            ]
      return (clr, txEvt)
    let netInfo = flip push signTransfer $ \ty -> do
          ni <- sampleNetInfo model
          return ((ty,) <$> ni)
    let mkModal (Just ti) (ty, ni) = Just $ lookupAndTransfer model ni ti ty
        mkModal Nothing _ = Nothing
    pure $ mempty & modalCfg_setModal .~ (attachWith mkModal (current transferInfo) netInfo)

amountFormWithMaxButton
  :: ( DomBuilder t m, MonadFix m
     , TriggerEvent t m
     , MonadHold t m
     , HasNetwork model t
     , HasLogger model t
     , HasCrypto key m
     , MonadJSM m
     )
  => model
  -> FormWidget t (Maybe ChainAccount)
  -> FormWidgetConfig t (Either String Decimal, Bool)
  -> m (FormWidget t (Either String Decimal, Bool))
amountFormWithMaxButton model ca cfg = do
  elClass "div" ("segment segment_type_tertiary labeled-input-inline") $ mdo
    divClass ("label labeled-input__label-inline") $ text "Amount (KDA)"
    let attrs = ffor maxE $ \isMaxed ->
          "disabled" =: (if isMaxed then Just "disabled" else Nothing)
        sv = Just $ leftmost
          [ maybe (Left "") (Right . normalizeDecimal . unAccountBalance) <$> maxedBalance
          , Left "" <$ ffilter not maxE
          ]
    amt <- amountFormWidget $ mkPfwc (fst <$> cfg)
      & setValue <>~ sv
      & initialAttributes <>~ ("class" =: "labeled-input__input")
      & modifyAttributes .~ attrs

    useEntireBalance <- do
      elKlass "label" ("input-max-toggle label checkbox checkbox_type_secondary") $ do
        cb <- checkboxFormWidget $ mkPfwc (snd <$> cfg)
        elClass "span" "checkbox__checkmark checkbox__checkmark_type_secondary" blank
        text "Max"
        pure cb

    let maxE = updated $ value useEntireBalance
    let maxSelected = ffilter id maxE
    details <- getAccountDetails model (fmapMaybe id $ tag (current $ value ca) maxSelected)
    let maxedBalance = join . fmap (^? (_AccountStatus_Exists . accountDetails_balance)) <$> details

    pure $ (,) <$> amt <*> useEntireBalance

getAccountDetails
  :: ( Reflex t, TriggerEvent t m, MonadJSM m
     , HasNetwork model t
     , MonadHold t m
     , Adjustable t m
     , HasLogger model t
     , HasCrypto key m
     )
  => model
  -> Event t ChainAccount
  -> m (Event t (Maybe (AccountStatus AccountDetails)))
getAccountDetails model eca = do
    let netAndCa = flip push eca $ \ca -> do
          ni <- sampleNetInfo model
          return $ (ca,) <$> ni
    dd <- networkHold (pure never) (go <$> netAndCa)
    pure $ switch $ current dd
  where
    go (ca, netInfo) = do
      let nodes = _sharedNetInfo_nodes netInfo
          chain = _ca_chain ca
          acct = _ca_account ca
          extractDetails Nothing = Nothing
          extractDetails (Just m) = Map.lookup acct m
      ks <- lookupKeySets (model ^. logger) (_sharedNetInfo_network netInfo)
                   nodes chain [acct]
      pure $ extractDetails <$> ks

transferModalTitle :: TransferType -> Text
transferModalTitle NormalTransfer = "Transfer"
transferModalTitle SafeTransfer = "Safe Transfer"

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
  -> TransferType
  -> Event t ()
  -> m (mConf, Event t ())
lookupAndTransfer model netInfo ti ty onCloseExternal = do
    let nodes = _sharedNetInfo_nodes netInfo
        fromAccount = _ca_account $ _ti_fromAccount ti
        fromChain = _ca_chain $ _ti_fromAccount ti
        toAccount = _ca_account $ _ti_toAccount ti
        toChain = _ca_chain $ _ti_toAccount ti
        accountNames = setify [fromAccount, toAccount]
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
            checkSendingAccountExists model netInfo ti ty fks tks
          mConf <- flatten =<< tagOnPostBuild conf
          let close = switch $ current closes
          pure (mConf, close)
        eWrapper (_, Nothing) = msgModal (transferModalTitle ty) $ text "Loading..."
        eWrapper (Nothing, _) = msgModal (transferModalTitle ty) $ text "Loading..."
    (conf, closes) <- splitDynPure <$> networkHold (msgModal (transferModalTitle ty) $ text "Querying keysets...")
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
    initReq <- doReqFailover envs (Api.local Api.apiV1Client cmd) >>= \case
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

    resolvedReq <- for initReq $ imapM $ \(AccountName name) details -> do
            let mref = details ^? _AccountStatus_Exists . accountDetails_guard . _AccountGuard_Other . Pact._GKeySetRef . to (\(Pact.KeySetName name) -> name)
                mbal = details ^? _AccountStatus_Exists . accountDetails_balance . (to unAccountBalance)
                updateBal old new = if old == new then old else new
            fmap (fromMaybe details) $ for (liftA2 (,) mbal mref) $ \(bal,ref) -> do
              let code = printf "{\"balance\" : (at 'balance (coin.details \"%s\")), \"guard\" : (describe-keyset \"%s\")}" name ref
              cmd <- simpleLocal Nothing networkName pm (T.pack code)
              doReqFailover envs (Api.local Api.apiV1Client cmd) >>= \case
                Left _ -> pure details
                Right cr -> case Pact._crResult cr of
                  Pact.PactResult (Right pv) -> do
                    putLog logL LevelInfo "lookupKeySets on ref: success"
                    let res = fromMaybe details $ do
                          guard' <- pv ^? Pact.Types.PactValue._PObject . (to Pact._objectMap) . (at "guard") . _Just . Pact.Types.PactValue._PGuard . (to (fromPactGuard mref))
                          balance <- pv ^? Pact.Types.PactValue._PObject . (to Pact._objectMap) . (at "balance") . _Just . Pact.Types.PactValue._PLiteral . (to (updateBal bal . _lDecimal))
                          return $ AccountStatus_Exists $ AccountDetails (AccountBalance balance) guard'
                    putLog logL LevelInfo $ tshow res
                    return res
                  Pact.PactResult (Left e) -> do
                    putLog logL LevelInfo $ "lookupKeySets failed on keyset-ref lookup:" <> tshow e
                    pure details


    liftIO $ trigger resolvedReq
  pure result

uiTransferButton
  :: ( DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => m (Dynamic t Bool)
uiTransferButton = mdo
  let buttonText = bool "Transfer Coins" "Hide Transfer" <$> isVisible
  click <- uiButton (headerBtnCfgPrimary & uiButtonCfg_class <>~ " main-header__account-button") $ do
    dynText buttonText
  isVisible <- toggle False click
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
  -> TransferType
  -> Map AccountName (AccountStatus AccountDetails)
  -> Map AccountName (AccountStatus AccountDetails)
  -> Workflow t m (mConf, Event t ())
checkSendingAccountExists model netInfo ti ty fks tks = do
    let fromAccount = _ca_account $ _ti_fromAccount ti
    case Map.lookup fromAccount fks of
      Just (AccountStatus_Exists ad) -> do
        checkReceivingAccount model netInfo ti ty fks tks (fromAccount, ad)
      _ -> Workflow $ do
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
  -> TransferType
  -> Map AccountName (AccountStatus AccountDetails)
  -> Map AccountName (AccountStatus AccountDetails)
  -> (AccountName, AccountDetails)
  -> Workflow t m (mConf, Event t ())
checkReceivingAccount model netInfo ti ty fks tks fromPair = do
    let toAccount = _ca_account $ _ti_toAccount ti
    case (Map.lookup toAccount tks, _ti_toKeyset ti) of
      -- TODO Might need more checks for cross-chain error cases
      (Just (AccountStatus_Exists (AccountDetails _ g)), Just userKeyset) -> do
        -- Use transfer-create, but check first to see whether it will fail
        let AccountGuard_KeySetLike (KeySetHeritage ks p _ref) = g
        let onChainKeyset = UserKeyset ks (parseKeysetPred p)
        if onChainKeyset /= userKeyset
          then Workflow $ do
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
            transferDialog model netInfo ti ty fks tks fromPair
      (Just (AccountStatus_Exists (AccountDetails _ g)), Nothing) -> do
        let 
          transferDialogWithWarn model netInfo ti ty fks tks fromPair = Workflow $ do
            close <- modalHeader $ text "Account Keyset"
            _ <- elClass "div" "modal__main" $ do
              el "h3" $ text "WARNING"
              el "div" $ text $ "The on-chain keyset of the receiving account does not match the account name. This may be an indicator of foul-play; you should confirm that the receiving keyset is the expected keyset before continuing"
              el "hr" blank
              el "div" $ text $ "If you are doing a cross-chain transfer to yourself, and see this message, you may want to reconsider, as it is possible that you don't have control over the account on the destination chain"
              el "hr" blank
              el "div" $ do
                dialogSectionHeading mempty "Destination Account Name:"
                mkLabeledInput False "Account Name" uiInputElement $ def
                  & initialAttributes .~ "disabled" =: "disabled"
                  & inputElementConfig_initialValue .~ (unAccountName toAccount)
                dialogSectionHeading mempty "Destination Account Guard:"
                uiDisplayKeyset g
            modalFooter $ do
              cancel <- cancelButton def "No, take me back"
              let cfg = def & uiButtonCfg_class <>~ "button_type_confirm"
              next <- uiButtonDyn cfg $ text "Yes, proceed to transfer"
              return ((mempty, close <> cancel),
                      (transferDialog model netInfo ti ty fks tks fromPair) <$ next)

          transferDialogWithKeysetCheck = case accountNameMatchesKeyset toAccount g of
            True -> transferDialog
            False -> transferDialogWithWarn
        if (_ca_chain $ _ti_fromAccount ti) /= (_ca_chain $ _ti_toAccount ti)
          then do
            case g of
              AccountGuard_KeySetLike (KeySetHeritage ks p _ref) ->
                let ti2 = ti { _ti_toKeyset = Just $ UserKeyset ks (parseKeysetPred p) }
                in transferDialogWithKeysetCheck model netInfo ti2 ty fks tks fromPair
              AccountGuard_Other _ -> transferDialogWithKeysetCheck model netInfo ti ty fks tks fromPair
          else
            -- Use transfer, probably show the guard at some point
            -- TODO check well-formedness of all keys in the keyset
            transferDialogWithKeysetCheck model netInfo ti ty fks tks fromPair
      (_, Just userKeyset) -> do
        -- Use transfer-create
        transferDialog model netInfo ti ty fks tks fromPair
      (_, Nothing) -> do
        -- If the account name looks like a public key, ask about making a keyset
        -- Otherwise throw an error
        handleMissingKeyset model netInfo ti ty fks tks fromPair

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
  -> TransferType
  -> Map AccountName (AccountStatus AccountDetails)
  -> Map AccountName (AccountStatus AccountDetails)
  -> (AccountName, AccountDetails)
  -> Workflow t m (mConf, Event t ())
handleMissingKeyset model netInfo ti ty fks tks fromPair = do
    let 
      toAccount = _ca_account $ _ti_toAccount ti
      toAccountText = unAccountName $ _ca_account $ _ti_toAccount ti
    case parsePubKeyOrKAccount toAccount of
      -- Vanity account name
      (_, Left _) -> Workflow $ do
        cancel <- fatalTransferError $
          el "div" $ text $ "Receiving account " <> toAccountText <> " does not exist. You must specify a keyset to create this account."
        return ((mempty, cancel), never)
      -- AccName "k:<pubkey>" --> Don't even ask approval to use it
      (True, Right pk) -> do
        let ti2 = ti { _ti_toKeyset = Just $ UserKeyset (Set.singleton pk) KeysAll }
        transferDialog model netInfo ti2 ty fks tks fromPair
      -- AccName: "<pubkey>" --> Ask for approval
      (False, Right pk) -> Workflow $ do
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
                  (transferDialog model netInfo ti2 ty fks tks fromPair) <$ next)

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
  -> TransferType
  -> Map AccountName (AccountStatus AccountDetails)
  -> Map AccountName (AccountStatus AccountDetails)
  -> (AccountName, AccountDetails)
  -> Workflow t m (mConf, Event t ())
transferDialog model netInfo ti ty fks tks unused = Workflow $ do
    -- TODO Clean up the unused parameter
    -- It contains the from account name and details retrieved from blockchain
    close <- modalHeader $ text $ transferModalTitle ty
    rec
      (currentTab, _done) <- transferTabs newTab
      (conf, meta, payload, dSignedCmd, destChainInfo) <- mainSection currentTab
      (cancel, newTab, next) <- footerSection currentTab meta dSignedCmd
    let backW = transferDialog model netInfo ti ty fks tks unused
    case destChainInfo of
      Nothing -> do
        let nextScreen = ffor (tag (current dSignedCmd) next) $ \case
              Nothing -> Workflow $ pure (mempty, never)
              Just sc -> previewDialog model netInfo ti payload sc backW $
                           sameChainTransferAndStatus model netInfo ti sc
        pure ((conf, close <> cancel), nextScreen)

      Just (dgp, dss) -> do
        let allDyns = (,,,,)
              <$> current payload
              <*> current dSignedCmd
              <*> current dgp
              <*> current dss
              <*> current meta
        let nextScreen = ffor (tag allDyns next) $ \case
              (_,Nothing,_,_,_) -> Workflow $ pure (mempty, never)
              (p,Just sc,gp,ss,meta') -> previewDialog model netInfo ti payload sc backW $
                                           crossChainTransferAndStatus model netInfo ti sc gp ss meta'
        pure ((conf, close <> cancel), nextScreen)
  where
    mainSection currentTab = elClass "div" "modal__main" $ do
      (conf, meta, destChainInfo) <- tabPane mempty currentTab TransferTab_Metadata $
        transferMetadata model netInfo fks tks ti ty
      let payload = buildUnsignedCmd netInfo ti ty <$> meta
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

previewTransaction
  :: ( MonadWidget t m
     , HasNetwork model t
     , HasLogger model t
     , HasTransactionLogger m
     )
  => model
  -> ChainId
  -> Dynamic t (Payload PublicMeta Text)
  -> Event t (Command Text)
  -> m ()
previewTransaction model chain payload cmd = do
    let mkReq c = [NetworkRequest c (ChainRef Nothing chain) Endpoint_Local]

    responses <- performLocalRead (model ^. logger) (model ^. network) (mkReq <$> cmd)

    (errors, resp) <- fmap fanThese $ performEvent $ ffor responses $ \case
      [(_, errorResult)] -> pure $ first prettyPrintNetworkErrors errorResult
      n -> do
        putLog model LevelWarn $ "Expected 1 response, but got " <> tshow (length n)
        pure $ This "Couldn't get a response from the node"

    dialogSectionHeading mempty "Transaction Result"
    void $ divClass "group segment transaction_details__raw-response"
      $ runWithReplace (text "Loading...") $ leftmost
      [ renderResult payload <$> resp
      , text <$> errors
      ]

renderResult
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t (Payload PublicMeta Text)
  -> (Maybe Gas, PactValue) -> m ()
renderResult payload (mgas, pactValue) = do
    case mgas of
      Nothing -> blank
      Just (Gas gas) -> uiPreviewItem "Gas Cost" $ dynText $ ((<> " KDA") . tshow . (fromIntegral gas *) . getGasPrice . _pmGasPrice . _pMeta) <$> payload
    uiPreviewItem "Result" $ text $ renderCompactText pactValue
  where
    getGasPrice (GasPrice p) = p

previewDialog
  :: ( MonadWidget t m
     , Monoid mConf
     , HasNetwork model t
     , HasLogger model t
     , HasTransactionLogger m
     )
  => model
  -> SharedNetInfo NodeInfo
  -> TransferInfo -- TODO Not principle of least context, but quick and dirty for now
  -> Dynamic t (Payload PublicMeta Text)
  -> Command Text
  -> Workflow t m (mConf, Event t ())
  -> Workflow t m (mConf, Event t ())
  -> Workflow t m (mConf, Event t ())
previewDialog model _netInfo ti payload cmd backW nextW = Workflow $ do
    close <- modalHeader $ text "Transfer Preview"
    _ <- elClass "div" "modal__main transaction_details" $ do
      dialogSectionHeading mempty "Summary"
      divClass "group" $ do
        transactionDisplayNetwork model

        uiPreviewItem "From Account" $ text $ unAccountName fromAccount
        uiPreviewItem "From Chain" $ text $ _chainId fromChain
        uiPreviewItem "To Account" $ text $ unAccountName toAccount
        uiPreviewItem "To Chain" $ text $ _chainId toChain
        uiPreviewItem "Amount" $ text $ tshow (_ti_amount ti) <> " KDA"
      pb <- getPostBuild
      previewTransaction model fromChain payload (cmd <$ pb)
      --submitTransactionWithFeedback model cmd fromAccount fromChain (fmap Right nodeInfos)
    (back, send) <- modalFooter $ (,)
      <$> cancelButton def "Back"
      <*> confirmButton def "Send Transfer"
    pure ((mempty, close), leftmost [ backW <$ back, nextW <$ send])
  where
    fromChain = _ca_chain $ _ti_fromAccount ti
    fromAccount = _ca_account $ _ti_fromAccount ti
    toChain = _ca_chain $ _ti_toAccount ti
    toAccount = _ca_account $ _ti_toAccount ti

uiPreviewItem :: DomBuilder t m => Text -> m a -> m a
uiPreviewItem label val =
  divClass "segment segment_type_tertiary labeled-input-inline" $ do
    divClass "label labeled-input__label-inline" (text label)
    divClass "labeled-preview" val

sameChainTransferAndStatus
  :: (MonadWidget t m, Monoid mConf, HasLogger model t, HasTransactionLogger m)
  => model
  -> SharedNetInfo NodeInfo
  -> TransferInfo -- TODO Not principle of least context, but quick and dirty for now
  -> Command Text
  -> Workflow t m (mConf, Event t ())
sameChainTransferAndStatus model netInfo ti cmd = Workflow $ do
    let nodeInfos = _sharedNetInfo_nodes netInfo
    close <- modalHeader $ text "Transfer Status"
    _ <- elClass "div" "modal__main transaction_details" $
      submitTransactionWithFeedback model cmd fromAccount fromChain (fmap Right nodeInfos)
    done <- modalFooter $ confirmButton def "Done"
    pure ((mempty, close <> done), never)
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
  -> TransferMeta
  -> Workflow t m (mConf, Event t ())
crossChainTransferAndStatus model netInfo ti cmd mdestGP destSigners meta = Workflow $ do
    let logL = model ^. logger
        nodeInfos = _sharedNetInfo_nodes netInfo
        toChainMeta = transferMetaToPublicMeta meta toChain
    close <- modalHeader $ text "Cross Chain Transfer"
    (resultOk, errMsg) <- elClass "div" "modal__main" $ do
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
          runUnfinishedCrossChainTransfer logL netInfo keys fromChain toChain mdestGP rk toChainMeta

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

      pure (resultOk0, errMsg0)
    done <- modalFooter $ do
      disableButton <- holdDyn True $ False <$ leftmost [ resultOk, () <$ errMsg]
      confirmButton (def { _uiButtonCfg_disabled = disableButton}) "Done"

    pure ((mempty, close <> done), never)
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
    (listenStatus, message, setMessage) <- pollForRequestKey clientEnvs $ Just <$> onRequestKey
  pure $ TransactionSubmitFeedback sendStatus listenStatus message

payloadToCommand :: Payload PublicMeta Text -> Command Text
payloadToCommand p =
    Pact.Command payloadText [] (hash $ T.encodeUtf8 payloadText)
  where
    payloadText = encodeAsText $ encode p

safeTransferEpsilon :: Decimal
safeTransferEpsilon = 0.000000000001

sameChainCmdAndData :: TransferType -> Text -> Text -> Maybe UserKeyset -> Text -> (Maybe Text, String)
sameChainCmdAndData ty fromAccount toAccount toKeyset amountText =
    case ty of
      NormalTransfer ->
        case toKeyset of
          Nothing ->
            (Nothing, printf "(coin.transfer %s %s %s)"
                        (show fromAccount) (show toAccount) amountText)
          Just ks ->
            (Just dataKey, printf "(coin.transfer-create %s %s (read-keyset %s) %s)"
                      (show fromAccount) (show toAccount) (show dataKey) amountText)
      SafeTransfer ->
        let amountExpr :: String = printf "(+ %s %s)" amountText (show safeTransferEpsilon)
            back :: String = printf "(coin.transfer %s %s %s)"
                      (show toAccount) (show fromAccount) (show safeTransferEpsilon)
        in case toKeyset of
             Nothing ->
               (Nothing, printf "(coin.transfer %s %s %s)\n%s"
                           (show fromAccount) (show toAccount) amountExpr back)
             Just _ ->
               (Just dataKey, printf "(coin.transfer-create %s %s (read-keyset %s) %s)\n%s"
                       (show fromAccount) (show toAccount) (show dataKey) amountExpr back)
  where
    dataKey = "ks"

buildUnsignedCmd
  :: SharedNetInfo NodeInfo
  -> TransferInfo
  -> TransferType
  -> TransferMeta
  -> Payload PublicMeta Text
buildUnsignedCmd netInfo ti ty tmeta = payload
  where
    network = _sharedNetInfo_network netInfo
    fromAccount = unAccountName $ _ca_account $ _ti_fromAccount ti
    fromChain = _ca_chain $ _ti_fromAccount ti
    toAccount = unAccountName $ _ca_account $ _ti_toAccount ti
    toChain = _ca_chain $ _ti_toAccount ti
    amount = _ti_amount ti
    amountText = showWithDecimal $ normalizeDecimal amount
    dataKey = "ks" :: Text
    (mDataKey, code) = if fromChain == toChain
             then sameChainCmdAndData ty fromAccount toAccount (_ti_toKeyset ti) amountText

             else -- cross-chain transfer
               (Just dataKey, printf "(coin.transfer-crosschain %s %s (read-keyset '%s) %s %s)"
                             (show fromAccount) (show toAccount) (T.unpack dataKey) (show toChain) amountText)
    tdata = maybe Null (\a -> object [ dataKey .= toJSON (userToPactKeyset a) ]) $ _ti_toKeyset ti
    meta = transferMetaToPublicMeta tmeta fromChain
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
  } deriving (Eq,Show)

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
  -> Map AccountName (AccountStatus AccountDetails)
  -> Map AccountName (AccountStatus AccountDetails)
  -> TransferInfo
  -> m (GasPayers t)
gasPayersSection model netInfo fks tks ti = do
    let fromChain = _ca_chain (_ti_fromAccount ti)
        fromAccount = _ca_account (_ti_fromAccount ti)
        toAccount = _ca_account (_ti_toAccount ti)
        toChain = _ca_chain (_ti_toAccount ti)
        defaultDestGasPayer = case Map.lookup toAccount tks of
          Just (AccountStatus_Exists dets)
            | _accountDetails_balance dets > AccountBalance 0 -> toAccount 
          _ -> AccountName "free-x-chain-gas"
    (dgp1, mdmgp2) <- if fromChain == toChain
      then do
        let initialGasPayer = if _ti_maxAmount ti then Nothing else Just (_ca_account $ _ti_fromAccount ti)
        let goodGasPayer a = if _ti_maxAmount ti && a == fromAccount
                               then Left $ "The account sending coins cannot be the gas payer on a max transfer"
                               else Right a
        (_,dgp1) <- uiAccountNameInput "Gas Paying Account" True initialGasPayer
                                       never (constDyn goodGasPayer)
        pure $ (dgp1, Nothing)
      else do
        let mkLabel c = T.pack $ printf "Gas Paying Account (Chain %s)" (T.unpack $ _chainId c)
        (_,dgp1) <- uiAccountNameInput (mkLabel fromChain) True (Just $ _ca_account $ _ti_fromAccount ti) never noValidation
        (_,dgp2) <- uiAccountNameInput (mkLabel toChain) True (Just defaultDestGasPayer) never noValidation
        pure $ (dgp1, Just dgp2)
    let
      getGasPayerKeys chain mgp = do
        case mgp of
          Nothing -> return never
          Just gp -> do
            -- I think lookupKeySets can't be done in the PushM monad
            evt <- lookupKeySets (model ^. logger) (_sharedNetInfo_network netInfo)
                          (_sharedNetInfo_nodes netInfo) chain [gp]
            return $ Map.lookup gp <$> fmapMaybe id evt

    -- Event t (Maybe AccountName)
    gp1Debounced <- debounce 1.0 $ updated dgp1
    gp1Keys <- networkHold (return never)
                           (getGasPayerKeys fromChain <$> gp1Debounced)
    let initialDetails = Map.lookup fromAccount fks
    gp1 <- foldDyn ($) (GasPayerDetails fromChain (Just fromAccount) initialDetails) $ leftmost
      [ (\a gp -> gp { gpdAccount = a }) <$> updated dgp1
      , (\keys gp -> gp { gpdDetails = keys }) <$> switch (current gp1Keys)
      ]
    --Outer maybe represents single chain transfer (so no second gas payer). Inner maybe represents
    --an account name that is less than 3 chars for the "Destination Gas Payer" input field
    mgp2 <- forM mdmgp2 $ \dmgp2 -> do
        -- Take the initial value of the destination gas payer and lookup its details
        initGasPayerDetails <- GasPayerDetails toChain (Just defaultDestGasPayer)
          <$$> getGasPayerKeys toChain (Just defaultDestGasPayer)
        updatedGP2 <- debounce 1.5 $ updated dmgp2
        let (invalidGasPayer, newAccName) = fanEither $ ffor updatedGP2 $ \case
              Nothing -> Left $ GasPayerDetails toChain Nothing Nothing
              Just accName -> Right accName
        -- gp2 and gp2Keys ---> is there a better way to do this??? Seems super redundant to
        -- create a dynamic just for an event
        gp2Keys <- networkHold (return never) (getGasPayerKeys toChain <$> updatedGP2)
        gp2 <- foldDyn ($) (GasPayerDetails toChain Nothing Nothing) $ leftmost
          [ (\a gp -> gp { gpdAccount = a }) <$> maybe never updated mdmgp2
          , (\keys gp -> gp { gpdDetails = keys }) <$> switch (current gp2Keys)
          ]
        dgp2 <- holdDyn (GasPayerDetails toChain Nothing Nothing) $ leftmost [
            initGasPayerDetails
          , invalidGasPayer
          , updated gp2
          ]
        return dgp2
    return $ GasPayers gp1 mgp2


data TransferMeta = TransferMeta
  { _transferMeta_senderAccount :: Maybe AccountName
  , _transferMeta_gasPrice :: GasPrice
  , _transferMeta_gasLimit :: GasLimit
  , _transferMeta_ttl :: TTLSeconds
  , _transferMeta_creationTime :: TxCreationTime
  , _transferMeta_sourceChainSigners :: [Signer]
  } deriving (Eq,Ord,Show)

transferMetaToPublicMeta :: TransferMeta -> ChainId -> PublicMeta
transferMetaToPublicMeta tmeta chainId =
  let
    lim = _transferMeta_gasLimit tmeta
    price = _transferMeta_gasPrice tmeta
    ttl = _transferMeta_ttl tmeta
    ct = _transferMeta_creationTime tmeta
    sender = maybe "" unAccountName $ _transferMeta_senderAccount tmeta
  in PublicMeta chainId sender lim price ttl ct

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

-- Noticeable debug string
flashyStr :: String -> String -> String
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
  -> TransferType
  -> m (mConf,
        Dynamic t TransferMeta,
        -- Destination chain gas payer and signer information when there is a cross-chain transfer
        Maybe (Dynamic t (Maybe (AccountName, AccountStatus AccountDetails)), Dynamic t [Signer]))
transferMetadata model netInfo fks tks ti ty = do
  let fromChain = _ca_chain $ _ti_fromAccount ti
      fromAccount = _ca_account $ _ti_fromAccount ti
      toChain = _ca_chain $ _ti_toAccount ti
      toAccount = _ca_account $ _ti_toAccount ti
      ks = Map.fromList [(fromChain, fks), (toChain, tks)]

  when (fromChain /= toChain) $ do
    dialogSectionHeading mempty "Important"
    divClass "group" $ text $ T.pack $ printf
      "This is a cross-chain transfer.  You must choose an account that has coins on chain %s as the chain %s gas payer otherwise your coins will not arrive!  They will be stuck in transit.  If this happens, they can still be recovered.  Save the request key and get someone with coins on that chain to finish the cross-chain transfer for you." (_chainId toChain) (_chainId toChain)
    el "br" blank

  dialogSectionHeading mempty "Gas Payers"
  (GasPayers srcPayer mdestPayer) <- divClass "group" $ gasPayersSection model netInfo fks tks ti
  let senderAction = getKeysetActionSingle fromChain (Just fromAccount) (Map.lookup fromAccount fks)
      getGpdAction (GasPayerDetails f a d) = getKeysetActionSingle f a d
      dSourceGasAction = getGpdAction <$> srcPayer
      dDestGasAction = getGpdAction <$$> mdestPayer
      toKeysetAction = getKeysetActionSingle toChain (Just toAccount) (Map.lookup toAccount tks)
      actions = do
        sga <- dSourceGasAction
        dga <- case dDestGasAction of
          Nothing -> pure []
          Just ddga -> do
            dga <- ddga
            pure [dga]
        let backAction = case (ty, _ti_toKeyset ti) of
              (SafeTransfer, Nothing) -> [toKeysetAction]
              (SafeTransfer, Just uks) ->
                if unambiguousKeyset uks
                  then [KeysetUnambiguous (toChain,toAccount,uks)]
                  else [KeysetAmbiguous (toChain,toAccount,uks)]
              (_, _) -> []
        pure $ Set.toList $ Set.fromList $ [senderAction, sga] <> backAction <> dga

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
      safeTxKeys = fromMaybe [] . Map.lookup (fromChain, toAccount) <$> signTuples

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
      rawAmount = _ti_amount ti
      amount = if ty == SafeTransfer then rawAmount + safeTransferEpsilon else rawAmount
      transferCap = if fromChain == toChain
                      then transferCapability fromAccount toAccount amount
                      else crosschainCapability fromAccount
      fromCapsA = foldr (addCap transferCap) <$> gasCaps <*> fromTxKeys
      allFromCaps = if ty == SafeTransfer
                      then foldr (addCap (transferCapability toAccount fromAccount safeTransferEpsilon))
                             <$> fromCapsA <*> safeTxKeys
                      else fromCapsA
      toCaps = foldr (addCap gasCapability) mempty <$> toGasKeys

      -- Convert that map into a [Signer]
      fromSigners = map mkSigner . Map.toList <$> allFromCaps
      toSigners = map mkSigner . Map.toList <$> toCaps

      mkDetails gpd =
        case (gpdAccount gpd, gpdDetails gpd) of
          (Just n, Just s) -> Just (n,s)
          (Just n, Nothing) -> (n,) <$> Map.lookup n tks
          _ -> Nothing
      destChainSigners = case mdestPayer of
        Nothing -> Nothing
        Just dp -> Just $ ( mkDetails <$> dp
                          , toSigners )

  dialogSectionHeading mempty "Transaction Settings"
  divClass "group" $ do
    let defaultLimit = if ty == SafeTransfer
          then Just 1200
          else if fromChain == toChain
            then Just 600
            else Just 400  -- Cross-chains need to be under 400 in order to use gas-station
    (conf, ttl, lim, price) <- uiMetaData model Nothing defaultLimit
    elAttr "div" ("style" =: "margin-top: 10px") $ do
      now <- fmap round $ liftIO $ getPOSIXTime
      let timeParser = maybe (Left "Not a valid creation time") Right . readMaybe . T.unpack
      ect <- mkLabeledInput True "Creation Time" (fmap snd . uiParsingInputElement timeParser)
        (def { _inputElementConfig_initialValue = tshow now})
      ct <- holdDyn now $ fmapMaybe hush $ updated ect
      let meta = TransferMeta
             <$> fmap gpdAccount srcPayer
             <*> price
             <*> lim
             <*> ttl
             <*> (TxCreationTime . ParsedInteger <$> ct)
             <*> fromSigners
      return (conf, meta, destChainSigners)

combineStatus :: AccountStatus a -> AccountStatus a -> AccountStatus a
combineStatus a@(AccountStatus_Exists _) _ = a
combineStatus _ a@(AccountStatus_Exists _) = a
combineStatus a _ = a

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
        errors = actions ^.. each . _KeysetError
    when (not $ null errors) $ divClass "group" $ do
      mapM_ (el "div" . text) errors
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

getKeysetActionSingle
  :: ChainId
  -> Maybe AccountName
  -> Maybe (AccountStatus AccountDetails)
  -> KeysetAction
getKeysetActionSingle _ Nothing _ = KeysetNoAction
getKeysetActionSingle c (Just a) mDetails =
  case mDetails of
    Nothing -> KeysetError $ T.pack $
      printf "Couldn't find account %s on chain %s" (unAccountName a) (_chainId c)
    Just AccountStatus_DoesNotExist -> KeysetError $ T.pack $
      printf "Account %s does not exist on chain %s" (unAccountName a) (_chainId c)
    Just AccountStatus_Unknown -> KeysetNoAction -- TODO not sure about this
    Just (AccountStatus_Exists (AccountDetails _ g)) -> do
      case g of
        AccountGuard_Other _ -> KeysetNoAction
        AccountGuard_KeySetLike (KeySetHeritage keys p _ref) -> do
          let ks = UserKeyset keys (parseKeysetPred p)
          if unambiguousKeyset ks
            then KeysetUnambiguous (c, a, ks)
            else KeysetAmbiguous (c, a, ks)

unambiguousKeyset :: UserKeyset -> Bool
unambiguousKeyset (UserKeyset keys p) =
  p == KeysAll || length keys == 1 || (length keys == 2 && p == Keys2)

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
  -- Incomplete pattern match should be fine here because all the possible
  -- commands are statically generated by Chainweaver.
  let Right sd = commandToSigData $ payloadToCommand payload
  let hash = toUntypedHash $ _sigDataHash sd
  _ <- divClass "group" $ do
    uiPreviewItem "Request Key" $ el "code" $ text (hashToText hash)

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
            let ju = (fromString $ T.unpack pubKeyText, Just $ UserSig $ keyToText s)
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
  let addSigs ss = sd { _sigDataSigs = ss }
  let signedCmd = addSigs <$> sigs

  rec
    keysetOpen <- toggle False clk
    (clk,(_,k)) <- controlledAccordionItem keysetOpen mempty
      (accordionHeaderBtn "Advanced Details and Signing Data") $ do
        transferDetails signedCmd

  return (either error id . sigDataToCommand <$> signedCmd)

data TransferDetails
  = TransferDetails_Yaml
  | TransferDetails_Json
  | TransferDetails_HashQR
  | TransferDetails_FullQR
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

showTransferDetailsTabName :: TransferDetails -> Text
showTransferDetailsTabName = \case
  TransferDetails_Json -> "JSON"
  TransferDetails_Yaml -> "YAML"
  TransferDetails_HashQR -> "Hash QR Code"
  TransferDetails_FullQR -> "Full Tx QR Code"

transferDetails
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t (SigData Text)
  -> m ()
transferDetails signedCmd = do
    divClass "tabset" $ mdo
      curSelection <- holdDyn TransferDetails_Json onTabClick
      (TabBar onTabClick) <- makeTabBar $ TabBarCfg
        { _tabBarCfg_tabs = [minBound .. maxBound]
        , _tabBarCfg_mkLabel = const $ text . showTransferDetailsTabName
        , _tabBarCfg_selectedTab = Just <$> curSelection
        , _tabBarCfg_classes = mempty
        , _tabBarCfg_type = TabBarType_Primary
        }

      tabPane mempty curSelection TransferDetails_Yaml $ do
        let preview = T.decodeUtf8 . Y.encode1Strict <$> signedCmd
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

#if !defined(ghcjs_HOST_OS)
      tabPane mempty curSelection TransferDetails_HashQR $ do
        let hashText = hashToText . toUntypedHash . _sigDataHash <$> signedCmd
            qrImage = QR.encodeText (QR.defaultQRCodeOptions QR.L) QR.Iso8859_1OrUtf8WithECI <$> hashText
            img = maybe "Error creating QR code" (QR.toPngDataUrlT 4 6) <$> qrImage
        el "div" $ text $ T.unwords
          [ "This QR code contains only the request key."
          , "It doesn't give any transaction information, so some wallets may not accept it."
          , "This is useful when you are signing your own transactions and don't want to transmit as much data."
          ]
        el "br" blank
        elDynAttr "img" (("src" =:) . LT.toStrict <$> img) blank
      tabPane mempty curSelection TransferDetails_FullQR $ do
        let yamlText = T.decodeUtf8 . Y.encode1Strict <$> signedCmd
            qrImage = QR.encodeText (QR.defaultQRCodeOptions QR.L) QR.Iso8859_1OrUtf8WithECI <$> yamlText
            img = maybe "Error creating QR code" (QR.toPngDataUrlT 4 4) <$> qrImage
        elDynAttr "img" (("src" =:) . LT.toStrict <$> img) blank
#else
      let notAvailMsg = el "ul" $ text "This feature is not currently available in the browser"
      tabPane mempty curSelection TransferDetails_HashQR notAvailMsg
      tabPane mempty curSelection TransferDetails_FullQR notAvailMsg
#endif

      pure ()

uiSigningInput
  :: ( MonadWidget t m
     , HasCrypto key m
     )
  => Hash
  -> PublicKey
  -> m (Dynamic t (PublicKeyHex, Maybe UserSig))
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

  pure $ (PublicKeyHex $ keyToText pubKey,) . hush <$> dE

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
