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

import           Control.Error hiding (bool)
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Aeson
import           Data.Decimal
import           Data.Default (Default (..))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Traversable
import           Data.Time.Clock.POSIX
import           Kadena.SigningApi (AccountName(..))
import           Pact.Parse
import qualified Pact.Server.ApiClient as Api
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
import           Pact.Types.Term (KeySet (..))
import           Reflex
import           Reflex.Dom.Core
import           Text.Printf
import           Text.Read (readMaybe)

import           Common.Foundation
import           Common.Wallet
import           Frontend.Crypto.Class
import           Frontend.Crypto.Ed25519
import           Frontend.Foundation
import           Frontend.JsonData
import           Frontend.Log
import           Frontend.Network
import           Frontend.PactQueries
import           Frontend.UI.Button
import           Frontend.UI.DeploymentSettings
import           Frontend.UI.Dialogs.Send
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
  , _ca_account :: Text -- TODO Might use a newtype wrapper and correct account validation
  } deriving (Eq,Ord,Show)

data ReceivingAccount = ReceivingAccount
  { _ra_chainAccount :: ChainAccount
  , _ra_keyset :: UserKeyset
  } deriving (Eq,Ord,Show)

uiChainAccount
  :: (MonadWidget t m, HasNetwork model t)
  => model
  -> Maybe (ChainId, AccountName)
  -> m (Dynamic t (Maybe ChainAccount))
uiChainAccount model iv = do
  cd <- userChainIdSelectDef (getChainsFromHomogenousNetwork model) (fst <$> iv)
  (_,a) <- uiAccountNameInput "Account Name" True (snd <$> iv) never noValidation
  return $ runMaybeT $ ChainAccount <$> MaybeT (value cd) <*> MaybeT (unAccountName <$$> a)

data TransferInfo = TransferInfo
  { _ti_fromAccount :: ChainAccount
  , _ti_amount :: Decimal -- Possibly use ParsedDecimal
  , _ti_toAccount :: ChainAccount
  , _ti_toKeyset :: Maybe UserKeyset
  } deriving (Eq,Ord,Show)

-- TODO This only for more convenient testing
userChainIdSelectDef
  :: MonadWidget t m
  => Dynamic t [ChainId]
  -> Maybe ChainId
  -> m ( Dropdown t (Maybe ChainId) )
userChainIdSelectDef options iv = do
  pb <- getPostBuild
  mkLabeledClsInput True "Chain ID"
    (uiChainSelectionWithUpdate options (iv <$ pb))

-- -> RoutedT t (R FrontendRoute) m ()
uiGenericTransfer
  :: ( MonadWidget t m
     , HasLogger model t
     , HasCrypto key (Performable m)
     , HasCrypto key m
     , HasNetwork model t
     , HasNetworkCfg (ModalCfg mConf t) t
     , HasWallet model key t
     , HasJsonData model t
     , Monoid mConf
     , Monoid (ModalCfg mConf t)
     , Flattenable (ModalCfg mConf t) t
     , HasModalCfg mConf (Modal mConf m t) t
     , HasWalletCfg (ModalCfg mConf t) key t
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
  elDynAttr "main" attrs $ do
    transferInfo <- divClass "transfer-fields" $ do
      (fromAcct,amount) <- divClass "transfer__left-pane" $ do
        el "h4" $ text "From"
        fca <- uiChainAccount model (Just $ (ChainId "0", AccountName "doug"))
        amt <- amountButton (Just 2)
        return (fca,amt)
      (toAcct,ks) <- divClass "transfer__right-pane" $ do
        el "h4" $ text "To"
        tca <- uiChainAccount model (Just $ (ChainId "0", AccountName "doug2"))
        rec
          keysetOpen <- toggle False clk
          (clk,(_,k)) <- controlledAccordionItem keysetOpen mempty
            (accordionHeaderBtn "Keyset") $ do
            keysetInputWidget Nothing
        return (tca,k)
      return $ runMaybeT $ TransferInfo <$>
        MaybeT fromAcct <*>
        MaybeT (hush <$> amount) <*>
        MaybeT toAcct <*>
        lift ks
    signTransfer <- divClass "transfer-fields submit" $ do
      confirmButton (def { _uiButtonCfg_disabled = (isNothing <$> transferInfo) }) "Sign & Transfer"
    let netInfo = flip push signTransfer $ \() -> sampleNetInfo model
    let mkModal (Just ti) ni = Just $ lookupAndTransfer model ni ti
        mkModal Nothing _ = Nothing
    pure $ mempty & modalCfg_setModal .~ (attachWith mkModal (current transferInfo) netInfo)

amountButton
  :: DomBuilder t m
  => Maybe Decimal
  -> m (Dynamic t (Either String Decimal))
amountButton iv =
 mkLabeledInput True "Amount" uiDecimalInputElement
   (def { _inputElementConfig_initialValue = maybe "" tshow iv})

lookupAndTransfer
  :: ( SendConstraints model mConf key t m
     , Flattenable mConf t
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
        accounts = setify [fromAccount, toAccount]
        accountNames = AccountName <$> accounts
        chains = setify [fromChain, toChain]
        code = renderCompactText $ accountDetailsObject accounts
        mkCmd c = do
          pm <- mkPublicMeta c
          buildCmd Nothing (_sharedNetInfo_network netInfo) pm [] [] code mempty mempty
    cmds <- mapM mkCmd chains
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

-- | Lookup the keyset of an account
lookupKeySets
  :: (HasCrypto key m, TriggerEvent t m, MonadJSM m)
  => Logger t
  -> NetworkName
  -- ^ Which network we are on
  -> [NodeInfo]
  -- ^ Envs which point to the appropriate chain
  -> ChainId
  -> [AccountName]
  -- ^ Account on said chain to find
  -> m (Event t (Maybe (Map AccountName (AccountStatus AccountDetails))))
lookupKeySets logL networkName nodes chainId accounts = do
  let code = renderCompactText $ accountDetailsObject (map unAccountName accounts)
  pm <- mkPublicMeta chainId
  cmd <- buildCmd Nothing networkName pm [] [] code mempty mempty
  (result, trigger) <- newTriggerEvent
  let envs = mkClientEnvs nodes chainId
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
  isVisible <- toggle False click
  return isVisible

data ExternalSignatory = Signature | PrivateKey
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

uiSigRadio
  :: (MonadWidget t m)
  => m (Dynamic t ExternalSignatory)
uiSigRadio = mdo
  dopt <- holdDyn Signature onRadioChange
  let
    mkLbl lbl cls =
      fst <$> elClass' "span" (renderClass cls) (text lbl)

    mkRadioOption lbl opt = divClass "create-account__gas-payer" $
      uiLabeledRadioView (mkLbl lbl) dopt opt

  onSig <- mkRadioOption "Signature" Signature
  onKey <- mkRadioOption "Private Key" PrivateKey
  let onRadioChange = leftmost [onSig, onKey]
  return dopt

checkSendingAccountExists
  :: ( SendConstraints model mConf key t m
     )
  => model
  -> SharedNetInfo NodeInfo
  -> TransferInfo
  -> Map AccountName (AccountStatus AccountDetails)
  -> Map AccountName (AccountStatus AccountDetails)
  -> Workflow t m (mConf, Event t ())
checkSendingAccountExists model netInfo ti fks tks = Workflow $ do
    let fromAccountText = _ca_account $ _ti_fromAccount ti
        fromAccount = AccountName fromAccountText
    case Map.lookup fromAccount fks of
      Just (AccountStatus_Exists ad) -> do
        checkReceivingAccount model netInfo ti fks tks (fromAccount, ad)
      _ -> do
        cancel <- fatalTransferError $
          text $ "Sending account " <> fromAccountText <> " does not exist."
        return ((mempty, cancel), never)

checkReceivingAccount
  :: ( SendConstraints model mConf key t m
     )
  => model
  -> SharedNetInfo NodeInfo
  -> TransferInfo
  -> Map AccountName (AccountStatus AccountDetails)
  -> Map AccountName (AccountStatus AccountDetails)
  -> (AccountName, AccountDetails)
  -> m ((mConf, Event t ()), Event t (Workflow t m (mConf, Event t ())))
checkReceivingAccount model netInfo ti fks tks fromPair = do
    let toAccount = AccountName $ _ca_account $ _ti_toAccount ti
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
            transferDialog model netInfo ti fks tks fromPair (Just userKeyset)
      (Just (AccountStatus_Exists (AccountDetails _ _)), Nothing) -> do
        -- Use transfer, probably show the guard at some point
        -- TODO check well-formedness of all keys in the keyset
        transferDialog model netInfo ti fks tks fromPair Nothing
      (_, Just userKeyset) -> do
        -- Use transfer-create
        transferDialog model netInfo ti fks tks fromPair (Just userKeyset)
      (_, Nothing) -> do
        -- If the account name looks like a public key, ask about making a keyset
        -- Otherwise throw an error
        handleMissingKeyset model netInfo ti fks tks fromPair

handleMissingKeyset
  :: ( SendConstraints model mConf key t m
     )
  => model
  -> SharedNetInfo NodeInfo
  -> TransferInfo
  -> Map AccountName (AccountStatus AccountDetails)
  -> Map AccountName (AccountStatus AccountDetails)
  -> (AccountName, AccountDetails)
  -> m ((mConf, Event t ()), Event t (Workflow t m (mConf, Event t ())))
handleMissingKeyset model netInfo ti fks tks fromPair = do
    let toAccountText = _ca_account $ _ti_toAccount ti
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
          let ks = UserKeyset (Set.singleton pk) KeysAll
          return ((mempty, close <> cancel),
                  Workflow (transferDialog model netInfo ti fks tks fromPair (Just ks)) <$ next)

transferDialog
  :: ( SendConstraints model mConf key t m
     )
  => model
  -> SharedNetInfo NodeInfo
  -> TransferInfo
  -> Map AccountName (AccountStatus AccountDetails)
  -> Map AccountName (AccountStatus AccountDetails)
  -> (AccountName, AccountDetails)
  -> Maybe UserKeyset
  -> m ((mConf, Event t ()), Event t (Workflow t m (mConf, Event t ())))
transferDialog model netInfo ti fks tks _ _ = do
    close <- modalHeader $ text "Sign Transfer"
    rec
      (currentTab, _done) <- transferTabs $ leftmost [prevTab, fmapMaybe id nextTab]
      (conf, meta) <- mainSection currentTab
      (cancel, prevTab, nextTab) <- footerSection currentTab
    pure ((conf, close <> cancel), never)
  where
    mainSection currentTab = elClass "div" "modal__main" $ do
      (conf, meta, toSigners) <- tabPane mempty currentTab TransferTab_Metadata $ transferMetadata model netInfo fks tks ti
      let unsignedCmd = buildUnsignedCmd netInfo ti <$> meta
      tabPane mempty currentTab TransferTab_Signatures $ transferSigs fks unsignedCmd
      return (conf, meta)
    footerSection currentTab = modalFooter $ do
      let (lbl, fanTag) = splitDynPure $ ffor currentTab $ \case
            TransferTab_Metadata -> ("Cancel", Left ())
            TransferTab_Signatures -> ("Back", Right TransferTab_Signatures)

      ev <- cancelButton def lbl
      let (cancel, back) = fanEither $ current fanTag <@ ev
          (name, disabled) = splitDynPure $ ffor currentTab $ \case
            TransferTab_Metadata -> ("Next", constDyn False) -- TODO Properly enable/disable Next button
            TransferTab_Signatures -> ("Preview", constDyn False)
          cfg = def
            & uiButtonCfg_class <>~ "button_type_confirm"
            & uiButtonCfg_disabled .~ join disabled
      next <- uiButtonDyn cfg $ dynText name
      let nextTab = ffor (current currentTab <@ next) $ \case
            TransferTab_Metadata -> Just TransferTab_Signatures
            TransferTab_Signatures -> Nothing
      pure (cancel, back, nextTab)

buildUnsignedCmd
  :: SharedNetInfo NodeInfo
  -> TransferInfo
  -> TransferMeta
  -> Command Text
buildUnsignedCmd netInfo ti tmeta = Pact.Command payloadText [] (hash $ T.encodeUtf8 payloadText)
  where
    network = _sharedNetInfo_network netInfo
    fromAccount = _ca_account $ _ti_fromAccount ti
    fromChain = _ca_chain $ _ti_fromAccount ti
    toAccount = _ca_account $ _ti_toAccount ti
    toChain = _ca_chain $ _ti_toAccount ti
    amount = _ti_amount ti
    dataKey = "ks"
    (mDataKey, code) = if fromChain == toChain
             then case _ti_toKeyset ti of
                    Nothing ->
                      (Nothing, printf "(coin.transfer %s %s %s)"
                                  (show fromAccount) (show toAccount) (show amount))
                    Just ks ->
                      (Just dataKey, printf "(coin.transfer-create %s %s (read-keyset '%s) %s)"
                                    (show fromAccount) (show toAccount) dataKey (show amount))
             else -- cross-chain transfer
               (Just dataKey, printf "(coin.transfer-crosschain %s %s (read-keyset '%s) %s %s)"
                             (show fromAccount) (show toAccount) dataKey (show toChain) (show amount))
    tdata = maybe Null (toJSON . userToPactKeyset) $ _ti_toKeyset ti
    lim = _transferMeta_gasLimit tmeta
    price = _transferMeta_gasPrice tmeta
    ttl = _transferMeta_ttl tmeta
    ct = _transferMeta_creationTime tmeta
    meta = PublicMeta fromChain fromAccount lim price ttl ct

    signers = _transferMeta_fromSigners tmeta

    payload = Payload
      { _pPayload = Exec (ExecMsg (T.pack code) tdata)
      , _pNonce = "chainweaver"
      , _pMeta = meta
      , _pSigners = signers
      , _pNetworkId = pure $ NetworkId $ textNetworkName network
      }

    payloadText = encodeAsText $ encode payload

--getUnambiguousSigners :: TransferMeta -> [Signer]
--getUnambiguousSigners tm = mkSigner <$> ks
--  where
--    ks = _transferMeta_keysetActions tm ^.. each . _KeysetUnambiguous
--    mkSigner (chain,account,UserKeyset ks p) =

--getFinalSigners :: TransferMeta -> [(ChainId, AccountName, PublicKey)]
--getFinalSigners tm =

--onToPreviewTransfer = flip push nextTab $ \case
--  Just _ -> pure Nothing
--  Nothing -> runMaybeT $ do
--    (fromGasPayer, mToGasPayer) <- MaybeT $ sample $ current mCaps
--    (toAccount, amount) <- MaybeT $ fmap hush $ sample $ current recipient
--    let mCCD = case mToGasPayer of
--          Just toGasPayer | toGasPayer /= fromGasPayer -> Just $ CrossChainData
--            { _crossChainData_recipientChainGasPayer = toGasPayer
--            }
--          _ -> Nothing
--    let transfer = TransferData
--          { _transferData_amount = amount
--          , _transferData_crossChainData = mCCD
--          , _transferData_fromAccount = fromAccount
--          , _transferData_fromGasPayer = fromGasPayer
--          , _transferData_toTxBuilder = toAccount
--          }
--    pure $ previewTransfer model transfer

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

gasPayersSection
  :: (MonadWidget t m, HasCrypto key m, HasLogger model t)
  => model
  -> SharedNetInfo NodeInfo
  -> TransferInfo
  -> m (Dynamic t [(ChainId, Maybe AccountName)], Dynamic t (Map ChainId (Map AccountName (AccountStatus AccountDetails))))
gasPayersSection model netInfo ti = do
    let fromChain = _ca_chain (_ti_fromAccount ti)
        toChain = _ca_chain (_ti_toAccount ti)
    dgps <- if fromChain == toChain
      then do
        (_,dgp1) <- uiAccountNameInput "Gas Paying Account" True (Just $ AccountName $ _ca_account $ _ti_fromAccount ti) never noValidation
        pure $ (:[]) . (fromChain,) <$> dgp1
      else do
        let mkLabel c = T.pack $ printf "Gas Paying Account (Chain %s)" (T.unpack $ _chainId c)
        (_,dgp1) <- uiAccountNameInput (mkLabel fromChain) True (Just $ AccountName $ _ca_account $ _ti_fromAccount ti) never noValidation
        (_,dgp2) <- uiAccountNameInput (mkLabel toChain) True Nothing never noValidation
        pure $ do
          gp1 <- dgp1
          gp2 <- dgp2
          pure [(fromChain, gp1), (toChain, gp2)]
    let getGasPayerKeys gps = do
          let accounts = catMaybes $ map snd gps
          resps <- forM (Set.toList $ Set.fromList $ map fst gps) $ \chain -> do
            -- I think lookupKeySets can't be done the PushM monad
            evt <- lookupKeySets (model ^. logger) (_sharedNetInfo_network netInfo)
                          (_sharedNetInfo_nodes netInfo) chain accounts
            return $ Map.singleton chain <$> fmapMaybe id evt
          foldDyn ($) mempty $ Map.union <$> mergeWith (Map.unionWith Map.union) resps
    debouncedGasPayers <- debounce 1.0 $ updated dgps
    gpKeys <- networkHold (return $ constDyn mempty) (getGasPayerKeys <$> debouncedGasPayers)
    return (dgps, join gpKeys)

data TransferMeta = TransferMeta
  { _transferMeta_gasPrice :: GasPrice
  , _transferMeta_gasLimit :: GasLimit
  , _transferMeta_ttl :: TTLSeconds
  , _transferMeta_creationTime :: TxCreationTime
  , _transferMeta_fromSigners :: [Signer]
  }

transferCapability :: AccountName -> AccountName -> Decimal -> SigCapability
transferCapability from to amount = SigCapability
  { _scName = QualifiedName { _qnQual = "coin", _qnName = "TRANSFER", _qnInfo = def }
  , _scArgs =
    [ PLiteral $ LString $ unAccountName from
    , PLiteral $ LString $ unAccountName to
    , PLiteral $ LDecimal amount
    ]
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

transferMetadata
  :: (MonadWidget t m, HasNetwork model t, HasNetworkCfg mConf t, Monoid mConf,
      HasCrypto key m, HasLogger model t)
  => model
  -> SharedNetInfo NodeInfo
  -> Map AccountName (AccountStatus AccountDetails)
  -> Map AccountName (AccountStatus AccountDetails)
  -> TransferInfo
  -> m (mConf, Dynamic t TransferMeta, Maybe (Dynamic t [Signer]))
transferMetadata model netInfo fks tks ti = do
  (dgps, gpDetails) <- gasPayersSection model netInfo ti
  let fromChain = _ca_chain $ _ti_fromAccount ti
      fromAccount = _ca_account $ _ti_fromAccount ti
      toChain = _ca_chain $ _ti_toAccount ti
      toAccount = _ca_account $ _ti_toAccount ti
      amount = _ti_amount ti
      ks = Map.fromList [(fromChain, fks), (toChain, tks)]
      getActions gps gpds = getKeysetActions ti
        (Map.unionWith (Map.unionWith combineStatus) ks gpds) gps
      actions = getActions <$> dgps <*> gpDetails

  esigners <- networkView (signersSection <$> actions)
  signTuples <- fmap join $ holdDyn (constDyn []) esigners

  -- To get the final list of signers we need to take all the public keys from
  -- each of the unambiguous keysets. Then we combine that with the signers
  -- specified by the user which came from the ambiguous keysets. For new we just
  -- ignore the KeysetError and KeysetNoAction constructors because doing so
  -- should only ever result in failed transactions. We can come back later and
  -- try to prevent them if it makes sense.
  let mkSigners tuples = do
        gps <- dgps
        as <- actions
        let getCaps c a =
              (if (c, Just a) `elem` gps then [gasCapability] else []) <>
              (if ChainAccount c (unAccountName a) == _ti_fromAccount ti
                 then [transferCapability (AccountName fromAccount) (AccountName toAccount) amount]
                 else [])

            ambig = Map.unionsWith (<>) $ map (\(c, a, pk) -> Map.singleton pk (getCaps c a)) tuples
            foo (c, a, ks) =
              map (\pk -> Map.singleton pk $ getCaps c a) (Set.toList $ _userKeyset_keys ks)
            unamb = Map.unionsWith (<>) $ concat $ map foo (as ^.. each . _KeysetUnambiguous)
        pure $ map (\(k,v) -> Signer Nothing (keyToText k) Nothing v) (Map.toList $ Map.unionWith (<>) ambig unamb)
      fromSigners = mkSigners =<< (filter (\(c,_,_) -> c == fromChain) <$> signTuples)
      toSigners = if fromChain == toChain
                    then Nothing
                    else Just $ mkSigners =<< (filter (\(c,_,_) -> c == toChain) <$> signTuples)

  -----
  el "h2" $ text "From Signers"
  divClass "group" $ do
    networkView (mapM_ (el "div" . text . tshow) <$> fromSigners)

  el "h2" $ text "To Signers"
  divClass "group" $ do
    case toSigners of
      Nothing -> blank
      Just s -> void $ networkView (mapM_ (el "div" . text . tshow) <$> s)
  -----

  dialogSectionHeading mempty "Transaction Settings"
  divClass "group" $ do
    (conf, ttl, lim, price) <- uiMetaData model Nothing Nothing
    elAttr "div" ("style" =: "margin-top: 10px") $ do
      now <- fmap round $ liftIO $ getPOSIXTime
      let timeParser = maybe (Left "Not a valid creation time") Right . readMaybe . T.unpack
      ect <- mkLabeledInput True "Creation Time" (uiParsingInputElement timeParser)
        (def { _inputElementConfig_initialValue = tshow now})
      ct <- holdDyn now $ fmapMaybe hush $ updated ect
      let meta = TransferMeta
             <$> price
             <*> lim
             <*> ttl
             <*> (TxCreationTime . ParsedInteger <$> ct)
             <*> fromSigners
      return (conf, meta, toSigners)
      -- TODO Add creationTime to uiMetaData dialog and default it to current time - 60s

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
    fromPair = (fromChain, Just $ AccountName fromAccount)
    signers = if fromPair `elem` gps then gps else (fromPair : gps)

-- | Returns a list of all the keys that should be signed with and their
-- associated chains and account names. These are either there because they were
-- unambiguous (keys-all or a single-key keyset) or the user selected them.
signersSection
  :: (MonadWidget t m)
  => [KeysetAction]
  -> m (Dynamic t [(ChainId, AccountName, PublicKey)])
signersSection actions = do
    let ambiguous = actions ^.. each . _KeysetAmbiguous
        ukToKeys (c,a,uk) = (c,a,) <$> Set.toList (_userKeyset_keys uk)
    if (null ambiguous)
      then return $ constDyn $ concat $ map ukToKeys $ actions ^.. each . _KeysetUnambiguous
      else do
        dialogSectionHeading mempty "Keys to Sign With"
        dynList <- divClass "group signing-ui-signers" $ do
          mapM singleSigner ambiguous
        return $ concat <$> distributeListOverDyn dynList

singleSigner
  :: (DomBuilder t m, PostBuild t m)
  => (ChainId, AccountName, UserKeyset)
  -> m (Dynamic t [(ChainId, AccountName, PublicKey)])
singleSigner (c, a, UserKeyset keys p) = do
    elClass "h3" ("heading heading_type_h3") $
      text $ unAccountName a <> " (" <> T.toLower (prettyPred $ renderKeysetPred p) <> ")"
    res <- forM (Set.toList keys) $ \key -> do
      c <- uiCheckbox "signing-ui-signers__signer" False def $ text $ keyToText key
      return $ bool Nothing (Just key) <$> value c
    pure $ map (c, a,) . catMaybes <$> distributeListOverDyn res

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

unambiguousKeyset :: Set PublicKey -> Text -> Bool
unambiguousKeyset keys p =
  p == "keys-all" || length keys == 1 || (length keys == 2 && p == "keys-2")

transferSigs
  :: (MonadWidget t m)
  => Map AccountName (AccountStatus AccountDetails)
  -> Dynamic t (Command Text)
  -> m ()
transferSigs fks cmd = do
  _ <- divClass "group" $ do
    mkLabeledInput True "Request Key" uiInputElement $ def
      & initialAttributes .~ "disabled" =: "disabled"
      & inputElementConfig_initialValue .~ ""
      & inputElementConfig_setValue .~ (hashToText . toUntypedHash . _cmdHash <$> updated cmd)

  dialogSectionHeading mempty "Signatures"
  --let pkWidget = text . T.decodeUtf8 . Pact._pubKey
  --let externalSig pk = do
  --      pkWidget pk
  --      let optMap = Map.fromList $ map (\a -> (a, tshow a)) [Signature, PrivateKey]
  --      uiDropdown Signature (constDyn optMap) def
  --      uiPrivateKeyInput (fromPactPublicKey pk) Nothing
  --      pure ()
  --    internalSig pk = do
  --      _ <- fmap value $ uiCheckbox "signing-ui-signers__signer" False def $
  --        pkWidget pk
  --      pure ()
  divClass "group signing-ui-signers" $ do
    --forM_ (Map.toList $ Pact._ksKeys fks) $ \pk -> do
    forM_ (Map.toList fks) $ \(a,as) -> do
      el "div" $ text $ unAccountName a
      --case as of
      --  AccountStatus_DoesNotExist
      --externalSig ad
    return ()

uiPrivateKeyInput
  :: MonadWidget t m
  => PublicKey
  -> Maybe Text
  -> m (Dynamic t (Maybe PrivateKey))
uiPrivateKeyInput pubKey iv = do
  let
    inp cfg = do
      ie <- mkLabeledInput False mempty uiInputElement cfg
      pure (ie
           , ( parsePrivateKey pubKey <$> value ie
             , parsePrivateKey pubKey <$> _inputElement_input ie
             )
           )

    inputCfg = def
      & initialAttributes .~ ("placeholder" =: "Private key")
      & inputElementConfig_initialValue .~ fold iv

    showPopover (_, (_, onInput)) = pure $
      either PopoverState_Error (const PopoverState_Disabled) <$> onInput

  (inputE, (dE, onE)) <- uiInputWithPopover
    inp
    (_inputElement_raw . fst)
    showPopover
    inputCfg

  pure $ (join . hush) <$> dE

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
