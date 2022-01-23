{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Frontend.UI.Dialogs.SigBuilder where

import           Control.Error hiding (bool, mapMaybe)
import           Control.Lens
import           Control.Monad (forM)
import qualified Data.Aeson as A
import           Data.Decimal (Decimal)
import           Data.Aeson.Parser.Internal (jsonEOF')
import           Data.Attoparsec.ByteString
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LB
import           Data.Functor (void)
import           Data.List (partition)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import           Data.YAML
import qualified Data.YAML.Aeson as Y
import           Pact.Types.ChainMeta
import           Pact.Types.ChainId   (networkId, NetworkId)
import           Pact.Types.Gas
import           Pact.Types.RPC
import           Pact.Types.Command
import           Pact.Types.Hash (Hash, hash, toUntypedHash, unHash, hashToText)
import           Pact.Types.SigData
import           Pact.Types.PactValue (PactValue(..))
import           Pact.Types.Capability
import           Pact.Types.Names (QualifiedName(..))
import           Pact.Types.Exp (Literal(..))
import           Pact.Types.Util (asString)
import           Pact.Types.Pretty
------------------------------------------------------------------------------
import           Reflex
import           Reflex.Dom hiding (Key, Command)
------------------------------------------------------------------------------
import           Common.Wallet
import           Frontend.Crypto.Class
import           Frontend.Foundation
import           Frontend.Log
import           Frontend.Network
import           Frontend.UI.Dialogs.DeployConfirmation
import           Frontend.UI.Modal
import           Frontend.UI.TabBar
import           Frontend.UI.Transfer
import           Frontend.UI.Widgets
import           Frontend.UI.Widgets.Helpers (dialogSectionHeading)
import           Frontend.Wallet
------------------------------------------------------------------------------
import Frontend.UI.Modal.Impl
------------------------------------------------------------------------------

type SigBuilderWorkflow t m model key =
  ( MonadWidget t m
  , HasNetwork model t
  , HasWallet model key t
  , HasCrypto key m
  , HasTransactionLogger m
  , HasLogger model t
  )

sigBuilderCfg
  :: SigBuilderWorkflow t m model key
  => model
  -> Event t ()
  -> m (ModalIdeCfg m key t)
sigBuilderCfg m evt = do
  pure $ mempty & modalCfg_setModal .~ (Just (uiSigBuilderDialog m) <$ evt)

uiSigBuilderDialog
  ::
  ( Monoid mConf
  , SigBuilderWorkflow t m model key
  )
  => model
  -> Event t ()
  -> m (mConf, Event t ())
uiSigBuilderDialog model _onCloseExternal = do
  dCloses <- workflow $ txnInputDialog model Nothing
  pure (mempty, switch $ current dCloses)


--------------------------------------------------------------------------------
-- Input Flow
--------------------------------------------------------------------------------

data SigBuilderRequest key = SigBuilderRequest
  { _sbr_sigData        :: SigData Text
  , _sbr_payload        :: Payload PublicMeta Text
  , _sbr_currentNetwork :: Maybe NetworkName
  , _sbr_cwKeys         :: [ Key key ]
  }

txnInputDialog
  :: SigBuilderWorkflow t m model key
  => model
  -> Maybe (SigData Text)
  -> Workflow t m (Event t ())
txnInputDialog model mInitVal = Workflow $ mdo
  onClose <- modalHeader $ text "Signature Builder"
  let cwKeys = IMap.elems <$> (model^.wallet_keys)
      selNodes = model ^. network_selectedNodes
      nid = (fmap (mkNetworkName . nodeVersion) . headMay . rights) <$> selNodes
      keysAndNet = current $ (,) <$> cwKeys <*> nid
  dmSigData <- modalMain $ divClass "group" $ parseInputToSigDataWidget mInitVal
  (onCancel, approve) <- modalFooter $ (,)
    <$> cancelButton def "Cancel"
    <*> confirmButton (def & uiButtonCfg_disabled .~ ( isNothing <$> dmSigData )) "Review"
  let approveE = fmapMaybe id $ tag (current dmSigData) approve
      sbr = attachWith (\(keys, net) (sd, pl) -> SigBuilderRequest sd pl net keys) keysAndNet approveE
  return (onCancel <> onClose, checkAndSummarize model <$> sbr)

data DataToBeSigned
  = DTB_SigData (SigData T.Text, Payload PublicMeta Text)
  | DTB_ErrorString String
  | DTB_EmptyString
  deriving (Show, Eq)

payloadToSigData :: Payload PublicMeta Text -> Text -> SigData Text
payloadToSigData parsedPayload payloadTxt = SigData cmdHash sigs $ Just payloadTxt
  where
    cmdHash = hash $ T.encodeUtf8 payloadTxt
    sigs = map (\s -> (PublicKeyHex $ _siPubKey s, Nothing)) $ _pSigners parsedPayload


parseInputToSigDataWidget
  :: MonadWidget t m
  => Maybe (SigData Text)
  -> m (Dynamic t (Maybe (SigData Text, Payload PublicMeta Text)))
parseInputToSigDataWidget mInit =
  divClass "group" $ do
    txt <- fmap value
      $ mkLabeledClsInput False "Paste SigData or Payload" $ \cls ->
          uiTxSigner mInit cls def
    let parsedBytes = parseBytes . T.encodeUtf8 <$> txt
    dyn_ $ ffor parsedBytes $ \case
      DTB_ErrorString e -> elClass "p" "error_inline" $ text $ T.pack e
      _ -> blank
    return $ validInput <$>  parsedBytes
  where
    parsePayload :: Text -> Either String (Payload PublicMeta Text)
    parsePayload cmdText =
      first (const "Invalid cmd field inside SigData.") $
        A.eitherDecodeStrict (T.encodeUtf8 cmdText)

    parseAndAttachPayload sd =
      case parsePayload =<< (justErr "Payload missing" $ _sigDataCmd sd) of
        Left e -> DTB_ErrorString e
        Right p -> DTB_SigData (sd, p)

    parseBytes "" = DTB_EmptyString
    parseBytes bytes =
      -- Parse the JSON, and consume all the input
      case parseOnly jsonEOF' bytes of
        Right val -> case A.fromJSON val of
          A.Success sigData -> parseAndAttachPayload sigData
          A.Error _ ->
            case A.fromJSON @(Payload PublicMeta Text) val of
              A.Success p -> parseAndAttachPayload $ payloadToSigData p $ encodeAsText $ LB.fromStrict bytes
              A.Error _ ->  case A.fromJSON @Text val of
                A.Success t ->
                  case (A.eitherDecode $ LB.fromStrict $ T.encodeUtf8 t) of
                    Left e -> DTB_ErrorString e
                    Right payload -> parseAndAttachPayload $ payloadToSigData payload t
                A.Error errorStr -> DTB_ErrorString errorStr

        -- We did not receive JSON, try parsing it as YAML
        Left _ -> case Y.decode1Strict bytes of
          Right sigData -> parseAndAttachPayload sigData
          Left (pos, errorStr) -> DTB_ErrorString $ prettyPosWithSource pos (LB.fromStrict bytes) errorStr
    validInput DTB_EmptyString = Nothing
    validInput (DTB_ErrorString _) = Nothing
    validInput (DTB_SigData info) = Just info

--------------------------------------------------------------------------------
-- Preprocessing and Handling
--------------------------------------------------------------------------------
warningDialog
  :: (MonadWidget t m)
  => Text
  -> Workflow t m (Event t ())
  -> Workflow t m (Event t ())
  -> Workflow t m (Event t ())
warningDialog warningMsg backW nextW = Workflow $ do
  onClose <- modalHeader $ text "Warning"
  void $ modalMain $ do
    el "h3" $ text "Warning"
    el "p" $ text warningMsg
  (back, next) <- modalFooter $ (,)
    <$> cancelButton def "Back"
    <*> confirmButton def "Continue"
  pure $ (onClose, leftmost [ backW <$ back , nextW <$ next ])

errorDialog
  :: (MonadWidget t m)
  => m ()
  -> Workflow t m (Event t ())
  -> Workflow t m (Event t ())
errorDialog errorMsg backW = Workflow $ do
  onClose <- modalHeader $ text "Error"
  void $ modalMain $ do
    el "h3" $ text "Error"
    errorMsg
  back <- modalFooter $ cancelButton def "Back"
  pure $ (onClose, backW <$ back)

-- Checks a list of workflow-based functions that take a next param which allows us to chain through
-- all errors and warnings. The difference between a "Warning" and an "Error" workflow is that the
-- warning allows the user to select to continue even if the predicate it checks against is true,
-- whereas an error will only continue if the predicate it checks for is false
checkAll
  :: a -> [(a -> a)] -> a
checkAll nextW [] = nextW
checkAll nextW [singleW] = singleW nextW
checkAll nextW (x:xs) = x $ checkAll nextW xs


checkAndSummarize
  :: SigBuilderWorkflow t m model key
  => model
  -> SigBuilderRequest key
  -> Workflow t m (Event t ())
checkAndSummarize model sbr =
  let errorWorkflows = [checkForHashMismatch, checkMissingSigs]
      warningWorkflows = [checkNetwork]
   in flip checkAll errorWorkflows $ checkAll nextW warningWorkflows
  where
    sigData = _sbr_sigData sbr
    backW = txnInputDialog model (Just sigData)
    nextW = approveSigDialog model sbr
    cwNetwork = _sbr_currentNetwork sbr
    payloadNetwork = mkNetworkName . view networkId
      <$> (_pNetworkId $ _sbr_payload sbr)
    checkForHashMismatch next = do
      let sbrHash = _sigDataHash sigData
          payloadTxt = _sigDataCmd sigData
      case fmap (hash . T.encodeUtf8) payloadTxt of
        Just payloadHash -> if sbrHash == payloadHash
          then next
          else flip errorDialog backW $ do
            el "p" $ text
              "Danger! The actual hash of the payload does not match the supplied hash"
            el "p" $ text $ "Supplied hash: " <> tshow sbrHash
            el "p" $ text $ "Actual hash: " <> tshow payloadHash
        -- This case shouldn't happen but we will fail anyways
        Nothing -> flip errorDialog backW $ text "Internal error -- Your supplied sig data does not contain a payload"

    networkWarning currentKnown mPayNet =
      case mPayNet of
        Nothing -> "The payload you are signing does not have an associated network"
        Just p -> "The payload you are signing has network id: \""
                  <> textNetworkName p <>
                  "\" but your active chainweaver node is on: \""
                  <> textNetworkName currentKnown <> "\""
    checkNetwork next =
      case (cwNetwork, payloadNetwork) of
         (Nothing, _) -> next
         (Just x, Just y)
           | x == y -> next
         (Just x, pNet) -> warningDialog (networkWarning x pNet) backW next

    checkMissingSigs next = let sigs = fmap snd $ _sigDataSigs sigData in
      case (length $ catMaybes sigs) == length sigs of
        False -> next
        True -> flip errorDialog backW $ text "Everything has been signed. There is nothing to add"


--------------------------------------------------------------------------------
-- Approval
--------------------------------------------------------------------------------
approveSigDialog
  :: SigBuilderWorkflow t m model key
  => model
  -> SigBuilderRequest key
  -> Workflow t m (Event t ())
approveSigDialog model sbr = Workflow $ do
  let sigData = _sbr_sigData sbr
      sdHash = toUntypedHash $ _sigDataHash $ _sbr_sigData sbr
      keys = fmap _key_pair $ _sbr_cwKeys sbr
      sigs = _sigDataSigs sigData --[(PublicKeyHex, Maybe UserSig)]
      p = _sbr_payload sbr
  onClose <- modalHeader $ text "Review Transaction"
  sbTabDyn <- fst <$> sigBuilderSummaryTabs never
  sigsOrKeysE <- modalMain $ do
    -- TODO: Do this in a way that isnt completely stupid
    -- The Summary tab produces a list of all external sigs -- when we switch tabs, we clear this
    -- list instead of accumulating
    eeSigList <- dyn $ ffor sbTabDyn $ \case
      SigBuilderTab_Summary ->
        updated . sequence <$>
          showSigsWidget p (_keyPair_publicKey <$> keys) sigs sdHash
      SigBuilderTab_Details ->  sigBuilderDetailsUI p >>  ([] <$) <$> getPostBuild
    switchHoldPromptly never eeSigList
  sigsOrKeys <- holdDyn [] sigsOrKeysE
  (back, sign) <- modalFooter $ (,)
    <$> cancelButton def "Back"
    <*> confirmButton def "Sign"
  let sigsOnSubmit = mapMaybe (\(a, mb) -> fmap (a,) mb) <$> current sigsOrKeys <@ sign
  let workflowEvent = leftmost
        [ txnInputDialog model (Just sigData) <$ back
        , signAndShowSigDialog model sbr keys (approveSigDialog model sbr) <$> sigsOnSubmit
        ]
  return (onClose, workflowEvent)


data SigBuilderTab = SigBuilderTab_Summary | SigBuilderTab_Details
  deriving (Show, Eq)

sigBuilderSummaryTabs
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Event t SigBuilderTab
  -> m (Dynamic t SigBuilderTab, Event t ())
sigBuilderSummaryTabs tabEv = do
  let f t0 g = case g t0 of
        Nothing -> (Just t0, Just ())
        Just t  -> (Just t, Nothing)
  rec
    (curSelection, done) <- mapAccumMaybeDyn f SigBuilderTab_Summary $ leftmost
      [ const . Just <$> onTabClick
      , const . Just <$> tabEv
      ]
    (TabBar onTabClick) <- makeTabBar $ TabBarCfg
      { _tabBarCfg_tabs = [SigBuilderTab_Summary, SigBuilderTab_Details]
      , _tabBarCfg_mkLabel = \_ -> displaySigBuilderTab
      , _tabBarCfg_selectedTab = Just <$> curSelection
      , _tabBarCfg_classes = mempty
      , _tabBarCfg_type = TabBarType_Secondary
      }
  pure (curSelection, done)
  where
    displaySigBuilderTab SigBuilderTab_Details = text "Details"
    displaySigBuilderTab SigBuilderTab_Summary = text "Summary"

showSigsWidget
  :: (MonadWidget t m, HasCrypto key m)
  => Payload PublicMeta Text
  -> [PublicKey]
  -> [(PublicKeyHex, Maybe UserSig)]
  -> Hash
  -> m [Dynamic t (PublicKeyHex, Maybe UserSig)]
showSigsWidget p cwKeys sigs sdHash = do
  let
    signers = p^.pSigners
    orderedSigs = catMaybes $ ffor signers $ \s ->
      ffor (lookup (PublicKeyHex $ _siPubKey s) sigs) $ \a-> (PublicKeyHex $ _siPubKey s, s, a)
    missingSigs = filter (\(_, _, sig) -> isNothing sig) orderedSigs
    (unscoped, scoped) = partition isUnscoped $ view _2 <$> missingSigs
    -- CwSigners we only need the signer structure;
    -- ExternalSigners we need a (pkh, Signer) tuple to make lookup more efficient
    -- when a new signature is added
    (cwSigners, externalSigners) = first (fmap (view _2)) $ partition isCWSigner missingSigs
    externalLookup = fmap (\(a, b, _) -> (a, b)) externalSigners

  void $ mkLabeledInput False "Hash"
    (\c -> uiInputElement $ c & initialAttributes %~ Map.insert "disabled" "") $ def
      & inputElementConfig_initialValue .~ hashToText sdHash

  rec showTransactionSummary (signersToSummary <$> dSigners) p
      dUnscoped <- ifEmptyBlankSigner unscoped $ do
        dialogSectionHeading mempty "Unscoped Signers"
        divClass "group" $ do
          fmap catMaybes $ mapM unscopedSignerRow unscoped

      dScoped <- ifEmptyBlankSigner scoped $ do
        dialogSectionHeading mempty "Scoped Signers"
        fmap catMaybes $ mapM scopedSignerRow scoped
      let sigsOrKeys = dScoped <> dUnscoped
          sigsOrKeys' = sequence sigsOrKeys
      -- This constructs the entire list of all signers going to be signed each time a new signer is
      -- added.
      dSigners <- foldDyn ($) cwSigners $ leftmost
        [ ffor (updated sigsOrKeys') $ \new _ ->
            let newSigners = catMaybes -- lookups shouldn't fail but we use this to get rid of the maybes
                  $ fmap (`lookup` externalLookup) -- use the pkh to get the official Signer structure
                  $ fmap fst -- we only care about the pubkey, not the sig
                  $ ffilter (isJust . snd) new -- get rid of unsigned elems
             in cwSigners <> newSigners
        ]
  pure sigsOrKeys
  where
    signersToSummary signers =
      let (trans, paysGas, unscoped) = foldr go (mempty, False, 0) signers
          pm = p ^. pMeta
          totalGas = fromIntegral (_pmGasLimit pm) * _pmGasPrice pm
          gas = if paysGas then Just totalGas else Nothing
       in TransactionSummary trans gas unscoped
    go signer (tokenMap, doesPayGas, unscopedCounter) =
      let capList = signer^.siCapList
          tokenTransfers = mapMaybe parseFungibleTransferCap capList
          tokenMap' = foldr (\(k, v) m -> Map.insertWith (+) k v m) tokenMap tokenTransfers
          signerHasGas = isJust $ find (\cap -> asString (_scName cap) == "coin.GAS") capList
      in if capList == [] then (tokenMap, doesPayGas, unscopedCounter + 1)
                          else (tokenMap', doesPayGas || signerHasGas, unscopedCounter)
    cwKeysPKH = PublicKeyHex . keyToText <$> cwKeys
    isCWSigner (pkh, _, _) = pkh `elem` cwKeysPKH
    isUnscoped (Signer _ _ _ capList) = capList == []
    ifEmptyBlankSigner l w = if l == [] then (blank >> pure []) else w
    unOwnedSigningInput s =
      let mPub = hush $ parsePublicKey $ _siPubKey s
       in case mPub of
            Nothing -> blank >> pure Nothing
            Just pub ->
              if pub `elem` cwKeys
                then blank >> pure Nothing
                else fmap Just $ uiSigningInput sdHash pub

    unscopedSignerRow s = do
      divClass "signer__pubkey" $ text $ _siPubKey s
      unOwnedSigningInput s

    scopedSignerRow signer = do
      divClass "group__signer" $ do
        visible <- divClass "signer__row" $ do
          let accordionCell o = (if o then "" else "accordion-collapsed ") <> "payload__accordion "
          rec
            clk <- elDynClass "div" (accordionCell <$> visible') $ accordionButton def
            visible' <- toggle True clk
          divClass "signer__pubkey" $ text $ _siPubKey signer
          pure visible'
        elDynAttr "div" (ffor visible $ bool ("hidden"=:mempty) mempty)$ do
          capListWidget $ _siCapList signer
        unOwnedSigningInput signer

parseFungibleTransferCap :: SigCapability -> Maybe (Text, Decimal)
parseFungibleTransferCap cap = transferCap cap
  where
    isFungible (QualifiedName _capModName capName _) =
      capName == "TRANSFER"
    transferCap (SigCapability modName
      [(PLiteral (LString _sender))
      ,(PLiteral (LString _receiver))
      ,(PLiteral (LDecimal amount))])
        | isFungible modName = Just (renderCompactText $ _qnQual modName, amount)
        | otherwise = Nothing
    transferCap _ = Nothing


data TransactionSummary = TransactionSummary
  { _ts_tokenTransfers :: Map Text Decimal
  , _ts_maxGas :: Maybe GasPrice -- Checks for GAS cap -- and then uses meta to calc
  , _ts_numUnscoped :: Int
  } deriving (Show, Eq)

showTransactionSummary
  :: MonadWidget t m
  => Dynamic t (TransactionSummary)
  -> Payload PublicMeta Text
  -> m ()
showTransactionSummary dSummary p = do
  dialogSectionHeading mempty "Impact Summary"
  divClass "group" $ dyn_ $ ffor dSummary $ \summary -> do
    let tokens = _ts_tokenTransfers summary
        kda = Map.lookup "coin" tokens
        tokens' = Map.delete "coin" tokens
    mkLabeledClsInput True "Tx Type" $ const $ text $ prpc $ p^.pPayload
    case _ts_maxGas summary of
      Nothing -> blank
      Just price -> void $ mkLabeledClsInput True "Max Gas Cost" $
        const $ text $ renderCompactText price <> " KDA"
    flip (maybe blank) kda $ \kdaAmount ->
      void $ mkLabeledClsInput True "Amount KDA" $ const $
        text $ showWithDecimal kdaAmount <> " KDA"
    if tokens' == mempty then blank else
      void $ mkLabeledClsInput True "Amount (Tokens)" $ const $ el "div" $
        forM_ (Map.toList tokens') $ \(name, amount) ->
          el "p" $ text $ showWithDecimal amount <> " " <> name
    void $ mkLabeledClsInput True "Unscoped Sigs" $
      const $ text $ tshow $ _ts_numUnscoped summary
  where
    prpc (Exec _) = "Exec"
    prpc (Continuation _) = "Continuation"


--------------------------------------------------------------------------------
-- Signature and Submission
--------------------------------------------------------------------------------
data SigDetails = SigDetails_Yaml | SigDetails_Json
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

showSigDetailsTabName :: SigDetails -> Text
showSigDetailsTabName SigDetails_Json = "JSON"
showSigDetailsTabName SigDetails_Yaml = "YAML"

signatureDetails
  :: (MonadWidget t m)
  => SigData Text
  -> m ()
signatureDetails sd = do
  void $ mkLabeledInput False "Hash"
    (\c -> uiInputElement $ c & initialAttributes %~ Map.insert "disabled" "") $ def
      & inputElementConfig_initialValue .~ hashToText (toUntypedHash $ _sigDataHash sd)

  divClass "tabset" $ mdo
    curSelection <- holdDyn SigDetails_Yaml onTabClick
    (TabBar onTabClick) <- makeTabBar $ TabBarCfg
      { _tabBarCfg_tabs = [minBound .. maxBound]
      , _tabBarCfg_mkLabel = const $ text . showSigDetailsTabName
      , _tabBarCfg_selectedTab = Just <$> curSelection
      , _tabBarCfg_classes = mempty
      , _tabBarCfg_type = TabBarType_Primary
      }

    tabPane mempty curSelection SigDetails_Yaml $ do
      let sigDataText = T.decodeUtf8 $ Y.encode1Strict sd
      void $ uiSignatureResult sigDataText
    tabPane mempty curSelection SigDetails_Json $ do
      let sigDataText = T.decodeUtf8 $ LB.toStrict $ A.encode $ A.toJSON sd
      void $ uiSignatureResult sigDataText
  pure ()
  where
    uiSignatureResult txt = do
      void $ uiTextAreaElement $ def
        & textAreaElementConfig_initialValue .~ txt
        & initialAttributes <>~ fold
          [ "disabled" =: "true"
          , "class" =: " labeled-input__input labeled-input__sig-builder"
          ]
      uiDetailsCopyButton $ constant txt

signAndShowSigDialog
  :: SigBuilderWorkflow t m model key
  => model
  -> SigBuilderRequest key
  -> [KeyPair key]  -- ChainweaverKeys
  -> Workflow t m (Event t ()) -- Workflow for going back
  -> [(PublicKeyHex, UserSig)] -- User-supplied Sigs
  -> Workflow t m (Event t ())
signAndShowSigDialog model sbr keys backW sigsOrPrivate = Workflow $ mdo
  onClose <- modalHeader $ text "Sig Data"
  -- This allows a "loading" page to render before we attempt to do the really computationally
  -- expensive sigs
  pb <- delay 0.1 =<< getPostBuild
  -- TODO: Can we forkIO the sig process and display something after they are done?
  dmCmd <- widgetHold (modalMain $ text "Loading Signatures ..." >> pure Nothing) $ ffor pb $ \_ ->
    modalMain $ do
      sd <- addSigsToSigData (_sbr_sigData sbr) keys sigsOrPrivate
      signatureDetails sd
      if Nothing `elem` (snd <$> _sigDataSigs sd)
        then pure Nothing
        else pure $ hush $ sigDataToCommand sd
  (back, done, submit) <- modalFooter $ (,,)
    <$> cancelButton def "Back"
    <*> confirmButton def "Done"
    <*> submitButton dmCmd

  let
      cmdAndNet = (,) <$> dmCmd <*> model ^. network_selectedNodes
      -- Gets rid of Maybe over Command by filtering
      eCmdAndNet = fmapMaybe (\(mCmd, ni) -> fmap (,ni) mCmd)
                     $ current cmdAndNet <@ submit
      -- Given M (a, b) and f :: a -> b -> c , give us M c
      fUncurry f functor = fmap (\tpl -> uncurry f tpl) functor
      p = _sbr_payload sbr
      chain = p^.pMeta.pmChainId
      sender = p^.pMeta.pmSender
      submitToNetworkE = transferAndStatus model (AccountName sender, chain) `fUncurry` eCmdAndNet
  return (onClose <> done, leftmost [backW <$ back, submitToNetworkE])
  where
    submitButton dmCmd = do
      let baseCfg = "class" =: "button button_type_confirm"
          dynAttr = ffor dmCmd $ \case
            Nothing -> baseCfg <> ("hidden" =: "true")
            Just _ -> baseCfg
      (e, _) <- elDynAttr' "button" dynAttr $ text "Submit to Network"
      pure $ domEvent Click e

addSigsToSigData
  :: (
       MonadJSM m
     , HasCrypto key m
     )
  => SigData Text
  -> [KeyPair key]
  -- ^ Keys which we are signing with
  -> [(PublicKeyHex, UserSig)]
  -> m (SigData Text)
addSigsToSigData sd signingKeys sigList = do
  let hashToSign = unHash $ toUntypedHash $ _sigDataHash sd
      someSigs = _sigDataSigs sd
      pubKeyToTxt =  PublicKeyHex . keyToText . _keyPair_publicKey
      cwSigners =  fmap (\x -> (pubKeyToTxt x, _keyPair_privateKey x)) signingKeys
      toPactSig sig = UserSig $ keyToText sig
  sigList' <- forM someSigs $ \(pkHex, mSig) -> case mSig of
    Just sig -> pure (pkHex, Just sig)
    Nothing -> case pkHex `lookup` cwSigners of
      Just (Just priv) -> do
        sig <- cryptoSign hashToSign priv
        pure (pkHex, Just $ toPactSig sig )
      _ -> pure (pkHex, pkHex `lookup` sigList)
  pure $ sd { _sigDataSigs = sigList' }

transferAndStatus
  :: (MonadWidget t m, HasLogger model t, HasTransactionLogger m)
  => model
  -> (AccountName, ChainId)
  -> Command Text
  -> [Either Text NodeInfo]
  -> Workflow t m (Event t ())
transferAndStatus model (sender, cid) cmd nodeInfos = Workflow $ do
    close <- modalHeader $ text "Transfer Status"
    _ <- elClass "div" "modal__main transaction_details" $
      submitTransactionWithFeedback model cmd sender cid nodeInfos
    done <- modalFooter $ confirmButton def "Done"
    pure (close <> done, never)

--------------------------------------------------------------------------------
-- Display Widgets
--------------------------------------------------------------------------------

sigBuilderDetailsUI
  :: MonadWidget t m
  => Payload PublicMeta Text
  -> m ()
sigBuilderDetailsUI p = do
  txMetaWidget (p^.pMeta) $ p^.pNetworkId
  pactRpcWidget  $ _pPayload p
  signerWidget $ p^.pSigners
  pure ()


txMetaWidget
  :: MonadWidget t m
  => PublicMeta
  -> Maybe NetworkId
  -> m ()
txMetaWidget pm mNet = do
  dialogSectionHeading mempty "Transaction Metadata"
  _ <- divClass "group segment" $ do
    case mNet of
      Nothing -> blank
      Just n ->
        mkLabeledClsInput True "Network" $ \_ -> text $ renderCompactText n
    mkLabeledClsInput True "Chain" $ const $ text $ renderCompactText $ pm^.pmChainId
    mkLabeledClsInput True "Gas Payer" $ \_ -> text $ pm^.pmSender
    mkLabeledClsInput True "Gas Price" $ \_ -> text $ renderCompactText $ pm^.pmGasPrice
    mkLabeledClsInput True "Gas Limit" $ \_ -> text $ renderCompactText $ pm^.pmGasLimit
    let totalGas = fromIntegral (_pmGasLimit pm) * _pmGasPrice pm
    mkLabeledClsInput True "Max Gas Cost" $ \_ -> text $ renderCompactText totalGas <> " KDA"
  pure ()

pactRpcWidget
  :: MonadWidget t m
  => PactRPC Text
  -> m ()
pactRpcWidget (Exec e) = do
  dialogSectionHeading mempty "Code"
  divClass "group" $ do
    el "code" $ text $ tshow $ pretty $ _pmCode e
  case _pmData e of
    A.Null -> blank
    jsonVal -> do
      dialogSectionHeading mempty "Data"
      divClass "group" $
        void $ uiTextAreaElement $ def
          & textAreaElementConfig_initialValue .~ (T.decodeUtf8 $ LB.toStrict $ A.encode jsonVal)
          & initialAttributes .~ "disabled" =: "" <> "style" =: "width: 100%"
pactRpcWidget (Continuation c) = do
  dialogSectionHeading mempty "Continuation Data"
  divClass "group" $ do
    mkLabeledClsInput True "Pact ID" $ \_ -> text $ renderCompactText $ _cmPactId c
    mkLabeledClsInput True "Step" $ \_ -> text $ tshow $ _cmStep c
    mkLabeledClsInput True "Rollback" $ \_ -> text $ tshow $ _cmRollback c
    mkLabeledClsInput True "Data" $ \_ -> do
      let contData = renderCompactText $ _cmData c
      void $ uiTextAreaElement $ def
        & textAreaElementConfig_initialValue .~ contData
        & initialAttributes .~ fold
          [ "disabled" =: "true"
          , "rows" =: "2"
          , "style" =: "color: black; width: 100%; height: auto;"
          ]
    mkLabeledClsInput True "Proof" $ \_ -> do
      let mProof = _cmProof c
      case mProof of
        Nothing -> blank
        Just p ->
          void $ uiTextAreaElement $ def
            & textAreaElementConfig_initialValue .~ (renderCompactText p)
            & initialAttributes .~ fold
              [ "disabled" =: "true"
              , "rows" =: "20"
              , "style" =: "color: black; width: 100%; height: auto;"
              ]

signerWidget
  :: (MonadWidget t m)
  => [Signer]
  -> m ()
signerWidget signers = do
  dialogSectionHeading mempty "Signers"
  forM_ signers $ \s ->
    divClass "group segment" $ do
      mkLabeledClsInput True "Key:" $ \_ -> text (renderCompactText $ s ^.siPubKey)
      mkLabeledClsInput True "Caps:" $ \_ -> capListWidget $ s^.siCapList

capListWidget :: MonadWidget t m => [SigCapability] -> m ()
capListWidget [] = text "Unscoped Signer"
capListWidget cl = do
  let
    capLines = foldl (\acc cap -> (renderCompactText cap):"":acc) [] cl
    txt = T.init $ T.init $ T.unlines capLines
    rows = tshow $ 2 * length capLines
  void $ uiTextAreaElement $ def
    & textAreaElementConfig_initialValue .~ txt
    & initialAttributes .~ fold
      [ "disabled" =: ""
      , "rows" =: rows
      , "style" =: "color: black; width: 100%; height: auto;"
      ]

networkWidget :: MonadWidget t m => Payload PublicMeta a -> m ()
networkWidget p =
  case p^.pNetworkId of
    Nothing -> blank
    Just n -> do
      dialogSectionHeading mempty "Network"
      divClass "group" $ text $ n ^. networkId

