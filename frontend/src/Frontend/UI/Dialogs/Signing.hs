{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Dialog presented for generating responses to signing API requests.
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.Signing
  ( uiSigning
  , uiQuickSign
  ) where

import           Control.Arrow                  ((&&&))
import           Control.Lens
import           Control.Monad                  ((<=<), void)
import           Data.Aeson
import qualified Data.ByteString.Lazy           as BSL
import           Data.Either
import qualified Data.IntMap                    as IMap
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.Encoding
import           Kadena.SigningApi
import           Kadena.SigningTypes
import           Pact.Parse
import           Pact.Types.Capability
import           Pact.Types.ChainId
import           Pact.Types.ChainMeta
import           Pact.Types.Command
import           Pact.Types.Exp
import           Pact.Types.Names
import           Pact.Types.PactValue
import           Pact.Types.Pretty
import           Pact.Types.RPC
import           Pact.Types.Runtime             (GasLimit (..), GasPrice (..))
import           Pact.Types.SigData
import qualified Pact.Types.Term                as Pact (PublicKey (..))
import           Pact.Types.Util                (asString)
import           Reflex
import           Reflex.Dom                     hiding (Value)

import           Control.Applicative            ((<|>))
import           Control.Monad                  (forM, join)
import           Data.Decimal                   (Decimal)
import           Data.Default
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import qualified Data.Text.Encoding             as T (encodeUtf8)

import           Frontend.Crypto.Class
import           Frontend.Crypto.Ed25519        (fromPactPublicKey)
import           Frontend.Foundation            hiding (Arg)
import           Frontend.JsonData
import           Frontend.Network
import           Frontend.UI.DeploymentSettings
import           Frontend.UI.Modal.Impl
import           Frontend.UI.TabBar
import           Frontend.UI.Widgets
import           Frontend.UI.Widgets.Helpers
import           Frontend.Wallet

-- import qualified Frontend.UI.Dialogs.SigBuilder as SB
import Frontend.UI.Dialogs.SigBuilder
import Frontend.Log

type HasUISigningModelCfg mConf key t =
  ( Monoid mConf, Flattenable mConf t, HasWalletCfg mConf key t
  , HasJsonDataCfg mConf t, HasNetworkCfg mConf t
  )

-- | Ask user for confirmation before deleting "something".
--
-- At the moment "something" is only keys, so we skip making "something"
-- configurable for now.
--
uiSigning
  :: forall key t m mConf
  . ( MonadWidget t m
    , HasUISigningModelCfg mConf key t
    , HasCrypto key (Performable m)
    , HasTransactionLogger m
    )
  => ModalIde m key t
  -> (SigningRequest, Either Text SigningResponse -> JSM ())
  -> Event t ()
  -> m (mConf, Event t ())
uiSigning ideL (signingRequest, writeSigningResponse) onCloseExternal = do
  onClose <- modalHeader $ text "Signing Request"

  (mConf, result, _) <- uiDeploymentSettings ideL $ DeploymentSettingsConfig
    { _deploymentSettingsConfig_chainId = case _signingRequest_chainId signingRequest of
        Just c -> \_ -> predefinedChainIdDisplayed c
        Nothing -> fmap value . userChainIdSelect . getChainsFromHomogenousNetwork
    , _deploymentSettingsConfig_userTab = Nothing
    , _deploymentSettingsConfig_code = pure $ _signingRequest_code signingRequest
    , _deploymentSettingsConfig_sender = case _signingRequest_sender signingRequest of
        Just sender -> \_ _ _ -> uiAccountFixed sender
        Nothing -> \_ _ _ -> uiAccountAny
    , _deploymentSettingsConfig_data = _signingRequest_data signingRequest
    , _deploymentSettingsConfig_nonce = _signingRequest_nonce signingRequest
    , _deploymentSettingsConfig_ttl = _signingRequest_ttl signingRequest
    , _deploymentSettingsConfig_gasLimit = _signingRequest_gasLimit signingRequest
    , _deploymentSettingsConfig_caps = Just $ _signingRequest_caps signingRequest
    , _deploymentSettingsConfig_extraSigners = fromPactPublicKey <$> fromMaybe [] (_signingRequest_extraSigners signingRequest)
    , _deploymentSettingsConfig_includePreviewTab = True
    }

  let response = deploymentResToResponse <$> result

  finished <- (performEvent . fmap (liftJSM . writeSigningResponse)) <=< headE $
    maybe (Left "Cancelled") Right <$> leftmost
      [ Just <$> response
      , Nothing <$ onCloseExternal
      , Nothing <$ onClose
      ]

  pure (mConf, finished)

  where
    deploymentResToResponse result =
      SigningResponse
        { _signingResponse_chainId = _deploymentSettingsResult_chainId result
        , _signingResponse_body = _deploymentSettingsResult_command result
        }

-- | Ask user for confirmation before deleting "something".
--
-- At the moment "something" is only keys, so we skip making "something"
-- configurable for now.
--
uiQuickSign
  :: forall key t m mConf model
  . ( MonadWidget t m
    , HasUISigningModelCfg mConf key t
    , HasCrypto key m
    , HasCrypto key (Performable m)
    , HasWallet model key t -- So we can determine which signing-reqs are for our keys
    , HasTransactionLogger m
    , HasNetwork model t
    , HasTransactionLogger m
    , HasLogger model t
    )
  => model -- ModalIde m key t
  -> (QuickSignRequest, Either Text QuickSignResponse -> JSM ())
  -> Event t ()
  -> m (mConf, Event t ())
uiQuickSign ideL (qsr, writeSigningResponse) onCloseExternal = (mempty, ) <$> do
  let sendResp = performEvent . fmap (liftJSM . writeSigningResponse . Right) -- We use Either for legacy reasons, but the quicksign protocol now allows for error handling in its response, so we just `const Right` now
      emptyReqResponse = do
        eClose <- failureModal "QuickSign request was empty" "" onCloseExternal
        sendResp $ QSR_Error QuicksignError_EmptyList <$ eClose
  if _quickSignRequest_csds qsr == []
    then emptyReqResponse
    else case partitionEithers $ fmap snd sdReqs of
       ([], reqs) -> quickSignModal ideL sendResp reqs onCloseExternal
       _ -> do
         eClose <- failureModal
           "Quicksign request was ill-formed"
           "One or more of the CommandSigDatas do not contain valid Commands"
           onCloseExternal
         let
           resps = ffor sdReqs $ \(orig, errOrSD) ->
             case errOrSD of
               Left e -> CSDResponse orig $ SO_Failure $ T.pack e
               Right _ -> CSDResponse orig SO_NoSig
         sendResp $ QSR_Response resps <$ eClose
  where
    csdToSigData (CommandSigData sl txt) =
      ffor (eitherDecodeStrict $ encodeUtf8 txt) $ \p ->
        let sd = payloadToSigData p txt
            sdSigList = ffor (unSignatureList sl) $ (\(CSDSigner k mSig) -> (k, mSig))
         in flip PayloadSigningRequest p $ sd { _sigDataSigs = sdSigList }

    sdReqs = ffor (_quickSignRequest_csds qsr) $ \req ->
      (req, csdToSigData req)

failureModal :: MonadWidget t m => Text -> Text -> Event t () -> m (Event t ())
failureModal msgHeader msg onCloseExternal = do
  onClose <- modalHeader $ text "QuickSign Failure"
  void $ modalMain $ do
    el "h3" $ text msgHeader
    text msg
  done <- modalFooter $ confirmButton def "Done"
  pure $ leftmost [done, onClose, onCloseExternal]

quickSignModal
  :: forall key t m mConf model
  . ( MonadWidget t m
    , HasCrypto key (Performable m)
    , HasWallet model key t
    , HasNetwork model t
    )
  => model
  -> (Event t QuickSignResponse -> m (Event t ()))
  -> [PayloadSigningRequest]
  -> Event t ()
  -> m (Event t ())
quickSignModal ideL writeSigningResponse payloads externalClose = fmap switchPromptlyDyn $ workflow $ Workflow $ do
  keysAndNet <- sample $ fetchKeysAndNetwork ideL
  runQuickSignChecks payloads (_srws_currentNetwork keysAndNet) $ do
    onClose <- modalHeader $ text "QuickSign Request"
    modalMain $ do
      qsTabDyn <- fst <$> sigBuilderTabs never
      dyn_ $ ffor qsTabDyn $ \case
        SigBuilderTab_Summary ->
          summarizeTransactions payloads keysAndNet
        SigBuilderTab_Details ->
          quickSignTransactionDetails payloads
      pure ()
    (reject, sign) <- modalFooter $ (,)
      <$> cancelButton def "Reject"
      <*> confirmButton def "Sign All"
    let noResponseEv = ffor (leftmost [reject, onClose, externalClose]) $
          const $ QSR_Error QuicksignError_Reject
    res <- writeSigningResponse noResponseEv
    pure (res, handleSigning payloads writeSigningResponse keysAndNet <$ sign)
  where
    runQuickSignChecks payloads cwNet qsWidget = do
    -- Only contains single network check atm, but we can add more in the future
      let
        toNetworkName = mkNetworkName . view networkId
        netCheck = flip filter payloads $ \(PayloadSigningRequest _ p) ->
          maybe False (\reqNet -> (fmap (== toNetworkName reqNet) cwNet) == Just True) $ p^.pNetworkId
      -- TODO: Will the network check make sense when using a pact-server instead of a node?
      if length netCheck /= length payloads
         then do
           let msgHeader = "Invalid NetworkID"
               msg = "One or more of the quicksign requests has a networkId that does not match current chainweaver network"
           onClose <- modalHeader $ text "QuickSign Failure"
           void $ modalMain $ do
             el "h3" $ text msgHeader
             text msg
           reject <- modalFooter $ confirmButton def "Done"
           res <- writeSigningResponse $ ffor (leftmost [reject, onClose]) $ const $
             QSR_Error $ QuicksignError_Other "Requests for network do not match the current node chainweaver is pointed at"
           pure (res, never)
         else qsWidget

-- Web-based signing is SLOOOOOW so this workflow adds a loading screen
handleSigning
  :: (MonadWidget t m, HasCrypto key (Performable m))
  => [PayloadSigningRequest]
  -> (Event t QuickSignResponse-> m (Event t ()))
  -> SigningRequestWalletState key
  -> Workflow t m (Event t ())
handleSigning payloadRequests writeSigningResponse keysAndNet = Workflow $ do
  void $ modalHeader $ text "Loading Signatures"
  pb <- delay 0.2 =<< getPostBuild
  modalMain $ text "Signing ..."
  let toSign = fmap _psr_sigData payloadRequests
  quickSignRes <- performEvent $ ffor pb $ const $
    -- TODO: ForkIO and have an event for each payload in addition to the final event. The list of
    -- events can be used to update the loading screen with: "x / y signatures left"
    forM toSign $ \sd -> do
      updatedSD <- addSigsToSigData sd (_srws_cwKeys keysAndNet) []
      let calcSigs = length . catMaybes . fmap snd . _sigDataSigs
          sigAdded = calcSigs updatedSD > calcSigs sd
      pure (updatedSD, sigAdded)
  let quickSignRes' = ffor quickSignRes $ QSR_Response . fmap sdToCSDResp
  res <- writeSigningResponse quickSignRes'
  pure (res, never)
  where
    sdSlToCSDSl = SignatureList . fmap (\(k, mSig) -> CSDSigner k mSig)
    sdToCSDResp ((SigData hash sl (Just a)), sigAdded) =
      let csd = CommandSigData (sdSlToCSDSl sl) a
          outcome = if sigAdded then SO_Success hash else SO_NoSig
       in CSDResponse csd outcome

-- |Summary view for quicksign ui
summarizeTransactions
  :: MonadWidget t m
  => [PayloadSigningRequest ]
  -> SigningRequestWalletState  key
  -> m ()
summarizeTransactions payloadReqs walletState = do
  let cwKeyset = Set.fromList $
        fmap (PublicKeyHex . keyToText . _keyPair_publicKey) $ _srws_cwKeys walletState
      signerMap = accumulateCWSigners payloadReqs cwKeyset
      mNetId = _srws_currentNetwork walletState
  impactSummary payloadReqs mNetId $ Set.fromList $ Map.keys signerMap
  dialogSectionHeading mempty "My Signers"
  forM_ (Map.toList signerMap) $ \(pkh, txList) ->
    signerSection True pkh $ capListWidgetWithHash txList
  pure ()

capListWidgetWithHash
  :: MonadWidget t m
  => [(Text, ChainId, [SigCapability], Maybe UserSig)]
  -> m ()
capListWidgetWithHash [] = text "Unscoped Signer"
capListWidgetWithHash cl = do
  forM_ cl $ \(hash, cid, txCapList, mSig) -> do
    let capLines = foldl (\acc cap -> (renderCompactText cap):"":acc) [] txCapList
        txt = if txCapList == [] then "Unscoped Signer" else T.init $ T.init $ T.unlines capLines
        --TODO: Guesses box size needs to always be 2x. It's a decent approx but should be
        --refactored
        rows = tshow $ 2 * length capLines
    elClass "h4" "heading heading_type_h4" $ text $ "Tx ID: " <> hash
    case mSig of
      Nothing -> blank
      Just (UserSig sig) -> do
        elClass "h4" "heading heading_type_h4" $ text $ "Provided Signature: "
        void $ uiTextAreaElement $ def
          & textAreaElementConfig_initialValue .~ sig
          & initialAttributes .~ fold
            [ "disabled" =: "true"
            , "style" =: "color: black; width: 100%; height: auto;"
            ]
    elClass "h4" "heading heading_type_h4" $ text $ "On Chain: " <> tshow cid
    _ <- uiTextAreaElement $ def
      & textAreaElementConfig_initialValue .~ txt
      & initialAttributes .~ fold
        [ "disabled" =: "true"
        , "rows" =: rows
        , "style" =: "color: black; width: 100%; height: auto;"
        ]
    pure ()

accumulateCWSigners
  :: [PayloadSigningRequest]
  -> Set PublicKeyHex
  -> Map PublicKeyHex [(Text, ChainId, [SigCapability], Maybe UserSig)]
accumulateCWSigners payloads cwKeyset = foldr
  (\(hash, cid, signers, sigList) signerMap -> foldr (go hash cid sigList) signerMap signers) mempty $
    getSigner <$> payloads
  where
    getSigner p =
      let hash = renderCompactText $ _sigDataHash $ _psr_sigData p
          chain = view (pMeta.pmChainId) $ _psr_payload p
          signers = _pSigners $ _psr_payload p
          sigList = _sigDataSigs $ _psr_sigData p
      in (hash, chain, signers, sigList)
    go hash cid sigList signer accum =
      let pkh = PublicKeyHex $ _siPubKey signer in
       if Set.notMember pkh cwKeyset
         then accum
         else Map.insertWith (<>) pkh [(hash, cid, _siCapList signer, join $ lookup pkh sigList)] accum


-- | Details page for quicksign transactions
quickSignTransactionDetails
  :: MonadWidget t m
  => [PayloadSigningRequest]
  -> m ()
quickSignTransactionDetails payloadReqs = do
  let payloadsAndMeta = ffor payloadReqs $ \(PayloadSigningRequest (SigData hash sigList _) p) ->
        (hash, sigList, p)
  dialogSectionHeading mempty $ "QuickSign Payloads ( " <> (tshow $ length payloadReqs) <> " total )"
  sequence_ $ ffor payloadsAndMeta $ flip txRow True
  where
    txRow (txId, sigList, p) startExpanded = divClass "payload__row" $ do
      visible <- divClass "group payload__header" $ do
        let accordionCell o = (if o then "" else "accordion-collapsed ") <> "payload__accordion "
        rec
          clk <- elDynClass "div" (accordionCell <$> visible') $ accordionButton def
          visible' <- toggle startExpanded clk
        divClass "payload__txid" $ text $ "Tx ID: " <> tshow txId
        pure visible'
      dyn_ $ ffor visible $ \case
        False -> blank
        True -> divClass "payload__background-wrapper" $
                  sigBuilderDetailsUI p sigList  "payload__info" $ Just "qs-wrapper"

data QuickSignSummary = QuickSignSummary
  { _qss_numSigs       :: Int
  , _qss_unscoped      :: Int
  , _qss_gasSubTotal   :: GasPrice
  , _qss_chainsSigned  :: Set ChainId
  , _qss_tokens        :: Map Text Decimal
  } deriving (Show, Eq)

instance Default QuickSignSummary where
  def = QuickSignSummary
    { _qss_numSigs = 0
    , _qss_unscoped = 0
    , _qss_gasSubTotal = 0
    , _qss_chainsSigned = mempty
    , _qss_tokens = mempty
    }

impactSummary
  :: MonadWidget t m
  => [PayloadSigningRequest]
  -> Maybe NetworkName
  -> Set PublicKeyHex
  -> m ()
impactSummary payloadReqs mNetId signerSet = do
  let signingImpact = foldr scrapePayloadFold def payloadReqs
  dialogSectionHeading mempty "Impact Summary"
  divClass "group" $ do
    maybe blank (mkCategory "Network" . text . textNetworkName) mNetId
    let tokens = _qss_tokens signingImpact
        kda = Map.lookup "coin" tokens
        tokens' = Map.delete "coin" tokens
    flip (maybe blank) kda $ \kdaAmount ->
      mkCategory "Spending" $ text $ showWithDecimal kdaAmount <> " KDA"
    if tokens' == mempty then blank else
      mkCategory "Spending (Tokens)" $ el "div" $
        forM_ (Map.toList tokens') $ \(name, amount) ->
          el "p" $ text $ showWithDecimal amount <> " " <> name
    case _qss_gasSubTotal signingImpact of
      0 -> blank
      price -> mkCategory "Max Gas Cost" $ text $ renderCompactText price <> " KDA"
    mkCategory "Number of Requests" $ text $ tshow $ length payloadReqs
    mkCategory "Number of Sigs Added" $ text $ tshow $ _qss_numSigs signingImpact
    let multipleChains = 1 < (Set.size $ _qss_chainsSigned signingImpact)
        chainHeader = bool "Via Chain" "Via Chains" multipleChains
    mkCategory chainHeader $ text $ T.intercalate ", " $ fmap renderCompactText $
        sortChainIds $ Set.toList $ _qss_chainsSigned signingImpact
    let unscoped = _qss_unscoped signingImpact
    if unscoped == 0 then blank else
      mkCategory "Unscoped Sigs Added" $ text $ tshow unscoped
  where
    mkCategory header content =
      void $ mkLabeledClsInput True header $ const content
    isSigning = flip Set.member signerSet . PublicKeyHex . _siPubKey
    scrapePayloadFold :: PayloadSigningRequest -> QuickSignSummary -> QuickSignSummary
    scrapePayloadFold (PayloadSigningRequest sd p) qss =
      let
          sigsRequired = Set.fromList $ fmap fst $ filter (\(_, mSig) -> isNothing mSig) $ _sigDataSigs sd
          signers = filter isSigning $ p^.pSigners
          signersPks = Set.fromList $ fmap (PublicKeyHex . _siPubKey) signers
          -- We do NOT count sigs that have already been added
          numSigsRequiredFromChainweaver = Set.size $ Set.intersection signersPks sigsRequired
          tokenMap = _qss_tokens qss
          -- We still display this data if a signature from a chainweaver key has already been added
          (tokenMap', unscoped, potentiallyPaysGas) =
            foldr scrapeSigner (tokenMap, 0, False) signers
          totalSigs = numSigsRequiredFromChainweaver + _qss_numSigs qss
          totalUnscoped = unscoped + _qss_unscoped qss
          chains = Set.insert (p^.pMeta.pmChainId) $ _qss_chainsSigned qss
          gasCost = if potentiallyPaysGas then (fromIntegral (_pmGasLimit pm) * _pmGasPrice pm) else 0
          pm = p ^. pMeta
          totalGasCost = gasCost + _qss_gasSubTotal qss
      in QuickSignSummary totalSigs totalUnscoped totalGasCost chains tokenMap'
    scrapeSigner signer (tokenMap, unscoped, paysGas) =
      let capList = signer^.siCapList
          tokenTransfers = mapMaybe parseFungibleTransferCap capList
            <> mapMaybe parseFungibleXChainTransferCap capList
          tokenMap' = foldr (\(k, v) m -> Map.insertWith (+) k v m) tokenMap tokenTransfers
          signerHasGasCap = isJust $ find (\cap -> asString (_scName cap) == "coin.GAS") capList
          isUnscoped = capList == []
          potentiallyPaysGas = isUnscoped || signerHasGasCap
       in (tokenMap', if isUnscoped then unscoped + 1 else unscoped, paysGas || potentiallyPaysGas)
