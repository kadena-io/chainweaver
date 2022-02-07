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
  -- , QuickSignSummary(..)
  ) where

import           Control.Arrow                  ((&&&))
import           Control.Lens
import           Control.Monad                  ((<=<))
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
  -> (Event t (Either Text SigningResponse) -> m (Event t ()))
  -> SigningRequest
  -> Event t ()
  -> m (mConf, Event t ())
uiSigning ideL writeSigningResponse signingRequest onCloseExternal = do
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

  finished <- writeSigningResponse <=< headE $
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
  -> (Event t (Either Text QuickSignResponse) -> m (Event t ()))
  -> QuickSignRequest
  -> Event t ()
  -> m (mConf, Event t ())
uiQuickSign ideL writeSigningResponse qsr _onCloseExternal = do
  let toPsrOrErr txt = ffor (eitherDecodeStrict $ encodeUtf8 txt) $ \p ->
        flip PayloadSigningRequest p $ payloadToSigData p txt
  case partitionEithers $ fmap toPsrOrErr $ _quickSignRequest_commands qsr of
    ([], payloads) -> do
      (mempty, ) <$> quickSignModal ideL writeSigningResponse payloads
    (es, _) -> do
      pb <- getPostBuild
      let res = Left $ "QuickSign request contained invalid commands:\n" <> T.unlines (map T.pack es)
      finished <- writeSigningResponse (res <$ pb)
      pure (mempty, finished)

-- -- |Matches ownedKeys against signers required by pact cmd; indexes them by public key
-- keysRequiredForPayload :: Map PublicKey (KeyPair key) -> Payload PublicMeta a -> [(PublicKey, KeyPair key)]
-- keysRequiredForPayload ownedKeys p = foldr foldRequiredKeys [] (p^.pSigners)
--   where
--     foldRequiredKeys payloadKey acc = let key = signerPublicKey payloadKey in
--       maybe acc (\kp -> (key, kp):acc) $ Map.lookup key ownedKeys

-- signerPublicKey :: Signer -> PublicKey
-- signerPublicKey = fromPactPublicKey . Pact.PublicKey . T.encodeUtf8 . view siPubKey

quickSignModal
  :: forall key t m mConf model
  . ( MonadWidget t m
    , HasCrypto key (Performable m)
    , HasWallet model key t
    , HasNetwork model t
    )
  => model
  -> (Event t (Either Text QuickSignResponse) -> m (Event t ()))
  -> [PayloadSigningRequest]
  -> m (Event t ())
quickSignModal ideL writeSigningResponse payloadRequests = do
  keysAndNet <- sample $ fetchKeysAndNet ideL
  onClose <- modalHeader $ text "QuickSign Request"
  modalMain $ do
    qsTabDyn <- fst <$> sigBuilderTabs never
    dyn_ $ ffor qsTabDyn $ \case
      SigBuilderTab_Summary ->
        summarizeTransactions payloadRequests keysAndNet
      SigBuilderTab_Details ->
        quickSignTransactionDetails payloadRequests
    pure ()
  (reject, sign) <- modalFooter $ (,)
    <$> cancelButton def "Reject"
    <*> confirmButton def "Sign All"
  let toSign = fmap _psr_sigData payloadRequests
      noResponseEv = ffor (leftmost [reject, onClose]) $
        const $ Left "QuickSign response rejected"
  quickSignRes <- performEvent $ ffor sign $ const $ fmap QuickSignResponse $
      forM toSign $ \sd -> addSigsToSigData sd (_srws_cwKeys keysAndNet) []
  writeSigningResponse $ leftmost [ noResponseEv, Right <$> quickSignRes ]

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
  -- impactSummary payloadReqs walletState
  dialogSectionHeading mempty "Chainweaver Signers"
  mapM_ signerSection $ Map.toList signerMap
  pure ()
  where
    --TODO: Rip out common part with sigbuilder section
    signerSection (pkh, capList) =
      divClass "group__signer" $ do
        visible <- divClass "signer__row" $ do
          let accordionCell o = (if o then "" else "accordion-collapsed ") <> "payload__accordion "
          rec
            clk <- elDynClass "div" (accordionCell <$> visible') $ accordionButton def
            visible' <- toggle True clk
          divClass "signer__pubkey" $ text $ unPublicKeyHex pkh
          pure visible'
        elDynAttr "div" (ffor visible $ bool ("hidden"=:mempty) mempty)$ do
          capListWidgetWithHash capList

-- impactSummary
--   :: MonadWidget t m
--   => [PayloadSigningRequest ]
--   -> SigningRequestWalletState  key
--   -> m ()
-- impactSummary payloadReqs walletState = do


capListWidgetWithHash
  :: MonadWidget t m
  => [(Text, [SigCapability])]
  -> m ()
--TODO: This case should never return
capListWidgetWithHash [] = text "Unscoped Signer"
capListWidgetWithHash cl = do
  forM_ cl $ \(hash, txCapList) -> do
    let capLines = foldl (\acc cap -> (renderCompactText cap):"":acc) [] txCapList
        txt = if txCapList == [] then "Unscoped Signer" else T.init $ T.init $ T.unlines capLines
        --TODO: This is pretty lazy -- clean it up before final version
        rows = tshow $ 2 * length capLines
    dialogSubSectionHeading mempty $ "Tx ID: " <> hash
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
  -> Map PublicKeyHex [(Text, [SigCapability])]
accumulateCWSigners payloads cwKeyset = foldr go mempty $
  (renderCompactText . _sigDataHash . _psr_sigData &&& _pSigners . _psr_payload)
    <$> payloads
  where
    f hash signer accum =
      let pkh = PublicKeyHex $ _siPubKey signer
       in if Set.notMember pkh cwKeyset then accum
          else Map.insertWith (<>)
                 pkh [(hash, _siCapList signer)] accum
    go (hash, signers) signerMap = foldr (f hash) signerMap signers



-- | Details page for quicksign transactions
quickSignTransactionDetails
  :: MonadWidget t m
  => [PayloadSigningRequest]
  -> m ()
quickSignTransactionDetails payloadReqs = do
  let p1:pRest = fmap (_sigDataHash . _psr_sigData &&& _psr_payload) payloadReqs
  dialogSectionHeading mempty "QuickSign Payloads"
  sequence_ $ (txRow p1 True):(ffor pRest $ \p -> txRow p False)
  where
    txRow (txId, p) startExpanded = do
      visible <- divClass "group payload__row" $ do
        let accordionCell o = (if o then "" else "accordion-collapsed ") <> "payload__accordion "
        rec
          clk <- elDynClass "div" (accordionCell <$> visible') $ accordionButton def
          visible' <- toggle startExpanded clk
        divClass "payload__txid" $ text $ "Tx ID: " <> tshow txId
        pure visible'
      elDynAttr "div" (ffor visible $ bool ("hidden"=:mempty) mempty)$ sigBuilderDetailsUI p

-- showTransactionSummary
--   :: MonadWidget t m
--   => Dynamic t (TransactionSummary)
--   -> Payload PublicMeta Text
--   -> m ()
-- showTransactionSummary dSummary p = do
--   dialogSectionHeading mempty "Impact Summary"
--   divClass "group" $ dyn_ $ ffor dSummary $ \summary -> do
--     let tokens = _ts_tokenTransfers summary
--         kda = Map.lookup "coin" tokens
--         tokens' = Map.delete "coin" tokens
--     mkLabeledClsInput True "Tx Type" $ const $ text $ prpc $ p^.pPayload
--     case _ts_maxGas summary of
--       Nothing -> blank
--       Just price -> void $ mkLabeledClsInput True "Max Gas Cost" $
--         const $ text $ renderCompactText price <> " KDA"
--     flip (maybe blank) kda $ \kdaAmount ->
--       void $ mkLabeledClsInput True "Amount KDA" $ const $
--         text $ showWithDecimal kdaAmount <> " KDA"
--     if tokens' == mempty then blank else
--       void $ mkLabeledClsInput True "Amount (Tokens)" $ const $ el "div" $
--         forM_ (Map.toList tokens') $ \(name, amount) ->
--           el "p" $ text $ showWithDecimal amount <> " " <> name
--     void $ mkLabeledClsInput True "Unscoped Sigs" $
--       const $ text $ tshow $ _ts_numUnscoped summary
--   where
--     prpc (Exec _) = "Exec"
--     prpc (Continuation _) = "Continuation"
----------------------------------------------------------------------------
-- | TODO:
--   - Taking out network, which should just fail on an initial parse
--   - Making certain factors Dynamics so that the user can add sigs/priv keys and update the
--     summary
--   - Deriving requests_ignored instead of making it a first-class field
--
--data QuickSignSummary = QuickSignSummary
--  { _quickSignSummary_activeOwnedKeys :: Set Pact.PublicKey -- Keys CW can automatically sign with
--  , _quickSignSummary_unOwnedKeys     :: Set Pact.PublicKey  -- Keys that aren't controlled by cw, but can still be signed with
--  , _quickSignSummary_totalKDA        :: Decimal
--  , _quickSignSummary_gasSubTotal     :: Decimal
--  , _quickSignSummary_chainsSigned    :: Set ChainId
--  , _quickSignSummary_requestsCanSign :: Int
--  , _quickSignSummary_network         :: [ NetworkId ]
--  } deriving (Show, Eq)

----TODO: Is a semigroup with (+) on decimals appropriate?
--instance Semigroup QuickSignSummary where
--  (<>) a b =
--    QuickSignSummary
--    { _quickSignSummary_activeOwnedKeys = _quickSignSummary_activeOwnedKeys a <> _quickSignSummary_activeOwnedKeys b
--    , _quickSignSummary_unOwnedKeys = _quickSignSummary_unOwnedKeys a <> _quickSignSummary_unOwnedKeys b
--    , _quickSignSummary_totalKDA = _quickSignSummary_totalKDA  a + _quickSignSummary_totalKDA  b
--    , _quickSignSummary_gasSubTotal = _quickSignSummary_gasSubTotal  a + _quickSignSummary_gasSubTotal b
--    , _quickSignSummary_chainsSigned = _quickSignSummary_chainsSigned a <> _quickSignSummary_chainsSigned b
--    , _quickSignSummary_requestsCanSign = _quickSignSummary_requestsCanSign a + _quickSignSummary_requestsCanSign b
--    , _quickSignSummary_network = _quickSignSummary_network a <> _quickSignSummary_network b
--    }
--instance Monoid QuickSignSummary where
--  mappend = (<>)
--  mempty = def

--instance Default QuickSignSummary where
--  def = QuickSignSummary mempty mempty 0 0 mempty 0 [ NetworkId ""]

---- | Folds over the all payloads and produces a summary of the "important" data
--generateQuickSignSummary :: [Payload PublicMeta Text] -> [PublicKey] -> QuickSignSummary
--generateQuickSignSummary payloads controlledKeys = fold $ join $ ffor payloads $ \p ->
--  let
--    signers = catMaybes $ ffor (p^.pSigners) $ \s ->
--      case Set.member (signerPublicKey s) pubKeySet of
--        True  -> Just (pactPubKeyFromSigner s, s^.siCapList)
--        False -> Nothing
--  in ffor signers $ \(pk, capList) ->
--    let coinCap = parseCoinTransferCap capList
--        coinCapAmount = fromMaybe 0 $ fmap _fungibleTransferCap_amount coinCap
--        networkId' = fromMaybe (NetworkId "Unknown Netork") $ p^.pNetworkId
--        chain = p^.pMeta.pmChainId
--        GasLimit lim = p^.pMeta.pmGasLimit
--        GasPrice (ParsedDecimal price) = p^.pMeta.pmGasPrice
--        maxGasCost = fromIntegral lim * price
--    in QuickSignSummary
--      { _quickSignSummary_activeOwnedKeys = Set.singleton pk
--      , _quickSignSummary_unOwnedKeys = mempty
--      , _quickSignSummary_totalKDA = coinCapAmount
--      , _quickSignSummary_gasSubTotal = maxGasCost
--      , _quickSignSummary_chainsSigned = Set.singleton chain
--      , _quickSignSummary_requestsCanSign = 1
--      , _quickSignSummary_network = [networkId']
--      }
--  where
--    pubKeySet = Set.fromList controlledKeys
--    pactPubKeyFromSigner = Pact.PublicKey . T.encodeUtf8 . view siPubKey

---------------------------------------------------------------------------------

-- data FungibleTransferCap = FungibleTransferCap
--   { _fungibleTransferCap_amount   :: Decimal
--   , _fungibleTransferCap_sender   :: Text
--   , _fungibleTransferCap_receiver :: Text
--   , _fungibleTransferCap_name     :: Text
--   } deriving (Show, Eq)

-- -- | Extracts information from a fungible token's TRANSFER cap
-- parseFungibleTransferCap :: [SigCapability] -> ModuleName -> Maybe FungibleTransferCap
-- parseFungibleTransferCap caps _modName = foldl' (\acc cap -> acc <|> transferCap cap) Nothing caps
--   where
--     isFungible (QualifiedName _capModName capName _) =
--       -- modName == capModName &&
--       capName == "TRANSFER"
--     transferCap (SigCapability modName
--       [(PLiteral (LString sender))
--       ,(PLiteral (LString receiver))
--       ,(PLiteral (LDecimal amount))])
--         | isFungible modName = Just $ FungibleTransferCap amount sender receiver "coin"
--         | otherwise = Nothing
--     transferCap _ = Nothing

-- -- | Parses `coin.coin.TRANSFER` cap
-- parseCoinTransferCap :: [SigCapability] -> Maybe FungibleTransferCap
-- parseCoinTransferCap caps = parseFungibleTransferCap caps $
--   ModuleName "coin" Nothing -- -$ Just $ NamespaceName "coin"
