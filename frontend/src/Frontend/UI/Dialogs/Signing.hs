{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Dialog presented for generating responses to signing API requests.
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.Signing
  ( uiSigning
  , uiQuickSign
  , QuickSignSummary(..)
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
    , HasCrypto key (Performable m)
    , HasWallet model key t -- So we can determine which signing-reqs are for our keys
    , HasTransactionLogger m
    )
  => model -- ModalIde m key t
  -> (Event t (Either Text QuickSignResponse) -> m (Event t ()))
  -> QuickSignRequest
  -> Event t ()
  -> m (mConf, Event t ())
uiQuickSign ideL writeSigningResponse qsr onCloseExternal = do
  case partitionEithers $ map (eitherDecodeStrict . encodeUtf8) $ _quickSignRequest_commands qsr of
    ([], payloads) -> quickSignModal ideL writeSigningResponse payloads onCloseExternal
    (es, _) -> do
      pb <- getPostBuild
      let res = Left $ "QuickSign request contained invalid commands:\n" <> T.unlines (map T.pack es)
      finished <- writeSigningResponse (res <$ pb)
      pure (mempty, finished)

-- |Matches ownedKeys against signers required by pact cmd; indexes them by public key
keysRequiredForPayload :: Map PublicKey (KeyPair key) -> Payload PublicMeta a -> [(PublicKey, KeyPair key)]
keysRequiredForPayload ownedKeys p = foldr foldRequiredKeys [] (p^.pSigners)
  where
    foldRequiredKeys payloadKey acc = let key = signerPublicKey payloadKey in
      maybe acc (\kp -> (key, kp):acc) $ Map.lookup key ownedKeys

signerPublicKey :: Signer -> PublicKey
signerPublicKey = fromPactPublicKey . Pact.PublicKey . T.encodeUtf8 . view siPubKey

quickSignModal
  :: forall key t m mConf model
  . ( MonadWidget t m
    , HasUISigningModelCfg mConf key t
    , HasCrypto key (Performable m)
    , HasWallet model key t
    , HasTransactionLogger m
    )
  => model
  -> (Event t (Either Text QuickSignResponse) -> m (Event t ()))
  -> [Payload PublicMeta Text]
  -> Event t ()
  -> m (mConf, Event t ())
quickSignModal ideL writeSigningResponse payloads _onCloseExternal = do
  -- key handling
  cwKeysets :: Map PublicKey (KeyPair key) <- sample $ current $
    fmap getKeyPairsFromStore $ ideL^.wallet.wallet_keys

  -- Modal events/ui
  onClose <- modalHeader $ text "Quick Signing Request"
  modalMain $ do
    qsTabDyn <- fst <$> quickSignTabs never
    dyn_ $ ffor qsTabDyn $ \case
      QuickSignTab_Summary -> summarizeTransactions payloads $ Map.keys cwKeysets
      QuickSignTab_AllTransactions -> quickSignTransactionDetails payloads
    pure ()
  -- TODO: Add a failure mode based on initial parse (reject multi-network, possible others)
  (reject, sign) <- modalFooter $ (,)
    <$> cancelButton def "Reject"
    <*> confirmButton def "Sign Transaction"

  -- QuickSignResponse processing
  let prepareDataForSignatures :: [(Payload PublicMeta Text, [(PublicKey, KeyPair key)])]
      prepareDataForSignatures = fmap (id &&& (keysRequiredForPayload cwKeysets)) payloads
  quickSignRes <- performEvent $ ffor sign $ \_-> do
    userSigs <- forM prepareDataForSignatures $ \(payload, keyPairList) ->
      buildSigDataWithPayload payload $ snd <$> keyPairList
    -- TODO - more helpful error msg
    pure $ bimap (const "Error signing command") QuickSignResponse $ sequence userSigs
  let noResponseEv = ffor (leftmost [reject, onClose]) $ const $ Left "QuickSign response rejected"
  finished <- writeSigningResponse $ leftmost [ noResponseEv, quickSignRes ]
  -- TODO: BUG: Left/Failure on out of focus (but not when "X" is pushed)
  pure (mempty, finished)
   where
     getKeyPairsFromStore keyStore =
       let keysets = IMap.elems keyStore
           toIndexablePair = (_keyPair_publicKey &&& id) . _key_pair
       in Map.fromList $ fmap toIndexablePair keysets

-- |Summary view for quicksign ui
summarizeTransactions
  :: MonadWidget t m
  => [Payload PublicMeta Text]
  -> [ PublicKey ]
  -> m ()
summarizeTransactions payloads cwKeys = do
  let summary = generateQuickSignSummary payloads cwKeys
  let amount = _quickSignSummary_totalKDA summary
      maxGasCost = _quickSignSummary_gasSubTotal summary
  divClass "amount" $ text $ "Amount: " <> tshow amount <> " KDA"
  divClass "gas" $ text $ "Max Gas Cost: " <> tshow maxGasCost <> " KDA"
  divClass "chain" $ case Set.elems (_quickSignSummary_chainsSigned summary) of
    [chain] -> text $ "Chain: " <>  tshow  chain
    chains  -> text $ "Transactions conducted on chains: " <> tshow chains
  divClass "caps" $ do
    el "div" $ text $ "Extra Caps: " <> "Not currently supported"
  divClass "rejected-txns" $ do
    text $ "Number of requests that cannot be signed for: " <> tshow (length payloads - _quickSignSummary_requestsCanSign summary)
  pure ()

-- | Details page for quicksign transactions
quickSignTransactionDetails :: MonadWidget t m => [ Payload PublicMeta Text ] -> m ()
quickSignTransactionDetails payloads = do
  dialogSectionHeading mempty "Signing Requests"
  divClass "group payload__header" $ do
    divClass "payload__accordion-header" $ text ""
    divClass "payload__index-header" $ text "Request Number"
    divClass "payload__signer-header" $ text "Signer"
  mapM_ txRow $ zip payloads [1..]
  where
    txRow (p, i) = do
      divClass "group" $ do
        visible <- divClass "payload__row" $ do
          let accordionCell o = (if o then "" else "accordion-collapsed ") <> "payload__accordion "
          rec
            clk <- elDynClass "div" (accordionCell <$> visible') $ accordionButton def
            visible' <- toggle False clk
          divClass "payload__tx-index" $ text $ tshow i <> "."
          divClass "payload__signer" $ text $ tshow $ _siPubKey $ head $ _pSigners p
          pure visible'
        elDynAttr "div" (ffor visible $ bool ("hidden"=:mempty) mempty)$ singleTransactionDetails p

singleTransactionDetails
  :: MonadWidget t m
  => Payload PublicMeta Text
  -> m ()
singleTransactionDetails p = do
  networkWidget p
  txMetaWidget $ _pMeta p
  pactRpcWidget (_pPayload p)
  signersWidget (_pSigners p)
   where
     networkWidget p' = case _pNetworkId p' of
       Nothing -> blank
       Just n -> do
         dialogSectionHeading mempty "Network"
         divClass "group" $ text $ _networkId n

----------------------------------------------------------------------------
-- | TODO:
--   - Taking out network, which should just fail on an initial parse
--   - Making certain factors Dynamics so that the user can add sigs/priv keys and update the
--     summary
--   - Deriving requests_ignored instead of making it a first-class field
--
data QuickSignSummary = QuickSignSummary
  { _quickSignSummary_activeOwnedKeys :: Set Pact.PublicKey -- Keys CW can automatically sign with
  , _quickSignSummary_unOwnedKeys     :: Set Pact.PublicKey  -- Keys that aren't controlled by cw, but can still be signed with
  , _quickSignSummary_totalKDA        :: Decimal
  , _quickSignSummary_gasSubTotal     :: Decimal
  , _quickSignSummary_chainsSigned    :: Set ChainId
  , _quickSignSummary_requestsCanSign :: Int
  , _quickSignSummary_network         :: [ NetworkId ]
  } deriving (Show, Eq)

--TODO: Is a semigroup with (+) on decimals appropriate?
instance Semigroup QuickSignSummary where
  (<>) a b =
    QuickSignSummary
    { _quickSignSummary_activeOwnedKeys = _quickSignSummary_activeOwnedKeys a <> _quickSignSummary_activeOwnedKeys b
    , _quickSignSummary_unOwnedKeys = _quickSignSummary_unOwnedKeys a <> _quickSignSummary_unOwnedKeys b
    , _quickSignSummary_totalKDA = _quickSignSummary_totalKDA  a + _quickSignSummary_totalKDA  b
    , _quickSignSummary_gasSubTotal = _quickSignSummary_gasSubTotal  a + _quickSignSummary_gasSubTotal b
    , _quickSignSummary_chainsSigned = _quickSignSummary_chainsSigned a <> _quickSignSummary_chainsSigned b
    , _quickSignSummary_requestsCanSign = _quickSignSummary_requestsCanSign a + _quickSignSummary_requestsCanSign b
    , _quickSignSummary_network = _quickSignSummary_network a <> _quickSignSummary_network b
    }
instance Monoid QuickSignSummary where
  mappend = (<>)
  mempty = def

instance Default QuickSignSummary where
  def = QuickSignSummary mempty mempty 0 0 mempty 0 [ NetworkId ""]

-- | Folds over the all payloads and produces a summary of the "important" data
generateQuickSignSummary :: [Payload PublicMeta Text] -> [PublicKey] -> QuickSignSummary
generateQuickSignSummary payloads controlledKeys = fold $ join $ ffor payloads $ \p ->
  let
    signers = catMaybes $ ffor (p^.pSigners) $ \s ->
      case Set.member (signerPublicKey s) pubKeySet of
        True  -> Just (pactPubKeyFromSigner s, s^.siCapList)
        False -> Nothing
  in ffor signers $ \(pk, capList) ->
    let coinCap = parseCoinTransferCap capList
        coinCapAmount = fromMaybe 0 $ fmap _fungibleTransferCap_amount coinCap
        networkId' = fromMaybe (NetworkId "Unknown Netork") $ p^.pNetworkId
        chain = p^.pMeta.pmChainId
        GasLimit lim = p^.pMeta.pmGasLimit
        GasPrice (ParsedDecimal price) = p^.pMeta.pmGasPrice
        maxGasCost = fromIntegral lim * price
    in QuickSignSummary
      { _quickSignSummary_activeOwnedKeys = Set.singleton pk
      , _quickSignSummary_unOwnedKeys = mempty
      , _quickSignSummary_totalKDA = coinCapAmount
      , _quickSignSummary_gasSubTotal = maxGasCost
      , _quickSignSummary_chainsSigned = Set.singleton chain
      , _quickSignSummary_requestsCanSign = 1
      , _quickSignSummary_network = [networkId']
      }
  where
    pubKeySet = Set.fromList controlledKeys
    pactPubKeyFromSigner = Pact.PublicKey . T.encodeUtf8 . view siPubKey

---------------------------------------------------------------------------------

data FungibleTransferCap = FungibleTransferCap
  { _fungibleTransferCap_amount   :: Decimal
  , _fungibleTransferCap_sender   :: Text
  , _fungibleTransferCap_receiver :: Text
  , _fungibleTransferCap_name     :: Text
  } deriving (Show, Eq)

-- | Extracts information from a fungible token's TRANSFER cap
parseFungibleTransferCap :: [SigCapability] -> ModuleName -> Maybe FungibleTransferCap
parseFungibleTransferCap caps _modName = foldl' (\acc cap -> acc <|> transferCap cap) Nothing caps
  where
    isFungible (QualifiedName _capModName capName _) =
      -- modName == capModName &&
      capName == "TRANSFER"
    transferCap (SigCapability modName
      [(PLiteral (LString sender))
      ,(PLiteral (LString receiver))
      ,(PLiteral (LDecimal amount))])
        | isFungible modName = Just $ FungibleTransferCap amount sender receiver "coin"
        | otherwise = Nothing
    transferCap _ = Nothing

-- | Parses `coin.coin.TRANSFER` cap
parseCoinTransferCap :: [SigCapability] -> Maybe FungibleTransferCap
parseCoinTransferCap caps = parseFungibleTransferCap caps $
  ModuleName "coin" Nothing -- -$ Just $ NamespaceName "coin"

---------------------------------------------------------------------------------
pactRpcWidget
  :: MonadWidget t m
  => PactRPC Text
  -> m ()
pactRpcWidget (Exec e) = do
  dialogSectionHeading mempty "Code"
  divClass "group" $ do
    el "code" $ text $ _pmCode e

  case _pmData e of
    Null -> blank
    _ -> do
      dialogSectionHeading mempty "Data"
      divClass "group" $ do
        uiTextAreaElement $ def
          & textAreaElementConfig_initialValue .~ (decodeUtf8 $ BSL.toStrict $ encode $ _pmData e)
          & initialAttributes .~ "disabled" =: "" <> "style" =: "width: 100%"
        pure ()
pactRpcWidget (Continuation c) = do
  dialogSectionHeading mempty "Continuation Data"
  divClass "group" $ do
    mkLabeledClsInput True "Pact ID" $ \_ -> text $ tshow $ _cmPactId c
    mkLabeledClsInput True "Step" $ \_ -> text $ tshow $ _cmStep c
    mkLabeledClsInput True "Rollback" $ \_ -> text $ tshow $ _cmRollback c
    mkLabeledClsInput True "Data" $ \_ -> text $ tshow $ _cmData c
    mkLabeledClsInput True "Proof" $ \_ -> text $ tshow $ _cmProof c

txMetaWidget
  :: MonadWidget t m
  => PublicMeta
  -> m ()
txMetaWidget pm = do
  dialogSectionHeading mempty "Transaction Metadata"
  _ <- divClass "group segment" $ do
    mkLabeledClsInput True "Chain" $ \_ -> text (renderCompactText $ _pmChainId pm)
    mkLabeledClsInput True "Gas Payer" $ \_ -> text (_pmSender pm)
    --mkLabeledClsInput True "Gas Price" $ \_ -> text (renderCompactText $ _pmGasPrice pm)
    --mkLabeledClsInput True "Gas Limit" $ \_ -> text (renderCompactText $ _pmGasLimit pm)
    let totalGas = fromIntegral (_pmGasLimit pm) * _pmGasPrice pm
    mkLabeledClsInput True "Max Gas Cost" $ \_ -> text $ renderCompactText totalGas <> " KDA"
  pure ()

signersWidget
  :: MonadWidget t m
  => [Signer]
  -> m ()
signersWidget ss = do
  dialogSectionHeading mempty "Signers"
  divClass "group segment" $ do
    mapM_ signerWidget ss

signerWidget
  :: MonadWidget t m
  => Signer
  -> m ()
signerWidget s = do
  el "div" $ do
    maybe blank (text . (<> ":") . tshow) $ _siScheme s
    text $ _siPubKey s
    maybe blank (text . (":" <>) . tshow) $ _siAddress s
  elAttr "ul" ("style" =: "margin-block-start: 0; margin-block-end: 0;") $
    mapM_ (elAttr "li" ("style" =: "list-style-type: none;") . text . renderCompactText) $ _siCapList s
  pure ()

data QuickSignTab
  = QuickSignTab_Summary
  | QuickSignTab_AllTransactions
  deriving (Eq, Ord, Show, Enum, Bounded)

displayQuickSignTab :: DomBuilder t m => QuickSignTab -> m ()
displayQuickSignTab = text . \case
  QuickSignTab_Summary         -> "Summary"
  QuickSignTab_AllTransactions -> "Advanced"

quickSignTabs
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Event t QuickSignTab
  -> m (Dynamic t QuickSignTab, Event t ())
quickSignTabs tabEv = do
  let f t0 g = case g t0 of
        Nothing -> (Just t0, Just ())
        Just t  -> (Just t, Nothing)
  rec
    (curSelection, done) <- mapAccumMaybeDyn f QuickSignTab_Summary $ leftmost
      [ const . Just <$> onTabClick
      , const . Just <$> tabEv
      ]
    (TabBar onTabClick) <- makeTabBar $ TabBarCfg
      { _tabBarCfg_tabs = [QuickSignTab_Summary, QuickSignTab_AllTransactions]
      -- { _tabBarCfg_tabs = [QuickSignTab_AllTransactions, QuickSignTab_Summary]
      , _tabBarCfg_mkLabel = \_ -> displayQuickSignTab
      , _tabBarCfg_selectedTab = Just <$> curSelection
      , _tabBarCfg_classes = mempty
      , _tabBarCfg_type = TabBarType_Secondary
      }
  pure (curSelection, done)
