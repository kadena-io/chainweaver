{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Dialog presented for generating responses to signing API requests.
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.Signing
  ( uiSigning
  , uiQuickSign
  ) where

import Control.Lens
import Control.Monad ((<=<))
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString (ByteString)
import Data.Either
import qualified Data.Map.Strict as Map
import qualified Data.IntMap as IMap
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Kadena.SigningApi
import Pact.Parse
import Pact.Types.Pretty
import Pact.Types.ChainId
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Exp
import Pact.Types.RPC
import Reflex
import Reflex.Dom

import Frontend.Crypto.Class
import Frontend.Crypto.Ed25519 (fromPactPublicKey)
import Frontend.Foundation hiding (Arg)
import Frontend.JsonData
import Frontend.Network
import Frontend.UI.DeploymentSettings
import Frontend.UI.Modal.Impl
import Frontend.UI.TabBar
import Frontend.UI.Widgets
import Frontend.UI.Widgets.Helpers
import Frontend.Wallet

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
        Nothing -> uiAccountDropdown def (pure $ \_ _ -> True) (pure id)
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
  case partitionEithers $ map parsePactPayload $ _quickSignRequest_commands qsr of
    ([], payloads) -> quickSignModal ideL writeSigningResponse payloads onCloseExternal
    (es, _) -> do
      pb <- getPostBuild
      let res = Left $ "QuickSign request contained invalid commands:\n" <> T.unlines (map T.pack es)
      finished <- writeSigningResponse (res <$ pb)
      pure (mempty, finished)

parsePactPayload :: Text -> Either String (Payload PublicMeta ParsedCode)
parsePactPayload t = traverse parsePact =<< eitherDecodeStrict (encodeUtf8 t)

-- data QuickSignStrategy = 
--     QuickSignStrategy_Default
--   | QuickSignStrategy_MultipleTransactions

-- requiredSignatures :: Payload PublicMeta ParsedCode -> [Payload PublicMeta ParsedCode]
-- requiredSignatures payloads = foldr f mempty
--   where 
--     f :: Payload a b -> [ Signers ] ->_  = 
--     f elem myKeys = 
--       let
--         requiredSigners = 

quickSignModal
  :: forall key t m mConf model
  . ( MonadWidget t m
    , HasUISigningModelCfg mConf key t
    , HasCrypto key (Performable m)
    , HasWallet model key t -- So we can determine which signing-reqs are for our keys
    , HasTransactionLogger m
    )
  => 
  -- ModalIde m key t
  model ->
  (Event t (Either Text QuickSignResponse) -> m (Event t ()))
  -> [Payload PublicMeta ParsedCode]
  -> Event t ()
  -> m (mConf, Event t ())
quickSignModal ideL writeSigningResponse payloads onCloseExternal = do
  let 
    -- Use this to check with all signing reqs that come in and have a list of all sigs that we can
    -- make with the current accounts that we have
    publicKeys = ffor (ideL ^. wallet ^. wallet_keys) $ \keyStore-> 
      toPactPublicKey . _keyPair_publicKey . _key_pair <$> IMap.elems keyStore

  onClose <- modalHeader $ text "Quick Signing Request"
  modalMain $ do 
    (qsTab, _) <- quickSignTabs never
    dyn $ ffor qsTab $ \case
      QuickSignTab_Summary -> do
        singleTransactionDetails $ head payloads
      QuickSignTab_AllTransactions -> do
        dialogSectionHeading mempty "Signing Requests"

        -- _ <- elAttr "table" mempty $ do
          -- el "colgroup" $ do
          --   elAttr "col" ("style" =: "width: 30px") blank
          --   elAttr "col" ("style" =: "width: 80px") blank
          --   elAttr "col" ("style" =: "width: 180px") blank


        divClass "group payload__header" $ do
          divClass "payload__accordion-header" $ text ""
          divClass "payload__index-header" $ text "Request Number"
          divClass "payload__signer-header" $ text "Signer"

        -- reqSigs <- requiredSignatures payloads
        mapM_ txRow $ zip payloads [1..]
        pure ()

        -- divClass "group" $ do
        --   el "div" $ text $ (tshow $ length payloads) <> " payloads to sign"
        --   el "ul" $ do
        --     mapM (el "li" . text . tshow) payloads
  (cancel, sign) <- modalFooter $ do
    cancel <- cancelButton def "Cancel"
    sign <- confirmButton def "Sign Transaction"
    pure (cancel, sign)
  pure (mempty, leftmost [onClose, cancel])

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
  => Payload PublicMeta ParsedCode
  -> m ()
singleTransactionDetails p = do
  networkWidget p
  txMetaWidget $ _pMeta p
  pactRpcWidget (_pPayload p)
  signersWidget (_pSigners p)

   where
     networkWidget p = case _pNetworkId p of
       Nothing -> blank
       Just n -> do
         dialogSectionHeading mempty "Network"
         divClass "group" $ text $ _networkId n

--  { _pPayload :: !(PactRPC c)
--  , _pNonce :: !Text
--  , _pMeta :: !m
--  , _pSigners :: ![Signer]
--  , _pNetworkId :: !(Maybe NetworkId)

--  { _pmChainId :: !ChainId
--    -- ^ platform-specific chain identifier, e.g. "0"
--  , _pmSender :: !Text
--    -- ^ sender gas account key
--  , _pmGasLimit :: !GasLimit
--    -- ^ gas limit (maximum acceptable gas units for tx)
--  , _pmGasPrice :: !GasPrice
--    -- ^ per-unit gas price
--  , _pmTTL :: !TTLSeconds
--    -- ^ TTL in seconds
--  , _pmCreationTime :: !TxCreationTime
--    -- ^ Creation time in seconds since UNIX epoch

pactRpcWidget
  :: MonadWidget t m
  => PactRPC ParsedCode
  -> m ()
pactRpcWidget (Exec e) = do
  dialogSectionHeading mempty "Code"
  divClass "group" $ do
    el "code" $ text $ _pcCode $ _pmCode e

  case _pmData e of
    Null -> blank
    _ -> do
      dialogSectionHeading mempty "Data"
      divClass "group" $ do
        pb <- getPostBuild
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

 -- { _siScheme :: !(Maybe PPKScheme)
 -- -- ^ PPKScheme, which is defaulted to 'defPPKScheme' if not present
 -- , _siPubKey :: !Text
 -- -- ^ pub key value
 -- , _siAddress :: !(Maybe Text)
 -- -- ^ optional "address", for different pub key formats like ETH
 -- , _siCapList :: [SigCapability]

data QuickSignTab
  = QuickSignTab_Summary
  | QuickSignTab_AllTransactions
  deriving (Eq, Ord, Show, Enum, Bounded)

displayQuickSignTab :: DomBuilder t m => QuickSignTab -> m ()
displayQuickSignTab = text . \case
  QuickSignTab_Summary -> "Summary"
  QuickSignTab_AllTransactions -> "Advanced"

quickSignTabs
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Event t QuickSignTab
  -> m (Dynamic t QuickSignTab, Event t ())
quickSignTabs tabEv = do
  let f t0 g = case g t0 of
        Nothing -> (Just t0, Just ())
        Just t -> (Just t, Nothing)
  rec
    (curSelection, done) <- mapAccumMaybeDyn f QuickSignTab_AllTransactions $ leftmost
      [ const . Just <$> onTabClick
      , const . Just <$> tabEv
      ]
    (TabBar onTabClick) <- makeTabBar $ TabBarCfg
      -- { _tabBarCfg_tabs = [QuickSignTab_Summary, QuickSignTab_AllTransactions]
      { _tabBarCfg_tabs = [QuickSignTab_AllTransactions, QuickSignTab_Summary]
      , _tabBarCfg_mkLabel = \_ -> displayQuickSignTab
      , _tabBarCfg_selectedTab = Just <$> curSelection
      , _tabBarCfg_classes = mempty
      , _tabBarCfg_type = TabBarType_Secondary
      }
  pure (curSelection, done)
