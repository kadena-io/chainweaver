{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ViewPatterns #-}
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

module Frontend.UI.Dialogs.Send.ManualTxBuilder
  ( uiManualTxBuilderInput
  , uiExplodedTxBuilder
  , recipientMatchesSenderTxBuilder
  ) where

import Control.Lens hiding (failover)
import Reflex
import Reflex.Dom

import Data.Text (Text)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson

import qualified Data.IntMap as IntMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import qualified Data.Text.Encoding as T

import Pact.Types.Runtime (ChainId)
import qualified Pact.Types.Pretty as Pact
import qualified Pact.Types.Term as Pact

import Common.Wallet
import Frontend.Foundation hiding (Arg)
import Frontend.TxBuilder
import Frontend.JsonData
import Frontend.Network
import Frontend.UI.Widgets
import Frontend.UI.Dialogs.AddVanityAccount.DefineKeyset (HasDefinedKeyset (..), DefinedKeyset (..), HasKeysetInputs (..), KeysetInputs (..), uiDefineKeyset, emptyKeysetPresets)
import Frontend.Wallet

recipientMatchesSenderTxBuilder
  :: (AccountName, ChainId)
  -> TxBuilder
  -> Bool
recipientMatchesSenderTxBuilder (fromName, fromChain) txb =
  _txBuilder_accountName txb == fromName &&
  _txBuilder_chainId txb == fromChain

cannotBeReceiverMsg, cannotInitiateNewXChainTfr :: Text
cannotBeReceiverMsg = "Sender cannot be the receiver of a transfer"
cannotInitiateNewXChainTfr = "Existing cross chain transfer in progress."

showManualTxBuilderPopover
  :: Maybe UnfinishedCrossChainTransfer
  -> (AccountName, ChainId)
  -> (AccountName, ChainId)
  -> PopoverState
showManualTxBuilderPopover mUcct (fromName, fromChain) (toName, toChain) =
  if fromName == toName && fromChain == toChain then
    PopoverState_Error cannotBeReceiverMsg
  else if toChain /= fromChain && isJust mUcct then
    PopoverState_Error cannotInitiateNewXChainTfr
  else
    PopoverState_Disabled

uiManualTxBuilderInput
  :: ( MonadWidget t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , ToJSON a
     )
  => Event t ()
  -> AccountName
  -> ChainId
  -> Maybe UnfinishedCrossChainTransfer
  -> Maybe a
  -> m ( TextAreaElement EventResult (DomBuilderSpace m) t
       , ( Event t (Either String TxBuilder)
         , Dynamic t (Either String TxBuilder)
         )
       )
uiManualTxBuilderInput onReset fromName fromChain mUcct mInitToAddress = do
  let
    renderTxBuilder = T.decodeUtf8 . LBS.toStrict . Aeson.encode
    validateTxBuilder = Aeson.eitherDecodeStrict . T.encodeUtf8

    showTxBuilderPopover (_, (onInput, _)) = pure $ ffor onInput $ \case
      Left _ -> PopoverState_Error "Invalid Tx Builder"
      Right txb -> showManualTxBuilderPopover mUcct (fromName, fromChain)
        ( _txBuilder_accountName txb
        , _txBuilder_chainId txb
        )

    txInput cfg = do
      ie <- uiTxBuilder Nothing cfg
      pure ( ie
           , ( validateTxBuilder <$> _textAreaElement_input ie
             , validateTxBuilder <$> value ie
             )
           )

  mkLabeledInput False "Tx Builder"
    (uiInputWithPopover txInput (_textAreaElement_raw . fst) showTxBuilderPopover) $ def
    & textAreaElementConfig_initialValue .~ (maybe "" renderTxBuilder mInitToAddress)
    & textAreaElementConfig_setValue .~ (mempty <$ onReset)

uiExplodedChainSelect
  :: forall t m model
     . ( MonadWidget t m
       , HasNetwork model t
       )
  => model
  -> Maybe UnfinishedCrossChainTransfer
  -> Event t (Maybe AccountName)
  -> Dynamic t (Maybe AccountName)
  -> AccountName
  -> ChainId
  -> Event t (Maybe ChainId)
  -> m ( Dropdown t (Maybe ChainId) )
uiExplodedChainSelect model mUcct onFromName dFromName fromName fromChain onTxChainId = do
  let
    chainSelect _ = elClass' "div" "segment_type_tertiary" $
      mkLabeledClsInput False "Chain ID" (uiChainSelectionWithUpdate (getChainsFromHomogenousNetwork model) onTxChainId)

    onNameChainUpdated (_, chainE) = leftmost
      [ (,) <$> current dFromName <@> _dropdown_change chainE
      , flip (,) <$> current (value chainE) <@> onFromName
      ]

    showPopover e = pure $ onNameChainUpdated e <&> \case
      (Just toName, Just toChain) ->
        showManualTxBuilderPopover mUcct (fromName, fromChain) (toName, toChain)
      _ ->
        PopoverState_Disabled

  fmap snd $ uiInputWithPopover chainSelect (_element_raw . fst) showPopover (def :: DropdownConfig t k)

uiExplodedTxBuilder
  :: forall t m model key
     . ( MonadWidget t m
       , HasWallet model key t
       , HasJsonData model t
       , HasNetwork model t
       )
  => model
  -> AccountName
  -> ChainId
  -> Maybe UnfinishedCrossChainTransfer
  -> Maybe TxBuilder
  -> m (Dynamic t (Maybe TxBuilder))
uiExplodedTxBuilder model fromName fromChain mUcct mInitToAddress = do
  let
    mkAlteredTxB mname mchain intKeys extKeys mPredicate = TxBuilder <$> mname <*> mchain
      <*> pure (fmap (\p -> toPactKeyset $ KeySetHeritage (intKeys <> extKeys) p Nothing) mPredicate)

    explodedTxB onTxAccountName onTxChainId keysetsPresets = do
      (onNameInput, dname) <- uiAccountNameInput "Account Name" False Nothing onTxAccountName noValidation

      chainE <- uiExplodedChainSelect model
        mUcct
        onNameInput
        dname
        fromName
        fromChain
        onTxChainId

      keyset <- fmap snd $ uiDefineKeyset model keysetsPresets

      let onKeysetChange = mconcat
            [ () <$ _keysetInputs_rowAddDelete (_definedKeyset_internalKeys keyset)
            , () <$ _keysetInputs_rowAddDelete (_definedKeyset_externalKeys keyset)
            , () <$ _keysetInputs_rowChange (_definedKeyset_internalKeys keyset)
            , () <$ _keysetInputs_rowChange (_definedKeyset_externalKeys keyset)
            , () <$ _definedKeyset_predicateChange keyset
            , () <$ onNameInput
            , () <$ _dropdown_change chainE
            ]

      pure $ (,) onKeysetChange $ mkAlteredTxB
        <$> dname
        <*> value chainE
        <*> _keysetInputs_set (_definedKeyset_internalKeys keyset)
        <*> _keysetInputs_set (_definedKeyset_externalKeys keyset)
        <*> _definedKeyset_predicate keyset

  rec
    onEitherTxB <- fmap (fst . snd) $
      uiManualTxBuilderInput onKeysetChange fromName fromChain mUcct mInitToAddress

    let
      onTxBAccountName = fmap (^? _Right . to _txBuilder_accountName) onEitherTxB
      onTxBChainId = fmap (^? _Right . to _txBuilder_chainId) onEitherTxB
      onTxBPredicate = fmap
        (^? _Right . to _txBuilder_keyset . _Just . to Pact._ksPredFun . to Pact.renderCompactText)
        onEitherTxB

      (onInternalKeys, onExternalKeys) = splitE $ attachWithMaybe
        (\keys -> either (const Nothing) (Just . mkKeysets keys))
        (current $ model ^. wallet_keys)
        onEitherTxB

    (onKeysetChange, dKeyset) <- explodedTxB onTxBAccountName onTxBChainId $ emptyKeysetPresets
      & definedKeyset_internalKeys . keysetInputs_rowAddDelete .~ onInternalKeys
      & definedKeyset_externalKeys . keysetInputs_rowAddDelete .~ onExternalKeys
      & definedKeyset_predicateChange .~ onTxBPredicate

  pure dKeyset

mkKeysets
  :: KeyStorage key
  -> TxBuilder
  -> (PatchIntMap (Maybe Int), PatchIntMap (Maybe Text))
mkKeysets keys tb =
  let
    tbKeys = Set.map fromPactPublicKey $ fold $ Pact._ksKeys <$> _txBuilder_keyset tb
    intKeys = ifoldMap
      (\i k -> let pk = _keyPair_publicKey $ _key_pair k
        in if pk `elem` tbKeys then Set.singleton (i,pk) else Set.empty
      ) keys

    extKeys = Set.filter
      (\k ->
         not (k `elem` Set.map snd intKeys) &&
         -- The FromJSON for Pact.PublicKey does not validate the text input.
         isJust ((textToKey $ keyToText k) :: Maybe PublicKey)
      )
      tbKeys

    toPatchIntMap :: (a -> b) -> Set.Set a -> PatchIntMap (Maybe b)
    toPatchIntMap f = PatchIntMap
      . ifoldMap (\n a -> IntMap.singleton n $ Just $ Just $ f a)
      . Set.toList
  in
    ( toPatchIntMap fst intKeys
    , toPatchIntMap keyToText extKeys
    )
