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
  , recipientMatchesSender
  ) where

import Control.Applicative (liftA2, (<|>))
import Control.Error (hush)
import Control.Lens hiding (failover)
import Reflex
import Reflex.Dom

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
import Frontend.UI.Dialogs.AddVanityAccount.DefineKeyset (DefinedKeyset (..), KeysetInputs (..), uiDefineKeyset, emptyKeysetPresets)
import Frontend.Wallet


recipientMatchesSender
  :: (AccountName, ChainId)
  -> TxBuilder
  -> Bool
recipientMatchesSender (fromName, fromChain) txb =
  _txBuilder_accountName txb == fromName &&
  _txBuilder_chainId txb == fromChain

uiManualTxBuilderInput
  :: ( MonadWidget t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , ToJSON a
     )
  => AccountName
  -> ChainId
  -> Maybe UnfinishedCrossChainTransfer
  -> Maybe a
  -> m ( TextAreaElement EventResult (DomBuilderSpace m) t
       , ( Event t (Either String TxBuilder)
         , Dynamic t (Either String TxBuilder)
         )
       )
uiManualTxBuilderInput fromName fromChain mUcct mInitToAddress = do
  let
    cannotBeReceiverMsg = "Sender cannot be the receiver of a transfer"
    cannotInitiateNewXChainTfr = "Existing cross chain transfer in progress."

    renderTxBuilder = T.decodeUtf8 . LBS.toStrict . Aeson.encode
    validateTxBuilder = Aeson.eitherDecodeStrict . T.encodeUtf8

    showTxBuilderPopover (_, (onInput, _)) = pure $ ffor onInput $ \case
      Left _ ->
        PopoverState_Error "Invalid Tx Builder"
      Right txb ->
        if recipientMatchesSender (fromName, fromChain) txb then
          PopoverState_Error cannotBeReceiverMsg
        else if _txBuilder_chainId txb /= fromChain && isJust mUcct then
          PopoverState_Error cannotInitiateNewXChainTfr
        else
          PopoverState_Disabled

    txInput cfg = do
      ie <- uiTxBuilder Nothing cfg
      pure ( ie
           , ( validateTxBuilder <$> _textAreaElement_input ie
             , validateTxBuilder <$> value ie
             )
           )

  mkLabeledInput False "Tx Builder"
    (uiInputWithPopover txInput (_textAreaElement_raw . fst) showTxBuilderPopover)
    $ def & textAreaElementConfig_initialValue .~ (maybe "" renderTxBuilder mInitToAddress)

uiExplodedTxBuilder
  :: ( MonadWidget t m
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
  (onEitherTxB, dEitherTxB) <- fmap snd $ uiManualTxBuilderInput fromName fromChain mUcct mInitToAddress

  let
    explodedTxB txAccName txChainId keysetsPresets = (,,)
      <$> uiAccountNameInput False txAccName noValidation
      <*> userChainIdSelectWithPreselect model False (constDyn txChainId)
      <*> fmap snd (uiDefineKeyset model keysetsPresets)

    onDefinedKeyset _    (Left _) = Nothing
    onDefinedKeyset keys (Right txb) = Just $ explodedTxB
      (Just $ _txBuilder_accountName txb)
      (Just $ _txBuilder_chainId txb)
      $ mkDefinedKeyset keys txb

  (mName, mChain, keySet) <- fmap fst $ runWithReplace (explodedTxB Nothing Nothing emptyKeysetPresets)
    $ attachWithMaybe onDefinedKeyset (current $ model ^. wallet_keys) onEitherTxB

  let
    mkAlteredTxB mname mchain intKeys extKeys mPredicate = TxBuilder <$> mname <*> mchain
      <*> pure (fmap (toPactKeyset $ intKeys <> extKeys) mPredicate)

    dAlteredTxBuilder = mkAlteredTxB <$> mName <*> mChain
      <*> _keysetInputs_set (_definedKeyset_internalKeys keySet)
      <*> _keysetInputs_set (_definedKeyset_externalKeys keySet)
      <*> _definedKeyset_predicate keySet

  pure $ liftA2 (<|>) dAlteredTxBuilder (hush <$> dEitherTxB)

mkDefinedKeyset
  :: Reflex t
  => KeyStorage key
  -> TxBuilder
  -> DefinedKeyset t
mkDefinedKeyset keys tb =
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
    DefinedKeyset
      { _definedKeyset_internalKeys = KeysetInputs
        { _keysetInputs_value = constDyn $ toPatchIntMap fst intKeys
        , _keysetInputs_set = constDyn (Set.map snd intKeys)
        }
      , _definedKeyset_externalKeys = KeysetInputs
        { _keysetInputs_value = constDyn $ toPatchIntMap keyToText extKeys
        , _keysetInputs_set = constDyn extKeys
        }
      , _definedKeyset_predicate = constDyn
        $ Pact.renderCompactText . Pact._ksPredFun <$> _txBuilder_keyset tb
      }
