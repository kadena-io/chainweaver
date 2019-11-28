{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Dialog for sending money between accounts
-- Copyright   :  (C) 2019 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.Send
  ( uiSendModal
  ) where

import Control.Error.Util (hush)
import Control.Lens
import Control.Monad (join, void, (<=<))
import Control.Monad.Trans.Except
import Data.Bifunctor (first)
import Data.Decimal (Decimal)
import Data.Either (isLeft, rights)
import Data.Text (Text)
import Kadena.SigningApi
import Pact.Types.Capability
import Pact.Types.ChainMeta
import Pact.Types.Exp
import Pact.Types.Names
import Pact.Types.PactValue
import Pact.Types.Term
import Reflex
import Reflex.Dom
import Safe (succMay, headMay)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T

import Common.Wallet
import Frontend.Crypto.Class (HasCrypto)
import Frontend.Foundation hiding (Arg)
import Frontend.KadenaAddress
import Frontend.Network
import Frontend.UI.DeploymentSettings
import Frontend.UI.Dialogs.DeployConfirmation (submitTransactionWithFeedback)
import Frontend.UI.Modal
import Frontend.UI.TabBar
import Frontend.UI.Widgets
import Frontend.Wallet

uiSendModal
  :: (SendConstraints model mConf key t m, Flattenable mConf t)
  => model -> Account key -> Event t () -> m (mConf, Event t ())
uiSendModal model sender _onCloseExternal = do
  (conf, closes) <- fmap splitDynPure $ workflow $ sendConfig model sender
  mConf <- flatten =<< tagOnPostBuild conf
  let close = switch $ current closes
  pure (mConf, close)

type SendConstraints model mConf key t m
  = (Monoid mConf, HasNetwork model t, HasNetworkCfg mConf t, HasWallet model key t, MonadWidget t m, PostBuild t m, HasCrypto key m)

sendPreview
  :: SendConstraints model mConf key t m
  => model -> Account key -> Account key -> KadenaAddress -> Decimal -> Workflow t m (mConf, Event t ())
sendPreview model sender gasPayer recipient amount = Workflow $ do
  close <- modalHeader $ text "Transaction Preview"
  elClass "div" "modal__main" $ do
    elClass "h2" "heading heading_type_h2" $ text "Destination"
    divClass "group" $ transactionDisplayNetwork model
    elClass "h2" "heading heading_type_h2" $ text "Participants"
    divClass "group" $ do
      mkLabeledInput True "Sender Address" uiInputElement $ def
        & initialAttributes .~ "disabled" =: "disabled"
        & inputElementConfig_initialValue .~ textKadenaAddress (accountToKadenaAddress sender)
      mkLabeledInput True "Gas Payer" uiInputElement $ def
        & initialAttributes .~ "disabled" =: "disabled"
        & inputElementConfig_initialValue .~ unAccountName (_account_name gasPayer)
      mkLabeledInput True "Recipient Address" uiInputElement $ def
        & initialAttributes .~ "disabled" =: "disabled"
        & inputElementConfig_initialValue .~ textKadenaAddress recipient
    elClass "h2" "heading heading_type_h2" $ text "Transaction Details"
    divClass "group" $ do
      mkLabeledInput True "Amount" (dimensionalInputWrapper "KDA" . uiInputElement) $ def
        & initialAttributes .~ "disabled" =: "disabled"
        & inputElementConfig_initialValue .~ tshow amount
      -- TODO The designs show gas fees here, but we can't get that information yet.
  (back, next) <- modalFooter $ do
    back <- cancelButton def "Back"
    next <- confirmButton def "Create Transaction"
    pure (back, next)
  let netInfo = do
        nodes <- current $ model ^. network_selectedNodes
        meta <- current $ model ^. network_meta
        let networkId = hush . mkNetworkName . nodeVersion <=< headMay $ rights nodes
        pure $ (nodes, meta, ) <$> networkId
      nextScreen = leftmost
        [ sendConfig model sender <$ back
        , attachWithMaybe (\ns _ -> sendDeploy model sender gasPayer recipient amount <$> ns) netInfo next
        ]
  pure ((mempty, close), nextScreen)

sendDeploy
  :: SendConstraints model mConf key t m
  => model
  -> Account key -- ^ From
  -> Account key -- ^ Gas payer
  -> KadenaAddress -- ^ To
  -> Decimal -- ^ Amount
  -> ([Either Text NodeInfo], PublicMeta, NetworkName) -- ^ Misc network information
  -> Workflow t m (mConf, Event t ())
sendDeploy _model sender gasPayer recipient amount (nodeInfos, publicMeta, networkId) = Workflow $ do
  let recipientCreated = _kadenaAddress_accountCreated recipient
  let code = T.unwords $
        [ "(coin." <> accountCreatedBool "transfer-create" "transfer" recipientCreated
        , tshow $ unAccountName $ _account_name sender
        , tshow $ unAccountName $ _kadenaAddress_accountName recipient
        , accountCreatedBool "(read-keyset 'key)" mempty recipientCreated
        , tshow amount
        , ")"
        ]
      signingPairs = L.nubBy (\x y -> _keyPair_publicKey x == _keyPair_publicKey y) [_account_key sender, _account_key gasPayer]
      transferCap = SigCapability
        { _scName = QualifiedName
          { _qnQual = "coin"
          , _qnName = "TRANSFER"
          , _qnInfo = def
          }
        , _scArgs =
          [ PLiteral $ LString $ unAccountName $ _account_name sender
          , PLiteral $ LString $ unAccountName $ _kadenaAddress_accountName recipient
          , PLiteral $ LDecimal amount
          ]
        }
      dat = case _kadenaAddress_accountCreated recipient of
        (AccountCreated False)
          | Right pk <- parsePublicKey (unAccountName $ _kadenaAddress_accountName recipient)
          -> HM.singleton "key" $ Aeson.toJSON $ KeySet [toPactPublicKey pk] (Name $ BareName "keys-all" def)
        _ -> mempty
      pkCaps = Map.unionsWith (<>)
        [ Map.singleton (_keyPair_publicKey $ _account_key gasPayer) [_dappCap_cap defaultGASCapability]
        , Map.singleton (_keyPair_publicKey $ _account_key sender) [transferCap]
        ]
      pm = publicMeta { _pmChainId = _account_chainId sender, _pmSender = unAccountName $ _account_name sender }
  close <- modalHeader $ text "Transaction Status"
  cmd <- buildCmd Nothing networkId pm signingPairs [] code dat pkCaps
  _ <- elClass "div" "modal__main transaction_details" $
    submitTransactionWithFeedback cmd (_account_chainId sender) nodeInfos
  done <- modalFooter $ confirmButton def "Done"
  pure
    ( (mempty, close <> done)
    , never
    )

sendConfig
  :: SendConstraints model mConf key t m
  => model -> Account key -> Workflow t m (mConf, Event t ())
sendConfig model sender = Workflow $ do
  close <- modalHeader $ text "Send"
  rec
    (currentTab, done) <- makeTabs $ attachWithMaybe (const . void . hush) (current recipient) next
    (conf, mCaps, recipient) <- mainSection currentTab
    (cancel, next) <- footerSection currentTab recipient mCaps
  let nextScreen = attachWithMaybe
        (\case (Just c, Right (r, a)) -> const $ Just $ sendPreview model sender c r a; _ -> const Nothing)
        ((,) <$> current mCaps <*> current recipient) done
  pure ((conf, close <> cancel), nextScreen)
  where
    mainSection currentTab = elClass "div" "modal__main" $ do
      (conf, recipient) <- tabPane mempty currentTab SendModalTab_Configuration $ do
        elClass "h2" "heading heading_type_h2" $ text "Destination"
        divClass "group" $ transactionDisplayNetwork model
        elClass "h2" "heading heading_type_h2" $ text "Recipient"
        recipient <- divClass "group" $ do
          kad <- mkLabeledInput True "Address" uiInputElement def
          let decoded = decodeKadenaAddressText <$> value kad
          (_, (amount, _)) <- mkLabeledInput True "Amount" (uiRealWithPrecisionInputElement maxCoinPrecision id) def
          pure $ runExceptT $ do
            r <- ExceptT $ first (\_ -> "Invalid kadena address") <$> decoded
            a <- ExceptT $ maybe (Left "Invalid amount") Right <$> amount
            pure (r, a)
        elClass "h2" "heading heading_type_h2" $ text "Transaction Settings"
        (conf, _, _) <- divClass "group" $ uiMetaData model Nothing Nothing
        pure (conf, recipient)
      mCaps <- tabPane mempty currentTab SendModalTab_Sign $ do
        fmap join . holdDyn (pure Nothing) <=< dyn $ ffor recipient $ \case
          Left e -> do
            divClass "group" $ text $ e <> ": please go back and check the recipient."
            pure $ pure Nothing
          Right _ -> do
            elClass "h2" "heading heading_type_h2" $ text "Gas Payer"
            divClass "group" $ elClass "div" "segment segment_type_tertiary labeled-input" $ do
              divClass "label labeled-input__label" $ text "Account Name"
              let cfg = def & dropdownConfig_attributes .~ pure ("class" =: "labeled-input__input")
                  chain = pure $ Just $ _account_chainId sender
              gasAcc <- uiSenderDropdown cfg never model chain
              let toAccount ma m = find ((ma ==) . Just . _account_name) $ fmapMaybe (\case SomeAccount_Account a -> Just a; _ -> Nothing) m
              pure $ toAccount <$> gasAcc <*> model ^. wallet_accounts
      pure (conf, mCaps, recipient)
    footerSection currentTab recipient mCaps = modalFooter $ do
      cancel <- cancelButton def "Cancel"
      let (name, disabled) = splitDynPure $ ffor currentTab $ \case
            SendModalTab_Configuration -> ("Next", fmap isLeft recipient)
            SendModalTab_Sign -> ("Preview", fmap isNothing mCaps)
      let cfg = def
            & uiButtonCfg_class <>~ "button_type_confirm"
            & uiButtonCfg_disabled .~ join disabled
      next <- uiButtonDyn cfg $ dynText name
      pure (cancel, next)

data SendModalTab
  = SendModalTab_Configuration
  | SendModalTab_Sign
  deriving (Eq, Ord, Show, Enum, Bounded)

displaySendModalTab :: DomBuilder t m => SendModalTab -> m ()
displaySendModalTab = text . \case
  SendModalTab_Configuration -> "Configuration"
  SendModalTab_Sign -> "Sign"

makeTabs
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Event t () -> m (Dynamic t SendModalTab, Event t ())
makeTabs next = do
  let initTab = SendModalTab_Configuration
      f t0 g = case g t0 of
        Nothing -> (Just t0, Just ())
        Just t -> (Just t, Nothing)
  rec
    (curSelection, done) <- mapAccumMaybeDyn f initTab $ leftmost
      [ const . Just <$> onTabClick
      , succMay <$ next
      ]
    (TabBar onTabClick) <- makeTabBar $ TabBarCfg
      { _tabBarCfg_tabs = [SendModalTab_Configuration, SendModalTab_Sign]
      , _tabBarCfg_mkLabel = \_ -> displaySendModalTab
      , _tabBarCfg_selectedTab = Just <$> curSelection
      , _tabBarCfg_classes = mempty
      , _tabBarCfg_type = TabBarType_Secondary
      }
  pure (curSelection, done)
