{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.UI.Transfer where

import Control.Error hiding (bool)
import Control.Lens
import Control.Monad.State.Strict
import Data.Decimal
import Data.Default (Default (..))
import Data.Text (Text)
import Kadena.SigningApi (AccountName(..))
import Obelisk.Route (R)
import Obelisk.Route.Frontend
import qualified Pact.Types.Lang as Pact
import Reflex
import Reflex.Dom.Core
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Data.Text.Encoding as T

import Common.Foundation
import Common.Route
import Common.Wallet
import Frontend.Crypto.Class
import Frontend.Crypto.Ed25519
import Frontend.Foundation
import Frontend.JsonData
import Frontend.Log
import Frontend.Network
import Frontend.Storage
import Frontend.UI.Button
import Frontend.UI.DeploymentSettings
import Frontend.UI.Dialogs.Send
import Frontend.UI.KeysetWidget
import Frontend.UI.Modal
import Frontend.UI.TabBar
import Frontend.UI.Widgets
import Frontend.UI.Widgets.Helpers
import Frontend.Wallet

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
  -> m (Dynamic t (Maybe ChainAccount))
uiChainAccount model = do
  cd <- userChainIdSelect $ getChainsFromHomogenousNetwork model
  (_,a) <- uiAccountNameInput "Account Name" True Nothing never noValidation
  return $ runMaybeT $ ChainAccount <$> MaybeT (value cd) <*> MaybeT (unAccountName <$$> a)

data TransferInfo = TransferInfo
  { _ti_fromAccount :: ChainAccount
  , _ti_amount :: Decimal -- Possibly use ParsedDecimal
  , _ti_toAccount :: ChainAccount
  , _ti_toKeyset :: Maybe UserKeyset
  } deriving (Eq,Ord,Show)

-- -> RoutedT t (R FrontendRoute) m ()
uiGenericTransfer
  :: ( MonadWidget t m
     , RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m
     , HasConfigs m
     , HasStorage m, HasStorage (Performable m)
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
          else ("class" =: "main transfer")
  elDynAttr "main" attrs $ do
    transferInfo <- divClass "transfer-fields" $ do
      (fromAcct,amount) <- divClass "transfer__left-pane" $ do
        el "h4" $ text "From"
        --fca <- uiChainAccount model
        cd <- userChainIdSelect $ getChainsFromHomogenousNetwork model
        (_,a) <- uiAccountNameInput "Account Name" True Nothing never noValidation
        let fca = runMaybeT $ ChainAccount <$> MaybeT (value cd) <*> MaybeT (unAccountName <$$> a)

        amt <- amountButton
        return (fca,amt)
      (toAcct,ks) <- divClass "transfer__right-pane" $ do
        el "h4" $ text "To"
        tca <- uiChainAccount model
        k <- keysetWidget Nothing
--        performEvent_ (liftIO . print <$> updated k)
        return (tca,k)
      return $ runMaybeT $ TransferInfo <$>
        MaybeT fromAcct <*>
        MaybeT (hush <$> amount) <*>
        MaybeT toAcct <*>
        lift ks
    signTransfer <- divClass "transfer-fields submit" $ do
      confirmButton (def { _uiButtonCfg_disabled = (isNothing <$> transferInfo) }) "Sign & Transfer"
    let netInfo = flip push signTransfer $ \() -> sampleNetInfo model
    let mkModal (Just ti) ni = Just $ lookupAndTransfer model ti ni
        mkModal Nothing _ = Nothing
    pure $ mempty & modalCfg_setModal .~ (attachWith mkModal (current transferInfo) netInfo)

amountButton :: DomBuilder t m => m (Dynamic t (Either String Decimal))
amountButton =
 mkLabeledInput True "Amount" uiDecimalInputElement def

lookupAndTransfer
  :: ( SendConstraints model mConf key t m
     , Flattenable mConf t
     )
  => model
  -> TransferInfo
  -> SharedNetInfo NodeInfo
  -> Event t ()
  -> m (mConf, Event t ())
lookupAndTransfer model ti netInfo onCloseExternal = do
    let nodes = _sharedNetInfo_nodes netInfo
        fromAccount = _ca_account $ _ti_fromAccount ti
        fromChain = _ca_chain $ _ti_fromAccount ti
    eks <- lookupKeySet (model ^. logger) (_sharedNetInfo_network netInfo)
                 nodes fromChain (AccountName fromAccount)
    let eWrapper (Left m) = do
          text m
          return (mempty, never)
        eWrapper (Right ks) = uiTransferModal model ti ks onCloseExternal
    (conf, closes) <- splitDynPure <$> networkHold (text "Querying sender keyset..." >> return (mempty, never)) (eWrapper <$> eks)
    mConf <- flatten =<< tagOnPostBuild conf
    return (mConf, switch $ current closes)

uiTransferButton
  :: ( DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => m (Dynamic t Bool)
uiTransferButton = mdo
  let buttonText = bool "Show Transfer" "Hide Transfer" <$> isVisible
  click <- uiButton (def & uiButtonCfg_class <>~ " main-header__account-button") $ do
    dynText buttonText
  isVisible <- toggle False click
  return isVisible

-- | A modal for handling sending coin
uiTransferModal
  :: ( SendConstraints model mConf key t m
     , Flattenable mConf t
     )
  => model
  -> TransferInfo
  -> Pact.KeySet
  -> Event t ()
  -> m (mConf, Event t ())
uiTransferModal model ti ks _onCloseExternal = do

  (conf, closes) <- fmap splitDynPure $ workflow $ signAndTransfer model ti ks

  mConf <- flatten =<< tagOnPostBuild conf
  let close = switch $ current closes
  pure (mConf, close)

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

signAndTransfer
  :: ( SendConstraints model mConf key t m
     )
  => model
  -> TransferInfo
  -> Pact.KeySet
  -> Workflow t m (mConf, Event t ())
signAndTransfer model ti ks = Workflow $ do
    close <- modalHeader $ text "Sign Transfer"
    rec
      (currentTab, _done) <- transferTabs $ leftmost [prevTab, fmapMaybe id nextTab]
      conf <- mainSection currentTab
      (cancel, prevTab, nextTab) <- footerSection currentTab
    pure ( (conf, close <> cancel)
         , leftmost
           [ never
           ]
         )
  where
    mainSection currentTab = elClass "div" "modal__main" $ do
      mconf <- tabPane mempty currentTab TransferTab_Metadata $ transferMetadata model ti
      tabPane mempty currentTab TransferTab_Signatures $ transferSigs ks
      return mconf
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

transferMetadata
  :: (MonadWidget t m, HasNetwork model t, HasNetworkCfg mConf t, Monoid mConf)
  => model
  -> TransferInfo
  -> m mConf
transferMetadata model ti = do
  (_,gp) <- uiAccountNameInput "Gas Paying Account" True (Just $ AccountName $ _ca_account $ _ti_fromAccount ti) never noValidation
  dialogSectionHeading mempty "Transaction Settings"
  (conf, _, _, _) <- divClass "group" $ uiMetaData model Nothing Nothing
  -- TODO Add creationTime to uiMetaData dialog and default it to current time - 60s
  return conf

transferSigs
  :: (MonadWidget t m)
  => Pact.KeySet
  -> m ()
transferSigs ks = do
  divClass "group" $ do
    mkLabeledInput True "Request Key" uiInputElement $ def
      & initialAttributes .~ "disabled" =: "disabled"
      & inputElementConfig_initialValue .~ "aZVR2QgI2UAoPtk6q4w4HmaiC4E7CyLP93zjb0V0gG4="

  dialogSectionHeading mempty "Signatures"
  let pkWidget = text . T.decodeUtf8 . Pact._pubKey
  let externalSig pk = do
        pkWidget pk
        let optMap = Map.fromList $ map (\a -> (a, tshow a)) [Signature, PrivateKey]
        uiDropdown Signature (constDyn optMap) def
        uiPrivateKeyInput (fromPactPublicKey pk) Nothing
        pure ()
      internalSig pk = do
        _ <- fmap value $ uiCheckbox "signing-ui-signers__signer" False def $
          pkWidget pk
        pure ()
  divClass "group signing-ui-signers" $ do
    forM_ (S.toList $ Pact._ksKeys ks) $ \pk -> do
      externalSig pk
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
