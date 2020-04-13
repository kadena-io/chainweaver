{-|

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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.UI.Transfer where

import           Control.Error hiding (bool)
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Decimal
import           Data.Default (Default (..))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Kadena.SigningApi (AccountName(..))
import           Obelisk.Route (R)
import           Obelisk.Route.Frontend
import qualified Pact.Server.ApiClient as Api
import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Lang as Pact
import           Pact.Types.PactValue
import           Pact.Types.Pretty
import           Reflex
import           Reflex.Dom.Core
import           Text.Printf

import           Common.Foundation
import           Common.Route
import           Common.Wallet
import           Frontend.Crypto.Class
import           Frontend.Crypto.Ed25519
import           Frontend.Foundation
import           Frontend.JsonData
import           Frontend.Log
import           Frontend.Network
import           Frontend.PactQueries
import           Frontend.Storage
import           Frontend.UI.Button
import           Frontend.UI.DeploymentSettings
import           Frontend.UI.Dialogs.Send
import           Frontend.UI.KeysetWidget
import           Frontend.UI.Modal
import           Frontend.UI.TabBar
import           Frontend.UI.Widgets
import           Frontend.UI.Widgets.Helpers
import           Frontend.Wallet

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
    let mkModal (Just ti) ni = Just $ lookupAndTransfer model ni ti
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
    etks <- lookupKeySets (model ^. logger) (_sharedNetInfo_network netInfo)
                 nodes toChain accountNames
    allKeys <- foldDyn ($) (Nothing, Nothing) $ leftmost
      [ (\ks (_,b) -> (Just ks, b)) <$> efks
      , (\ks (a,_) -> (a, Just ks)) <$> etks
      ]
    let eWrapper (Just f, Just t) = do
          let fks = fromMaybe mempty f
          let tks = fromMaybe mempty t
          (conf, closes) <- fmap splitDynPure $ workflow $ signAndTransfer model netInfo ti fks tks
          mConf <- flatten =<< tagOnPostBuild conf
          let close = switch $ current closes
          pure (mConf, close)
        eWrapper (_, Nothing) = msgModal "Sign Transfer" $ text "Loading..."
        eWrapper (Nothing, _) = msgModal "Sign Transfer" $ text "Loading..."
    (conf, closes) <- splitDynPure <$> networkHold (msgModal "Sign Transfer" $ text "Querying sender keyset...")
                        (eWrapper <$> ffilter (\(a,b) -> isJust a && isJust b) (updated allKeys))
    mConf <- flatten =<< tagOnPostBuild conf
    return (mConf, switch $ current closes)
  where
    setify :: Ord a => [a] -> [a]
    setify = Set.toList . Set.fromList

msgModal :: (DomBuilder t m, PostBuild t m, Monoid mConf) => Text -> m a -> m (mConf, Event t ())
msgModal headerMsg body = do
  close <- modalHeader $ text headerMsg
  modalMain body
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
      Left es -> pure Nothing
      Right cr -> case Pact._crResult cr of
        Pact.PactResult (Right pv) -> case parseAccountDetails pv of
          Left e -> pure Nothing
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

signAndTransfer
  :: ( SendConstraints model mConf key t m
     )
  => model
  -> SharedNetInfo NodeInfo
  -> TransferInfo
  -> Map AccountName (AccountStatus AccountDetails)
  -> Map AccountName (AccountStatus AccountDetails)
  -> Workflow t m (mConf, Event t ())
signAndTransfer model netInfo ti fks tks = Workflow $ do
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
    nodes = _sharedNetInfo_nodes netInfo
    mainSection currentTab = elClass "div" "modal__main" $ do
      mconf <- tabPane mempty currentTab TransferTab_Metadata $ transferMetadata model fks tks ti
      tabPane mempty currentTab TransferTab_Signatures $ transferSigs fks tks
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


isMissingGasPayer
  :: Map ChainId (Map AccountName (AccountStatus AccountDetails))
  -> (ChainId, Maybe AccountName)
  -> Bool
isMissingGasPayer keysets (cid, Nothing) = False
isMissingGasPayer keysets (cid, Just a) = isNothing $ Map.lookup a =<< Map.lookup cid keysets

gasPayersSection
  :: (MonadWidget t m, HasNetwork model t)
  => model
  -> TransferInfo
  -> m (Dynamic t [(ChainId, Maybe AccountName)])
gasPayersSection model ti = do
    let fromChain = _ca_chain (_ti_fromAccount ti)
        toChain = _ca_chain (_ti_toAccount ti)
    if fromChain == toChain
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

signersSection
  :: (MonadWidget t m, HasNetwork model t)
  => model
  -> TransferInfo
  -> Map ChainId (Map AccountName (AccountStatus AccountDetails))
  -> [(ChainId, Maybe AccountName)]
--  -> ((ChainId, AccountName), Maybe (Map AccountName (AccountStatus AccountDetails)))
  -> m ()
signersSection model ti ks gps = do
    dialogSectionHeading mempty "Signers"
    let fromAccount = _ca_account $ _ti_fromAccount ti
        fromChain = _ca_chain $ _ti_fromAccount ti
        from = (fromChain, Just $ AccountName fromAccount)
        signers = if from `elem` gps then gps else (from : gps)

    mapM_ (singleSigner ks) signers

singleSigner
  :: (DomBuilder t m, PostBuild t m)
  => Map ChainId (Map AccountName (AccountStatus AccountDetails))
  -> (ChainId, Maybe AccountName)
  -> m ()
singleSigner _ (_,Nothing) = blank
singleSigner ks (c,Just a) = do
  case Map.lookup a =<< Map.lookup c ks of
    Nothing -> el "div" $ text $ "Couldn't find key for account " <> unAccountName a
    Just AccountStatus_DoesNotExist -> el "div" $ text $ "Account " <> unAccountName a <> " does not exist"
    Just (AccountStatus_Exists (AccountDetails bal guard)) -> do
      case guard of
        AccountGuard_Other _ -> el "div" $ text $ "Account " <> unAccountName a <> " guard is not a keyset"
        AccountGuard_KeySet keys pred -> do
          if pred == "keys-all" || length keys == 1 || (length keys == 2 && pred == "keys-2")
            then blank
            else do
              elClass "h3" ("heading heading_type_h3") $ text $ unAccountName a
              divClass "group signing-ui-signers" $ do
                forM (Set.toList keys) $ \key ->
                  uiCheckbox "signing-ui-signers__signer" False def $
                    text $ keyToText key
              return ()

transferMetadata
  :: (MonadWidget t m, HasNetwork model t, HasNetworkCfg mConf t, Monoid mConf)
  => model
  -> Map AccountName (AccountStatus AccountDetails)
  -> Map AccountName (AccountStatus AccountDetails)
  -> TransferInfo
  -> m mConf
transferMetadata model fks tks ti = do
  gps <- gasPayersSection model ti
  let fromAccount = _ca_account $ _ti_fromAccount ti
      fromChain = _ca_chain $ _ti_fromAccount ti
      toAccount = _ca_account $ _ti_toAccount ti
      toChain = _ca_chain $ _ti_toAccount ti
      ks = Map.fromList [(fromChain, fks), (toChain, tks)]

  -- TODO Get keysets for gas payers that we don't have yet
  networkView (signersSection model ti ks <$> gps)

  dialogSectionHeading mempty "Transaction Settings"
  (conf, _, _, _) <- divClass "group" $ uiMetaData model Nothing Nothing
  -- TODO Add creationTime to uiMetaData dialog and default it to current time - 60s
  return conf

transferSigs
  :: (MonadWidget t m)
  => Map AccountName (AccountStatus AccountDetails)
  -> Map AccountName (AccountStatus AccountDetails)
  -> m ()
transferSigs fks tks = do
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
    text "TODO transferSigs"
    --forM_ (Set.toList $ Pact._ksKeys ks) $ \pk -> do
    forM_ [Pact.PublicKey "abc123"] $ \pk -> do
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
