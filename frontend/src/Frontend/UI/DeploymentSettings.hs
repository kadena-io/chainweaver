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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Little widget providing a UI for deployment related settings.
--
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)

module Frontend.UI.DeploymentSettings
  ( -- * Settings
    DeploymentSettingsConfig (..)
  , DeploymentSettingsResult (..)

    -- * Helpers
  , buildDeploymentSettingsResult
  , defaultGASCapability

    -- * Values for _deploymentSettingsConfig_chainId:
  , predefinedChainIdSelect
  , predefinedChainIdDisplayed
  , userChainIdSelect
  , userChainIdSelectWithPreselect
  , uiChainSelection

    -- * Tab Helpers
  , DeploymentSettingsView (..)
  , showSettingsTabName
  , prevView
  , nextView
  , buildDeployTabFooterControls
  , buildDeployTabs
  , defaultTabViewProgressButtonLabel

    -- * Widgets
  , uiDeploymentSettings
  , uiDeployDestination
  , uiDeployMetaData
  , uiCfg
  , uiSenderCapabilities
  , uiMetaData
  , uiDeployPreview

  , uiSenderFixed
  , uiSenderDropdown

  , transactionInputSection
  , transactionHashSection
  , transactionDisplayNetwork
    -- * Useful re-exports
  , Identity (runIdentity)
  ) where

import Control.Applicative ((<|>), empty)
import Control.Arrow (first, (&&&))
import Control.Error (fmapL, hoistMaybe, headMay)
import Control.Error.Util (hush)
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Data.Decimal (roundTo)
import Data.Either (rights, isLeft)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.These (These(This))
import Data.Traversable (for, sequence)
import Kadena.SigningApi
import Pact.Compile (compileExps, mkTextInfo)
import Pact.Parse
import Pact.Types.Capability
import Pact.Types.ChainMeta (PublicMeta (..), TTLSeconds (..))
import Pact.Types.PactValue (toPactValue)
import Pact.Types.Pretty
import Pact.Types.Runtime (App(..), GasLimit (..), GasPrice (..), Name(..), Term(..), hashToText, toUntypedHash)
import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.CssClass (elKlass)
import Safe (readMay)
import qualified Data.Aeson as Aeson
import qualified Data.IntMap as IM
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Pact.Types.Capability as PC
import qualified Pact.Types.ChainId as Pact
import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Info as PI
import qualified Pact.Types.Names as PN

import Common.Network
import Common.Wallet
import Frontend.Crypto.Class
import Frontend.Crypto.Ed25519 (keyToText)
import Frontend.Foundation
import Frontend.JsonData
import Frontend.Network
import Frontend.UI.Dialogs.NetworkEdit
import Frontend.UI.JsonData
import Frontend.UI.Modal
import Frontend.UI.TabBar
import Frontend.UI.Widgets
import Frontend.UI.Widgets.Helpers (preventScrollWheelAndUpDownArrow)
import Frontend.Wallet

-- | Config for the deployment settings widget.
data DeploymentSettingsConfig t m model a = DeploymentSettingsConfig
  { _deploymentSettingsConfig_userTab     :: Maybe (Text, m a)
    -- ^ Some optional extra tab. fst is the tab's name, snd is its content.
  , _deploymentSettingsConfig_userSections :: [(Text, m a)]
    -- ^ Some optional extra input, placed between chainId selection and transaction
    -- settings. fst is the section's name, snd is its content.
  , _deploymentSettingsConfig_chainId     :: model -> m (Dynamic t (Maybe Pact.ChainId))
    -- ^ ChainId selection widget.
    --   You can pick (predefinedChainIdSelect someId) - for not showing a
    --   widget at all, but having `uiDeploymentSettings` use the provided one.
    --
    --   Or you can use `userChainIdSelect` for having the user pick a chainid.
  , _deploymentSettingsConfig_sender :: model -> Dynamic t (Maybe ChainId)  -> m (Dynamic t (Maybe AccountName))
    -- ^ Sender selection widget. Use 'uiSenderFixed' or 'uiSenderDropdown'.
    -- Note that uiSenderDropdown has a setSender event, but because this UI only "Apply to All"s on the non-GAS capabilities
    -- this event is not exposed here. Just pass in never to get a function compatible with here.
  , _deploymentSettingsConfig_data        :: Maybe Aeson.Object
    -- ^ Data selection. If 'Nothing', uses the users setting (and allows them
    -- to alter it). Otherwise, it remains fixed.
  , _deploymentSettingsConfig_code :: Dynamic t Text
    -- ^ Code that is being deployed
  , _deploymentSettingsConfig_nonce :: Maybe Text
    -- ^ Nonce. 'Nothing' will autogenerate a nonce based on the current time.
  , _deploymentSettingsConfig_ttl :: Maybe TTLSeconds
    -- ^ TTL. Overridable by the user.
  , _deploymentSettingsConfig_gasLimit :: Maybe GasLimit
    -- ^ Gas Limit. Overridable by the user.
  , _deploymentSettingsConfig_caps :: Maybe [DappCap]
    -- ^ Capabilities. When missing, lets the user enter arbitrary capabilities.
  , _deploymentSettingsConfig_extraSigners :: [PublicKey]
    -- ^ Extra signers to be added to the command. The dApp should fill in the
    -- signatures as required upon receiving the response.
  , _deploymentSettingsConfig_includePreviewTab :: Bool
    -- ^ Whether or not to show the preview tab on the deployment dialog.
  }

data CapabilityInputRow t = CapabilityInputRow
  { _capabilityInputRow_empty :: Dynamic t Bool
  , _capabilityInputRow_value :: Dynamic t (Map AccountName [SigCapability])
  , _capabilityInputRow_account :: Dynamic t (Maybe AccountName)
  , _capabilityInputRow_cap :: Dynamic t (Either String SigCapability)
  }

data DeploymentSettingsView
  = DeploymentSettingsView_Custom Text -- ^ An optional additonal tab.
  | DeploymentSettingsView_Cfg -- ^ Actual settings like gas price/limit, ...
  | DeploymentSettingsView_Keys -- ^ Select keys for signing the transaction.
  | DeploymentSettingsView_Preview -- ^ Attempt to preview this deployments affect on the chain
  deriving (Eq,Ord)

showSettingsTabName :: DeploymentSettingsView -> Text
showSettingsTabName (DeploymentSettingsView_Custom n) = n
showSettingsTabName DeploymentSettingsView_Keys       = "Sign"
showSettingsTabName DeploymentSettingsView_Preview    = "Preview"
showSettingsTabName DeploymentSettingsView_Cfg        = "Configuration"

-- | Get the previous view, taking into account the custom user tab.
prevView :: Maybe DeploymentSettingsView -> DeploymentSettingsView -> Maybe DeploymentSettingsView
prevView custom = \case
  DeploymentSettingsView_Custom _ -> Nothing
  DeploymentSettingsView_Cfg -> custom
  DeploymentSettingsView_Keys -> Just DeploymentSettingsView_Cfg
  DeploymentSettingsView_Preview -> Just DeploymentSettingsView_Keys

-- | Get the next view.
nextView :: Bool -> DeploymentSettingsView -> Maybe DeploymentSettingsView
nextView includePreviewTab = \case
  DeploymentSettingsView_Custom _ -> Just DeploymentSettingsView_Cfg
  DeploymentSettingsView_Cfg -> Just DeploymentSettingsView_Keys
  DeploymentSettingsView_Keys | includePreviewTab -> Just DeploymentSettingsView_Preview
                              | otherwise -> Nothing
  DeploymentSettingsView_Preview -> Nothing

data DeploymentSettingsResult key = DeploymentSettingsResult
  { _deploymentSettingsResult_gasPrice :: GasPrice
  , _deploymentSettingsResult_signingKeys :: [KeyPair key]
  , _deploymentSettingsResult_signingAccounts :: Set AccountName
  , _deploymentSettingsResult_sender :: AccountName
  , _deploymentSettingsResult_chainId :: ChainId
  , _deploymentSettingsResult_code :: Text
  , _deploymentSettingsResult_command :: Pact.Command Text
  , _deploymentSettingsResult_wrappedCommand :: Either String (Pact.Command Text)
  -- ^ This differs from 'command' because this wraps the code with balance
  -- checks for a /local request. This should never be actually deployed.
  , _deploymentSettingsResult_accountsToTrack :: Set AccountName
  }

publicKeysForAccounts :: IntMap (SomeAccount key) -> Map AccountName a -> Map PublicKey a
publicKeysForAccounts allAccounts caps =
  let accountsToKey = flip IM.foldMapWithKey allAccounts $ \_ -> \case
        SomeAccount_Account a -> Map.singleton (_account_name a) (_account_key a)
        SomeAccount_Deleted -> Map.empty
      toPublicKey (name, cs) = do
        KeyPair pk _ <- Map.lookup name accountsToKey
        pure (pk, cs)
  in
    Map.fromList $ fmapMaybe toPublicKey $ Map.toList caps

lookupAccountByName :: ChainId -> AccountName -> Accounts key -> Maybe (Account key)
lookupAccountByName c n = fmap getFirst . foldMap f
  where f = \case
          SomeAccount_Account a | _account_name a == n, _account_chainId a == c -> Just $ First a
          _ -> Nothing

buildDeploymentSettingsResult
  :: ( HasNetwork model t
     , HasJsonData model t
     , HasWallet model key t
     , Monad (Dynamic t)
     , Monad (Performable m)
     , MonadJSM (Performable m)
     , HasCrypto key (Performable m)
     )
  => model
  -> Dynamic t (Maybe AccountName)
  -> Dynamic t (Set AccountName)
  -> Dynamic t (Maybe ChainId)
  -> Dynamic t (Map AccountName [SigCapability])
  -> Dynamic t TTLSeconds
  -> Dynamic t GasLimit
  -> Dynamic t Text
  -> DeploymentSettingsConfig t m model a
  -> Dynamic t (Maybe (Performable m (DeploymentSettingsResult key)))
buildDeploymentSettingsResult m mSender signers cChainId capabilities ttl gasLimit code settings = runMaybeT $ do
  selNodes <- lift $ m ^. network_selectedNodes
  networkId <- hoistMaybe $ hush . mkNetworkName . nodeVersion =<< headMay (rights selNodes)
  sender <- MaybeT mSender
  chainId <- MaybeT cChainId
  caps <- lift capabilities
  signs <- lift signers
  let signing = signs <> (Set.insert sender $ Map.keysSet caps)
      deploySettingsJsonData = fromMaybe mempty $ _deploymentSettingsConfig_data settings
  jsonData' <- lift $ either (const mempty) id <$> m ^. jsonData . jsonData_data
  ttl' <- lift ttl
  limit <- lift gasLimit
  lastPublicMeta <- lift $ m ^. network_meta
  let publicMeta = lastPublicMeta
        { _pmChainId = chainId
        , _pmGasLimit = limit
        , _pmSender = unAccountName sender
        , _pmTTL = ttl'
        }
  code' <- lift code
  allAccounts <- lift $ m ^. wallet_accounts
  -- Make an effort to ensure the sender account has enough balance to actually
  -- pay the gas. This won't work if the user selects an account on a different
  -- chain, but that's another issue.
  for_ (lookupAccountByName chainId sender allAccounts) $ \senderAccount -> case _account_balance senderAccount of
    Nothing -> empty
    Just b -> let GasLimit lim = _pmGasLimit publicMeta
                  GasPrice (ParsedDecimal price) = _pmGasPrice publicMeta
               in guard $ unAccountBalance b > fromIntegral lim * price
  let pkCaps = publicKeysForAccounts allAccounts caps
  pure $ do
    let signingPairs = getSigningPairs signing allAccounts
    cmd <- buildCmd
      (_deploymentSettingsConfig_nonce settings)
      networkId publicMeta signingPairs
      (_deploymentSettingsConfig_extraSigners settings)
      code' (HM.union jsonData' deploySettingsJsonData) pkCaps
    wrappedCmd <- for (wrapWithBalanceChecks signing code') $ \wrappedCode -> do
      buildCmd
        (_deploymentSettingsConfig_nonce settings)
        networkId publicMeta signingPairs
        (_deploymentSettingsConfig_extraSigners settings)
        wrappedCode (HM.union jsonData' deploySettingsJsonData) pkCaps
    pure $ DeploymentSettingsResult
      { _deploymentSettingsResult_gasPrice = _pmGasPrice publicMeta
      , _deploymentSettingsResult_signingKeys = signingPairs
      , _deploymentSettingsResult_signingAccounts = signing
      , _deploymentSettingsResult_sender = sender
      , _deploymentSettingsResult_wrappedCommand = wrappedCmd
      , _deploymentSettingsResult_accountsToTrack = signing
      , _deploymentSettingsResult_chainId = chainId
      , _deploymentSettingsResult_command = cmd
      , _deploymentSettingsResult_code = code'
      }

buildDeployTabs
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => Maybe DeploymentSettingsView
  -> Bool
  -> Event t (DeploymentSettingsView -> Maybe DeploymentSettingsView)
  -> m ( Dynamic t DeploymentSettingsView
       , Event t ()
       , Event t DeploymentSettingsView
       )
buildDeployTabs mUserTabName includePreviewTab controls = mdo
  let initTab = fromMaybe DeploymentSettingsView_Cfg mUserTabName
      f thisView g = case g thisView of
        Just view' -> (Just view', Nothing)
        Nothing -> (Nothing, Just ())
  (curSelection, done) <- mapAccumMaybeDyn f initTab $ leftmost
    [ const . Just <$> onTabClick
    , controls
    ]
  (TabBar onTabClick) <- makeTabBar $ TabBarCfg
    { _tabBarCfg_tabs = availableTabs
    , _tabBarCfg_mkLabel = const $ text . showSettingsTabName
    , _tabBarCfg_selectedTab = Just <$> curSelection
    , _tabBarCfg_classes = mempty
    , _tabBarCfg_type = TabBarType_Secondary
    }
  pure (curSelection, done, onTabClick)
  where
    userTabs = maybeToList mUserTabName
    stdTabs = [DeploymentSettingsView_Cfg, DeploymentSettingsView_Keys]
    withPreview = if includePreviewTab then [DeploymentSettingsView_Preview] else []
    availableTabs = userTabs <> stdTabs <> withPreview

defaultTabViewProgressButtonLabel :: DeploymentSettingsView -> Text
defaultTabViewProgressButtonLabel DeploymentSettingsView_Preview = "Submit"
defaultTabViewProgressButtonLabel _ = "Next"

buildDeployTabFooterControls
  :: ( PostBuild t m
     , DomBuilder t m
     )
  => Maybe DeploymentSettingsView
  -> Bool
  -> Dynamic t DeploymentSettingsView
  -> (DeploymentSettingsView -> Text)
  -> Dynamic t Bool
  -> m (Event t (DeploymentSettingsView -> Maybe DeploymentSettingsView))
buildDeployTabFooterControls mUserTabName includePreviewTab curSelection stepFn hasResult = do
  let backConfig = def & uiButtonCfg_class .~ ffor curSelection
        (\s -> if s == fromMaybe DeploymentSettingsView_Cfg mUserTabName then "hidden" else "")

      tabToBeDisabled = if includePreviewTab
        then DeploymentSettingsView_Preview
        else DeploymentSettingsView_Keys

      shouldBeDisabled tab hasRes = tab == tabToBeDisabled && hasRes
      isDisabled = shouldBeDisabled <$> curSelection <*> hasResult

  back <- uiButtonDyn backConfig $ text "Back"
  next <- uiButtonDyn
    (def & uiButtonCfg_class .~ "button_type_confirm" & uiButtonCfg_disabled .~ isDisabled)
    $ dynText (stepFn <$> curSelection)

  pure $ leftmost
    [ nextView includePreviewTab <$ next
    , prevView mUserTabName <$ back
    ]

-- | Show settings related to deployments to the user.
--
--
--   the right keys, ...
uiDeploymentSettings
  :: forall key t m model mConf a
  . ( MonadWidget t m, HasNetwork model t, HasWallet model key t
    , Monoid mConf , HasNetworkCfg mConf t
    , HasJsonDataCfg mConf t, Flattenable mConf t, HasJsonData model t
    , HasCrypto key (Performable m)
    )
  => model
  -> DeploymentSettingsConfig t m model a
  -> m (mConf, Event t (DeploymentSettingsResult key), Maybe a)
uiDeploymentSettings m settings = mdo
    let code = _deploymentSettingsConfig_code settings
    (curSelection, done, _) <- buildDeployTabs mUserTabName (_deploymentSettingsConfig_includePreviewTab settings) controls
    (conf, result, ma) <- elClass "div" "modal__main transaction_details" $ do

      mRes <- traverse (uncurry $ tabPane mempty curSelection) mUserTabCfg

      (cfg, cChainId, ttl, gasLimit, _) <- tabPane mempty curSelection DeploymentSettingsView_Cfg $
        uiCfg (Just code) m
          (_deploymentSettingsConfig_chainId settings $ m)
          (_deploymentSettingsConfig_ttl settings)
          (_deploymentSettingsConfig_gasLimit settings)
          (_deploymentSettingsConfig_userSections settings)

      (mSender, signers, capabilities) <- tabPane mempty curSelection DeploymentSettingsView_Keys $
        uiSenderCapabilities m cChainId (_deploymentSettingsConfig_caps settings)
          $ (_deploymentSettingsConfig_sender settings) m cChainId

      when (_deploymentSettingsConfig_includePreviewTab settings) $ tabPane mempty curSelection DeploymentSettingsView_Preview $ do
        let currentNode = headMay . rights <$> (m ^. network_selectedNodes)
            mNetworkId = (hush . mkNetworkName . nodeVersion =<<) <$> currentNode

            mHeadAccount = fmap _account_name . findFirstVanityAccount <$> (m ^. wallet_accounts)
            mHeadChain = (headMay =<<) . fmap getChains <$> currentNode

            aSender = (<|>) <$> mSender <*> mHeadAccount
            aChainId = (<|>) <$> cChainId <*> mHeadChain

            uiPreviewPane = uiDeployPreview m settings
              <$> (m ^. wallet_accounts)
              <*> signers
              <*> gasLimit
              <*> ttl
              <*> code
              <*> (m ^. network_meta)
              <*> capabilities
              <*> (m ^. jsonData . jsonData_data)
              <*> mNetworkId
              <*> aChainId
              <*> aSender

        dyn_ $ curSelection >>= \case
          DeploymentSettingsView_Preview -> uiPreviewPane
          _ -> constDyn blank

      pure
        ( cfg & networkCfg_setSender .~ fmapMaybe (fmap unAccountName) (updated mSender)
        , buildDeploymentSettingsResult m mSender signers cChainId capabilities ttl gasLimit code settings
        , mRes
        )

    command <- performEvent $ tagMaybe (current result) done
    controls <- modalFooter $ buildDeployTabFooterControls
      mUserTabName
      (_deploymentSettingsConfig_includePreviewTab settings)
      curSelection
      defaultTabViewProgressButtonLabel
      (isNothing <$> result)

    pure (conf, command, ma)
    where
      mUserTabCfg  = first DeploymentSettingsView_Custom <$> _deploymentSettingsConfig_userTab settings
      mUserTabName = fmap fst mUserTabCfg

-- | Use a predefined chain id, don't let the user pick one.
predefinedChainIdSelect
  :: (Reflex t, Monad m)
  => Pact.ChainId
  -> model
  -> m (Dynamic t (Maybe Pact.ChainId))
predefinedChainIdSelect chanId _ = pure . pure . pure $ chanId

-- | Use a predefined immutable chain id, but display it too.
predefinedChainIdDisplayed
  :: DomBuilder t m
  => Pact.ChainId
  -> model
  -> m (Dynamic t (Maybe Pact.ChainId))
predefinedChainIdDisplayed cid _ = do
  _ <- mkLabeledInput True "Chain ID" uiInputElement $ def
    & initialAttributes %~ Map.insert "disabled" ""
    & inputElementConfig_initialValue .~ _chainId cid
  pure $ pure $ pure cid

-- | Let the user pick a chain id.
userChainIdSelect
  :: (MonadWidget t m, HasNetwork model t
     )
  => model
  -> m (MDynamic t Pact.ChainId)
userChainIdSelect m =
  userChainIdSelectWithPreselect m (constDyn Nothing)

-- | Let the user pick a chain id but preselect a value
userChainIdSelectWithPreselect
  :: (MonadWidget t m, HasNetwork model t
     )
  => model
  -> Dynamic t (Maybe Pact.ChainId)
  -> m (MDynamic t Pact.ChainId)
userChainIdSelectWithPreselect m mChainId = mkLabeledClsInput True "Chain ID" (uiChainSelection mNodeInfo mChainId)
  where mNodeInfo = (^? to rights . _head) <$> m ^. network_selectedNodes

uiDeployDestination
  :: ( MonadWidget t m
     , HasNetwork model t
     )
  => model
  -> m (Dynamic t (f Pact.ChainId))
  -> m (Dynamic t (f Pact.ChainId))
uiDeployDestination m wChainId = do
  divClass "title" $ text "Destination"
  elKlass "div" ("group segment") $ do
    transactionDisplayNetwork m
    wChainId

uiDeployMetaData
  :: ( MonadWidget t m
     , HasNetwork model t
     , HasNetworkCfg mConf t
     , Monoid mConf
     )
  => model
  -> Maybe TTLSeconds
  -> Maybe GasLimit
  -> m (mConf, Dynamic t TTLSeconds, Dynamic t GasLimit)
uiDeployMetaData m mTTL mGasLimit = do
  divClass "title" $ text "Settings"
  elKlass "div" ("group segment") $ uiMetaData m mTTL mGasLimit

-- | UI for asking the user about data needed for deployments/function calling.
uiCfg
  :: ( MonadWidget t m
     , HasNetwork model t
     , HasNetworkCfg mConf t
     , Monoid mConf
     , Flattenable mConf t
     , HasJsonDataCfg mConf t
     , HasWallet model key t
     , HasJsonData model t
     , Traversable g
     )
  => Maybe (Dynamic t Text)
  -> model
  -> m (Dynamic t (f Pact.ChainId))
  -> Maybe TTLSeconds
  -> Maybe GasLimit
  -> g (Text, m a)
  -> m ( mConf
       , Dynamic t (f Pact.ChainId)
       , Dynamic t TTLSeconds
       , Dynamic t GasLimit
       , g a
       )
uiCfg mCode m wChainId mTTL mGasLimit userSections = do
  -- General deployment configuration
  let mkGeneralSettings = do
        traverse_ (\c -> transactionInputSection c Nothing) mCode
        cId <- uiDeployDestination m wChainId

        -- Customisable user provided UI section
        fa <- for userSections $ \(title, body) -> do
          divClass "title" $ text title
          elKlass "div" ("group segment") body

        (cfg, ttl, gasLimit) <- uiDeployMetaData m mTTL mGasLimit
        pure (cfg, cId, ttl, gasLimit, fa)

  rec
    let mkAccordionControlDyn initActive = foldDyn (const not) initActive
          $ leftmost [eGeneralClicked , eAdvancedClicked]

    dGeneralActive <- mkAccordionControlDyn True
    dAdvancedActive <- mkAccordionControlDyn False

    (eGeneralClicked, pairA) <- controlledAccordionItem dGeneralActive mempty (text "General") mkGeneralSettings
    divClass "title" blank
    (eAdvancedClicked, pairB) <- controlledAccordionItem dAdvancedActive mempty (text "Advanced") $ do
      -- We don't want to change focus when keyset events occur, so consume and do nothing
      -- with the given elements and their dynamic
      divClass "title" $ text "Data"
      uiJsonDataSetFocus (\_ _ -> pure ()) (\_ _ -> pure ()) (m ^. wallet) (m ^. jsonData)
  pure $ snd pairA & _1 <>~ snd pairB

transactionHashSection :: MonadWidget t m => Pact.Command Text -> m ()
transactionHashSection cmd = void $ do
  mkLabeledInput True "Transaction Hash" (\c -> uiInputElement $ c & initialAttributes %~ Map.insert "disabled" "") $ def
    & inputElementConfig_initialValue .~ hashToText (toUntypedHash $ Pact._cmdHash cmd)

transactionInputSection
  :: MonadWidget t m
  => Dynamic t Text
  -> Maybe (Pact.Command Text)
  -> m ()
transactionInputSection code cmd = do
  divClass "title" $ text "Input"
  divClass "group" $ do
    for_ cmd transactionHashSection
    pb <- getPostBuild
    _ <- mkLabeledClsInput True "Raw Command" $ \cls -> uiTextAreaElement $ def
      & textAreaElementConfig_setValue .~ leftmost [updated code, tag (current code) pb]
      & initialAttributes .~ "disabled" =: "" <> "style" =: "width: 100%" <> "class" =: renderClass cls
    pure ()

transactionDisplayNetwork :: (MonadWidget t m, HasNetwork model t) => model -> m ()
transactionDisplayNetwork m = void $ mkLabeledClsInput True "Network" $ \_ -> do
  netStat <- queryNetworkStatus (m ^. network_networks) (m ^. network_selectedNetwork)
  uiNetworkStatus "" netStat
  dynText $ textNetworkName <$> m ^. network_selectedNetwork

-- | ui for asking the user about meta data needed for the transaction.
uiMetaData
  :: forall t m model mConf
     . ( DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m
       , HasNetwork model t, HasNetworkCfg mConf t, Monoid mConf
       )
  => model -> Maybe TTLSeconds -> Maybe GasLimit -> m (mConf, Dynamic t TTLSeconds, Dynamic t GasLimit)
uiMetaData m mTTL mGasLimit = do
    pbGasPrice <- tag (current $ _pmGasPrice <$> m ^. network_meta) <$> getPostBuild

    let
      txnSpeedSliderEl setExternal cls = uiSlider cls (text "Slow") (text "Fast") $ def
        & inputElementConfig_initialValue .~ (showGasPrice $ scaleGPtoTxnSpeed defaultTransactionGasPrice)
        & initialAttributes .~ "min" =: "1" <> "max" =: "1001" <> "step" =: "1"
        & inputElementConfig_setValue .~ leftmost
          [ setExternal
          , showGasPrice . scaleGPtoTxnSpeed <$> pbGasPrice -- Initial value (from storage)
          ]

      gasPriceInputBox
        :: Event t Text
        -> InputElementConfig EventResult t (DomBuilderSpace m)
        -> m (Event t GasPrice)
      gasPriceInputBox setExternal conf = fmap (view _3) $ uiGasPriceInputField $ conf
        & initialAttributes %~ addToClassAttr "input-units"
        & inputElementConfig_initialValue .~ showGasPrice defaultTransactionGasPrice
        & inputElementConfig_setValue .~ leftmost
          [ setExternal
          , showGasPrice <$> pbGasPrice -- Initial value (from storage)
          ]

    onGasPrice <- mdo
      tsEl <- mkLabeledClsInput True "Transaction Speed" (txnSpeedSliderEl setPrice)
      let setSpeed = fmapMaybe (fmap scaleTxnSpeedToGP . parseGasPrice) $ _inputElement_input tsEl
      gpInput <- mkLabeledInput True "Gas Price" (gasPriceInputBox $ fmap showGasPrice setSpeed) def
      let setPrice = fmap (showGasPrice . scaleGPtoTxnSpeed) gpInput
      pure $ leftmost [gpInput, setSpeed]

    let initGasLimit = fromMaybe defaultTransactionGasLimit mGasLimit
    pbGasLimit <- case mGasLimit of
      Just _ -> pure never
      Nothing -> tag (current $ fmap _pmGasLimit $ m ^. network_meta) <$> getPostBuild

    let
      mkGasLimitInput
        :: InputElementConfig EventResult t (DomBuilderSpace m)
        -> m (Event t Integer)
      mkGasLimitInput conf = dimensionalInputWrapper "Units" $ fmap snd $ uiIntInputElement (Just 0) (Just chainwebGasLimitMaximum) $ conf
        & inputElementConfig_initialValue .~ showGasLimit initGasLimit
        & inputElementConfig_setValue .~ fmap showGasLimit pbGasLimit
        & inputElementConfig_elementConfig . elementConfig_eventSpec %~ preventScrollWheelAndUpDownArrow @m

    onGasLimit <- (fmap . fmap) (GasLimit . ParsedInteger) $ mkLabeledInput True "Gas Limit" mkGasLimitInput def

    gasLimit <- holdDyn initGasLimit $ leftmost [onGasLimit, pbGasLimit]

    let mkTransactionFee c = fmap (view _1) $ uiGasPriceInputField $ c
          & initialAttributes %~ Map.insert "disabled" ""

    _ <- mkLabeledInputView True "Max Transaction Fee"  mkTransactionFee $
      ffor (m ^. network_meta) $ \pm -> showGasPrice $ fromIntegral (_pmGasLimit pm) * _pmGasPrice pm

    pbTTL <- case mTTL of
      Just _ -> pure never
      Nothing -> tag (current $ fmap _pmTTL $ m ^. network_meta) <$> getPostBuild
    let secondsInDay = 60 * 60 * 24
        initTTL = fromMaybe defaultTransactionTTL mTTL
        ttlInput cls = elKlass "div" cls $ mdo
          let conf = def
                & initialAttributes .~ "min" =: "1" <> "max" =: T.pack (show secondsInDay) <> "step" =: "1"
                & inputElementConfig_setValue .~ fmap showTtl pbTTL
                & inputElementConfig_initialValue .~ showTtl initTTL
          sliderEl <- uiSlider "" (text "1 second") (text "1 day") $ conf
            & inputElementConfig_setValue .~ _inputElement_input inputEl
          (inputEl, inputEv) <- dimensionalInputWrapper "Seconds" $ uiIntInputElement (Just 0) (Just secondsInDay) $ conf
            & inputElementConfig_setValue .~ _inputElement_input sliderEl
          pure $ leftmost
            [ TTLSeconds . ParsedInteger <$> inputEv
            , fmapMaybe (readPact (TTLSeconds . ParsedInteger)) $ _inputElement_input sliderEl
            ]

    horizontalDashedSeparator

    onTTL <- mkLabeledClsInput True "Request Expires" ttlInput
    ttl <- holdDyn initTTL $ leftmost [onTTL, pbTTL]

    pure
      ( mempty
        & networkCfg_setGasPrice .~ onGasPrice
        & networkCfg_setGasLimit .~ onGasLimit
        & networkCfg_setTTL .~ onTTL
      , ttl
      , gasLimit
      )

  where

      shiftGP :: GasPrice -> GasPrice -> GasPrice -> GasPrice -> GasPrice -> GasPrice
      shiftGP oldMin oldMax newMin newMax x =
        let GasPrice (ParsedDecimal gp) = (newMax-newMin)/(oldMax-oldMin)*(x-oldMin)+newMin
         in GasPrice $ ParsedDecimal $ roundTo maxCoinPrecision gp

      scaleTxnSpeedToGP :: GasPrice -> GasPrice
      scaleTxnSpeedToGP = shiftGP 1 1001 (1e-12) (1e-3)

      scaleGPtoTxnSpeed :: GasPrice -> GasPrice
      scaleGPtoTxnSpeed = shiftGP (1e-12) (1e-3) 1 1001

      parseGasPrice :: Text -> Maybe GasPrice
      parseGasPrice t = GasPrice . ParsedDecimal . roundTo maxCoinPrecision <$> readMay (T.unpack t)

      showGasLimit :: GasLimit -> Text
      showGasLimit (GasLimit (ParsedInteger i)) = tshow i

      showGasPrice :: GasPrice -> Text
      showGasPrice (GasPrice (ParsedDecimal i)) = tshow i

      showTtl :: TTLSeconds -> Text
      showTtl (TTLSeconds (ParsedInteger i)) = tshow i

      readPact wrapper =  fmap wrapper . readMay . T.unpack

-- | Set the sender to a fixed value
uiSenderFixed :: DomBuilder t m => AccountName -> m (Dynamic t (Maybe AccountName))
uiSenderFixed sender = do
  _ <- uiInputElement $ def
    & initialAttributes %~ Map.insert "disabled" ""
    & inputElementConfig_initialValue .~ unAccountName sender
  pure $ pure $ pure sender

mkChainTextAccounts
  :: (Reflex t, HasWallet model key t, HasNetwork model t)
  => model
  -> Dynamic t (Maybe ChainId)
  -> Dynamic t (Either Text (Map AccountName Text))
mkChainTextAccounts m mChainId = runExceptT $ do
  netId <- lift $ m ^. network_selectedNetwork
  -- I guess you don't actually need this anymore, but it's probably good to leave this
  -- gate into things
  _ <- ExceptT $ note "You must select a chain ID before choosing an account" <$> mChainId
  accounts <- lift $ m ^. wallet_accounts
  let accountsOnChain = Map.fromList $ fmapMaybe
        (someAccount
          Nothing -- If it is deleted, ignore it
          -- We explicitly allow accounts on different chains here because this gives the user maximum
          -- flexibility (chainweavers 1-1 key to account view of the world doesn't always reflect reality)
          (\a -> (_account_name a, unAccountName $ _account_name a) <$ guard (_account_network a == netId))
        )
        (IM.elems accounts)
  when (Map.null accountsOnChain) $ throwError "No accounts on current chain"
  pure accountsOnChain

-- | Let the user pick a sender
uiSenderDropdown
  :: ( Adjustable t m, PostBuild t m, DomBuilder t m
     , MonadHold t m, MonadFix m
     , HasWallet model key t
     , HasNetwork model t
     )
  => DropdownConfig t (Maybe AccountName)
  -> Event t (Maybe AccountName)
  -> model
  -> Dynamic t (Maybe ChainId)
  -> m (Dynamic t (Maybe AccountName))
uiSenderDropdown uCfg setSender m chainId = do
  let
    textAccounts = mkChainTextAccounts m chainId
    dropdownItems =
      either
          (Map.singleton Nothing)
          (Map.insert Nothing "Choose an Account" . Map.mapKeys Just)
      <$> textAccounts
  choice <- dropdown Nothing dropdownItems $ uCfg
    & dropdownConfig_setValue .~ leftmost [Nothing <$ updated chainId, setSender]
    & dropdownConfig_attributes <>~ pure ("class" =: "labeled-input__input select select_mandatory_missing")
  pure $ value choice


-- | Let the user pick signers
uiSignerList
  :: ( Adjustable t m, PostBuild t m, DomBuilder t m
     , MonadHold t m
     , HasWallet model key t
     , HasNetwork model t
     )
  => model
  -> Dynamic t (Maybe ChainId)
  -> m (Dynamic t (Set AccountName))
uiSignerList m chainId = do
  let textAccounts = mkChainTextAccounts m chainId
  divClass "title" $ text "Unrestricted Signing Accounts"
  eSwitchSigners <- divClass "group signing-ui-signers" $
    dyn $ ffor textAccounts $ \case
      Left e -> do
        text e
        pure $ constDyn Set.empty
      Right accts -> do
        cbs <- for (Map.toList accts) $ \(an, aTxt) -> do
          cb <- uiCheckbox "signing-ui-signers__signer" False def $ text aTxt
          pure $ bool Nothing (Just an) <$> _checkbox_value cb
        pure $ fmap (Set.fromList . catMaybes) $ sequence $ cbs
  signers <- join <$> holdDyn (constDyn Set.empty) eSwitchSigners
  pure signers

uiChainSelection
  :: MonadWidget t m
  => Dynamic t (Maybe NodeInfo)
  -> Dynamic t (Maybe Pact.ChainId)
  -> CssClass
  -> m (Dynamic t (Maybe Pact.ChainId))
uiChainSelection info mPreselected cls = do
  pb <- getPostBuild

  let
    chains = map (id &&& _chainId) . maybe [] getChains <$> info
    mkPlaceHolder cChains = if null cChains then "No chains available" else "Select chain"
    mkOptions cs = Map.fromList $ (Nothing, mkPlaceHolder cs) : map (first Just) cs

    staticCls = cls <> "select"
    mkDynCls v = if isNothing v then "select_mandatory_missing" else mempty

  rec
    let allCls = renderClass <$> fmap mkDynCls d <> pure staticCls
        cfg = def
          & dropdownConfig_attributes .~ (("class" =:) <$> allCls)
          & dropdownConfig_setValue .~ (current mPreselected <@ pb)

    d <- _dropdown_value <$> dropdown Nothing (mkOptions <$> chains) cfg
  pure d

parseSigCapability :: Text -> Either String SigCapability
parseSigCapability txt = parsed >>= compiled >>= parseApp
  where
    parseApp ts = case ts of
      [(TApp (App (TVar (QName q) _) as _) _)] -> SigCapability q <$> mapM toPV as
      _ -> Left $ "Sig capability parse failed: Expected single qualified capability in form (qual.DEFCAP arg arg ...)"
    compiled Pact.ParsedCode{..} = fmapL (("Sig capability parse failed: " ++) . show) $
      compileExps (mkTextInfo _pcCode) _pcExps
    parsed = parsePact txt
    toPV a = fmapL (("Sig capability argument parse failed, expected simple pact value: " ++) . T.unpack) $ toPactValue a

-- | Display a single row for the user to enter a custom capability and
-- account to attach
capabilityInputRow
  :: MonadWidget t m
  => Maybe DappCap
  -> m (Dynamic t (Maybe AccountName))
  -> m (CapabilityInputRow t)
capabilityInputRow mCap mkSender = elClass "tr" "table__row" $ do
  (emptyCap, parsed) <- elClass "td" "table__cell_padded" $ mdo
    cap <- uiInputElement $ def
      & inputElementConfig_initialValue .~ foldMap (renderCompactText . _dappCap_cap) mCap
      & initialAttributes .~
        (let (cls, dis) = maybe mempty (const (" input_transparent grant-capabilities-static-input", "disabled" =: "true")) mCap
        in mconcat
          [ "placeholder" =: "(module.capability arg1 arg2)"
          , "class" =: ("input_width_full" <> cls)
          , dis
          ])
      & modifyAttributes .~ ffor errors (\e -> "style" =: ("background-color: #fdd" <$ guard e))
    emptyCap <- holdUniqDyn $ T.null <$> value cap
    let parsed = parseSigCapability <$> value cap
        showError = (\p e -> isLeft p && not e) <$> parsed <*> emptyCap
        errors = leftmost
          [ tag (current showError) (domEvent Blur cap)
          , False <$ _inputElement_input cap
          ]
    pure (emptyCap, parsed)
  account <- elClass "td" "table__cell_padded" mkSender

  pure $ CapabilityInputRow
    { _capabilityInputRow_empty = emptyCap
    , _capabilityInputRow_value = emptyCap >>= \case
      True -> pure mempty
      False -> fmap (fromMaybe mempty) $ runMaybeT $ do
        a <- MaybeT account
        p <- MaybeT $ either (const Nothing) pure <$> parsed
        pure $ Map.singleton a [p]
    , _capabilityInputRow_account = account
    , _capabilityInputRow_cap = parsed
    }

-- | Display a dynamic number of rows for the user to enter custom capabilities
capabilityInputRows
  :: forall t m. MonadWidget t m
  => Event t ()  -- Add new row
  -> m (Dynamic t (Maybe AccountName))
  -> m (Dynamic t (Map AccountName [SigCapability]), Dynamic t Int)
capabilityInputRows addNew mkSender = do
  rec
    (im0, im') <- traverseIntMapWithKeyWithAdjust (\_ _ -> capabilityInputRow Nothing mkSender) IM.empty $ leftmost
      -- Delete rows, but ensure we don't delete them all
      [ PatchIntMap <$> deletions
      -- Add a new row when all rows are used
      , attachWith (\i _ -> PatchIntMap (IM.singleton i (Just ()))) nextKeyToUse addNew
      ]
    results :: Dynamic t (IntMap (CapabilityInputRow t))
      <- foldDyn applyAlways im0 im'
    let nextKeyToUse = maybe 0 (succ . fst) . IM.lookupMax <$> current results

        decideDeletions :: Int -> CapabilityInputRow t -> Event t (IntMap (Maybe ()))
        decideDeletions i row = IM.singleton i Nothing <$
          -- Deletions caused by users entering GAS
          (ffilter (either (const False) isGas) . updated $ _capabilityInputRow_cap row)
        deletions = switch . current $ IM.foldMapWithKey decideDeletions <$> results

  pure ( fmap (Map.unionsWith (<>) . IM.elems) $ traverse _capabilityInputRow_value =<< results
       , fmap length results
       )

-- | Widget for selection of sender and signing keys.
uiSenderCapabilities
  :: forall key t m model. (MonadWidget t m, HasWallet model key t, HasNetwork model t)
  => model
  -> Dynamic t (Maybe Pact.ChainId)
  -> Maybe [DappCap]
  -> m (Dynamic t (Maybe AccountName))
  -> m (Dynamic t (Maybe AccountName), Dynamic t (Set AccountName), Dynamic t (Map AccountName [SigCapability]))
uiSenderCapabilities m cid mCaps mkSender = do
  let senderDropdown setSender = uiSenderDropdown def setSender m cid
      staticCapabilityRow sender cap = do
        el "td" $ text $ _dappCap_role cap
        el "td" $ text $ renderCompactText $ _dappCap_cap cap
        acc <- el "td" $ sender
        pure $ CapabilityInputRow
          { _capabilityInputRow_empty = pure False
          , _capabilityInputRow_value = maybe mempty (\s -> Map.singleton s [_dappCap_cap cap]) <$> acc
          , _capabilityInputRow_account = acc
          , _capabilityInputRow_cap = pure $ Right $ _dappCap_cap cap
          }

      staticCapabilityRows setSender caps = fmap combineMaps $ for caps $ \cap ->
        elClass "tr" "table__row" $ _capabilityInputRow_value <$> staticCapabilityRow (uiSenderDropdown def setSender m cid) cap

      combineMaps :: (Semigroup v, Ord k) => [Dynamic t (Map k v)] -> Dynamic t (Map k v)
      combineMaps = fmap (Map.unionsWith (<>)) . sequence

  eAddCap <- divClass "grant-capabilities-title" $ do
    divClass "title grant-capabilities-title__title" $ text "Grant Capabilities"
    case mCaps of
      Nothing -> addButton (def & uiButtonCfg_class <>~ " grant-capabilities-title__add-button")
      Just _  -> pure never

  -- Capabilities
  (mGasAcct, capabilities) <- divClass "group" $ mdo
    (mGasAcct', capabilities', rowCount) <- elAttr "table" ("class" =: "table" <> "style" =: "width: 100%; table-layout: fixed;") $ case mCaps of
      Nothing -> do
        el "thead" $ el "tr" $ do
          elClass "th" "table__heading table__cell_padded" $ text "Capability"
          elClass "th" "table__heading table__cell_padded" $ text "Account"
        el "tbody" $ do
          gas<- capabilityInputRow (Just defaultGASCapability) mkSender
          (rest, restCount) <- capabilityInputRows eAddCap (senderDropdown (Just <$> eApplyToAll))
          pure ( _capabilityInputRow_account gas
               , combineMaps [(_capabilityInputRow_value gas), rest]
               , fmap (1+) restCount
               )
      Just caps -> do
        el "thead" $ el "tr" $ do
          elClass "th" "table__heading" $ text "Role"
          elClass "th" "table__heading" $ text "Capability"
          elClass "th" "table__heading" $ text "Account"
        el "tbody" $ do
          gas <- staticCapabilityRow mkSender defaultGASCapability
          rest <- staticCapabilityRows (Just <$> eApplyToAll) $ filter (not . isGas . _dappCap_cap) caps
          pure ( _capabilityInputRow_account gas
               , combineMaps [(_capabilityInputRow_value gas),rest]
               , constDyn 0
               )

    -- If the gas capability is set, we enable the button that will set every other
    -- capability's sender from the gas account.
    eApplyToAllClick <- (switchHold never =<<) $ dyn $ ffor rowCount $ \n ->
      if n > 1 then divClass "grant-capabilities-apply-all-wrapper" $ uiButtonDyn
        (btnCfgSecondary
          & uiButtonCfg_disabled .~ (isNothing <$> mGasAcct')
          & uiButtonCfg_class .~ (constDyn $ "grant-capabilities-apply-all")
        )
        (text "Apply gas payer to all")
      else
        pure never

    let eApplyToAll = fmapMaybe id $ current mGasAcct' <@ eApplyToAllClick

    pure (mGasAcct', capabilities')


  signers <- uiSignerList m cid

  pure (mGasAcct, signers, capabilities)

isGas :: SigCapability -> Bool
isGas = (^. to PC._scName . to PN._qnName . to (== "GAS"))

-- parsed: "{\"role\": \"GAS\", \"description\": \"Pay the GAS required for this transaction\", \"cap\": {\"args\": [\"doug\",], \"name\": \"coin.FUND_TX\"}}"
defaultGASCapability :: DappCap
defaultGASCapability = DappCap
  { _dappCap_role = "GAS"
  , _dappCap_description = "Pay the GAS required for this transaction"
  , _dappCap_cap = PC.SigCapability
    { PC._scName = PN.QualifiedName
      { PN._qnQual = PN.ModuleName
        { PN._mnName = "coin"
        , PN._mnNamespace = Nothing
        }
      , PN._qnName = "GAS"
      , PN._qnInfo = PI.mkInfo "coin.GAS"
      }
    , PC._scArgs = []
    }
  }

uiDeployPreview
  :: ( MonadWidget t m
     , HasNetwork model t
     , HasCrypto key (Performable m)
     )
  => model
  -> DeploymentSettingsConfig t m model a
  -> Accounts key
  -> Set AccountName
  -> GasLimit
  -> TTLSeconds
  -> Text
  -> PublicMeta
  -> Map AccountName [SigCapability]
  -> Either JsonError Aeson.Object
  -> Maybe NetworkName
  -> Maybe ChainId
  -> Maybe AccountName
  -> m ()
uiDeployPreview _ _ _ _ _ _ _ _ _ _ _ _ Nothing = text "No valid GAS payer accounts in Wallet."
uiDeployPreview _ _ _ _ _ _ _ _ _ _ _ Nothing _ = text "Please select a Chain."
uiDeployPreview _ _ _ _ _ _ _ _ _ _ Nothing _ _ = text "No network nodes configured."
uiDeployPreview model settings accounts signers gasLimit ttl code lastPublicMeta capabilities jData (Just networkId) (Just chainId) (Just sender) = do
  pb <- getPostBuild

  let deploySettingsJsonData = fromMaybe mempty $ _deploymentSettingsConfig_data settings
      jsonData0 = fromMaybe mempty $ hush jData
      signing = signers <> (Set.insert sender $ Map.keysSet capabilities)
      pkCaps = publicKeysForAccounts accounts capabilities
      signingPairs = getSigningPairs signing accounts

  let publicMeta = lastPublicMeta
        { _pmChainId = chainId
        , _pmGasLimit = gasLimit
        , _pmSender = unAccountName sender
        , _pmTTL = ttl
        }

      nonce = _deploymentSettingsConfig_nonce settings
      extraSigners = _deploymentSettingsConfig_extraSigners settings
      jsondata = HM.union jsonData0 deploySettingsJsonData

  eCmds <- performEvent $ ffor pb $ \_ -> do
    c <- buildCmd nonce networkId publicMeta signingPairs extraSigners code jsondata pkCaps
    wc <- for (wrapWithBalanceChecks signing code) $ \wrappedCode -> do
      buildCmd nonce networkId publicMeta signingPairs extraSigners wrappedCode jsondata pkCaps
    pure (c, wc)

  dyn_ =<< holdDyn (text "Preparing transaction preview...")
    (uiPreviewResponses signing <$> eCmds)
  where
    uiPreviewResponses signing (cmd, wrappedCmd) = elClass "div" "modal__main transaction_details" $ do
      pb <- getPostBuild
      transactionInputSection (pure code) (pure cmd)
      divClass "title" $ text "Destination"
      _ <- divClass "group segment" $ do
        transactionDisplayNetwork model
        predefinedChainIdDisplayed chainId model

      let accountsToTrack = getAccounts signing
          localReq = case wrappedCmd of
            Left _e -> []
            Right cmd0 -> pure $ NetworkRequest
              { _networkRequest_cmd = cmd0
              , _networkRequest_chainRef = ChainRef Nothing chainId
              , _networkRequest_endpoint = Endpoint_Local
              }
      responses <- performLocalRead (model ^. network) $ localReq <$ pb
      (errors, resp) <- fmap fanThese $ performEvent $ ffor responses $ \case
        [(_, errorResult)] -> parseNetworkErrorResult parseWrappedBalanceChecks errorResult
        n -> do
          liftIO $ T.putStrLn $ "Expected 1 response, but got " <> tshow (length n)
          pure $ This "Couldn't get a response from the node"

      divClass "title" $ text "Anticipated Transaction Impact"
      divClass "group segment" $ do
        let tableAttrs = "style" =: "table-layout: fixed; width: 100%" <> "class" =: "table"
        elAttr "table" tableAttrs $ do
          el "thead" $ el "tr" $ do
            let th = elClass "th" "table__heading" . text
            th "Account Name"
            th "Public Key"
            th "Change in Balance"
          accountBalances <- flip Map.traverseWithKey accountsToTrack $ \acc pk -> do
            bal <- holdDyn Nothing $ leftmost [Just Nothing <$ errors, Just . Map.lookup acc . fst <$> resp]
            pure (pk, bal)
          el "tbody" $ void $ flip Map.traverseWithKey accountBalances $ \acc (pk, balance) -> el "tr" $ do
            let displayBalance = \case
                  Nothing -> "Loading..."
                  Just Nothing -> "Error"
                  Just (Just b) -> tshow (unAccountBalance b) <> " KDA"
            el "td" $ text $ unAccountName acc
            el "td" $ divClass "wallet__key" $ text $ keyToText pk
            el "td" $ dynText $ displayBalance <$> balance

      divClass "title" $ text "Raw Response"
      void $ divClass "group segment transaction_details__raw-response" $ runWithReplace (text "Loading...") $ leftmost
        [ text . renderCompactText . snd <$> resp
        , text <$> errors
        ]

    getAccounts :: Set AccountName -> Map AccountName PublicKey
    getAccounts = Map.restrictKeys (IntMap.foldr f Map.empty accounts)
      where f = \case
              SomeAccount_Deleted -> id
              SomeAccount_Account a -> Map.insert (_account_name a) $ _keyPair_publicKey $ _account_key a
