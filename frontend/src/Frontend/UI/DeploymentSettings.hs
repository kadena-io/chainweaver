{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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

import Control.Applicative ((<|>), liftA2)
import Control.Arrow (first)
import Control.Error (fmapL, headMay, (!?), failWith, failWithM)
import Control.Error.Util (hush)
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Data.Decimal (roundTo)
import Data.Dependent.Sum (DSum(..))
import Data.Either (rights, isLeft)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Set (Set)
import Data.Some (Some(Some), withSome)
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
import Frontend.Log
import Frontend.Network
import Frontend.UI.JsonData
import Frontend.UI.Modal
import Frontend.UI.TabBar
import Frontend.UI.Widgets
import Frontend.UI.Widgets.Helpers (preventScrollWheelAndUpDownArrow,dialogSectionHeading)
import Frontend.Wallet

-- | Config for the deployment settings widget.
data DeploymentSettingsConfig t m model a = DeploymentSettingsConfig
  { _deploymentSettingsConfig_userTab     :: Maybe (Text, m a)
    -- ^ Some optional extra tab. fst is the tab's name, snd is its content.
  , _deploymentSettingsConfig_chainId     :: model -> m (Dynamic t (Maybe Pact.ChainId))
    -- ^ ChainId selection widget.
    --   You can pick (predefinedChainIdSelect someId) - for not showing a
    --   widget at all, but having `uiDeploymentSettings` use the provided one.
    --
    --   Or you can use `userChainIdSelect` for having the user pick a chainid.
  , _deploymentSettingsConfig_sender
    :: model
    -> Dynamic t (Maybe ChainId)
    -> Event t (Maybe (Some AccountRef))
    -> m (Dynamic t (Maybe (Some AccountRef)))
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
  , _capabilityInputRow_value :: Dynamic t (Map (Some AccountRef) [SigCapability])
  , _capabilityInputRow_account :: Dynamic t (Maybe (Some AccountRef))
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
  , _deploymentSettingsResult_signingAccounts :: Set (Some AccountRef)
  , _deploymentSettingsResult_sender :: Some AccountRef
  , _deploymentSettingsResult_chainId :: ChainId
  , _deploymentSettingsResult_code :: Text
  , _deploymentSettingsResult_command :: Pact.Command Text
  , _deploymentSettingsResult_wrappedCommand :: Either String (Pact.Command Text)
  -- ^ This differs from 'command' because this wraps the code with balance
  -- checks for a /local request. This should never be actually deployed.
  , _deploymentSettingsResult_accountsToTrack :: Set (Some AccountRef)
  }

publicKeysForAccounts :: Accounts -> Map (Some AccountRef) a -> Map PublicKey a
publicKeysForAccounts allAccounts caps =
  let toPublicKey (ref, cs) = do
        pk <- accountKey <$> lookupAccountRef ref allAccounts
        pure (pk, cs)
  in Map.fromList $ fmapMaybe toPublicKey $ Map.toList caps

lookupAccountBalance :: Some AccountRef -> Accounts -> Maybe (Maybe AccountBalance)
lookupAccountBalance ref = fmap accountBalance . lookupAccountRef ref

data DeploymentSettingsResultError
  = DeploymentSettingsResultError_GasPayerIsNotValid (Some AccountRef)
  | DeploymentSettingsResultError_InvalidNetworkName Text
  | DeploymentSettingsResultError_NoSenderSelected
  | DeploymentSettingsResultError_NoChainIdSelected
  | DeploymentSettingsResultError_NoNodesAvailable
  | DeploymentSettingsResultError_InvalidJsonData JsonError
  | DeploymentSettingsResultError_NoAccountsOnNetwork NetworkName
  | DeploymentSettingsResultError_InsufficientFundsOnGasPayer
  deriving Eq

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
  -> Dynamic t (Maybe (Some AccountRef))
  -> Dynamic t (Maybe (Some AccountRef))
  -> Dynamic t (Set (Some AccountRef))
  -> Dynamic t (Maybe ChainId)
  -> Dynamic t (Map (Some AccountRef) [SigCapability])
  -> Dynamic t TTLSeconds
  -> Dynamic t GasLimit
  -> Dynamic t Text
  -> DeploymentSettingsConfig t m model a
  -> Dynamic t (Either DeploymentSettingsResultError (Performable m (DeploymentSettingsResult key)))
buildDeploymentSettingsResult m mSender mGasPayer signers cChainId capabilities ttl gasLimit code settings = runExceptT $ do
  selNodes <- lift $ m ^. network_selectedNodes
  networkName <- lift $ m ^. network_selectedNetwork
  networkId <- liftEither
    . over _Left DeploymentSettingsResultError_InvalidNetworkName . mkNetworkName . nodeVersion
    =<< failWith DeploymentSettingsResultError_NoNodesAvailable (headMay $ rights selNodes)

  sender <- mSender !? DeploymentSettingsResultError_NoSenderSelected
  chainId <- cChainId !? DeploymentSettingsResultError_NoChainIdSelected

  caps <- lift capabilities
  signs <- lift signers
  let signingAccounts = signs <> (Set.insert sender $ Map.keysSet caps)
      deploySettingsJsonData = fromMaybe mempty $ _deploymentSettingsConfig_data settings
  jsonData' <- ExceptT $ over (mapped . _Left) DeploymentSettingsResultError_InvalidJsonData $ m ^. jsonData . jsonData_data
  ttl' <- lift ttl
  limit <- lift gasLimit
  lastPublicMeta <- lift $ m ^. network_meta
  let publicMeta = lastPublicMeta
        { _pmChainId = chainId
        , _pmGasLimit = limit
        , _pmSender = withSome sender accountRefToName
        , _pmTTL = ttl'
        }
  code' <- lift code
  keys <- lift $ m ^. wallet_keys
  allAccounts <- failWithM (DeploymentSettingsResultError_NoAccountsOnNetwork networkName)
    $ Map.lookup networkName . unAccountStorage <$> m ^. wallet_accounts

  -- Make an effort to ensure the gas payer (if there is one) account has enough balance to actually
  -- pay the gas. This won't work if the user selects an account on a different
  -- chain, but that's another issue.
  gasPayer <- lift mGasPayer
  case gasPayer of
    Nothing -> pure () -- No gas payer selected, move along
    Just gp ->  for_ (lookupAccountBalance gp allAccounts) $ \case
      -- Gas Payer selected but they're not an account?!
      Nothing -> throwError $ DeploymentSettingsResultError_GasPayerIsNotValid gp
      Just b -> let GasLimit lim = _pmGasLimit publicMeta
                    GasPrice (ParsedDecimal price) = _pmGasPrice publicMeta
                 in unless (unAccountBalance b > fromIntegral lim * price) $
                     throwError DeploymentSettingsResultError_InsufficientFundsOnGasPayer

  let pkCaps = publicKeysForAccounts allAccounts caps
  pure $ do
    let signingPairs = getSigningPairs keys allAccounts signingAccounts
    cmd <- buildCmd
      (_deploymentSettingsConfig_nonce settings)
      networkId publicMeta signingPairs
      (_deploymentSettingsConfig_extraSigners settings)
      code' (HM.union jsonData' deploySettingsJsonData) pkCaps
    wrappedCmd <- for (wrapWithBalanceChecks signingAccounts code') $ \wrappedCode -> do
      buildCmd
        (_deploymentSettingsConfig_nonce settings)
        networkId publicMeta signingPairs
        (_deploymentSettingsConfig_extraSigners settings)
        wrappedCode (HM.union jsonData' deploySettingsJsonData) pkCaps
    pure $ DeploymentSettingsResult
      { _deploymentSettingsResult_gasPrice = _pmGasPrice publicMeta
      , _deploymentSettingsResult_signingKeys = signingPairs
      , _deploymentSettingsResult_signingAccounts = signingAccounts
      , _deploymentSettingsResult_sender = sender
      , _deploymentSettingsResult_wrappedCommand = wrappedCmd
      , _deploymentSettingsResult_accountsToTrack = signingAccounts
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
  let backConfig = btnCfgTertiary & uiButtonCfg_class .~ ffor curSelection
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
    , HasCrypto key (Performable m), HasLogger model t
    )
  => model
  -> DeploymentSettingsConfig t m model a
  -> m (mConf, Event t (DeploymentSettingsResult key), Maybe a)
uiDeploymentSettings m settings = mdo
    let code = _deploymentSettingsConfig_code settings
    (curSelection, done, _) <- buildDeployTabs mUserTabName (_deploymentSettingsConfig_includePreviewTab settings) controls
    (conf, result, ma) <- elClass "div" "modal__main transaction_details" $ do

      mRes <- traverse (uncurry $ tabPane mempty curSelection) mUserTabCfg

      (cfg, cChainId, mSender, ttl, gasLimit, _) <- tabPane mempty curSelection DeploymentSettingsView_Cfg $
        uiCfg (Just code) m
          (_deploymentSettingsConfig_chainId settings $ m)
          (_deploymentSettingsConfig_ttl settings)
          (_deploymentSettingsConfig_gasLimit settings)
          Nothing
          (Just advancedAccordion)
          (_deploymentSettingsConfig_sender settings)

      (mGasPayer, signers, capabilities) <- tabPane mempty curSelection DeploymentSettingsView_Keys $ do
        dyn_ $ ffor result $ \case
          Left (DeploymentSettingsResultError_GasPayerIsNotValid _) -> divClass "group segment" $
            text "Selected account for 'coin.GAS' capability does not exist on this chain."
          _ ->
            blank

        uiSenderCapabilities m cChainId (_deploymentSettingsConfig_caps settings) mSender
          $ (_deploymentSettingsConfig_sender settings) m cChainId

      when (_deploymentSettingsConfig_includePreviewTab settings) $ tabPane mempty curSelection DeploymentSettingsView_Preview $ do
        let currentNode = headMay . rights <$> (m ^. network_selectedNodes)
            mNetworkId = (hush . mkNetworkName . nodeVersion =<<) <$> currentNode

            accounts = liftA2 (Map.findWithDefault mempty) (m ^. network_selectedNetwork) (unAccountStorage <$> m ^. wallet_accounts)
            mHeadAccount = fmap (\(n, c, _) -> Some $ AccountRef_Vanity n c) . findFirstVanityAccount (const True) <$> accounts
            mHeadChain = (headMay =<<) . fmap getChains <$> currentNode

            aSender = (<|>) <$> mSender <*> mHeadAccount
            aChainId = (<|>) <$> cChainId <*> mHeadChain

            uiPreviewPane = uiDeployPreview m settings
              <$> (m ^. wallet_keys)
              <*> accounts
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
        ( cfg & networkCfg_setSender .~ fmapMaybe (fmap (\(Some x) -> accountRefToName x)) (updated mSender)
        , buildDeploymentSettingsResult m mSender mGasPayer signers cChainId capabilities ttl gasLimit code settings
        , mRes
        )

    command <- performEvent $ tagMaybe (current $ fmap hush result) done
    controls <- modalFooter $ buildDeployTabFooterControls
      mUserTabName
      (_deploymentSettingsConfig_includePreviewTab settings)
      curSelection
      defaultTabViewProgressButtonLabel
      (isLeft <$> result)

    pure (conf, command, ma)
    where
      mUserTabCfg  = first DeploymentSettingsView_Custom <$> _deploymentSettingsConfig_userTab settings
      mUserTabName = fmap fst mUserTabCfg


uiDeployDestination
  :: ( MonadWidget t m
     , HasNetwork model t
     )
  => model
  -> m (Dynamic t (f Pact.ChainId))
  -> m (Dynamic t (f Pact.ChainId))
uiDeployDestination m wChainId = do
  dialogSectionHeading mempty "Destination"
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
  dialogSectionHeading mempty "Settings"
  elKlass "div" ("group segment") $ uiMetaData m mTTL mGasLimit

-- | UI for asking the user about data needed for deployments/function calling.
uiCfg
  :: ( MonadWidget t m
     , HasNetwork model t
     , HasNetworkCfg mConf t
     , Monoid mConf
     , Traversable g
     )
  => Maybe (Dynamic t Text)
  -> model
  -> m (Dynamic t (Maybe Pact.ChainId))
  -> Maybe TTLSeconds
  -> Maybe GasLimit
  -> g (Text, m a)
  -> Maybe (model -> Dynamic t Bool -> m (Event t (), ((), mConf)))
  -> (model -> Dynamic t (Maybe ChainId) -> Event t (Maybe (Some AccountRef)) -> m (Dynamic t (Maybe (Some AccountRef))))
  -> m ( mConf
       , Dynamic t (Maybe Pact.ChainId)
       , Dynamic t (Maybe (Some AccountRef))
       , Dynamic t TTLSeconds
       , Dynamic t GasLimit
       , g a
       )
uiCfg mCode m wChainId mTTL mGasLimit userSections otherAccordion mSenderSelect = do
  -- General deployment configuration
  let mkGeneralSettings = do
        traverse_ (\c -> transactionInputSection c Nothing) mCode
        cId <- uiDeployDestination m wChainId

        dialogSectionHeading mempty "Transaction Sender"
        mSender <- elKlass "div" ("group segment") $ mkLabeledClsInput True "Account" $ \_ -> do
          mSenderSelect m cId never

        -- Customisable user provided UI section
        fa <- for userSections $ \(title, body) -> do
          dialogSectionHeading mempty title
          elKlass "div" ("group segment") body

        (cfg, ttl, gasLimit) <- uiDeployMetaData m mTTL mGasLimit
        pure (cfg, cId, mSender, ttl, gasLimit, fa)

  rec
    let mkAccordionControlDyn initActive = foldDyn (const not) initActive
          $ leftmost [eGeneralClicked, otherClicked]

    dGeneralActive <- mkAccordionControlDyn True

    (eGeneralClicked, pairA) <- case otherAccordion of
      Nothing -> (never,) . ((),) <$>  mkGeneralSettings
      Just _ -> controlledAccordionItem dGeneralActive "deploy-settings-accordion-header__general"
        (accordionHeaderBtn "General")
        mkGeneralSettings

    (otherClicked, transformCfg) <- case otherAccordion of
      Nothing -> pure (never, id)
      Just mk -> do
        (clk, pairB) <- mk m =<< mkAccordionControlDyn False
        pure (clk, (_1 <>~ snd pairB))

  pure $ transformCfg $ snd pairA

advancedAccordion
  :: MonadWidget t m
  => Flattenable mConf t
  => HasWallet model key t
  => HasJsonData model t
  => HasJsonDataCfg mConf t
  => Monoid mConf
  => model
  -> Dynamic t Bool
  -> m (Event t (), ((), mConf))
advancedAccordion m active = do
  controlledAccordionItem active mempty (accordionHeaderBtn "Advanced") $ do
    -- We don't want to change focus when keyset events occur, so consume and do nothing
    -- with the given elements and their dynamic
    dialogSectionHeading mempty "Data"
    uiJsonDataSetFocus (\_ _ -> pure ()) (\_ _ -> pure ()) (m ^. wallet) (m ^. jsonData)

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
  dialogSectionHeading mempty "Input"
  divClass "group" $ do
    for_ cmd transactionHashSection
    pb <- getPostBuild
    _ <- mkLabeledClsInput True "Raw Command" $ \cls -> uiTextAreaElement $ def
      & textAreaElementConfig_setValue .~ leftmost [updated code, tag (current code) pb]
      & initialAttributes .~ "disabled" =: "" <> "style" =: "width: 100%" <> "class" =: renderClass cls
    pure ()

transactionDisplayNetwork :: (MonadWidget t m, HasNetwork model t) => model -> m ()
transactionDisplayNetwork m = void $ mkLabeledClsInput True "Network" $ \_ -> do
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
      tsEl <- divClass "deploy-meta-cfg__txn-speed" $
        mkLabeledClsInput True "Transaction Speed" (txnSpeedSliderEl setPrice)
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

    onTTL <- divClass "deploy-meta-cfg__request-expires"
      $ mkLabeledClsInput True "Request Expires" ttlInput
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
uiSenderFixed :: DomBuilder t m => AccountName -> m (Dynamic t (Maybe (Some AccountRef)))
uiSenderFixed sender = do
  _ <- uiInputElement $ def
    & initialAttributes %~ Map.insert "disabled" ""
    & inputElementConfig_initialValue .~ unAccountName sender
  pure $ pure $ pure $ Some $ AccountRef_Vanity sender "chain" -- TODO check the chain

mkChainTextAccounts
  :: (Reflex t, HasWallet model key t, HasNetwork model t)
  => model
  -> Dynamic t (Maybe ChainId)
  -> Dynamic t (Either Text (Map (Some AccountRef) Text))
mkChainTextAccounts m mChainId = runExceptT $ do
  netId <- lift $ m ^. network_selectedNetwork
  chain <- ExceptT $ note "You must select a chain ID before choosing an account" <$> mChainId
  accountsOnNetwork <- ExceptT $ note "No accounts on current network" . Map.lookup netId . unAccountStorage <$> m ^. wallet_accounts
  let mkVanity n chainMap
        | Map.member chain chainMap = Map.singleton (Some $ AccountRef_Vanity n chain) (unAccountName n)
        | otherwise = mempty
      vanityAccounts = Map.foldMapWithKey mkVanity $ _accounts_vanity accountsOnNetwork
      mkNonVanity pk chainMap
        | Map.member chain chainMap = Map.singleton (Some $ AccountRef_NonVanity pk chain) (keyToText pk)
        | otherwise = mempty
      nonVanityAccounts = Map.foldMapWithKey mkNonVanity $ _accounts_nonVanity accountsOnNetwork
      accountsOnChain = vanityAccounts <> nonVanityAccounts
  when (Map.null accountsOnChain) $ throwError "No accounts on current chain"
  pure accountsOnChain

-- | Let the user pick a sender
uiSenderDropdown
  :: ( Adjustable t m, PostBuild t m, DomBuilder t m
     , MonadHold t m, MonadFix m
     , HasWallet model key t
     , HasNetwork model t
     )
  => DropdownConfig t (Maybe (Some AccountRef))
  -> model
  -> Dynamic t (Maybe ChainId)
  -> Event t (Maybe (Some AccountRef))
  -> m (Dynamic t (Maybe (Some AccountRef)))
uiSenderDropdown uCfg m chainId setSender = do
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
  -> m (Dynamic t (Set (Some AccountRef)))
uiSignerList m chainId = do
  let textAccounts = mkChainTextAccounts m chainId
  dialogSectionHeading mempty "Unrestricted Signing Accounts"
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
  -> m (Dynamic t (Maybe (Some AccountRef)))
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
  -> m (Dynamic t (Maybe (Some AccountRef)))
  -> m (Dynamic t (Map (Some AccountRef) [SigCapability]), Dynamic t Int)
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
  -> Dynamic t (Maybe (Some AccountRef))
  -> (Event t (Maybe (Some AccountRef)) -> m (Dynamic t (Maybe (Some AccountRef))))
  -> m (Dynamic t (Maybe (Some AccountRef)), Dynamic t (Set (Some AccountRef)), Dynamic t (Map (Some AccountRef) [SigCapability]))
uiSenderCapabilities m cid mCaps mSender mkGasPayer = do
  let senderDropdown setGasPayer = uiSenderDropdown def m cid setGasPayer
      staticCapabilityRow sender cap = do
        elClass "td" "grant-capabilities-static-row__wrapped-cell" $ text $ _dappCap_role cap
        elClass "td" "grant-capabilities-static-row__wrapped-cell" $ text $ renderCompactText $ _dappCap_cap cap
        acc <- el "td" $ sender
        pure $ CapabilityInputRow
          { _capabilityInputRow_empty = pure False
          , _capabilityInputRow_value = maybe mempty (\s -> Map.singleton s [_dappCap_cap cap]) <$> acc
          , _capabilityInputRow_account = acc
          , _capabilityInputRow_cap = pure $ Right $ _dappCap_cap cap
          }

      staticCapabilityRows setGasPayer caps = fmap combineMaps $ for caps $ \cap ->
        elClass "tr" "table__row" $ _capabilityInputRow_value <$> staticCapabilityRow (uiSenderDropdown def m cid setGasPayer) cap

      combineMaps :: (Semigroup v, Ord k) => [Dynamic t (Map k v)] -> Dynamic t (Map k v)
      combineMaps = fmap (Map.unionsWith (<>)) . sequence

  eAddCap <- divClass "grant-capabilities-title" $ do
    dialogSectionHeading "grant-capabilities-title__title" "Grant Capabilities"
    case mCaps of
      Nothing -> addButton (def & uiButtonCfg_class <>~ " grant-capabilities-title__add-button")
      Just _  -> pure never

  -- Capabilities
  (mGasAcct, capabilities) <- divClass "group" $ mdo
    let eDefaultGasPayerToSender = gate (current $ isNothing <$> mGasAcct') $ updated mSender
    (mGasAcct', capabilities', rowCount) <- elAttr "table" ("class" =: "table" <> "style" =: "width: 100%; table-layout: fixed;") $ case mCaps of
      Nothing -> do
        el "thead" $ el "tr" $ do
          elClass "th" "table__heading table__cell_padded" $ text "Capability"
          elClass "th" "table__heading table__cell_padded" $ text "Account"
        el "tbody" $ do
          gas <- capabilityInputRow (Just defaultGASCapability) (mkGasPayer eDefaultGasPayerToSender)
          (rest, restCount) <- capabilityInputRows eAddCap (senderDropdown (Just <$> eApplyToAll))
          pure ( _capabilityInputRow_account gas
               , combineMaps [(_capabilityInputRow_value gas), rest]
               , fmap (1+) restCount
               )
      Just caps -> do
        el "thead" $ el "tr" $ do
          elAttr "th" ("class" =: "table__heading" <> "width" =: "23%") $ text "Role"
          elAttr "th" ("class" =: "table__heading") $ text "Capability"
          elAttr "th" ("class" =: "table__heading" <> "width" =: "30%") $ text "Account"
        el "tbody" $ do
          gas <- staticCapabilityRow (mkGasPayer eDefaultGasPayerToSender) defaultGASCapability
          rest <- staticCapabilityRows (Just <$> eApplyToAll) $ filter (not . isGas . _dappCap_cap) caps
          pure ( _capabilityInputRow_account gas
               , combineMaps [(_capabilityInputRow_value gas),rest]
               , constDyn (1 {- Gas payer -} + length caps)
               )

    -- If the gas capability is set, we enable the button that will set every other
    -- capability's sender from the gas account.
    eApplyToAllClick <- (switchHold never =<<) $ dyn $ ffor rowCount $ \n ->
      if n > 1 then do
        eAllGasPayer <- divClass "grant-capabilities-apply-all-wrapper" $ uiButtonDyn
          (btnCfgSecondary
            & uiButtonCfg_disabled .~ (isNothing <$> mGasAcct')
            & uiButtonCfg_class .~ (constDyn $ "grant-capabilities-apply-all")
          )
          (text "Apply to all")

        pure $ current mGasAcct' <@ eAllGasPayer
      else
        pure never

    let eApplyToAll = fmapMaybe id eApplyToAllClick

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
     , HasLogger model t
     , HasCrypto key (Performable m)
     )
  => model
  -> DeploymentSettingsConfig t m model a
  -> KeyStorage key
  -> Accounts
  -> Set (Some AccountRef)
  -> GasLimit
  -> TTLSeconds
  -> Text
  -> PublicMeta
  -> Map (Some AccountRef) [SigCapability]
  -> Either JsonError Aeson.Object
  -> Maybe NetworkName
  -> Maybe ChainId
  -> Maybe (Some AccountRef)
  -> m ()
uiDeployPreview _ _ _ _ _ _ _ _ _ _ _ _ _ Nothing = text "No valid GAS payer accounts in Wallet."
uiDeployPreview _ _ _ _ _ _ _ _ _ _ _ _ Nothing _ = text "Please select a Chain."
uiDeployPreview _ _ _ _ _ _ _ _ _ _ _ Nothing _ _ = text "No network nodes configured."
uiDeployPreview model settings keys accounts signers gasLimit ttl code lastPublicMeta capabilities jData (Just networkId) (Just chainId) (Just sender) = do
  pb <- getPostBuild

  let deploySettingsJsonData = fromMaybe mempty $ _deploymentSettingsConfig_data settings
      jsonData0 = fromMaybe mempty $ hush jData
      signing = signers <> (Set.insert sender $ Map.keysSet capabilities)
      pkCaps = publicKeysForAccounts accounts capabilities
      signingPairs = getSigningPairs keys accounts signing

  let publicMeta = lastPublicMeta
        { _pmChainId = chainId
        , _pmGasLimit = gasLimit
        , _pmSender = withSome sender accountRefToName
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
    uiPreviewResponses signing (cmd, wrappedCmd) = do
      pb <- getPostBuild

      unless (any (any isGas) capabilities) $ do
        dialogSectionHeading mempty "Notice"
        divClass "group segment" $ mkLabeledView True mempty
          $ text "A 'Gas Payer' has not been selected for this transaction. Are you sure this is correct?"

      transactionInputSection (pure code) (pure cmd)
      dialogSectionHeading mempty  "Destination"
      _ <- divClass "group segment" $ do
        transactionDisplayNetwork model
        predefinedChainIdDisplayed chainId model

      dialogSectionHeading mempty  "Transaction Sender"
      _ <- divClass "group segment" $ mkLabeledClsInput True "Account" $ \_ -> do
        uiSenderFixed $ AccountName $ withSome sender accountRefToName

      let accountsToTrack = getAccounts signing
          localReq = case wrappedCmd of
            Left _e -> []
            Right cmd0 -> pure $ NetworkRequest
              { _networkRequest_cmd = cmd0
              , _networkRequest_chainRef = ChainRef Nothing chainId
              , _networkRequest_endpoint = Endpoint_Local
              }
      responses <- performLocalRead (model ^. logger) (model ^. network) $ localReq <$ pb
      (errors, resp) <- fmap fanThese $ performEvent $ ffor responses $ \case
        [(_, errorResult)] -> parseNetworkErrorResult (model ^. logger) parseWrappedBalanceChecks errorResult
        n -> do
          putLog model LevelWarn $ "Expected 1 response, but got " <> tshow (length n)
          pure $ This "Couldn't get a response from the node"

      dialogSectionHeading mempty "Anticipated Transaction Impact"
      divClass "group segment" $ do
        let tableAttrs = "style" =: "table-layout: fixed; width: 100%" <> "class" =: "table"
        elAttr "table" tableAttrs $ do
          el "thead" $ el "tr" $ do
            let th = elClass "th" "table__heading" . text
            th "Account Name"
            th "Public Key"
            th "Change in Balance"
          accountBalances <- flip Map.traverseWithKey accountsToTrack $ \acc pk -> do
            bal <- holdDyn Nothing $ leftmost [Just Nothing <$ errors, Just . join . Map.lookup acc . fst <$> resp]
            pure (pk, bal)
          el "tbody" $ void $ flip Map.traverseWithKey accountBalances $ \acc (pk, balance) -> el "tr" $ do
            let displayBalance = \case
                  Nothing -> "Loading..."
                  Just Nothing -> "Error"
                  Just (Just b) -> tshow (unAccountBalance b) <> " KDA"
            el "td" $ text $ unAccountName acc
            el "td" $ divClass "wallet__key" $ text $ keyToText pk
            el "td" $ dynText $ displayBalance <$> balance

      dialogSectionHeading mempty "Raw Response"
      void $ divClass "group segment transaction_details__raw-response" $ runWithReplace (text "Loading...") $ leftmost
        [ text . renderCompactText . snd <$> resp
        , text <$> errors
        ]

    getAccounts :: Set (Some AccountRef) -> Map AccountName PublicKey
    getAccounts = Map.mapKeys (\(Some x) -> AccountName $ accountRefToName x) . Map.restrictKeys accs
      where
        accs = flip foldAccounts accounts $ \case
          a@(r :=> _) -> Map.singleton (Some r) $ accountKey a
