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
{-# LANGUAGE TypeApplications #-}

-- | Little widget providing a UI for deployment related settings.
--
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)

module Frontend.UI.DeploymentSettings
  ( -- * Settings
    DeploymentSettingsConfig (..)
  , DeploymentSettingsResult (..)

    -- * Errors
  , DeploymentSettingsResultError (..)
  , renderDeploymentSettingsResultError
    -- * Helpers
  , TxnSenderTitle (..)
  , getTxnSenderTitle
  , buildPayload
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
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Pact.Types.Capability as PC
import qualified Pact.Types.ChainId as Pact
import qualified Pact.Types.Command as Pact
import           Pact.Types.Names (ModuleName)
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
import Frontend.UI.FormWidget
import Frontend.UI.JsonData
import Frontend.UI.KeysetWidget
import Frontend.UI.Modal
import Frontend.UI.TabBar
import Frontend.UI.Widgets
import Frontend.UI.Widgets.Helpers (preventUpAndDownArrow, preventScrollWheel, dialogSectionHeading)
import Frontend.Wallet
import Frontend.UI.Dialogs.Signing.Common (uiSignatures)

-- | Config for the deployment settings widget.
data DeploymentSettingsConfig t m model a = DeploymentSettingsConfig
  { _deploymentSettingsConfig_userTab     :: Maybe (Text, m a)
    -- ^ Some optional extra tab. fst is the tab's name, snd is its content.
  , _deploymentSettingsConfig_chainId     :: model -> m (Dynamic t (Maybe Pact.ChainId))
    -- ^ ChainId selection widget.
  , _deploymentSettingsConfig_sender
    :: model
    -> Dynamic t (Maybe ChainId)
    -> Event t (Maybe AccountName)
    -> m (Dynamic t (Maybe (AccountName, Account)))
    -- ^ Sender selection widget. Use 'uiAccountFixed' or 'uiSenderDropdown'.
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
  , _capabilityInputRow_value :: Dynamic t (Maybe (PublicKey, SigCapability))
  , _capabilityInputRow_pubKey :: Dynamic t (Maybe PublicKey)
  , _capabilityInputRow_cap :: Dynamic t (Either String SigCapability)
  }


data DeploymentSettingsView
  = DeploymentSettingsView_Custom Text -- ^ An optional additonal tab.
  | DeploymentSettingsView_Cfg -- ^ Actual settings like gas price/limit, ...
  | DeploymentSettingsView_Keys -- ^ Select keys for signing the transaction.
  | DeploymentSettingsView_Signing -- ^ Provide a singature or private key for each external public key
  | DeploymentSettingsView_Preview -- ^ Attempt to preview this deployments affect on the chain
  deriving (Eq,Ord)

showSettingsTabName :: DeploymentSettingsView -> Text
showSettingsTabName (DeploymentSettingsView_Custom n) = n
showSettingsTabName DeploymentSettingsView_Keys       = "Sign"
showSettingsTabName DeploymentSettingsView_Preview    = "Preview"
showSettingsTabName DeploymentSettingsView_Signing    = "External Signatures"
showSettingsTabName DeploymentSettingsView_Cfg        = "Configuration"

tabsToShow :: Maybe DeploymentSettingsView -> Bool -> Bool -> [DeploymentSettingsView]
tabsToShow mUserTabName includePreview includeExternalSignatures
  = (maybeToList mUserTabName)
  <> stdTabs
  <> (if includeExternalSignatures then [DeploymentSettingsView_Signing] else [])
  <> (if includePreview then [DeploymentSettingsView_Preview] else [])

  where
    stdTabs = [DeploymentSettingsView_Cfg, DeploymentSettingsView_Keys]

-- | Get the previous view, taking into account the custom user tab.
prevView :: Maybe DeploymentSettingsView -> Bool -> DeploymentSettingsView -> Maybe DeploymentSettingsView
prevView custom showingSigningTab = \case
  DeploymentSettingsView_Custom _ -> Nothing
  DeploymentSettingsView_Cfg -> custom
  DeploymentSettingsView_Keys -> Just DeploymentSettingsView_Cfg
  DeploymentSettingsView_Signing -> Just DeploymentSettingsView_Keys
  DeploymentSettingsView_Preview | showingSigningTab -> Just DeploymentSettingsView_Signing
                                 | otherwise -> Just DeploymentSettingsView_Keys

-- | Get the next view.
nextView :: Bool -> Bool -> DeploymentSettingsView -> Maybe DeploymentSettingsView
nextView includePreviewTab showingSigningTab = \case
  DeploymentSettingsView_Custom _ -> Just DeploymentSettingsView_Cfg
  DeploymentSettingsView_Cfg -> Just DeploymentSettingsView_Keys
  DeploymentSettingsView_Keys | showingSigningTab -> Just DeploymentSettingsView_Signing
                              | includePreviewTab -> Just DeploymentSettingsView_Preview
                              | otherwise -> Nothing
  DeploymentSettingsView_Signing | includePreviewTab -> Just DeploymentSettingsView_Preview
                                 | otherwise -> Nothing
  DeploymentSettingsView_Preview -> Nothing

data DeploymentSettingsResult key = DeploymentSettingsResult
  { _deploymentSettingsResult_sender :: AccountName
  , _deploymentSettingsResult_chainId :: ChainId
  , _deploymentSettingsResult_command :: Pact.Command Text
  }

lookupAccountBalance :: AccountName -> ChainId -> Map AccountName (AccountInfo Account) -> Maybe (AccountStatus AccountBalance)
lookupAccountBalance name chain m = fmap _accountDetails_balance <$> m ^? ix name . accountInfo_chains . ix chain . account_status

-- Overapproximation
data DeploymentSettingsResultError
  = DeploymentSettingsResultError_GasPayerIsNotValid AccountName
  | DeploymentSettingsResultError_NoSenderSelected
  | DeploymentSettingsResultError_NoChainIdSelected
  | DeploymentSettingsResultError_NoNodesAvailable
  | DeploymentSettingsResultError_InvalidJsonData JsonError
  | DeploymentSettingsResultError_NoAccountsOnNetwork
  | DeploymentSettingsResultError_InsufficientFundsOnGasPayer
  | DeploymentSettingsResultError_NoPayloadYet
  deriving Eq

renderDeploymentSettingsResultError :: DeploymentSettingsResultError -> Text
renderDeploymentSettingsResultError = \case
  DeploymentSettingsResultError_GasPayerIsNotValid a -> "Invalid gas payer: " <> unAccountName a
  DeploymentSettingsResultError_NoSenderSelected -> "No sender selected"
  DeploymentSettingsResultError_NoChainIdSelected -> "No chain selected"
  DeploymentSettingsResultError_NoNodesAvailable -> "No nodes available on network"
  DeploymentSettingsResultError_InvalidJsonData err -> "JSON error: " <> showJsonError err
  DeploymentSettingsResultError_NoAccountsOnNetwork -> "No accounts on network"
  DeploymentSettingsResultError_InsufficientFundsOnGasPayer -> "Gas payer does not have sufficient funds"
  DeploymentSettingsResultError_NoPayloadYet -> "Payload has not been built yet"

buildPayload
  :: ( HasNetwork model t
     , HasJsonData model t
     , HasWallet model key t
     , Reflex t
     , Monad (Performable m)
     , MonadJSM (Performable m)
     , HasCrypto key (Performable m)
     )
  => model
  -> Dynamic t (Maybe AccountName)
  -> Dynamic t (Set PublicKey)
  -> Dynamic t (Maybe ChainId)
  -> Dynamic t (Map PublicKey [SigCapability])
  -> Dynamic t TTLSeconds
  -> Dynamic t GasLimit
  -> Dynamic t Text
  -> DeploymentSettingsConfig t m model a
  -> Dynamic t (Either DeploymentSettingsResultError (Performable m (Pact.Payload PublicMeta Text)))
buildPayload m mSender signers cChainId capabilities ttl gasLimit code settings = runExceptT $ do
  selNodes <- lift $ m ^. network_selectedNodes
  networkName <- lift $ m ^. network_selectedNetwork
  fungible <- lift $ m^. wallet_fungible
  networkId <- mkNetworkName . nodeVersion <$> failWith DeploymentSettingsResultError_NoNodesAvailable (headMay $ rights selNodes)

  chainId <- cChainId !? DeploymentSettingsResultError_NoChainIdSelected

  -- Don't require a signer because gas may not be required on private blockchains
  sender <- lift $ fromMaybe (AccountName "") <$> mSender

  caps <- lift capabilities
  signs <- lift signers

  jsonData' <- ExceptT $ over (mapped . _Left) DeploymentSettingsResultError_InvalidJsonData $ m ^. jsonData . to getJsonDataObjectStrict
  ttl' <- lift ttl
  limit <- lift gasLimit
  lastPublicMeta <- lift $ m ^. network_meta
  code' <- lift code
  allAccounts <- failWithM DeploymentSettingsResultError_NoAccountsOnNetwork
    $ Map.lookup networkName . unAccountData <$> m ^. wallet_accounts

  let deploySettingsJsonData = fromMaybe mempty $ _deploymentSettingsConfig_data settings
      publicMeta = lastPublicMeta
        { _pmChainId = chainId
        , _pmGasLimit = limit
        , _pmSender = unAccountName sender
        , _pmTTL = ttl'
        }

  -- Make an effort to ensure the gas payer (if there is one) account has enough balance to actually
  -- pay the gas. This won't work if the user selects an account on a different
  -- chain, but that's another issue.
  gasPayer <- lift mSender
  case gasPayer of
    Nothing -> pure () -- No gas payer selected, move along
    Just gp | (fungible /= kdaToken) ->  pure () -- You can't pay gas with non-kda, and we don't store that data
                                               -- In the future perhaps we can perform a live check
    Just gp ->  for_ (lookupAccountBalance gp chainId allAccounts) $ \case
      -- Gas Payer selected but they're not an account?!
      -- TODO: More precise error types for better (any) user feedback on config tab ?
      AccountStatus_Unknown -> throwError $ DeploymentSettingsResultError_GasPayerIsNotValid gp
      AccountStatus_DoesNotExist -> throwError $ DeploymentSettingsResultError_GasPayerIsNotValid gp
      AccountStatus_Exists b ->
        let GasLimit lim = _pmGasLimit publicMeta
            GasPrice (ParsedDecimal price) = _pmGasPrice publicMeta
        in unless (unAccountBalance b > fromIntegral lim * price) $
             throwError DeploymentSettingsResultError_InsufficientFundsOnGasPayer

  let
    allPublicKeys = Map.keysSet caps <> signs

  return $ buildExecPayload
    (_deploymentSettingsConfig_nonce settings)
    networkId publicMeta
    (Set.toList allPublicKeys <> _deploymentSettingsConfig_extraSigners settings)
    code' (HM.union jsonData' deploySettingsJsonData) caps

-- TODO: are we worried about multiple private keys with the same pubkey key?
splitKeysInAndOutOfStore :: KeyStorage key -> Set PublicKey -> ([KeyPair key], Set PublicKey)
splitKeysInAndOutOfStore keyStore pubKeys = (pubKeysWithPrivateKeys, pubKeysWithoutPrivateKeys)
  where
    kpInWalletMap = Map.fromList . fmap ((\pk -> (_keyPair_publicKey pk, pk)) . _key_pair) . IM.elems $ keyStore
    pubKeysWithPrivateKeys =  Map.elems $ Map.restrictKeys kpInWalletMap pubKeys
    pubKeysWithoutPrivateKeys = pubKeys Set.\\ Map.keysSet kpInWalletMap

buildDeployTabs
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => Maybe DeploymentSettingsView
  -> Bool
  -> Dynamic t Bool
  -> Event t (DeploymentSettingsView -> Maybe DeploymentSettingsView)
  -> m ( Dynamic t DeploymentSettingsView
       , Event t ()
       , Event t DeploymentSettingsView
       )
buildDeployTabs mUserTabName includePreviewTab dShowExternalSignature controls = mdo
  let initTab = fromMaybe DeploymentSettingsView_Cfg mUserTabName
      f thisView g = case g thisView of
        Just view' -> (Just view', Nothing)
        Nothing -> (Nothing, Just ())
  (curSelection, done) <- mapAccumMaybeDyn f initTab $ leftmost
    [ const . Just <$> onTabClick
    , controls
    ]
  (TabBar onTabClick) <- makeTabBarDyn $ TabBarDynCfg
    { _tabBarDynCfg_tabs = tabsToShow mUserTabName includePreviewTab <$> dShowExternalSignature
    , _tabBarDynCfg_mkLabel = const $ text . showSettingsTabName
    , _tabBarDynCfg_selectedTab = Just <$> curSelection
    , _tabBarDynCfg_classes = mempty
    , _tabBarDynCfg_type = TabBarType_Secondary
    }
  pure (curSelection, done, onTabClick)

defaultTabViewProgressButtonLabel :: DeploymentSettingsView -> Text
defaultTabViewProgressButtonLabel DeploymentSettingsView_Preview = "Submit"
defaultTabViewProgressButtonLabel DeploymentSettingsView_Signing = "Sign"
defaultTabViewProgressButtonLabel _ = "Next"

buildDeployTabFooterControls
  :: ( PostBuild t m
     , DomBuilder t m
     )
  => Maybe DeploymentSettingsView
  -> Bool
  -> Behavior t Bool
  -> Dynamic t DeploymentSettingsView
  -> (DeploymentSettingsView -> Text)
  -> Dynamic t Bool
  -> m (Event t (DeploymentSettingsView -> Maybe DeploymentSettingsView))
buildDeployTabFooterControls mUserTabName includePreviewTab bShowExternalSignature curSelection nextBtnTextForTab hasResult = do
  let backConfig = btnCfgTertiary & uiButtonCfg_class .~ ffor curSelection
        (\s -> if s == fromMaybe DeploymentSettingsView_Cfg mUserTabName then "hidden" else "")

      tabToBeDisabled = if includePreviewTab
        then DeploymentSettingsView_Preview
        else DeploymentSettingsView_Keys

      shouldBeDisabled tab hasRes = tab == tabToBeDisabled && hasRes
      isDisabled = shouldBeDisabled <$> curSelection <*> hasResult

  back <- cancelButton backConfig "Back"
  next <- uiButtonDyn
    (def & uiButtonCfg_class .~ "button_type_confirm" & uiButtonCfg_disabled .~ isDisabled)
    $ dynText (nextBtnTextForTab <$> curSelection)

  pure $ leftmost
    [ nextView includePreviewTab <$> bShowExternalSignature <@ next
    , prevView mUserTabName <$> bShowExternalSignature <@ back
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
    , HasTransactionLogger m
    , HasCrypto key m
    )
  => model
  -> DeploymentSettingsConfig t m model a
  -> m (mConf, Event t (DeploymentSettingsResult key), Maybe a)
uiDeploymentSettings m settings = mdo
    let code = _deploymentSettingsConfig_code settings
    (curSelection, done, _) <- buildDeployTabs mUserTabName (_deploymentSettingsConfig_includePreviewTab settings) showSigning controls
    (conf, result, ma, showSigning) <- elClass "div" "modal__main transaction_details" $ do

      mRes <- traverse (uncurry $ tabPane mempty curSelection) mUserTabCfg

      (cfg, cChainId, mSender, ttl, gasLimit, _, _) <- tabPane mempty curSelection DeploymentSettingsView_Cfg $
        uiCfg (Just code) m
          (_deploymentSettingsConfig_chainId settings $ m)
          (_deploymentSettingsConfig_ttl settings)
          (_deploymentSettingsConfig_gasLimit settings)
          Nothing
          TxnSenderTitle_Default
          (Just advancedAccordion)
          (_deploymentSettingsConfig_sender settings)

      (signers, capabilities) <- tabPane mempty curSelection DeploymentSettingsView_Keys $ do
        uiSenderCapabilities m (_deploymentSettingsConfig_caps settings)

      let dErrorOrPayload = buildPayload m mSender signers cChainId capabilities ttl gasLimit code settings

      ePayload <- performEvent . snd . fanEither $ updated dErrorOrPayload

      let
        publicKeys = signers <> (Map.keysSet <$> capabilities)
        eUpdatedSignaturesUi = current (uiSignatures <$> (m ^.wallet_keys) <*> (fmap Set.toList publicKeys)) <@> ePayload
        payloadContainsExternalKeys = fmap (not . Set.null . snd) $ splitKeysInAndOutOfStore <$> (m ^. wallet_keys) <*> publicKeys

      res <- tabPane mempty curSelection DeploymentSettingsView_Signing $ do
        -- TODO initial condition seems like an error that should not exist.
        dErrorOrCommand <- fmap join . networkHold
          (return . constDyn $ Left DeploymentSettingsResultError_NoPayloadYet)
          $ (fmap . fmap . fmap) Right eUpdatedSignaturesUi

        -- Doing these checks again seems wrong, we have another runExceptT in buildPayload with these same checks.
        return . runExceptT $ do
          -- Only doing this becuase we need to bring over the errors from buildPayload
          -- Might not have to if this code was in buildPayload
          erorrOrPayload <- lift dErrorOrPayload
          case erorrOrPayload of
            Left e -> throwError e
            Right _ -> return ()

          v <- ExceptT $ dErrorOrCommand
          chainId <- cChainId !? DeploymentSettingsResultError_NoChainIdSelected
          sender <- lift $ fromMaybe (AccountName "") <$> mSender
          return $ DeploymentSettingsResult
            { _deploymentSettingsResult_sender = sender
            , _deploymentSettingsResult_chainId = chainId
            , _deploymentSettingsResult_command = v
            }

      when (_deploymentSettingsConfig_includePreviewTab settings) $ tabPane mempty curSelection DeploymentSettingsView_Preview $ do
        let currentNode = headMay . rights <$> (m ^. network_selectedNodes)
            mNetworkId = ffor currentNode $ fmap $ mkNetworkName . nodeVersion

            accounts = liftA2 (Map.findWithDefault mempty) (m ^. network_selectedNetwork) (unAccountData <$> m ^. wallet_accounts)
            mHeadAccount = fmap fst . Map.lookupMin <$> accounts
            mHeadChain = (headMay =<<) . fmap getChains <$> currentNode

            aSender = (<|>) <$> mSender <*> mHeadAccount
            aChainId = (<|>) <$> cChainId <*> mHeadChain

            uiPreviewPane = uiDeployPreview m settings
              <$> (m ^. wallet_keys)
              <*> signers
              <*> gasLimit
              <*> ttl
              <*> code
              <*> (m ^. network_meta)
              <*> capabilities
              <*> (m ^. jsonData . to getJsonDataObjectStrict)
              <*> (preview _Left <$> res)
              <*> mNetworkId
              <*> aChainId
              <*> aSender

        dyn_ $ curSelection >>= \case
          DeploymentSettingsView_Preview -> uiPreviewPane
          _ -> constDyn blank

      pure
        ( cfg & networkCfg_setSender .~ fmapMaybe (fmap unAccountName) (updated mSender)
        , res
        , mRes
        , payloadContainsExternalKeys
        )
    let
      command = tagMaybe (current $ fmap hush result) done
    controls <- modalFooter $ buildDeployTabFooterControls
      mUserTabName
      (_deploymentSettingsConfig_includePreviewTab settings)
      (current showSigning)
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
  -> m (mConf, Dynamic t TTLSeconds, Dynamic t GasLimit, Dynamic t GasPrice)
uiDeployMetaData m mTTL mGasLimit = do
  dialogSectionHeading mempty "Settings"
  elKlass "div" ("group segment") $ uiMetaData m mTTL mGasLimit

data TxnSenderTitle
  = TxnSenderTitle_Default
  | TxnSenderTitle_GasPayer
  | TxnSenderTitle_Other Text

getTxnSenderTitle :: TxnSenderTitle -> Text
getTxnSenderTitle = \case
  TxnSenderTitle_Default -> "Transaction Sender"
  TxnSenderTitle_GasPayer -> "Gas Payer"
  TxnSenderTitle_Other t -> t

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
  -> TxnSenderTitle
  -> Maybe (model -> Dynamic t Bool -> m (Event t (), ((), mConf)))
  -> ( model
       -> Dynamic t (Maybe ChainId)
       -> Event t (Maybe AccountName)
       -> m (Dynamic t (Maybe (AccountName, Account)))
     )
  -> m ( mConf
       , Dynamic t (Maybe Pact.ChainId)
       , Dynamic t (Maybe AccountName)
       , Dynamic t TTLSeconds
       , Dynamic t GasLimit
       , Dynamic t GasPrice
       , g a
       )
uiCfg mCode m wChainId mTTL mGasLimit userSections txnSenderTitle otherAccordion mSenderSelect = do
  -- General deployment configuration
  let mkGeneralSettings = do
        traverse_ (\c -> transactionInputSection c Nothing) mCode
        cId <- uiDeployDestination m wChainId

        dialogSectionHeading mempty $ getTxnSenderTitle txnSenderTitle
        mSender <- elKlass "div" ("group segment") $ mkLabeledClsInput True "Account" $ \_ -> do
          (fmap . fmap) fst <$> mSenderSelect m cId never
          -- (a,_) <- accountNameFormWidget noValidation $ mkCfg Nothing
          --            & primFormWidgetConfig_initialAttributes .~ ("class" =: "labeled-input__input")
          -- return $ value a

        -- Customisable user provided UI section
        fa <- for userSections $ \(title, body) -> do
          dialogSectionHeading mempty title
          elKlass "div" ("group segment") body

        (cfg, ttl, gasLimit, gasPrice) <- uiDeployMetaData m mTTL mGasLimit
        pure (cfg, cId, mSender, ttl, gasLimit, gasPrice, fa)

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
  mkLabeledInput True "Request Key" (\c -> uiInputElement $ c & initialAttributes %~ Map.insert "disabled" "") $ def
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
       , MonadJSM m, MonadJSM (Performable m)
       , PerformEvent t m
       , GhcjsDomSpace ~ DomBuilderSpace m
       )
  => model
  -> Maybe TTLSeconds
  -> Maybe GasLimit
  -> m ( mConf
       , Dynamic t TTLSeconds
       , Dynamic t GasLimit
       , Dynamic t GasPrice
       )
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
      gasPriceInputBox setExternal conf = fmap (view _3) $ uiGasPriceInputField never $ conf
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
    gasPrice <- holdDyn defaultTransactionGasPrice $ leftmost [pbGasPrice, onGasPrice]

    let initGasLimit = fromMaybe defaultTransactionGasLimit mGasLimit
    pbGasLimit <- case mGasLimit of
      Just _ -> pure never
      Nothing -> tag (current $ fmap _pmGasLimit $ m ^. network_meta) <$> getPostBuild

    let
      mkGasLimitInput
        :: InputElementConfig EventResult t (DomBuilderSpace m)
        -> m (Event t Integer)
      mkGasLimitInput conf = dimensionalInputFeedbackWrapper (Just "Units") $ do
        (i, e) <- uiIntInputElement (Just 0) (Just chainwebGasLimitMaximum) $ conf
          & inputElementConfig_initialValue .~ showGasLimit initGasLimit
          & inputElementConfig_setValue .~ fmap showGasLimit pbGasLimit
          & inputElementConfig_elementConfig . elementConfig_eventSpec %~ preventUpAndDownArrow @m
        preventScrollWheel $ _inputElement_raw i
        pure e

    onGasLimit <- (fmap . fmap) (GasLimit . ParsedInteger) $ mkLabeledInput True "Gas Limit" mkGasLimitInput def

    gasLimit <- holdDyn initGasLimit $ leftmost [onGasLimit, pbGasLimit]

    let mkTransactionFee c = fmap (view _1) $ uiGasPriceInputField never $ c
          & initialAttributes %~ Map.insert "disabled" ""

    _ <- mkLabeledInputView True "Max Transaction Fee"  mkTransactionFee $
      ffor (m ^. network_meta) $ \pm -> showGasPrice $ fromIntegral (_pmGasLimit pm) * _pmGasPrice pm

    pbTTL <- case mTTL of
      Just _ -> pure never
      Nothing -> tag (current $ fmap _pmTTL $ m ^. network_meta) <$> getPostBuild
    let secondsInDay = 60 * 60 * 24
        maxTTL = secondsInDay * 2
        minTTL = 60
        prettyTTL s = tshow s <> case s of
          1 -> " second"
          _ -> " seconds"

        initTTL = fromMaybe defaultTransactionTTL mTTL
        ttlInput cls = elKlass "div" cls $ mdo
          let conf = def
                & inputElementConfig_initialValue .~ showTtl initTTL
          sliderEl <- uiSlider "" (text $ prettyTTL minTTL) (text "1 day") $ conf
            & initialAttributes .~ "min" =: (tshow minTTL) <> "max" =: T.pack (show secondsInDay) <> "step" =: "1"
            & inputElementConfig_setValue .~ leftmost [_inputElement_input inputEl, showTtl <$> pbTTL]
          (inputEl, inputEv) <- dimensionalInputFeedbackWrapper (Just "Seconds") $ uiIntInputElement (Just minTTL) (Just maxTTL) $ conf
            & inputElementConfig_setValue .~ leftmost [_inputElement_input sliderEl, showTtl <$> pbTTL]
            & inputElementConfig_elementConfig . elementConfig_eventSpec %~ preventUpAndDownArrow @m
          preventScrollWheel $ _inputElement_raw inputEl
          pure $ leftmost
            [ TTLSeconds . ParsedInteger <$> inputEv
            , fmapMaybe (readPact (TTLSeconds . ParsedInteger)) $ _inputElement_input sliderEl
            ]

    horizontalDashedSeparator

    onTTL <- divClass "deploy-meta-cfg__request-expires"
      $ mkLabeledClsInput True "Request Expires (TTL)" ttlInput
    ttl <- holdDyn initTTL $ leftmost [onTTL, pbTTL]
    pure
      ( mempty
        & networkCfg_setGasPrice .~ onGasPrice
        & networkCfg_setGasLimit .~ onGasLimit
        & networkCfg_setTTL .~ onTTL
      , ttl
      , gasLimit
      , gasPrice
      )

  where

      shiftGP :: GasPrice -> GasPrice -> GasPrice -> GasPrice -> GasPrice -> GasPrice
      shiftGP oldMin oldMax newMin newMax x =
        let GasPrice (ParsedDecimal gp) = (newMax-newMin)/(oldMax-oldMin)*(x-oldMin)+newMin
         in GasPrice $ ParsedDecimal $ roundTo maxCoinPrecision gp

      slowGasPrice = 1e-8
      fastGasPrice = 1e-5

      scaleTxnSpeedToGP :: GasPrice -> GasPrice
      scaleTxnSpeedToGP = shiftGP 1 1001 slowGasPrice fastGasPrice

      scaleGPtoTxnSpeed :: GasPrice -> GasPrice
      scaleGPtoTxnSpeed = shiftGP slowGasPrice fastGasPrice 1 1001

      parseGasPrice :: Text -> Maybe GasPrice
      parseGasPrice t = GasPrice . ParsedDecimal . roundTo maxCoinPrecision <$> readMay (T.unpack t)

      showGasLimit :: GasLimit -> Text
      showGasLimit (GasLimit (ParsedInteger i)) = tshow i

      showGasPrice :: GasPrice -> Text
      showGasPrice (GasPrice (ParsedDecimal i)) = tshow i

      showTtl :: TTLSeconds -> Text
      showTtl (TTLSeconds (ParsedInteger i)) = tshow i

      readPact wrapper =  fmap wrapper . readMay . T.unpack

-- | Let the user pick signers
uiSignerList
  :: (MonadWidget t m, HasWallet model key t)
  => model
  -> Dynamic t (Map PublicKey [SigCapability])
  -> m (Dynamic t (Set PublicKey))
uiSignerList m dCapMap = do
  dialogSectionHeading mempty "Unrestricted Signing Keys"
  divClass "group signing-ui-signers" $ do
    let
      dKeysWithCaps = ffor dCapMap $ \capMap -> Map.keysSet . Map.filter (not . null) $ capMap

      selectMsgKey = PublicKeyText ""
      doAddDel yesno = fmap (yesno selectMsgKey) . updated

      pkt2kp (PublicKeyText "") = Nothing
      pkt2kp (PublicKeyText keyText) = textToKey keyText

      -- TODO: Dont use keys that have capabilities, indicate that those keys cant/wont be used or signers
    dKeyInputMap <- growingList
      (const $ pubKeyFormWidget (m ^. wallet_keys))
      (AllowAddNewRow $ doAddDel (/=) . _formWidget_value)
      (AllowDeleteRow $ doAddDel (==) . _formWidget_value)
      selectMsgKey
      never
      (mkCfg mempty)
    let
      keys = Set.fromList . IM.elems . IM.mapMaybe pkt2kp <$> joinDynThroughIntMap (value <$$> dKeyInputMap)
    return $ Set.difference <$> keys <*> dKeysWithCaps
    -- pure $ Map.keysSet . Map.filter id <$> joinDynThroughMap results

parseSigCapability :: Text -> Either String SigCapability
parseSigCapability txt = parsed >>= compiled >>= parseApp
  where
    parseApp ts = case ts of
      [(TApp (App (TVar (QName q) _) as _) _)] -> SigCapability q <$> mapM toPV as
      _ -> Left $ "Sig capability parse failed: Expected single qualified capability in form (qual.DEFCAP arg arg ...)"
    compiled parsedPactCode = fmapL (("Sig capability parse failed: " ++) . show) $
      compileExps (mkTextInfo $ Pact._pcCode parsedPactCode) (Pact._pcExps parsedPactCode)
    parsed = parsePact txt
    toPV a = fmapL (("Sig capability argument parse failed, expected simple pact value: " ++) . T.unpack)
      $ toPactValue a

-- | Display a single row for the user to enter a custom capability and
-- account to attach
capabilityInputRow
  :: forall t m
     . MonadWidget t m
  => Maybe DappCap
  -> m (Dynamic t (Maybe PublicKey))
  -> m (CapabilityInputRow t)
capabilityInputRow mCap keyPairSelector = elClass "tr" "table__row" $ do
  let
    uiCapInput cfg0 = do
      i <- uiInputElement cfg0
      pure ( i
           , ( parseSigCapability <$> value i
             , _inputElement_input i
             )
           )

    showCapPopover (_, (_, onInput)) = pure $ ffor onInput $ \case
      "" -> PopoverState_Disabled
      inp -> case parseSigCapability inp of
        Left e -> PopoverState_Error $ T.pack e
        Right _ -> PopoverState_Disabled

  (emptyCap, parsed) <- elClass "td" "table__cell_padded" $ mdo
    (cap, (parsed, _)) <- uiInputWithPopover uiCapInput (_inputElement_raw . fst) showCapPopover $ def
      & inputElementConfig_initialValue .~ foldMap (renderCompactText . _dappCap_cap) mCap
      & initialAttributes .~ (
        let (cls, dis) = maybe
              mempty
              (const (" input_transparent grant-capabilities-static-input", "disabled" =: "true"))
              mCap
        in mconcat
          [ "placeholder" =: "(module.capability arg1 arg2)"
          , "class" =: ("input_width_full" <> cls)
          , dis
          ]
        )

    emptyCap <- holdUniqDyn $ T.null <$> value cap

    pure (emptyCap, parsed)
  dkp <- elClass "td" "table__cell_padded" keyPairSelector

  pure $ CapabilityInputRow
    { _capabilityInputRow_empty = emptyCap
    , _capabilityInputRow_value = emptyCap >>= \case
      True -> pure Nothing
      False -> runMaybeT $ do
        kp <- MaybeT dkp
        p <- MaybeT $ either (const Nothing) pure <$> parsed
        pure (kp, p)
    , _capabilityInputRow_pubKey = dkp
    , _capabilityInputRow_cap = parsed
    }

-- | Display a dynamic number of rows for the user to enter custom capabilities
capabilityInputRows
  :: forall t m. MonadWidget t m
  => Event t ()  -- Add new row
  -> m (Dynamic t (Maybe PublicKey))
  -> m (Dynamic t (Map PublicKey [SigCapability]), Dynamic t Int)
capabilityInputRows addNew keysSelector = do
  rec
    (im0, im') <- traverseIntMapWithKeyWithAdjust (\_ _ -> capabilityInputRow Nothing keysSelector) IM.empty $ leftmost
      [ -- Add a new row when all rows are used
        attachWith (\i _ -> PatchIntMap (IM.singleton i (Just ()))) nextKeyToUse addNew
      ]
    results :: Dynamic t (IntMap (CapabilityInputRow t))
      <- foldDyn applyAlways im0 im'
    let nextKeyToUse = maybe 0 (succ . fst) . IM.lookupMax <$> current results
        mkSingleton = fmap $ maybe Map.empty $ \(a,b) -> a =: [b]

  pure ( fmap (Map.unionsWith (<>) . IM.elems) $ traverse (mkSingleton . _capabilityInputRow_value) =<< results
       , fmap length results
       )

-- | Widget for selection of sender and signing keys.
uiSenderCapabilities
  :: forall key t m model. (MonadWidget t m, HasWallet model key t)
  => model
  -> Maybe [DappCap]
  -> m (Dynamic t (Set PublicKey), Dynamic t (Map PublicKey [SigCapability]))
uiSenderCapabilities m mCaps = do
  let
      -- TODO: With the uiKeyPairDropdown it the user can select an empty item.
      -- Now no empty item shows, but the user can delete all the text instead.
      -- Also, once item is selected the others dont show becuase its a combobox thing, might be ok idk
      pubKeyInput :: Event t PublicKey -> m (Dynamic t (Maybe PublicKey))
      pubKeyInput ev = do
        let
          pkt2kp (PublicKeyText "") = Nothing
          pkt2kp (PublicKeyText keyText) = textToKey keyText

          cfg = (mkCfg $ PublicKeyText "")
            & formWidgetConfig_setValue .~ Just ((PublicKeyText . keyToText) <$> ev)

        wgt <- pubKeyFormWidget (m ^. wallet_keys) cfg

        return $ pkt2kp <$> _formWidget_value wgt

      staticCapabilityRow dd cap = do
        elClass "td" "grant-capabilities-static-row__wrapped-cell" $ text $ _dappCap_role cap
        elClass "td" "grant-capabilities-static-row__wrapped-cell" $ text $ renderCompactText $ _dappCap_cap cap
        dkp <- el "td" dd
        pure $ CapabilityInputRow
          { _capabilityInputRow_empty = pure False
          , _capabilityInputRow_value = fmap (\kp -> (kp, _dappCap_cap cap)) <$> dkp
          , _capabilityInputRow_pubKey = dkp
          , _capabilityInputRow_cap = pure $ Right $ _dappCap_cap cap
          }

      staticCapabilityRows dd caps = fmap combineMaps $ for caps $ \cap ->
        elClass "tr" "table__row" $ (mkSingleton . _capabilityInputRow_value)
          <$> staticCapabilityRow dd cap

      mkSingleton = fmap $ maybe mempty $ \(a,b) -> a =: [b]

      combineMaps :: (Semigroup v, Ord k) => [Dynamic t (Map k v)] -> Dynamic t (Map k v)
      combineMaps = fmap (Map.unionsWith (<>)) . sequence

  eAddCap <- divClass "grant-capabilities-title" $ do
    dialogSectionHeading "grant-capabilities-title__title" "Grant Capabilities"
    case mCaps of
      Nothing -> addButton (def & uiButtonCfg_class <>~ " grant-capabilities-title__add-button")
      Just _  -> pure never

  -- Capabilities
  capabilities <- divClass "group" $ mdo
    (mGasCapKey, capabilities', rowCount) <- elAttr "table" ("class" =: "table" <> "style" =: "width: 100%; table-layout: fixed;") $ case mCaps of
      Nothing -> do
        el "thead" $ el "tr" $ do
          elClass "th" "table__heading table__cell_padded" $ text "Capability"
          elClass "th" "table__heading table__cell_padded" $ text "Signing Key"
        el "tbody" $ do
          gas <- capabilityInputRow (Just defaultGASCapability) (pubKeyInput never)
          (rest, restCount) <- capabilityInputRows eAddCap (pubKeyInput eApplyToAll)
          pure ( _capabilityInputRow_pubKey gas
               , combineMaps [(mkSingleton $ _capabilityInputRow_value gas), rest]
               , fmap (1+) restCount
               )
      Just caps -> do
        el "thead" $ el "tr" $ do
          elAttr "th" ("class" =: "table__heading" <> "width" =: "23%") $ text "Role"
          elAttr "th" ("class" =: "table__heading") $ text "Capability"
          elAttr "th" ("class" =: "table__heading" <> "width" =: "30%") $ text "Public Key"
        el "tbody" $ do
          gas <- staticCapabilityRow (pubKeyInput never) defaultGASCapability
          rest <- staticCapabilityRows (pubKeyInput eApplyToAll) $ filter (not . isGas . _dappCap_cap) caps
          pure ( _capabilityInputRow_pubKey gas
               , combineMaps [(mkSingleton $ _capabilityInputRow_value gas),rest]
               , constDyn (1 {- Gas payer -} + length caps)
               )

    -- If the gas capability is set, we enable the button that will set every other
    -- capability's sender from the gas account.
    eApplyToAll <- (switchHold never =<<) $ dyn $ ffor rowCount $ \n ->
      if n > 1 then do
        eAllGasPayer <- divClass "grant-capabilities-apply-all-wrapper" $ uiButtonDyn
          (btnCfgSecondary
            & uiButtonCfg_disabled .~ (isNothing <$> mGasCapKey)
            & uiButtonCfg_class .~ (constDyn $ "grant-capabilities-apply-all")
          )
          (text "Apply to all")

        pure $ fmapMaybe id $ current mGasCapKey <@ eAllGasPayer
      else
        pure never

    pure capabilities'

  signers <- uiSignerList m capabilities

  pure (signers, capabilities)

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
     , HasWallet model key t
     , HasTransactionLogger m
     )
  => model
  -> DeploymentSettingsConfig t m model a
  -> KeyStorage key
  -> Set PublicKey
  -> GasLimit
  -> TTLSeconds
  -> Text
  -> PublicMeta
  -> Map PublicKey [SigCapability]
  -> Either JsonError Aeson.Object
  -> Maybe DeploymentSettingsResultError
  -> Maybe NetworkName
  -> Maybe ChainId
  -> Maybe AccountName
  -> m ()
uiDeployPreview _ _ _ _ _ _ _ _ _ _ _ _ _ Nothing = text "No valid GAS payer accounts in Wallet."
uiDeployPreview _ _ _ _ _ _ _ _ _ _ _ _ Nothing _ = text "Please select a Chain."
uiDeployPreview _ _ _ _ _ _ _ _ _ _ _ Nothing _ _ = text "No network nodes configured."
uiDeployPreview model settings keyStore signers gasLimit ttl code lastPublicMeta capabilities jData resultError (Just networkId) (Just chainId) (Just sender) = do
  pb <- getPostBuild

  let deploySettingsJsonData = fromMaybe mempty $ _deploymentSettingsConfig_data settings
      jsonData0 = fromMaybe mempty $ hush jData
      -- TODO: If we have PublicKeys that are not in the keyStore then signingPairs does not contain all the keys in the Paylaod!
      signingPairs = fst $ splitKeysInAndOutOfStore keyStore (Map.keysSet capabilities <> signers)

      isChainwebNode (NodeType_Pact _) = False
      isChainwebNode (NodeType_Chainweb _) = True

      dIsChainWebNode = maybe False (isChainwebNode . _nodeInfo_type) . headMay . rights
        <$> model ^. network_selectedNodes

  let publicMeta = lastPublicMeta
        { _pmChainId = chainId
        , _pmGasLimit = gasLimit
        , _pmSender = unAccountName sender
        , _pmTTL = ttl
        }

      nonce = _deploymentSettingsConfig_nonce settings
      extraSigners = _deploymentSettingsConfig_extraSigners settings
      jsondata = HM.union jsonData0 deploySettingsJsonData

  let
    mkBuildCmd code0 = buildCmd nonce networkId publicMeta signingPairs
      extraSigners code0 jsondata capabilities

  eCmds <- performEvent $ ffor (current dIsChainWebNode <@ pb) $ \onChainweb -> do
    c <- mkBuildCmd code
    wc <-
      if onChainweb then
        Just <$> for (wrapWithBalanceChecks (Set.singleton sender) code) mkBuildCmd
      else
        pure Nothing
    pure (c, wc)

  void $ runWithReplace
    (text "Preparing transaction preview...")
    ( uiPreviewResponses
      <$> current (model ^. network_selectedNetwork)
      <*> current (model ^. wallet_accounts)
      <*> current dIsChainWebNode
      <@> eCmds
    )
  where
    uiPreviewResponses networkName accountData isChainwebNode (cmd, mWrappedCmd) = do
      pb <- getPostBuild

      for_ resultError $ \err -> do
        dialogSectionHeading mempty "Error"
        divClass "group segment" $ mkLabeledView True mempty
          $ text $ renderDeploymentSettingsResultError err
        text "Cannot submit. Preview will try to fill in the blanks with default values"

      unless (any (any isGas) capabilities) $ do
        dialogSectionHeading mempty "Notice"
        divClass "group segment" $ mkLabeledView True mempty
          $ text "A 'Gas Payer' has not been selected for this transaction. Are you sure this is correct?"

      transactionInputSection (pure code) (pure cmd)
      dialogSectionHeading mempty  "Destination"
      _ <- divClass "group segment" $ do
        transactionDisplayNetwork model
        predefinedChainIdDisplayed chainId

      dialogSectionHeading mempty  "Transaction Sender"
      _ <- divClass "group segment" $ mkLabeledClsInput True "Account" $ \_ -> do
        uiAccountFixed sender

      let accountsToTrack = getAccounts networkName accountData
          localReq =
            if isChainwebNode then
              case mWrappedCmd of
                Nothing -> []
                Just (Left _e) -> []
                Just (Right cmd0) -> pure $ NetworkRequest
                  { _networkRequest_cmd = cmd0
                  , _networkRequest_chainRef = ChainRef Nothing chainId
                  , _networkRequest_endpoint = Endpoint_Local
                  }
            else
              pure $ NetworkRequest
                { _networkRequest_cmd = cmd
                , _networkRequest_chainRef = ChainRef Nothing chainId
                , _networkRequest_endpoint = Endpoint_Local
                }

          parseChainwebWrapped =
            if isChainwebNode then
              parseWrappedBalanceChecks
            else
              -- Non-chainweb nodes won't have the expected contracts to utilise wrapped
              -- balance checks, so we don't know what structure to expect here.
              -- Kuro returns a (PLiteral (LString ...))
              -- Chainweb returns a (PObject ...)
              \pv -> Right (fmap (const Nothing) accountsToTrack, pv)

      responses <- performLocalRead (model ^. logger) (model ^. network) $ localReq <$ pb
      (errors, resp) <- fmap fanThese $ performEvent $ ffor responses $ \case
        [(_, errorResult)] -> parseNetworkErrorResult
          (model ^. logger)
          parseChainwebWrapped
          errorResult
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
          accountBalances <- flip Map.traverseWithKey accountsToTrack $ \acc pks -> do
            bal <- holdDyn Nothing $ leftmost
              [ Just Nothing <$ errors
              , Just . join . Map.lookup acc . fst <$> resp
              ]
            pure (pks, bal)
          el "tbody" $ void $ flip Map.traverseWithKey accountBalances $ \acc (pks, balance) -> el "tr" $ do
            let displayBalance = \case
                  Nothing -> "Loading..."
                  Just Nothing -> "Error"
                  Just (Just b) -> tshow (unAccountBalance b) <> " KDA"

                wrapEllipsis =
                  elClass "div" "preview-acc-key" . text

            el "td" $ wrapEllipsis $ unAccountName acc
            el "td" $ for_ pks $ \pk -> divClass "wallet__key" $ wrapEllipsis $ keyToText pk
            el "td" $ dynText $ displayBalance <$> balance

      dialogSectionHeading mempty "Raw Response"
      void $ divClass "group segment transaction_details__raw-response"
        $ runWithReplace (text "Loading...") $ leftmost
        [ text . renderCompactText . snd <$> resp
        , text <$> errors
        ]

    getAccounts :: NetworkName -> AccountData -> Map AccountName (Set PublicKey)
    getAccounts net (AccountData m) = fromMaybe mempty $ do
      allAccounts <- Map.lookup net m
      let accs = Map.restrictKeys allAccounts (Set.singleton sender)
      pure $ ffor accs $ \info -> case Map.lookup chainId $ _accountInfo_chains info of
        Just status | AccountStatus_Exists details <- _account_status status
          -> details ^. accountDetails_guard . _AccountGuard_KeySetLike . ksh_keys
        _ -> mempty
