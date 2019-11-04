{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE RecordWildCards       #-}

-- | Little widget providing a UI for deployment related settings.
--
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)

module Frontend.UI.DeploymentSettings
  ( -- * Settings
    DeploymentSettingsConfig (..)
  , DeploymentSettingsResult (..)
    -- * Values for _deploymentSettingsConfig_chainId:
  , predefinedChainIdSelect
  , predefinedChainIdDisplayed
  , userChainIdSelect
  , uiChainSelection
    -- * Widgets
  , uiDeploymentSettings
  , uiSigningKeys
  , uiSenderFixed
  , uiSenderDropdown
  , transactionInputSection
  , transactionHashSection
  , transactionDisplayNetwork
    -- * Useful re-exports
  , Identity (runIdentity)
  ) where

import Control.Applicative (liftA2, liftA3)
import Control.Arrow (first, (&&&))
import Control.Error (fmapL, hoistMaybe, headMay)
import Control.Error.Util (hush)
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Either (rights, isLeft)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Traversable (for)
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
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Pact.Types.ChainId as Pact
import qualified Pact.Types.Command as Pact

import Common.Network
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
import qualified Frontend.AppCfg as AppCfg

import qualified Pact.Types.Capability as PC
import qualified Pact.Types.Names as PN
import qualified Pact.Types.Info as PI

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
  , _deploymentSettingsConfig_sender :: model -> Dynamic t (Maybe ChainId) -> m (Dynamic t (Maybe AccountName))
    -- ^ Sender selection widget. Use 'uiSenderFixed' or 'uiSenderDropdown'.
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
  deriving (Eq,Ord)

showSettingsTabName :: DeploymentSettingsView -> Text
showSettingsTabName (DeploymentSettingsView_Custom n) = n
showSettingsTabName DeploymentSettingsView_Keys       = "Sign"
showSettingsTabName DeploymentSettingsView_Cfg        = "Configuration"

-- | Get the previous view, taking into account the custom user tab.
prevView :: Maybe DeploymentSettingsView -> DeploymentSettingsView -> Maybe DeploymentSettingsView
prevView custom = \case
  DeploymentSettingsView_Custom _ -> Nothing
  DeploymentSettingsView_Cfg -> custom
  DeploymentSettingsView_Keys -> Just DeploymentSettingsView_Cfg

-- | Get the next view.
nextView :: DeploymentSettingsView -> Maybe DeploymentSettingsView
nextView = \case
  DeploymentSettingsView_Custom _ -> Just DeploymentSettingsView_Cfg
  DeploymentSettingsView_Cfg -> Just DeploymentSettingsView_Keys
  DeploymentSettingsView_Keys -> Nothing

data DeploymentSettingsResult = DeploymentSettingsResult
  { _deploymentSettingsResult_gasPrice :: GasPrice
  , _deploymentSettingsResult_signingKeys :: [KeyPair]
  , _deploymentSettingsResult_sender :: AccountName
  , _deploymentSettingsResult_chainId :: ChainId
  , _deploymentSettingsResult_code :: Text
  , _deploymentSettingsResult_command :: Pact.Command Text
  }

-- | Show settings related to deployments to the user.
--
--
--   the right keys, ...
uiDeploymentSettings
  :: forall t m model mConf a
  . ( MonadWidget t m, HasNetwork model t, HasWallet model t
    , Monoid mConf , HasNetworkCfg mConf t
    , HasJsonDataCfg mConf t, Flattenable mConf t, HasJsonData model t
    )
  => model
  -> DeploymentSettingsConfig t m model a
  -> m (mConf, Event t DeploymentSettingsResult, Maybe a)
uiDeploymentSettings m settings = mdo
    let code = _deploymentSettingsConfig_code settings
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
    (conf, result, ma) <- elClass "div" "modal__main transaction_details" $ do

      mRes <- traverse (uncurry $ tabPane mempty curSelection) mUserTabCfg

      (cfg, cChainId, ttl, gasLimit) <- tabPane mempty curSelection DeploymentSettingsView_Cfg $
        uiCfg code m
          (_deploymentSettingsConfig_chainId settings $ m)
          (_deploymentSettingsConfig_ttl settings)
          (_deploymentSettingsConfig_gasLimit settings)

      (mSender, capabilities) <- tabPane mempty curSelection DeploymentSettingsView_Keys $
        uiSenderCapabilities m cChainId (_deploymentSettingsConfig_caps settings)
          $ (_deploymentSettingsConfig_sender settings) m cChainId

      let result' = runMaybeT $ do
            selNodes <- lift $ m ^. network_selectedNodes
            networkId <- hoistMaybe $ hush . mkNetworkName . nodeVersion =<< headMay (rights selNodes)
            sender <- MaybeT mSender
            chainId <- MaybeT cChainId
            caps <- lift capabilities
            let signing = Set.mapMonotonic unAccountName . Set.insert sender $ Map.keysSet caps
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
            keys <- lift $ m ^. wallet_keys
            let toPublicKey (AccountName acc, cs) = do
                  KeyPair pk _ <- Map.lookup acc keys
                  pure (pk, cs)
                pkCaps = Map.fromList . fmapMaybe toPublicKey $ Map.toList caps
            pure $ do
              let signingPairs = getSigningPairs signing keys
              cmd <- buildCmd
                (_deploymentSettingsConfig_nonce settings)
                networkId publicMeta signingPairs
                (_deploymentSettingsConfig_extraSigners settings)
                code' jsonData' pkCaps
              pure $ DeploymentSettingsResult
                { _deploymentSettingsResult_gasPrice = _pmGasPrice publicMeta
                , _deploymentSettingsResult_signingKeys = signingPairs
                , _deploymentSettingsResult_sender = sender
                , _deploymentSettingsResult_chainId = chainId
                , _deploymentSettingsResult_command = cmd
                , _deploymentSettingsResult_code = code'
                }
      pure
        ( cfg & networkCfg_setSender .~ fmapMaybe (fmap unAccountName) (updated mSender)
        , result'
        , mRes
        )
    command <- performEvent $ tagMaybe (current result) done

    controls <- modalFooter $ do
      let backConfig = def & uiButtonCfg_class .~ ffor curSelection
            (\s -> if s == fromMaybe DeploymentSettingsView_Cfg mUserTabName then "hidden" else "")
      back <- uiButtonDyn backConfig $ text "Back"
      let shouldBeDisabled tab res = tab == DeploymentSettingsView_Keys && isNothing res
          isDisabled = shouldBeDisabled <$> curSelection <*> result
      next <- uiButtonDyn (def & uiButtonCfg_class .~ "button_type_confirm" & uiButtonCfg_disabled .~ isDisabled) $ dynText $ ffor curSelection $ \case
        DeploymentSettingsView_Keys -> "Preview"
        _ -> "Next"
      pure $ leftmost
        [ nextView <$ next
        , prevView mUserTabName <$ back
        ]

    pure (conf, command, ma)
    where
      mUserTabCfg  = first DeploymentSettingsView_Custom <$> _deploymentSettingsConfig_userTab settings
      mUserTabName = fmap fst mUserTabCfg
      userTabs = maybeToList mUserTabName
      stdTabs = [DeploymentSettingsView_Cfg, DeploymentSettingsView_Keys]
      availableTabs = userTabs <> stdTabs


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
  _ <- mkLabeledInput uiInputElement "Chain ID" $ def
    & initialAttributes %~ Map.insert "disabled" ""
    & inputElementConfig_initialValue .~ _chainId cid
  pure $ pure $ pure cid

-- | Let the user pick a chain id.
userChainIdSelect
  :: (MonadWidget t m, HasNetwork model t
     )
  => model
  -> m (MDynamic t Pact.ChainId)
userChainIdSelect m = mkLabeledClsInput (uiChainSelection mNodeInfo) "Chain ID"
  where
    mNodeInfo = (^? to rights . _head) <$> m ^. network_selectedNodes



-- | UI for asking the user about data needed for deployments/function calling.
uiCfg
  :: ( MonadWidget t m
     , HasNetwork model t
     , HasNetworkCfg mConf t
     , Monoid mConf
     , Flattenable mConf t
     , HasJsonDataCfg mConf t
     , HasWallet model t
     , HasJsonData model t
     )
  => Dynamic t Text
  -> model
  -> m (Dynamic t (f Pact.ChainId))
  -> Maybe TTLSeconds
  -> Maybe GasLimit
  -> m ( mConf
       , Dynamic t (f Pact.ChainId)
       , Dynamic t TTLSeconds
       , Dynamic t GasLimit
       )
uiCfg code m wChainId mTTL mGasLimit = do
  -- General deployment configuration
  let mkGeneralSettings = do
        divClass "title" $ text "Input"
        divClass "group" $ do
          pb <- getPostBuild
          _ <- flip mkLabeledClsInput "Raw Command" $ \cls -> uiTextAreaElement $ def
            & textAreaElementConfig_setValue .~ leftmost [updated code, tag (current code) pb]
            & initialAttributes .~ "disabled" =: "" <> "style" =: "width: 100%" <> "class" =: renderClass cls
          pure ()
        divClass "title" $ text "Destination"
        cId <- elKlass "div" ("group segment") $ do
          transactionDisplayNetwork m
          chain <- wChainId
          pure chain
        divClass "title" $ text "Settings"
        (cfg, ttl, gasLimit) <- elKlass "div" ("group segment") $
          uiMetaData m mTTL mGasLimit
        pure (cfg, cId, ttl, gasLimit)

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
  mkLabeledInput (\c -> uiInputElement $ c & initialAttributes %~ Map.insert "disabled" "") "Transaction Hash" $ def
    & inputElementConfig_initialValue .~ hashToText (toUntypedHash $ Pact._cmdHash cmd)

transactionInputSection :: MonadWidget t m => Text -> Pact.Command Text -> m ()
transactionInputSection code cmd = do
  divClass "title" $ text "Input"
  divClass "group" $ do
    transactionHashSection cmd
    _ <- flip mkLabeledClsInput "Raw Command" $ \cls -> uiTextAreaElement $ def
      & textAreaElementConfig_initialValue .~ code
      & initialAttributes .~ "disabled" =: "" <> "style" =: "width: 100%" <> "class" =: renderClass cls
    pure ()

transactionDisplayNetwork :: (MonadWidget t m, HasNetwork model t) => model -> m ()
transactionDisplayNetwork m = void $ flip mkLabeledClsInput "Network" $ \_ -> do
  divClass "title" $ do
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
    eGasPrice <- tag (current $ _pmGasPrice <$> m ^. network_meta) <$> getPostBuild

    let
      txnSpeedSliderEl gpEl conf = uiSliderInputElement (text "Slow") (text "Fast") $ conf
        & inputElementConfig_initialValue .~ (showGasPrice $ scaleGPtoTxnSpeed defaultTransactionGasPrice)
        & initialAttributes .~ "min" =: "1" <> "max" =: "1001" <> "step" =: "1"
        & inputElementConfig_setValue .~ leftmost
          [ showGasPrice . scaleGPtoTxnSpeed <$> eGasPrice
          , parseAndScaleWith scaleGPtoTxnSpeed gpEl
          ]

      gasPriceInputEl
        :: InputElement EventResult (DomBuilderSpace m) t
        -> InputElementConfig EventResult t (DomBuilderSpace m)
        -> m (InputElement EventResult (DomBuilderSpace m) t)
      gasPriceInputEl tsEl conf = uiRealWithPrecisionInputElement maxCoinPricePrecision $ conf
        & inputElementConfig_initialValue .~ showGasPrice defaultTransactionGasPrice
        & inputElementConfig_setValue .~ leftmost
          [ showGasPrice <$> eGasPrice
          , parseAndScaleWith scaleTxnSpeedToGP tsEl
          ]
        & inputElementConfig_elementConfig . elementConfig_eventSpec %~ preventScrollWheelAndUpDownArrow @m

    onGasPriceTxt <- mdo
      tsEl <- mkLabeledInput (txnSpeedSliderEl gpEl) "Transaction Speed" def
      gpEl <- mkLabeledInput (gasPriceInputEl tsEl) "Gas Price (KDA)" def
      pure $ leftmost [_inputElement_input gpEl, parseAndScaleWith scaleTxnSpeedToGP tsEl]

    let initGasLimit = fromMaybe defaultTransactionGasLimit mGasLimit
    pbGasLimit <- case mGasLimit of
      Just _ -> pure never
      Nothing -> tag (current $ fmap _pmGasLimit $ m ^. network_meta) <$> getPostBuild

    let
      mkGasLimitInput
        :: InputElementConfig EventResult t (DomBuilderSpace m)
        -> m (InputElement EventResult (DomBuilderSpace m) t)
      mkGasLimitInput conf = uiIntInputElement $ conf
        & inputElementConfig_initialValue .~ showGasLimit initGasLimit
        & inputElementConfig_setValue .~ fmap showGasLimit pbGasLimit
        & inputElementConfig_elementConfig . elementConfig_eventSpec %~ preventScrollWheelAndUpDownArrow @m

    onGasLimitTxt <- fmap _inputElement_input $ mkLabeledInput mkGasLimitInput "Gas Limit (units)" def
    let onGasLimit = fmapMaybe (readPact (GasLimit . ParsedInteger)) onGasLimitTxt

    gasLimit <- holdDyn initGasLimit $ leftmost [onGasLimit, pbGasLimit]

    let mkTransactionFee c = uiRealWithPrecisionInputElement maxCoinPricePrecision $ c
          & initialAttributes %~ Map.insert "disabled" ""
    _ <- mkLabeledInputView mkTransactionFee "Max Transaction Fee (KDA)" $
      ffor (m ^. network_meta) $ \pm -> showGasPrice $ fromIntegral (_pmGasLimit pm) * _pmGasPrice pm

    let ttlInput conf = mdo
          sliderEl <- uiSliderInputElement (text "1 second") (text "1 day") $ conf
            & inputElementConfig_setValue .~ _inputElement_input inputEl
          inputEl <- uiIntInputElement $ conf
            & inputElementConfig_setValue .~ _inputElement_input sliderEl
          pure $ leftmost [_inputElement_input inputEl, _inputElement_input sliderEl]
    pbTTL <- case mTTL of
      Just _ -> pure never
      Nothing -> tag (current $ fmap _pmTTL $ m ^. network_meta) <$> getPostBuild
    let secondsInDay :: Int = 60 * 60 * 24
        initTTL = fromMaybe defaultTransactionTTL mTTL
    onTtlTxt <- mkLabeledInput ttlInput "Request Expires (seconds)" $ def
      & initialAttributes .~ "min" =: "1" <> "max" =: T.pack (show secondsInDay) <> "step" =: "1"
      & inputElementConfig_setValue .~ fmap showTtl pbTTL
      & inputElementConfig_initialValue .~ showTtl initTTL
    let onTTL = fmapMaybe (readPact (TTLSeconds . ParsedInteger)) onTtlTxt
    ttl <- holdDyn initTTL $ leftmost [onTTL, pbTTL]

    pure
      ( mempty
        & networkCfg_setGasPrice .~ eParsedGasPrice onGasPriceTxt
        & networkCfg_setGasLimit .~ onGasLimit
        & networkCfg_setTTL .~ onTTL
      , ttl
      , gasLimit
      )

  where

      shiftGP :: GasPrice -> GasPrice -> GasPrice -> GasPrice -> GasPrice -> GasPrice
      shiftGP oldMin oldMax newMin newMax x =
        (newMax-newMin)/(oldMax-oldMin)*(x-oldMin)+newMin

      scaleTxnSpeedToGP :: GasPrice -> GasPrice
      scaleTxnSpeedToGP = shiftGP 1 1001 (1e-12) (1e-8)

      scaleGPtoTxnSpeed :: GasPrice -> GasPrice
      scaleGPtoTxnSpeed = shiftGP (1e-12) (1e-8) 1 1001

      parseAndScaleWith
        :: (GasPrice -> GasPrice)
        -> InputElement er d t
        -> Event t Text
      parseAndScaleWith f =
        fmap (showGasPrice . f) . eParsedGasPrice . _inputElement_input

      eParsedGasPrice :: Event t Text -> Event t GasPrice
      eParsedGasPrice = fmapMaybe (readPact (GasPrice . ParsedDecimal))

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

-- | Let the user pick a sender
uiSenderDropdown
  :: ( Adjustable t m, PostBuild t m, DomBuilder t m
     , MonadHold t m, MonadFix m
     , HasWallet model t
     )
  => DropdownConfig t (Maybe AccountName)
  -> model
  -> Dynamic t (Maybe ChainId)
  -> m (Dynamic t (Maybe AccountName))
uiSenderDropdown uCfg m chainId = do
  let textAccounts
        | AppCfg.isChainweaverAlpha = Map.insert Nothing "Choose an account"
          . Map.mapKeys (hush . mkAccountName)
          . Map.mapWithKey const <$> (m ^. wallet_keys)
        | otherwise =
          let mkTextAccounts mChain chains = case mChain of
                Nothing -> Map.singleton Nothing "You must select a chain ID before choosing an account"
                Just chain -> case Map.lookup chain chains of
                  Just accounts | not (Map.null accounts) ->
                                  Map.insert Nothing "Choose an account" $ Map.mapKeysMonotonic Just $ Map.mapWithKey (\k _ -> unAccountName k) accounts
                  _ -> Map.singleton Nothing "No accounts on current chain"
           in mkTextAccounts <$> chainId <*> m ^. wallet_accountGuards
  choice <- dropdown Nothing textAccounts $ uCfg
    & dropdownConfig_setValue .~ (if AppCfg.isChainweaverAlpha then never else Nothing <$ updated chainId)
    & dropdownConfig_attributes <>~ pure ("class" =: "labeled-input__input select select_mandatory_missing")
  pure $ value choice

uiChainSelection
  :: MonadWidget t m
  => Dynamic t (Maybe NodeInfo)
  -> CssClass
  -> m (Dynamic t (Maybe Pact.ChainId))
uiChainSelection info cls = mdo
    let
      chains = map (id &&& _chainId) . maybe [] getChains <$> info
      mkPlaceHolder cChains = if null cChains then "No chains available" else "Select chain"
      mkOptions cs = Map.fromList $ (Nothing, mkPlaceHolder cs) : map (first Just) cs

      staticCls = cls <> "select"
      mkDynCls v = if isNothing v then "select_mandatory_missing" else mempty
      allCls = renderClass <$> fmap mkDynCls d <> pure staticCls

      cfg = def & dropdownConfig_attributes .~ (("class" =:) <$> allCls)

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
  (empty, parsed) <- elClass "td" "table__cell_padded" $ mdo
    cap <- uiInputElement $ def
      & inputElementConfig_initialValue .~ foldMap (renderCompactText . _dappCap_cap) mCap
      & initialAttributes .~
        "placeholder" =: "(module.capability arg1 arg2)" <>
        "class" =: (maybe id (const (<> " input_transparent")) mCap) "input_width_full" <>
        (maybe mempty (const $ "disabled" =: "true") mCap)
      & modifyAttributes .~ ffor errors (\e -> "style" =: ("background-color: #fdd" <$ guard e))
    empty <- holdUniqDyn $ T.null <$> value cap
    let parsed = parseSigCapability <$> value cap
        showError = (\p e -> isLeft p && not e) <$> parsed <*> empty
        errors = leftmost
          [ tag (current showError) (domEvent Blur cap)
          , False <$ _inputElement_input cap
          ]
    pure (empty, parsed)
  account <- elClass "td" "table__cell_padded" mkSender

  pure $ CapabilityInputRow
    { _capabilityInputRow_empty = empty
    , _capabilityInputRow_value = empty >>= \case
      True -> pure mempty
      False -> fmap (fromMaybe mempty) $ runMaybeT $ do
        a <- MaybeT account
        p <- MaybeT $ either (const Nothing) pure <$> parsed
        pure $ Map.singleton a [p]
    , _capabilityInputRow_account = account
    , _capabilityInputRow_cap = parsed
    }

-- | Display a single row for an empty capability
emptyCapability :: DomBuilder t m => m a -> m a
emptyCapability m = elClass "tr" "table__row" $ do
  elClass "td" "table__cell_padded" $ text "Empty capability"
  elClass "td" "table__cell_padded" m

-- | Display a dynamic number of rows for the user to enter custom capabilities
capabilityInputRows
  :: forall t m. MonadWidget t m
  => m (Dynamic t (Maybe AccountName))
  -> m (Dynamic t (Map AccountName [SigCapability]))
capabilityInputRows mkSender = do
  rec
    (im0, im') <- traverseIntMapWithKeyWithAdjust (\_ _ -> capabilityInputRow Nothing mkSender) (IM.singleton 0 ()) $ leftmost
      -- Delete rows, but ensure we don't delete them all
      [ PatchIntMap <$> gate canDelete deletions
      -- Add a new row when all rows are used
      , attachWith (\i _ -> PatchIntMap (IM.singleton i (Just ()))) nextKeyToUse $ ffilter not $ updated anyEmpty
      ]
    results :: Dynamic t (IntMap (CapabilityInputRow t))
      <- foldDyn applyAlways im0 im'
    let nextKeyToUse = maybe 0 (succ . fst) . IM.lookupMax <$> current results
        canDelete = (> 1) . IM.size <$> current results

        anyEmpty = fmap or $ traverse _capabilityInputRow_empty =<< results

        decideDeletions :: Int -> CapabilityInputRow t -> Event t (IntMap (Maybe ()))
        decideDeletions i row = IM.singleton i Nothing <$ leftmost
          -- Deletions caused by rows becoming empty
          [ void . ffilter id . updated $ _capabilityInputRow_empty row
          -- Deletions caused by users entering GAS
          , void . ffilter (either (const False) isGas) . updated $ _capabilityInputRow_cap row
          ]
        deletions = switch . current $ IM.foldMapWithKey decideDeletions <$> results

  pure $
    fmap (Map.unionsWith (<>) . IM.elems) $ traverse _capabilityInputRow_value =<< results

-- | Widget for selection of sender and signing keys.
uiSenderCapabilities
  :: forall t m model. (MonadWidget t m, HasWallet model t)
  => model
  -> Dynamic t (Maybe Pact.ChainId)
  -> Maybe [DappCap]
  -> m (Dynamic t (Maybe AccountName))
  -> m (Dynamic t (Maybe AccountName), Dynamic t (Map AccountName [SigCapability]))
uiSenderCapabilities m cid mCaps mkSender = do
  let staticCapabilityRow sender cap = do
        el "td" $ text $ _dappCap_role cap
        el "td" $ text $ renderCompactText $ _dappCap_cap cap
        acc <- el "td" $ sender
        pure $ CapabilityInputRow
          { _capabilityInputRow_empty = pure False
          , _capabilityInputRow_value = maybe mempty (\s -> Map.singleton s [_dappCap_cap cap]) <$> acc
          , _capabilityInputRow_account = acc
          , _capabilityInputRow_cap = pure $ Right $ _dappCap_cap cap
          }

      staticCapabilityRows caps = fmap (fmap (Map.unionsWith (<>)) . sequence) $ for caps $ \cap ->
        elClass "tr" "table__row" $ _capabilityInputRow_value <$> staticCapabilityRow (uiSenderDropdown def m cid) cap

      -- Deliberately lift over the `Maybe` too, so we short circuit if anything
      -- is missing.
      combineMaps = liftA3 $ \a b c -> Map.unionsWith (<>) [a, b, c]

  divClass "title" $ text "Roles"

  -- Capabilities
  divClass "group" $ elAttr "table" ("class" =: "table" <> "style" =: "width: 100%; table-layout: fixed;") $ case mCaps of
    Nothing -> el "tbody" $ do
      empty <- emptyCapability mkSender
      let emptySig = maybe Map.empty (\a -> Map.singleton a []) <$> empty
      gas <- capabilityInputRow (Just defaultGASCapability) mkSender
      rest <- capabilityInputRows (uiSenderDropdown def m cid)
      pure (_capabilityInputRow_account gas, combineMaps (_capabilityInputRow_value gas) rest emptySig)
    Just caps -> do
      el "thead" $ el "tr" $ do
        elClass "th" "table__heading" $ text "Role"
        elClass "th" "table__heading" $ text "Capability"
        elClass "th" "table__heading" $ text "Account"
      el "tbody" $ do
        empty <- emptyCapability mkSender
        let emptySig = maybe Map.empty (\a -> Map.singleton a []) <$> empty
        gas <- staticCapabilityRow mkSender defaultGASCapability
        rest <- staticCapabilityRows $ filter (not . isGas . _dappCap_cap) caps
        pure (_capabilityInputRow_account gas, combineMaps (_capabilityInputRow_value gas) rest emptySig)

isGas :: SigCapability -> Bool
isGas = (^. to PC._scName . to PN._qnName . to (== "GAS"))

uiSigningKeys :: (MonadWidget t m, HasWallet model t) => model -> m (Dynamic t (Set KeyName))
uiSigningKeys model = do
  let keyMap = model ^. wallet_keys
      tableAttrs =
        "style" =: "table-layout: fixed; width: 100%" <> "class" =: "table"
  boxValues <- elAttr "table" tableAttrs $ do
    chosenKeys <- el "tbody" $ listWithKey keyMap $ \name key -> signingItem (name, key)
    dynText $ ffor keyMap $ \keys -> if Map.null keys then "No keys ..." else ""
    pure chosenKeys
  return $ Map.keysSet . Map.filter id <$> joinDynThroughMap boxValues

------------------------------------------------------------------------------
-- | Display a key as list item together with it's name.
signingItem
  :: MonadWidget t m
  => (KeyName, Dynamic t KeyPair)
  -> m (Dynamic t Bool)
signingItem (n, _) = do
    elClass "tr" "table__row checkbox-container" $ do
      (e, ()) <- el' "td" $ text n
      let onTextClick = domEvent Click e
      elClass "td" "signing-selector__check-box-cell" $ mdo
        let
          val = _checkbox_value box
          cfg = toggleCheckbox val onTextClick
        box <- mkCheckbox cfg
        pure val
  where
    mkCheckbox uCfg = do
      uiCheckbox "signing-selector__check-box-label" False uCfg blank

toggleCheckbox :: Reflex t => Dynamic t Bool -> Event t a -> CheckboxConfig t
toggleCheckbox val =
  (\v -> def { _checkboxConfig_setValue = v }) . fmap not . tag (current val)

-- parsed: "{\"role\": \"GAS\", \"description\": \"Pay the GAS required for this transaction\", \"cap\": {\"args\": [\"doug\",], \"name\": \"coin.GAS\"}}"
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
