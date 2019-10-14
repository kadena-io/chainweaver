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

------------------------------------------------------------------------------
import           Control.Arrow               (first)
import           Data.Either                 (rights)
import           Control.Arrow               ((&&&))
import           Control.Lens
import           Control.Monad
import           Control.Error.Util (hush)
import qualified Data.Aeson as Aeson
import qualified Data.Map                    as Map
import           Data.Set                    (Set)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding as T
import           Data.Traversable (for)
import           Pact.Parse
import qualified Pact.Types.ChainId as Pact
import           Pact.Types.ChainMeta        (PublicMeta (..), TTLSeconds (..))
import qualified Pact.Types.Command as Pact
import           Pact.Types.Runtime          (GasLimit (..), GasPrice (..), PactHash, hashToText, toUntypedHash, hash)
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Contrib.CssClass (elKlass)
import           Safe                        (readMay)
------------------------------------------------------------------------------
import           Common.Network
import           Frontend.Foundation
import           Frontend.JsonData
import           Frontend.Network
import           Frontend.UI.Dialogs.NetworkEdit
import           Frontend.UI.JsonData
import           Frontend.UI.Modal
import           Frontend.UI.TabBar
import           Frontend.UI.Widgets
import           Frontend.UI.Widgets.Helpers (preventScrollWheelAndUpDownArrow)
import           Frontend.Wallet
------------------------------------------------------------------------------

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
  , _deploymentSettingsConfig_sender      :: model -> Dynamic t (Maybe ChainId) -> m (Dynamic t (Maybe AccountName))
    -- ^ Sender selection widget. Use 'uiSenderFixed' or 'uiSenderDropdown'.
  , _deploymentSettingsConfig_data        :: Maybe Aeson.Object
    -- ^ Data selection. If 'Nothing', uses the users setting (and allows them
    -- to alter it). Otherwise, it remains fixed.
  , _deploymentSettingsConfig_defEndpoint :: Maybe Endpoint
    -- ^ What `Endpoint` to select by default. If 'Nothing', the endpoint UI is
    -- omitted entirely.
  , _deploymentSettingsConfig_code :: Dynamic t Text
    -- ^ Code that is being deployed
  , _deploymentSettingsConfig_nonce :: Maybe Text
    -- ^ Nonce. 'Nothing' will autogenerate a nonce based on the current time.
  , _deploymentSettingsConfig_ttl :: Maybe TTLSeconds
    -- ^ TTL. Overridable by the user.
  , _deploymentSettingsConfig_gasLimit :: Maybe GasLimit
    -- ^ Gas Limit. Overridable by the user.
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
  , _deploymentSettingsResult_endpoint :: Maybe Endpoint
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
    (conf, chainId, sender, x, ma) <- elClass "div" "modal__main transaction_details" $ do

      mRes <- traverse (uncurry $ tabPane mempty curSelection) mUserTabCfg

      (cfg, cChainId, cEndpoint, ttl, gasLimit) <- tabPane mempty curSelection DeploymentSettingsView_Cfg $
        uiCfg code m
          (_deploymentSettingsConfig_chainId settings $ m)
          (_deploymentSettingsConfig_defEndpoint settings)
          (_deploymentSettingsConfig_ttl settings)
          (_deploymentSettingsConfig_gasLimit settings)

      (mSender, signingKeys) <- tabPane mempty curSelection DeploymentSettingsView_Keys $
        uiSigningKeysSender m $ (_deploymentSettingsConfig_sender settings) m cChainId

      pure
        ( cfg
        , cChainId
        , mSender
        , do
          signingKeys' <- current signingKeys
          jsonData' <- either (const mempty) id <$> current (m ^. jsonData . jsonData_data)
          ttl' <- current ttl
          gasLimit' <- current gasLimit
          pm <- current $ m ^. network_meta
          let publicMeta cid s = pm
                { _pmChainId = cid
                , _pmGasLimit = gasLimit'
                , _pmSender = unAccountName s
                , _pmTTL = ttl'
                }
          code' <- current code
          mEndpoint' <- traverse current cEndpoint
          allKeys <- current $ m ^. wallet_keys
          pure (mEndpoint', code', jsonData', signingKeys', allKeys, publicMeta)
        , mRes
        )

    controls <- modalFooter $ do
      let backConfig = def & uiButtonCfg_class .~ ffor curSelection
            (\s -> if s == fromMaybe DeploymentSettingsView_Cfg mUserTabName then "hidden" else "")
      back <- uiButtonDyn backConfig $ text "Back"
      let shouldBeDisabled tab chain account =
            tab == DeploymentSettingsView_Keys && (isNothing account || isNothing chain)
          isDisabled = shouldBeDisabled <$> curSelection <*> chainId <*> sender
      next <- uiButtonDyn (def & uiButtonCfg_class .~ "button_type_confirm" & uiButtonCfg_disabled .~ isDisabled) $ dynText $ ffor curSelection $ \case
        DeploymentSettingsView_Keys -> "Preview"
        _ -> "Next"
      pure $ leftmost
        [ nextView <$ next
        , prevView mUserTabName <$ back
        ]

    let sign (networkName, Just chain, Just account, (endpoint, code', data', signing, keys, pm)) () = Just $ do
          let signingKeys = getSigningPairs signing keys
              pm' = pm chain account
          cmd <- buildCmd (_deploymentSettingsConfig_nonce settings) networkName pm' signingKeys code' data'
          pure $ DeploymentSettingsResult
            { _deploymentSettingsResult_gasPrice = _pmGasPrice pm'
            , _deploymentSettingsResult_signingKeys = signingKeys
            , _deploymentSettingsResult_sender = account
            , _deploymentSettingsResult_endpoint = endpoint
            , _deploymentSettingsResult_chainId = chain
            , _deploymentSettingsResult_command = cmd
            , _deploymentSettingsResult_code = code'
            }
        sign _ _ = Nothing
    command <- performEvent $ attachWithMaybe sign
      ((,,,) <$> current (m ^. network_selectedNetwork) <*> current chainId <*> current sender <*> x)
      done
    pure (conf & networkCfg_setSender .~ fmapMaybe (fmap unAccountName) (updated sender), command, ma)
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
  -> Maybe Endpoint
  -> Maybe TTLSeconds
  -> Maybe GasLimit
  -> m ( mConf
       , Dynamic t (f Pact.ChainId)
       , Maybe (Dynamic t Endpoint)
       , Dynamic t TTLSeconds
       , Dynamic t GasLimit
       )
uiCfg code m wChainId ep mTTL mGasLimit = do
  -- General deployment configuration
  let mkGeneralSettings = do
        divClass "title" $ text "Input"
        divClass "group" $ do
          _ <- mkLabeledInputView (\c -> uiInputElement $ c & initialAttributes %~ Map.insert "disabled" "") "Transaction Hash" $ hashToText . toUntypedHash . id @PactHash . hash . T.encodeUtf8 <$> code
          pb <- getPostBuild
          _ <- flip mkLabeledClsInput "Raw Command" $ \cls -> uiTextAreaElement $ def
            & textAreaElementConfig_setValue .~ leftmost [updated code, tag (current code) pb]
            & initialAttributes .~ "disabled" =: "" <> "style" =: "width: 100%" <> "class" =: renderClass cls
          pure ()
        divClass "title" $ text "Destination"
        (cId, endpoint) <- elKlass "div" ("group segment") $ do
          endpoint <- uiEndpoint m ep
          chain <- wChainId
          pure (chain, endpoint)
        divClass "title" $ text "Settings"
        (cfg, ttl, gasLimit) <- elKlass "div" ("group segment") $
          uiMetaData m mTTL mGasLimit
        pure (cfg, cId, endpoint, ttl, gasLimit)

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

transactionHashSection :: MonadWidget t m => Dynamic t Text -> m ()
transactionHashSection code = void $ do
  mkLabeledInputView (\c -> uiInputElement $ c & initialAttributes %~ Map.insert "disabled" "") "Transaction Hash" $
    hashToText . toUntypedHash . id @PactHash . hash . T.encodeUtf8 <$> code

transactionInputSection :: MonadWidget t m => Dynamic t Text -> m ()
transactionInputSection code = do
  divClass "title" $ text "Input"
  divClass "group" $ do
    transactionHashSection code
    pb <- getPostBuild
    _ <- flip mkLabeledClsInput "Raw Command" $ \cls -> uiTextAreaElement $ def
      & textAreaElementConfig_setValue .~ leftmost [updated code, tag (current code) pb]
      & initialAttributes .~ "disabled" =: "" <> "style" =: "width: 100%" <> "class" =: renderClass cls
    pure ()

transactionDisplayNetwork :: (MonadWidget t m, HasNetwork model t) => model -> m ()
transactionDisplayNetwork m = void $ flip mkLabeledClsInput "Network" $ \_ -> do
  divClass "title" $ do
    netStat <- queryNetworkStatus (m ^. network_networks) (m ^. network_selectedNetwork)
    uiNetworkStatus "" netStat
    dynText $ textNetworkName <$> m ^. network_selectedNetwork

-- | UI for asking the user about endpoint (`Endpoint` & `ChainId`) for deployment.
--
--   If a `ChainId` is passed in, the user will not be asked for one.
--
--   The given `EndPoint` will be the default in the dropdown.
uiEndpoint
  :: (MonadWidget t m, HasNetwork model t)
  => model
  -> Maybe Endpoint
  -> m (Maybe (Dynamic t Endpoint))
uiEndpoint m ep = do
    transactionDisplayNetwork m
    for ep $ \e -> do
      de <- mkLabeledClsInput (uiEndpointSelection e) "Access"
      divClass "detail" $ dynText $ ffor de $ \case
        Endpoint_Local -> "Read some data from the blockchain. No gas fees required."
        Endpoint_Send -> "Send a transaction to the blockchain. You'll need to pay gas fees for the transaction to be included."
      pure de

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
  _ <- mkLabeledInput uiInputElement "Sender" $ def
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
  -- Unused temporarily for ALPHA testing phase
  -- let mkTextAccounts mChain chains = case mChain of
  --       Nothing -> Map.singleton Nothing "You must select a chain ID before choosing an account"
  --       Just chain -> case Map.lookup chain chains of
  --         Just accounts | not (Map.null accounts) ->
  --           Map.insert Nothing "Choose an account" $ Map.mapKeysMonotonic Just $ Map.mapWithKey (\k _ -> unAccountName k) accounts
  --         _ -> Map.singleton Nothing "No accounts on current chain"
  --     textAccounts = mkTextAccounts <$> chainId <*> m ^. wallet_accountGuards
  --
  -- choice <- dropdown Nothing textAccounts $ uCfg
  --   & dropdownConfig_setValue .~ (Nothing <$ updated chainId)
  --   & dropdownConfig_attributes <>~ pure ("class" =: "labeled-input__input select select_mandatory_missing select_type_primary")
  -- pure $ value choice

  fmap (fmap (hush . mkAccountName) . value) $ uiInputElement $ def
    & inputElementConfig_setValue .~ (mempty <$ updated chainId)
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("placeholder" =: "Please enter the gas payer account name" <>
         "style" =: "width:100%"
        )

-- | Widget (dropdown) for letting the user choose between /send and /local endpoint.
--
uiEndpointSelection :: MonadWidget t m => Endpoint -> CssClass -> m (Dynamic t Endpoint)
uiEndpointSelection initial cls = divClass (renderClass $ "button_group" <> cls) $ do
  rec
    selectedEndpoint <- holdDyn initial . leftmost <=< for [Endpoint_Local, Endpoint_Send] $ \endpoint -> do
      let mkClass = ffor selectedEndpoint $ \e -> if endpoint == e then "chosen" else ""
      e <- uiButtonDyn (def & uiButtonCfg_class .~ mkClass) $ text $ displayEndpoint endpoint
      pure $ endpoint <$ e
  pure selectedEndpoint

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

      staticCls = cls <> "select select_type_primary"
      mkDynCls v = if isNothing v then "select_mandatory_missing" else mempty
      allCls = renderClass <$> fmap mkDynCls d <> pure staticCls

      cfg = def & dropdownConfig_attributes .~ (("class" =:) <$> allCls)

    d <- _dropdown_value <$> dropdown Nothing (mkOptions <$> chains) cfg
    pure d


-- | Widget for selection of sender and signing keys.
uiSigningKeysSender
  :: forall t m model. (MonadWidget t m, HasWallet model t)
  => model
  -> m (Dynamic t (Maybe AccountName))
  -> m (Dynamic t (Maybe AccountName), Dynamic t (Set KeyName))
uiSigningKeysSender model mkSender = do
  divClass "title" $ text "Gas Payer"

  sender <- divClass "group" mkSender

  divClass "title" $ text "Transaction Signer"
  keys <- divClass "group" $ uiSigningKeys model
  return (sender, keys)

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
