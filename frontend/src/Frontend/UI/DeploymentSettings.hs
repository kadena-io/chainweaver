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

-- | Little widget providing a UI for deployment related settings.
--
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)

module Frontend.UI.DeploymentSettings
  ( -- * Settings
    DeploymentSettingsConfig (..)
    -- * Values for _deploymentSettingsConfig_chainId:
  , predefinedChainIdSelect
  , userChainIdSelect
    -- * Widgets
  , uiDeploymentSettings
  , uiSigningKeys
    -- * Useful re-exports
  , Identity (runIdentity)
  ) where

------------------------------------------------------------------------------
import           Control.Arrow               (first)
import Data.Either (rights)
import           Control.Arrow               ((&&&))
import           Control.Lens
import           Control.Monad
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Pact.Types.ChainMeta        (PublicMeta (..))
import           Pact.Types.Runtime          (GasLimit (..), GasPrice (..))
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Contrib.CssClass (elKlass)
import           Safe                        (readMay)
------------------------------------------------------------------------------
import           Common.Network
import           Frontend.Foundation
import           Frontend.ModuleExplorer     (TransactionInfo (..))
import           Frontend.Network
import           Frontend.UI.TabBar
import           Frontend.UI.Widgets
import           Frontend.Wallet
------------------------------------------------------------------------------

-- | Config for the deployment settings widget.
data DeploymentSettingsConfig t f m model a = DeploymentSettingsConfig
  { _deploymentSettingsConfig_userTab     :: Maybe (Text, m a)
    -- ^ Some optional extra tab. fst is the tab's name, snd is its content.
  , _deploymentSettingsConfig_chainId     :: model -> m (Dynamic t (f ChainId))
    -- ^ ChainId selection widget.
    --   You can pick (predefinedChainIdSelect someId) - for not showing a
    --   widget at all, but having `uiDeploymentSettings` use the provided one.
    --
    --   Or you can use `userChainIdSelect` for having the user pick a chainid.
  , _deploymentSettingsConfig_defEndpoint :: Endpoint
    -- ^ What `Endpoint` to select by default.
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


-- | Show settings related to deployments to the user.
--
--
--   the right keys, ...
uiDeploymentSettings
  :: forall t f m model mConf a
  . ( MonadWidget t m, HasNetwork model t, HasWallet model t
    , Monoid mConf , HasNetworkCfg mConf t, Applicative f
    )
  => model
  -> DeploymentSettingsConfig t f m model a
  -> m (mConf, Dynamic t (f TransactionInfo), Maybe a)
uiDeploymentSettings m (DeploymentSettingsConfig mUserTab mkWChainId endpoint) = mdo
    let initTab = fromMaybe DeploymentSettingsView_Cfg mUserTabName
    curSelection <- holdDyn initTab onTabClick
    (TabBar onTabClick) <- makeTabBar $ TabBarCfg
      { _tabBarCfg_tabs = availableTabs
      , _tabBarCfg_mkLabel = const $ text . showSettingsTabName
      , _tabBarCfg_selectedTab = Just <$> curSelection
      , _tabBarCfg_classes = mempty
      , _tabBarCfg_type = TabBarType_Secondary
      }
    elClass "div" "segment" $ do

      mRes <- traverse (uncurry $ tabPane mempty curSelection) mUserTabCfg

      (cfg, cChainId, cEndpoint) <- tabPane mempty curSelection DeploymentSettingsView_Cfg $
        uiCfg m (mkWChainId m) endpoint

      signingKeys <- tabPane mempty curSelection DeploymentSettingsView_Keys $ do
        let selectedSender = Set.singleton . _pmSender <$> m ^. network_meta
        uiSigningKeys selectedSender m

      pure
        ( cfg
        , do
            sigKeys <- signingKeys
            ep <- cEndpoint
            chain <- cChainId
            pure $ TransactionInfo <$> (pure sigKeys) <*> chain <*> (pure ep)
        , mRes
        )
    where
      mUserTabCfg  = first DeploymentSettingsView_Custom <$> mUserTab
      mUserTabName = fmap fst mUserTabCfg
      userTabs = maybeToList mUserTabName
      stdTabs = [DeploymentSettingsView_Cfg, DeploymentSettingsView_Keys]
      availableTabs = userTabs <> stdTabs


-- | Use a predefined chain id, don't let the user pick one.
predefinedChainIdSelect
  :: (Reflex t, Monad m)
  => ChainId
  -> model
  -> m (Dynamic t (Identity ChainId))
predefinedChainIdSelect chanId _ = pure . pure . pure $ chanId


-- | Let the user pick a chain id.
userChainIdSelect
  :: (MonadWidget t m, HasNetwork model t
     )
  => model
  -> m (MDynamic t ChainId)
userChainIdSelect m = mkLabeledClsInput (uiChainSelection mNodeInfo) labelText
  where
    mNodeInfo = (^? to rights . _head) <$> m ^. network_selectedNodes

    labelText = ffor (m ^. network_selectedNetwork) $
      \n -> "Network [ " <> textNetworkName n <> " ]"


-- | UI for asking the user about data needed for deployments/function calling.
uiCfg
  :: (MonadWidget t m, HasNetwork model t, HasNetworkCfg mConf t, Monoid mConf
     )
  => model
  -> m (Dynamic t (f ChainId))
  -> Endpoint
  -> m (mConf, Dynamic t (f ChainId), Dynamic t Endpoint)
uiCfg m wChainId ep = do
  (cId, endpoint) <- elKlass "div" ("group segment") $
     uiEndpoint wChainId ep
  cfg <- elKlass "div" ("group segment") $
    uiMetaData m
  pure (cfg, cId, endpoint)


-- | UI for asking the user about endpoint (`Endpoint` & `ChainId`) for deployment.
--
--   If a `ChainId` is passed in, the user will not be asked for one.
--
--   The given `EndPoint` will be the default in the dropdown.
uiEndpoint
  :: MonadWidget t m
  => m (Dynamic t (f ChainId))
  -> Endpoint
  -> m (Dynamic t (f ChainId), Dynamic t Endpoint)
uiEndpoint wChainId ep = do

    selChain <- wChainId

    selEndpoint <- mkLabeledClsInput (uiEndpointSelection ep) "Endpoint"

    pure (selChain, selEndpoint)


-- | ui for asking the user about meta data needed for the transaction.
uiMetaData
  :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m
    , HasNetwork model t, HasNetworkCfg mConf t, Monoid mConf
     )
  => model -> m mConf
uiMetaData m  = do

    onSender <- mkLabeledInput (senderDropdown $ m ^. network_meta) "Sender" def

    onGasPriceTxt <- mkLabeledInputView uiRealInputElement "Gas price" $
      fmap (showGasPrice . _pmGasPrice) $ m ^. network_meta

    onGasLimitTxt <- mkLabeledInputView uiIntInputElement "Gas limit" $
      fmap (showGasLimit . _pmGasLimit) $ m ^. network_meta

    pure $ mempty
      & networkCfg_setSender .~ onSender
      & networkCfg_setGasPrice .~ fmapMaybe (readPact GasPrice) onGasPriceTxt
      & networkCfg_setGasLimit .~ fmapMaybe (readPact GasLimit) onGasLimitTxt

  where

      showGasLimit :: GasLimit -> Text
      showGasLimit (GasLimit i) = tshow i

      showGasPrice :: GasPrice -> Text
      showGasPrice (GasPrice i) = tshow i

      senderDropdown meta uCfg = do
        let itemDom v = elAttr "option" ("value" =: v) $ text v
        onSet <- tagOnPostBuild $ _pmSender <$> meta
        let
          cfg = uCfg
            & selectElementConfig_setValue .~ onSet
        (se, ()) <- uiSelectElement cfg $ do
          traverse_ itemDom $ Map.keys chainwebDefaultSenders
        pure $ _selectElement_change se

      readPact wrapper =  fmap wrapper . readMay . T.unpack


-- | Widget (dropdown) for letting the user choose between /send and /local endpoint.
--
uiEndpointSelection :: MonadWidget t m => Endpoint -> CssClass -> m (Dynamic t Endpoint)
uiEndpointSelection initial cls = do
  let endpoints = Map.fromList $ map (id &&& displayEndpoint) $ [minBound .. maxBound]
      allCls = renderClass $ cls <> "select"
      cfg = def & dropdownConfig_attributes .~ pure ("class" =: allCls)
  d <- dropdown initial (pure endpoints) cfg
  pure $ _dropdown_value d


uiChainSelection
  :: MonadWidget t m
  => Dynamic t (Maybe NodeInfo)
  -> CssClass
  -> m (Dynamic t (Maybe ChainId))
uiChainSelection info cls = mdo
    let
      chains = map (id &&& tshow) . maybe [] getChains <$> info
      mkPlaceHolder cChains = if null cChains then "No chains available" else "Select chain"
      mkOptions cs = Map.fromList $ (Nothing, mkPlaceHolder cs) : map (first Just) cs

      staticCls = cls <> "select select_type_primary"
      mkDynCls v = if isNothing v then "select_mandatory_missing" else mempty
      allCls = renderClass <$> fmap mkDynCls d <> pure staticCls

      cfg = def & dropdownConfig_attributes .~ (("class" =:) <$> allCls)

    d <- _dropdown_value <$> dropdown Nothing (mkOptions <$> chains) cfg
    pure d


-- | Widget for selection of signing keys.
uiSigningKeys
  :: forall t m model. (MonadWidget t m, HasWallet model t)
  => Dynamic t (Set KeyName) -- ^ Any keys that should be preselected (and can't be unchecked by the user).
  -> model
  -> m (Dynamic t (Set KeyName))
uiSigningKeys preselected aWallet = do
  let keyMap = aWallet ^. wallet_keys
      tableAttrs =
        "style" =: "table-layout: fixed; width: 100%" <> "class" =: "table"
  boxValues <- elAttr "table" tableAttrs $ do
    -- el "thead" $ elClass "tr" "table__row" $ do
    --   elClass "th" "table__heading" $ text "Sign with Key"
    --   elClass "th" "table__heading" $ text ""
    el "tbody" $ listWithKey keyMap $ \name key -> signingItem preselected (name, key)
  dyn_ $ ffor keyMap $ \keys -> when (Map.null keys) $ text "No keys ..."
  return $ do -- The Dynamic monad
    m :: Map KeyName (Dynamic t Bool) <- boxValues
    ps <- traverse (\(k,v) -> (k,) <$> v) $ Map.toList m
    return $ Set.fromList $ map fst $ filter snd ps


------------------------------------------------------------------------------
-- | Display a key as list item together with it's name.
signingItem
  :: MonadWidget t m
  => Dynamic t (Set KeyName)
  -> (KeyName, Dynamic t KeyPair)
  -> m (Dynamic t Bool)
signingItem preselected (n, _) = do
    elClass "tr" "table__row checkbox-container" $ do
      (e, ()) <- el' "td" $ text n
      let onTextClick = domEvent Click e
      elClass "td" "signing-selector__check-box-cell" $ mdo
        let
          val = _checkbox_value box
          isPreselected = Set.member n <$> preselected
          cfg = toggleCheckbox val $ gate (not <$> current isPreselected) onTextClick
        box <- mkCheckbox cfg isPreselected
        pure val
  where
    mkCheckbox uCfg isPreselected = do
      onIsPreselected <- tagOnPostBuild <=< holdUniqDyn $ isPreselected
      let
        cfg = uCfg
          & checkboxConfig_attributes %~ ((fmap updateAttrs isPreselected) <*>)
          & checkboxConfig_setValue %~ \old -> leftmost [onIsPreselected, old]
      uiCheckbox "signing-selector__check-box-label" False cfg blank

    updateAttrs = \case
      False -> (<> mempty)
      True  -> (<> "disabled" =: "true")


toggleCheckbox :: Reflex t => Dynamic t Bool -> Event t a -> CheckboxConfig t
toggleCheckbox val =
  (\v -> def { _checkboxConfig_setValue = v }) . fmap not . tag (current val)
