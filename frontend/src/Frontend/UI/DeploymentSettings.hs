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
import           Control.Arrow               ((&&&))
import           Control.Lens
import           Control.Monad
import           Control.Monad.Ref           (MonadRef, Ref)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Maybe   (MaybeT (..), runMaybeT)
import           Data.Functor.Identity
import           Data.IORef                  (IORef)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Pact.Parse                  (ParsedDecimal (..),
                                              ParsedInteger (..))
import           Pact.Types.ChainMeta        (PublicMeta (..))
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Contrib.CssClass (elKlass)
import           Safe                        (readMay)
import qualified Text.URI                    as URI
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
uiDeploymentSettings m cfg@(DeploymentSettingsConfig mUserTab mkWChainId endpoint) = mdo
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

      signingKeys <- tabPane mempty curSelection DeploymentSettingsView_Keys $
        uiSigningKeys m

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
    mNodeInfo = (^? _2 . _Right) <$> m ^. network_selectedNetwork

    labelText = ffor (m ^. network_selectedNetwork) $ \case
      (_, Right info) -> "Node [ " <> (URI.render $ _nodeInfo_baseUri info) <> " ]"
      (_, Left err)   -> "> Network is unreachable at the moment. <"


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
     uiEndpoint m wChainId ep
  cfg <- elKlass "div" ("group segment") $
    uiMetaData m
  pure (cfg, cId, endpoint)


-- | UI for asking the user about endpoint (`Endpoint` & `ChainId`) for deployment.
--
--   If a `ChainId` is passed in, the user will not be asked for one.
--
--   The given `EndPoint` will be the default in the dropdown.
uiEndpoint
  :: ( MonadWidget t m, HasNetwork model t)
  => model
  -> m (Dynamic t (f ChainId))
  -> Endpoint
  -> m (Dynamic t (f ChainId), Dynamic t Endpoint)
uiEndpoint m wChainId ep = do

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
      fmap (showParsedDecimal . _pmGasPrice) $ m ^. network_meta

    onGasLimitTxt <- mkLabeledInputView uiIntInputElement "Gas limit" $
      fmap (showParsedInteger . _pmGasLimit) $ m ^. network_meta

    pure $ mempty
      & networkCfg_setSender .~ onSender
      & networkCfg_setGasPrice .~ fmapMaybe (readPact ParsedDecimal) onGasPriceTxt
      & networkCfg_setGasLimit .~ fmapMaybe (readPact ParsedInteger) onGasLimitTxt

  where

      showParsedInteger :: ParsedInteger -> Text
      showParsedInteger (ParsedInteger i) = tshow i

      showParsedDecimal :: ParsedDecimal -> Text
      showParsedDecimal (ParsedDecimal i) = tshow i

      senderDropdown meta uCfg = do
        let itemDom v = elAttr "option" ("value" =: v) $ text v
        onSet <- tagOnPostBuild $ _pmSender <$> meta
        let
          cfg = uCfg
            & selectElementConfig_setValue .~ onSet
        (se, ()) <- uiSelectElement cfg $ do
          traverse_ itemDom $ Map.keys chainwebDefaultSenders
        text $ "Note: Make sure to sign with this sender's key."
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
      mkOptions cs = Map.fromList $ (Nothing, "Select chain") : map (first Just) cs

      staticCls = cls <> "select select_type_primary"
      mkDynCls v = if isNothing v then "select_mandatory_missing" else mempty
      allCls = renderClass <$> fmap mkDynCls d <> pure staticCls

      cfg = def & dropdownConfig_attributes .~ (("class" =:) <$> allCls)

    d <- _dropdown_value <$> dropdown Nothing (mkOptions <$> chains) cfg
    pure d


-- | Widget for selection of signing keys.
uiSigningKeys
  :: forall t m model. (MonadWidget t m, HasWallet model t)
  => model
  -> m (Dynamic t (Set KeyName))
uiSigningKeys aWallet = do
  let keyMap = aWallet ^. wallet_keys
      tableAttrs =
        "style" =: "table-layout: fixed; width: 100%" <> "class" =: "table"
  boxValues <- elAttr "table" tableAttrs $ do
    -- el "thead" $ elClass "tr" "table__row" $ do
    --   elClass "th" "table__heading" $ text "Sign with Key"
    --   elClass "th" "table__heading" $ text ""
    el "tbody" $ listWithKey keyMap $ \name key -> signingItem (name, key)
  dyn_ $ ffor keyMap $ \keys -> when (Map.null keys) $ text "No keys ..."
  return $ do -- The Dynamic monad
    m :: Map KeyName (Dynamic t Bool) <- boxValues
    ps <- traverse (\(k,v) -> (k,) <$> v) $ Map.toList m
    return $ Set.fromList $ map fst $ filter snd ps


------------------------------------------------------------------------------
-- | Display a key as list item together with it's name.
signingItem
  :: MonadWidget t m
  => (Text, Dynamic t KeyPair)
  -> m (Dynamic t Bool)
signingItem (n, _) = do
    elClass "tr" "table__row checkbox-container" $ mdo
      (e, ()) <- el' "td" $ text n
      let onTextClick = domEvent Click e
      box <- elClass "td" "signing-selector__check-box-cell" $ do
        let cfg = toggleCheckbox box onTextClick
        uiCheckbox "signing-selector__check-box-label" False cfg blank
      pure (value box)

toggleCheckbox :: Reflex t => Checkbox t -> Event t a -> CheckboxConfig t
toggleCheckbox box =
  (\v -> def { _checkboxConfig_setValue = v }) . fmap not . tag (current $ value box)
