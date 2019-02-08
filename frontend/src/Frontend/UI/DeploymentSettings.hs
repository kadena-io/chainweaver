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
  ( uiDeploymentSettings
  , signingKeysWidget
  ) where

------------------------------------------------------------------------------
import           Control.Arrow               (first)
import           Control.Lens
import           Control.Monad
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Pact.Parse                  (ParsedDecimal (..),
                                              ParsedInteger (..))
import           Pact.Types.Command          (PublicMeta (..))
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Contrib.CssClass (elKlass)
import           Reflex.Dom.Contrib.CssClass (addToClassAttr)
import           Safe                        (readMay)
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Foundation
import           Frontend.UI.TabBar
import           Frontend.UI.Widgets
import           Frontend.Wallet
------------------------------------------------------------------------------

data DeploymentSettingsView
  = DeploymentSettingsView_Custom Text -- ^ An optional additonal tab.
  | DeploymentSettingsView_Settings -- ^ Actual settings like gas price/limit, ...
  | DeploymentSettingsView_Keys -- ^ Select keys for signing the transaction.
  deriving (Eq,Ord)

showSettingsTabName :: DeploymentSettingsView -> Text
showSettingsTabName (DeploymentSettingsView_Custom n) = n
showSettingsTabName DeploymentSettingsView_Keys       = "Sign"
showSettingsTabName DeploymentSettingsView_Settings   = "Settings"

-- | Show settings related to deployments to the user.
--
--
--   the right keys, ...
uiDeploymentSettings
  :: ( MonadWidget t m, HasBackend model t, HasWallet model t
     , Monoid mConf , HasBackendCfg mConf t
     )
  => model
  -> Maybe (Text, m a) -- ^ An optional additional tab.
  -> m (mConf, Dynamic t (Set KeyName), Maybe a)
uiDeploymentSettings m mUserTab = mdo
    let initTab = fromMaybe DeploymentSettingsView_Settings mUserTabName
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

      cfg <- tabPane mempty curSelection DeploymentSettingsView_Settings $
        elKlass "div" ("group") $ do

          onGasPriceTxt <- mkLabeledInput uiRealInputElement "Gas price" $
            fmap (showParsedDecimal . _pmGasPrice) $ m ^. backend_meta

          onGasLimitTxt <- mkLabeledInput uiIntInputElement "Gas limit" $
            fmap (showParsedInteger . _pmGasLimit) $ m ^. backend_meta

          onSender <- mkLabeledInput uiInputElement "Sender" $
            fmap _pmSender $ m ^. backend_meta

          onChainId <- mkLabeledInput uiInputElement "Chain id" $
            fmap _pmChainId $ m ^. backend_meta

          pure $ mempty
            & backendCfg_setSender .~ onSender
            & backendCfg_setChainId .~ onChainId
            & backendCfg_setGasPrice .~ fmapMaybe (readPact ParsedDecimal) onGasPriceTxt
            & backendCfg_setGasLimit .~ fmapMaybe (readPact ParsedInteger) onGasLimitTxt
      signingKeys <- tabPane mempty curSelection DeploymentSettingsView_Keys $
        signingKeysWidget m

      pure (cfg, signingKeys, mRes)
    where
      showParsedInteger :: ParsedInteger -> Text
      showParsedInteger (ParsedInteger i) = tshow i

      showParsedDecimal :: ParsedDecimal -> Text
      showParsedDecimal (ParsedDecimal i) = tshow i

      readPact wrapper =  fmap wrapper . readMay . T.unpack

      mUserTabCfg  = first DeploymentSettingsView_Custom <$> mUserTab
      mUserTabName = fmap fst mUserTabCfg
      userTabs = maybeToList mUserTabName
      stdTabs = [DeploymentSettingsView_Settings, DeploymentSettingsView_Keys]
      availableTabs = userTabs <> stdTabs

-- | Make labeled and segmented input.
mkLabeledInput
  :: (DomBuilder t m, er ~ EventResult, PostBuild t m, MonadFix m
     , MonadHold t m
     )
  => (InputElementConfig er t (DomBuilderSpace m) -> m (InputElement er (DomBuilderSpace m) t))
  -> Text -> Dynamic t Text -> m (Event t Text)
mkLabeledInput mkInput n v = elClass "div" "segment segment_type_tertiary labeled-input" $ do
  divClass "label labeled-input__label" $ text n
  uiInputView mkInput (def & initialAttributes %~ addToClassAttr "labeled-input__input") v


-- | Widget for selection of signing keys.
signingKeysWidget
  :: forall t m model. (MonadWidget t m, HasWallet model t)
  => model
  -> m (Dynamic t (Set KeyName))
signingKeysWidget aWallet = do
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
    elClass "tr" "table__row" $ do
      el "td" $ text n
      box <- elClass "td" "signing-selector__check-box-cell" $
        uiCheckbox "signing-selector__check-box-label" False def blank
      pure (value box)

