{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Tab bar widget for chainweaver.
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.UI.TabBar
  ( -- * Types and Classess
    MkTabLabel
  , TabBarType (..)
  , TabBarCfg (..)
  , HasTabBarCfg (..)
  , TabBar (..)
  , HasTabBar (..)
    -- * Creation
  , makeTabBar
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Reflex
import           Reflex.Dom.Core
------------------------------------------------------------------------------
import           Frontend.Foundation
import           Frontend.UI.Button
------------------------------------------------------------------------------


-- | A tab needs to have a label, so let the user provide some creator function.
type MkTabLabel t m tab = Dynamic t Bool -> tab -> m ()

-- | UI styling: Primary/secondary?
data TabBarType
  = TabBarType_Primary
  | TabBarType_Secondary


-- | Tab bar configuration.
data TabBarCfg t m tab = TabBarCfg
    { _tabBarCfg_tabs :: [tab] -- ^ The available tabs
    , _tabBarCfg_mkLabel :: MkTabLabel t m tab -- ^ Function for printing labels for the tabs.
    , _tabBarCfg_selectedTab :: MDynamic t tab -- ^ The currently selected tab.
    , _tabBarCfg_classes :: CssClass -- ^ Additional CSS classes for this widget.
    , _tabBarCfg_type :: TabBarType -- ^ What style to use.
    }

makePactLenses ''TabBarCfg

-- Not very useful for this config:
{- instance (Reflex t, DomBuilder t m) => Default (TabBarCfg t m tab) where -}
{-   def = TabBarCfg  -}
{-     { _tabBarCfg_tabs = [] -}
{-     , _tabBarCfg_mkLabel = \_ _ -> text "Unnamed tab" -}
{-     , _tabBarCfg_selectedTab = pure Nothing -}
{-     , _tabBarCfg_classes = mempty -}
{-     , _tabBarCfg_type = TabBarType_Primary -}
{-     } -}

newtype TabBar t tab = TabBar
    { _tabBar_clicked    :: Event t tab
    -- ^ Some tab selector got clicked/selected by the user.
    }

makePactLenses ''TabBar


makeTabBar
  :: (DomBuilder t m, PostBuild t m, Eq tab)
  => TabBarCfg t m tab
  -> m (TabBar t tab)
makeTabBar cfg = do
  let
    typeCls =
      case _tabBarCfg_type cfg of
        TabBarType_Primary   -> "tab-nav_type_primary"
        TabBarType_Secondary -> "tab-nav_type_secondary"

  elKlass "div" ("tab-nav" <> typeCls <> _tabBarCfg_classes cfg) $ do
    selEvs <- traverse (tabSelector cfg) $ _tabBarCfg_tabs cfg
    pure $ TabBar $ leftmost selEvs


tabSelector
  :: (PostBuild t m, DomBuilder t m, Eq tab)
  => TabBarCfg t m tab -> tab -> m (Event t tab)
tabSelector cfg ourTab = do
  let
    baseCfg = "tab-nav__button" <>
      case _tabBarCfg_type cfg of
        TabBarType_Primary   -> "tab-nav__button_type_primary"
        TabBarType_Secondary -> "tab-nav__button_type_secondary"

    renderLabel = _tabBarCfg_mkLabel cfg

    f sel = baseCfg <> if sel then "tab-nav__button_active" else mempty
    isSelected = (Just ourTab ==) <$> _tabBarCfg_selectedTab cfg

  onClick <- uiButtonDyn (def & uiButtonCfg_class .~ fmap f isSelected) $
    renderLabel isSelected ourTab
  pure $ ourTab <$ onClick
