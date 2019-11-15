{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--
module Frontend.UI.Settings where

import Control.Lens

import Data.Text (Text)
import Reflex
import Reflex.Dom.Core
import Obelisk.Generated.Static

import Frontend.AppCfg (EnabledSettings(..))
import Frontend.Foundation
import Frontend.Network
import Frontend.UI.Dialogs.NetworkEdit (uiNetworkEdit)
import Frontend.UI.Modal

type HasUiSettingModelCfg model mConf key m t =
  ( Monoid mConf
  , Flattenable mConf t
  , HasModalCfg mConf (Modal mConf m t) t
  , Monoid (ModalCfg mConf t)
  , Flattenable (ModalCfg mConf t) t
  , HasNetworkCfg (ModalCfg mConf t) t
  )

uiSettings
  :: forall t m key model mConf
  . (MonadWidget t m, HasNetwork model t, HasUiSettingModelCfg model mConf key m t)
  => EnabledSettings -> model -> m mConf
uiSettings enabledSettings model = do
  configs <- sequence $ catMaybes $
    [ includeSetting _enabledSettings_network $ settingItem "Network" (static @"img/network.svg") (uiNetworkEdit model)
    ]
  pure $ fold configs
  where
    includeSetting f s = if (f enabledSettings) then Just s else Nothing

settingItem
  :: forall t m mConf
  . (DomBuilder t m, Monoid mConf, HasModalCfg mConf (Modal mConf m t) t)
  => Text -> Text -> Modal mConf m t -> m mConf
settingItem title iconUrl modal = do
  (elt, _) <- elAttr' "div" ("class" =: "settings__cell") $ do
    elAttr "div" ("class" =: "settings__cell-icon" <> "style" =: ("background-image: url(" <> iconUrl <>")")) $ blank
    elClass "div" "settings__cell-header" $ text title
  let
    eModal :: Event t (Maybe (Modal mConf m t))
    eModal = Just modal <$ domEvent Click elt
  pure $ mempty & modalCfg_setModal .~ eModal
