{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
import Frontend.UI.Dialogs.ChangePassword (uiChangePasswordDialog)
import Frontend.UI.Dialogs.ExportWallet (uiExportWalletDialog)
import Frontend.UI.Dialogs.TxLogs (uiTxLogs)
import Frontend.UI.IconGrid (IconGridCellConfig(..), iconGridCell)
import Frontend.UI.Modal


type HasUiSettingModelCfg model mConf key m t =
  ( Monoid mConf
  , Flattenable mConf t
  , HasModalCfg mConf (Modal mConf m t) t
  , Monoid (ModalCfg mConf t)
  , Flattenable (ModalCfg mConf t) t
  , HasNetworkCfg (ModalCfg mConf t) t
  , HasTransactionLogger m
  )

uiSettings
  :: forall t m key model mConf
     . ( MonadWidget t m
       , HasNetwork model t
       , HasUiSettingModelCfg model mConf key m t
       )
  => EnabledSettings
  -> model
  -> m mConf
uiSettings enabledSettings model = elClass "div" "icon-grid" $ do
  netCfg <- settingItem "Network" (static @"img/network.svg") (uiNetworkEdit model)
  configs <- sequence $ catMaybes $
    [ ffor (_enabledSettings_changePassword enabledSettings) $ \changePassword -> do
      settingItem "Change Password" (static @"img/lock-light.svg") (uiChangePasswordDialog changePassword)
    , ffor (_enabledSettings_exportWallet enabledSettings) $ \exportWallet-> do
      -- TODO: Need to center the svg properly
      settingItem "Export Wallet" (static @"img/export.svg") (uiExportWalletDialog exportWallet)
    , includeSetting _enabledSettings_transactionLog $ settingItem "Transaction Log" (static @"img/network.svg")
        $ uiTxLogs model
    ]
  pure $ netCfg <> fold configs
  where
    _includeSetting f s = if f enabledSettings then Just s else Nothing

settingItem
  :: forall t m mConf
  . (DomBuilder t m, Monoid mConf, HasModalCfg mConf (Modal mConf m t) t)
  => Text -> Text -> Modal mConf m t -> m mConf
settingItem title iconUrl modal = do
  eClick <- iconGridCell $ IconGridCellConfig
    { _iconGridCellConfig_title = title
    , _iconGridCellConfig_iconUrl = iconUrl
    , _iconGridCellConfig_desc = Nothing
    }
  let
    eModal :: Event t (Maybe (Modal mConf m t))
    eModal = Just modal <$ eClick
  pure $ mempty & modalCfg_setModal .~ eModal
