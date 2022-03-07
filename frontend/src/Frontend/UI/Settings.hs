{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
--
module Frontend.UI.Settings where

import Control.Lens

import Data.Text (Text)
import Reflex
import Reflex.Dom.Core
import Obelisk.Generated.Static

import Frontend.Crypto.Class (HasCrypto)
import Frontend.AppCfg (EnabledSettings(..), FileFFI)
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
  , HasCrypto key (Performable m)

  )

uiSettings
  :: forall t m key model mConf
     . ( MonadWidget t m
       , HasNetwork model t
       , HasUiSettingModelCfg model mConf key m t
       )
  => EnabledSettings key t m
  -> model
  -> FileFFI t m
  -> m mConf
uiSettings enabledSettings model fileFFI = elClass "div" "icon-grid" $ do

  netCfg <- settingItemOnClick "Network" $(static "img/network.svg") $ \onOpen -> uiNetworkEdit model
    <$> current (model ^. network_selectedNetwork)
    <*> current (model ^. network_networks)
    <@ onOpen

  configs <- sequence $ catMaybes $
    [ ffor (_enabledSettings_changePassword enabledSettings) $ \changePassword -> do
      settingItem "Change Password" $(static "img/lock-light.svg") (uiChangePasswordDialog changePassword)
    , ffor (_enabledSettings_exportWallet enabledSettings) $ \exportWallet-> do
      settingItem "Export Wallet" $(static "img/export.svg") (uiExportWalletDialog exportWallet)
    , includeSetting _enabledSettings_transactionLog $ settingItem "Transaction Log" $(static "img/transaction-logs.svg")
        $ uiTxLogs fileFFI
    ]
  pure $ netCfg <> fold configs
  where
    includeSetting f s = if f enabledSettings then Just s else Nothing

settingItemOnClick
  :: ( DomBuilder t m
     , Monoid mConf
     , HasModalCfg mConf (Modal mConf m t) t
     )
  => Text
  -> Text
  -> (Event t () -> Event t (Modal mConf m t))
  -> m mConf
settingItemOnClick title iconUrl modalFn = do
  eClick <- iconGridCell $ IconGridCellConfig
    { _iconGridCellConfig_title = title
    , _iconGridCellConfig_iconUrl = iconUrl
    , _iconGridCellConfig_desc = Nothing
    }
  pure $ mempty & modalCfg_setModal .~ (Just <$> modalFn eClick)

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
