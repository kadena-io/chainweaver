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

-- | Confirmation dialog for deploying modules and calling functions on the
-- network.
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.DeployConfirmation
  ( uiDeployConfirmation
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad                  (void)
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (Text)
import           Data.Void                      (Void)
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.Ide
import           Frontend.Network
import           Frontend.UI.DeploymentSettings
import           Frontend.UI.Modal
------------------------------------------------------------------------------

-- | Confirmation dialog for deployments.
--
--   User can make sure to deploy to the right network, has the right keysets,
--   the right keys, ...
uiDeployConfirmation
  :: forall t m a. MonadWidget t m
  => Text
  -> Ide a t
  -> Event t () -> m (IdeCfg Void t, Event t ())
uiDeployConfirmation code ideL _onClose = do
  onClose <- modalHeader $ text "Deployment Settings"
  (settingsCfg, result, _) <- uiDeploymentSettings ideL $ DeploymentSettingsConfig
    { _deploymentSettingsConfig_chainId = userChainIdSelect
    , _deploymentSettingsConfig_defEndpoint = Just Endpoint_Send
    , _deploymentSettingsConfig_userTab = Nothing
    , _deploymentSettingsConfig_code = pure code
    , _deploymentSettingsConfig_sender = uiSenderDropdown def
    , _deploymentSettingsConfig_data = Nothing
    , _deploymentSettingsConfig_ttl = Nothing
    , _deploymentSettingsConfig_nonce = Nothing
    , _deploymentSettingsConfig_gasLimit = Nothing
    }
  let req = ffor result $ \(me, c, cmd) -> [NetworkRequest cmd (ChainRef Nothing c) (fromMaybe Endpoint_Local me)]
      cfg = mempty & networkCfg_deployCode .~ req
  pure (cfg <> settingsCfg, onClose <> void req)
