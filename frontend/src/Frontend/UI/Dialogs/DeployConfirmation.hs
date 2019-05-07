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
import           Data.Void                      (Void)
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.Ide
import           Frontend.ModuleExplorer        (HasModuleExplorerCfg (..))
import           Frontend.Network
import           Frontend.UI.DeploymentSettings
import           Frontend.UI.Modal
import           Frontend.UI.Widgets
------------------------------------------------------------------------------


-- | Confirmation dialog for deployments.
--
--   User can make sure to deploy to the right network, has the right keysets,
--   the right keys, ...
uiDeployConfirmation
  :: forall t m a. MonadWidget t m
  => Ide a t
  -> m (IdeCfg Void t, Event t ())
uiDeployConfirmation ideL = do
  onClose <- modalHeader $ text "Deployment Settings"
  modalMain $ do
    (settingsCfg, transInfo, _) <- modalBody $
      uiSegment mempty $
        uiDeploymentSettings ideL $ DeploymentSettingsConfig
          { _deploymentSettingsConfig_chainId = userChainIdSelect
          , _deploymentSettingsConfig_defEndpoint = Endpoint_Send
          , _deploymentSettingsConfig_userTab = Nothing
          }

    modalFooter $ do
      onCancel <- cancelButton def "Cancel"
      text " "
      let isDisabled = maybe True (const False) <$> transInfo
      onConfirm <- confirmButton (deployBtnCfg isDisabled) "Deploy"

      -- TODO: Use `networkCfg_deployCode` instead.
      let cfg = mempty & moduleExplorerCfg_deployEditor .~
            fmapMaybe id (tagPromptlyDyn transInfo onConfirm)
      pure (cfg <> settingsCfg, leftmost [onClose, onCancel, onConfirm])

  where

    deployBtnCfg isDisabled = def
      & uiButtonCfg_disabled .~ isDisabled
      & uiButtonCfg_title .~ fmap deployToolTip isDisabled

    deployToolTip isDisabled = Just $
      if isDisabled
         then "You have to pick a chain!"
         else "Deploy to chain."
