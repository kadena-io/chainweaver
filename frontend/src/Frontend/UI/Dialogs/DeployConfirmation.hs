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
import           Control.Arrow                  ((&&&))
import           Control.Lens
import           Data.Bifunctor
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Void                      (Void)
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.Foundation
import           Frontend.Ide
import           Frontend.ModuleExplorer        (HasModuleExplorerCfg (..),
                                                 TransactionInfo (..))
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
    (settingsCfg, transInfo) <- modalBody $ do
      (uNetwork, uEndpoint) <- uiSegment mempty $ do
        elClass "h2" "heading heading_type_h2" $ text "Choose Server and Endpoint"
        divClass "target-selection" $ do
          uChain <- uiChainSelection (fmap (^? _2 . _Right) . _network_selectedNetwork $ _ide_network ideL)
          uEndpoint <- uiEndpointSelection Endpoint_Send
          pure (uChain, uEndpoint)

      (settingsCfg, signingKeys, _) <- uiSegment mempty $
        uiDeploymentSettings ideL Nothing

      pure
        ( settingsCfg
        , do
            s <- signingKeys
            mb <- uNetwork
            e <- uEndpoint
            pure $ TransactionInfo s <$> mb <*> pure e
        )

    modalFooter $ do
      onCancel <- cancelButton def "Cancel"
      text " "
      let isDisabled = maybe True (const False) <$> transInfo
      onConfirm <- confirmButton (def & uiButtonCfg_disabled .~ isDisabled) "Deploy"

      -- TODO: Use `networkCfg_deployCode` instead.
      let cfg = mempty & moduleExplorerCfg_deployEditor .~
            fmapMaybe id (tagPromptlyDyn transInfo onConfirm)
      pure (cfg <> settingsCfg, leftmost [onClose, onCancel, onConfirm])


uiChainSelection
  :: MonadWidget t m
  => Dynamic t (Maybe NodeInfo)
  -> m (Dynamic t (Maybe ChainId))
uiChainSelection info = do
  let chains = map (id &&& tshow) . maybe [] getChains <$> info
      mkOptions cs = Map.fromList $ (Nothing, "Deployment Chain") : map (first Just) cs

      cfg = def & dropdownConfig_attributes .~ pure ("class" =: "select select_type_primary target_selection_chain")
  d <- dropdown Nothing (mkOptions <$> chains) cfg
  pure $ _dropdown_value d
