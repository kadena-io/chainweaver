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
-- backend.
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.UI.Dialogs.DeployConfirmation
  ( uiDeployConfirmation
  ) where

------------------------------------------------------------------------------
import           Control.Arrow           ((&&&))
import           Control.Lens
import qualified Data.Map                as Map
import           Data.Maybe
import           Data.Text               (Text)
import           Reflex
import           Reflex.Dom.SemanticUI   hiding (mainWidget)

import           Frontend.Backend
import           Frontend.Crypto.Ed25519 (keyToText)
import           Frontend.Foundation
import           Frontend.Ide            (HasIde (..), HasIdeCfg (..), Ide (..),
                                          IdeCfg)
import           Frontend.JsonData       (HasJsonData (..))
import           Frontend.UI.JsonData    (uiJsonData)
import           Frontend.UI.Wallet      (uiWallet)
import           Frontend.Wallet
import           Frontend.Wallet         (HasWallet (..))
import           Frontend.Widgets

-- | Are we deploying a contract or calling a function?
{- data DeployConfirmationType -}
{-   = DeployConfirmationType_FunCall -}
{-   | DeployConfirmationType_Deploy -}

-- | Confirmation dialog for deployments.
--
--   User can make sure to deploy to the right backend, has the right keysets,
--   the right keys, ...
uiDeployConfirmation
  :: MonadWidget t m
  => Ide t
  -> m ( IdeCfg t  -- ^ Any changes the user might have triggered.
       , Event t () -- ^ On close.
       )
uiDeployConfirmation ideL =
  elClass "div" "ui standard modal pact-modal" $ do
    elClass "div" "header" $ text "Check your deployment settings"

    -- PostBuild does not trigger for some reason ... not working in
    -- networkView widget?
    (onPostBuild, fakePostBuild) <- newTriggerEvent
    liftIO $ fakePostBuild ()

    _ <- widgetHold (text "nope") $ text "hahahahaahahah" <$ onPostBuild
    bCfg <- elClass "div" "ui segment" $ do
      elClass "div" "ui header" $ text "Backend"

      bI <- input (def & inputConfig_action .~ Static (Just RightAction)) $ do
        let dropdownConfig = def
              & dropdownConfig_placeholder .~ "Deployment Target"
        fmap value $ dropdown dropdownConfig Nothing $ TaggedDynamic $
          ffor (ideL ^. ide_backend . backend_backends) $
            Map.fromList . fmap (\(k, _) -> (k, text $ unBackendName k)) . maybe [] Map.toList
      pure $ mempty & ideCfg_setDeployBackend  .~ leftmost
        [ updated bI
        , Nothing <$ onPostBuild -- Reset to Nothing, so user has to explicitely
        -- pick a backend for deployments to work. Also currently this is needed to
        -- keep the dropdown with our model in sync.
        ]

    aCfg <- elClass "div" "content ui fluid accordion" $ do
      jsonCfg <- accordionItem True "ui json-data-accordion-item" "Data" $
        elClass "div" "json-data full-size" $
          uiJsonData (ideL ^. ide_wallet) (ideL^. ide_jsonData)

      keysCfg <- accordionItem True "ui keys" "Keys" $
        elClass "div" "ui segment" $
          uiWallet $ ideL ^. ide_wallet
      pure $ mconcat [ jsonCfg, keysCfg ]
    elClass "div" "actions" $ do
      onCancel <- makeClickable $ elClass' "div" "ui black deny button" $
        text "Cancel"
      onAccept <- makeClickable $ elClass' "div" "ui positive right button" $
        text "Deploy"

      let
        lCfg = mempty & ideCfg_deploy .~ onAccept
      pure (aCfg <> bCfg <> lCfg, leftmost [onCancel, onAccept])


