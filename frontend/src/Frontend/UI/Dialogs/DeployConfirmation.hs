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
import           Data.Bifunctor
import qualified Data.Map                as Map
import           Data.Maybe
import           Data.Text               (Text)
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Crypto.Ed25519 (keyToText)
import           Frontend.Foundation
import           Frontend.Ide
import           Frontend.JsonData       (HasJsonData (..))
import           Frontend.UI.JsonData    (uiJsonData)
import           Frontend.UI.Wallet
import           Frontend.Wallet
import           Frontend.Wallet         (HasWallet (..))
import           Frontend.Widgets
------------------------------------------------------------------------------

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
       )
uiDeployConfirmation ideL = do
  bCfg <- elClass "div" "ui segment" $ do
    el "h3" $ text "Choose a server "

    let backends = ffor (_backend_backends $ _ide_backend ideL) $
          fmap (\(k, _) -> (k, unBackendName k)) . maybe [] Map.toList
        mkOptions bs = Map.fromList $ (Nothing, "Deployment Target") : map (first Just) bs
    d <- dropdown Nothing (mkOptions <$> backends) def
    pure $ mempty & ideCfg_setDeployBackend .~ updated (value d)

  aCfg <- elClass "div" "key-chooser" $ do
    el "h3" $ text "Choose keys to sign with"
    keysCfg <- elClass "div" "ui segment" $
      uiAvailableKeys $ ideL ^. ide_wallet
    pure $ mempty
      { _ideCfg_wallet = keysCfg
      }
  pure (aCfg <> bCfg)
