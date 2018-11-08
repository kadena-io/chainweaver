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
import           Control.Monad
import           Data.Bifunctor
import qualified Data.Map                as Map
import           Data.Maybe
import           Data.Set                (Set)
import qualified Data.Set                as Set
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
  -> m (Dynamic t (Maybe TransactionInfo))
uiDeployConfirmation ideL = do
  el "h3" $ text "Choose a server "

  let backends = ffor (_backend_backends $ _ide_backend ideL) $
        fmap (\(k, _) -> (k, unBackendName k)) . maybe [] Map.toList
      mkOptions bs = Map.fromList $ (Nothing, "Deployment Target") : map (first Just) bs
  d <- dropdown Nothing (mkOptions <$> backends) def
  pure $ value d

  signingKeys <- elClass "div" "key-chooser" $ do
    el "h3" $ text "Choose keys to sign with"
    signingKeysWidget $ _ide_wallet ideL
  pure $ do
    s <- signingKeys
    mb <- value d
    pure $ TransactionInfo s <$> mb

signingKeysWidget
  :: MonadWidget t m
  => Wallet t
  -> m (Dynamic t (Set KeyName))
signingKeysWidget aWallet = do
  let keyMap = aWallet ^. wallet_keys
  boxValues <- elAttr "table" ("style" =: "table-layout: fixed; width: 100%") $ do
    el "thead" $ el "tr" $ do
      elClass "th" "left-col" $ text "Key Name"
      el "th" $ text "Signing"
    el "tbody" $ listWithKey keyMap $ \name key -> signingItem (name, key)
  dyn_ $ ffor keyMap $ \keys -> when (Map.null keys) $ text "No keys ..."
  return $ do -- The Dynamic monad
    m <- boxValues -- Dynamic (Map KeyName (Dynamic Bool))
    ps <- sequence $ (\(k,v) -> (k,) <$> v) <$> Map.toList m
    return $ Set.fromList $ map fst $ filter snd ps


------------------------------------------------------------------------------
-- | Display a key as list item together with it's name.
signingItem
  :: MonadWidget t m
  => (Text, Dynamic t KeyPair)
  -> m (Dynamic t Bool)
signingItem (n, k) = do
    el "tr" $ do
      el "td" $ text n
      box <- elClass "td" "centercell" $ checkbox False $ def
      pure (value box)
