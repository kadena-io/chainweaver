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
{-# LANGUAGE TypeApplications      #-}

-- | Dialog presented for generating responses to signing API requests.
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.Signing
  ( uiSigning
  ) where

import Control.Monad ((<=<))
import Kadena.SigningApi
import Reflex
import Reflex.Dom

import Frontend.AppCfg
import Frontend.Crypto.Ed25519 (fromPactPublicKey)
import Frontend.Foundation hiding (Arg)
import Frontend.JsonData
import Frontend.Network
import Frontend.UI.DeploymentSettings
import Frontend.UI.Dialogs.DeployConfirmation (DeployConfirmationConfig (..), fullDeployFlowWithSubmit)
import Frontend.UI.Modal.Impl
import Frontend.Wallet

type HasUISigningModelCfg mConf t =
  ( Monoid mConf, Flattenable mConf t, HasWalletCfg mConf t
  , HasJsonDataCfg mConf t, HasNetworkCfg mConf t
  )

-- | Ask user for confirmation before deleting "something".
--
-- At the moment "something" is only keys, so we skip making "something"
-- configurable for now.
--
uiSigning
  :: forall t m mConf
  . ( MonadWidget t m
    , HasUISigningModelCfg mConf t
    )
  => AppCfg t m
  -> ModalIde m t
  -> SigningRequest
  -> Event t ()
  -> m (mConf, Event t ())
uiSigning appCfg ideL signingRequest onCloseExternal = do
  let runner = do
        (mConf, result, _) <- uiDeploymentSettings ideL $ DeploymentSettingsConfig
          { _deploymentSettingsConfig_chainId = case _signingRequest_chainId signingRequest of
              Just c -> predefinedChainIdDisplayed c
              Nothing -> userChainIdSelect
          , _deploymentSettingsConfig_defEndpoint = Nothing
          , _deploymentSettingsConfig_userTab = Nothing
          , _deploymentSettingsConfig_code = pure $ _signingRequest_code signingRequest
          , _deploymentSettingsConfig_sender = case _signingRequest_sender signingRequest of
              Just sender -> \_ _ -> uiSenderFixed sender
              Nothing -> uiSenderDropdown def
          , _deploymentSettingsConfig_data = _signingRequest_data signingRequest
          , _deploymentSettingsConfig_nonce = _signingRequest_nonce signingRequest
          , _deploymentSettingsConfig_ttl = _signingRequest_ttl signingRequest
          , _deploymentSettingsConfig_gasLimit = _signingRequest_gasLimit signingRequest
          , _deploymentSettingsConfig_caps = Just $ _signingRequest_caps signingRequest
          , _deploymentSettingsConfig_extraSigners = fromPactPublicKey <$> fromMaybe [] (_signingRequest_extraSigners signingRequest)
          }
        pure (mConf, result)


  (conf, done) <- fullDeployFlowWithSubmit
    (DeployConfirmationConfig "Signing Request" "Signing Preview" "Confirm Signature" disregardSuccessStatus)
    ideL
    signSubmit
    runner
    onCloseExternal

  finished <- performEvent . fmap (liftJSM . _appCfg_signingResponse appCfg) <=< headE $
    maybe (Left "Cancelled") Right <$> leftmost [done, Nothing <$ onCloseExternal]

  pure (conf, finished)
  where
    -- The confirm process should proceed regardless of the response from the network, the
    -- failure of this is the responsibility of the dApp. This ensures the confirm button
    -- is not disabled and that the network response is treated as a "success"
    disregardSuccessStatus = (|| True)

    signSubmit _chain result _done next _nodes = do
      let sign = SigningResponse
            { _signingResponse_chainId = _deploymentSettingsResult_chainId result
            , _signingResponse_body = _deploymentSettingsResult_command result
            }
      -- This is the end of our work flow, so return our done event on the completion of the signing.
      -- Should some feedback be added to this to ensure that people don't spam the button?
      pure $ Left sign <$ next

