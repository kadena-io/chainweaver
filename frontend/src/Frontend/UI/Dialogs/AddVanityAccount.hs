module Frontend.UI.Dialogs.AddVanityAccount
  (
  ) where


import Frontend.UI.DeploymentSettings
import Frontend.UI.Dialogs.DeployConfirmation (DeployConfirmationConfig (..), fullDeployFlowWithSubmit)

-- Allow the user to create a 'vanity' account, which is an account with a custom name
-- that lives on the chain. Requires GAS to create.

type HasUISigningModelCfg mConf key t =
  ( Monoid mConf, Flattenable mConf t, HasWalletCfg mConf key t
  , HasJsonDataCfg mConf t, HasNetworkCfg mConf t
  )


uiAddVanityAccount
  :: forall key t m mConf
  . ( MonadWidget t m
    , HasUISigningModelCfg mConf key t
    , HasCrypto key (Performable m)
    )
  => AppCfg key t m
  -> ModalIde m key t
  -> Event t ()
  -> m (mConf, Event t ())
uiAddVanityAccount appCfg ideL onClose = do
  let runner = do
        (mConf, result, _) <- uiDeploymentSettings ideL $ DeploymentSettingsConfig
          { _deploymentSettingsConfig_chainId = userChainIdSelect
          , _deploymentSettingsConfig_userTab = Nothing
          , _deploymentSettingsConfig_userSection = Nothing
          , _deploymentSettingsConfig_code = Nothing
          , _deploymentSettingsConfig_sender = uiSenderDropdown def
          , _deploymentSettingsConfig_data = Nothing
          , _deploymentSettingsConfig_nonce = Nothing
          , _deploymentSettingsConfig_ttl = Nothing
          , _deploymentSettingsConfig_gasLimit = defaultGasLimit
          , _deploymentSettingsConfig_caps = Nothing
          , _deploymentSettingsConfig_extraSigners = []
          }
        pure (mConf, result)

  (conf, done) <- fullDeployFlowWithSubmit
    (DeployConfirmationConfig "Add New Vanity Account" "Next" "Create Vanity Account" disregardSuccessStatus)
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
