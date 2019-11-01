{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
module Frontend.UI.Dialogs.AddVanityAccount
  ( uiAddVanityAccount
  ) where


import Control.Applicative (liftA2)

import Reflex
import Reflex.Dom.Core
import Reflex.Dom.Contrib.CssClass (renderClass)

import Reflex.Network.Extended (Flattenable)

import Frontend.UI.Modal.Impl (ModalIde)
import Frontend.UI.Widgets
import Frontend.UI.DeploymentSettings
import Frontend.UI.Dialogs.DeployConfirmation (DeployConfirmationConfig (..), fullDeployFlow)

import Frontend.AppCfg
import Frontend.Crypto.Class (HasCrypto)
import Frontend.JsonData
import Frontend.Network (HasNetworkCfg, defaultTransactionGasLimit)
import Frontend.Wallet (HasWalletCfg)

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
uiAddVanityAccount _appCfg ideL onClose = do
  let inputElem lbl wrapperCls = divClass wrapperCls $ flip mkLabeledClsInput lbl
        $ \cls -> uiInputElement $ def & initialAttributes .~ "class" =: renderClass cls

  let accountDetails = Just $ ("Reference Data",) $ liftA2 (,)
        (inputElem "Account Name" "vanity-account-create__account-name")
        (inputElem "Notes" "vanity-account-create__notes")

  let runner = do
        (mConf, result, _) <- uiDeploymentSettings ideL $ DeploymentSettingsConfig
          { _deploymentSettingsConfig_chainId = userChainIdSelect
          , _deploymentSettingsConfig_userTab = Nothing
          , _deploymentSettingsConfig_userSection = accountDetails
          , _deploymentSettingsConfig_code = constDyn "(coin.create-account )"
          , _deploymentSettingsConfig_sender = uiSenderDropdown def
          , _deploymentSettingsConfig_data = Nothing
          , _deploymentSettingsConfig_nonce = Nothing
          , _deploymentSettingsConfig_ttl = Nothing
          , _deploymentSettingsConfig_gasLimit = Just defaultTransactionGasLimit
          , _deploymentSettingsConfig_caps = Nothing
          , _deploymentSettingsConfig_extraSigners = []
          }
        pure (mConf, result)

  let cfg = DeployConfirmationConfig "Add New Vanity Account" "Next" "Create Vanity Account" id

  fullDeployFlow cfg ideL runner onClose
