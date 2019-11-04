{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.UI.Dialogs.AddVanityAccount
  ( uiAddVanityAccount
  ) where

import Control.Error (hush)
import Control.Lens ((^.))
import Control.Applicative (liftA2)
import Data.Maybe (isNothing)
import Data.Text (Text)

import Reflex
import Reflex.Dom.Core
import Reflex.Dom.Contrib.CssClass (renderClass)

import Reflex.Extended (tagOnPostBuild)
import Reflex.Network.Extended (Flattenable, flatten)

import Frontend.UI.Modal.Impl (ModalIde,modalFooter, modalHeader)
import Frontend.UI.Widgets
import Frontend.UI.DeploymentSettings
import Frontend.UI.Dialogs.DeployConfirmation (deploySubmit)

import Frontend.AppCfg
import Frontend.Crypto.Class (HasCrypto)
import Frontend.JsonData
import Frontend.Ide (_ide_wallet)
import Frontend.Network (HasNetworkCfg, defaultTransactionGasLimit, networkCfg_setSender, network_selectedNodes)
import Frontend.Wallet (HasWalletCfg,unAccountName, checkAccountNameValidity)

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
uiAddVanityAccount _appCfg ideL _onCloseExternal = do
  rec
    onClose <- modalHeader $ dynText title
    result <- workflow $ uiAddVanityAccountSettings ideL
    let (title, (done', conf')) = fmap splitDynPure $ splitDynPure result
  conf <- flatten =<< tagOnPostBuild conf'
  let done = switch $ current done'
  pure (conf, leftmost [done, onClose])

uiAddVanityAccountSettings
  :: forall key t m mConf
  . ( MonadWidget t m
    , HasUISigningModelCfg mConf key t
    , HasCrypto key (Performable m)
    )
  => ModalIde m key t
  -> Workflow t m (Text, (Event t (), mConf))
uiAddVanityAccountSettings ideL = Workflow $ do
  let inputElem lbl wrapperCls = divClass wrapperCls $ flip mkLabeledClsInput lbl
        $ \cls -> uiInputElement $ def & initialAttributes .~ "class" =: (renderClass cls)

      validateAccountName = checkAccountNameValidity $ _ide_wallet ideL

      uiAccountNameInput = divClass "vanity-account-create__account-name" $ do
        dEitherAccName <- (validateAccountName <*>) . value <$>
          inputElem "Account Name" "vanity-account-create__account-name-input"

        _ <- divClass "vanity-account-create__account-name-error" $
          dyn_ $ ffor dEitherAccName $ either text (const blank)

        pure $ hush <$> dEitherAccName

      uiAcc = Just $ ("Reference Data",) $ liftA2 (,)
        uiAccountNameInput
        (inputElem "Notes" "vanity-account-create__notes")

      mkCode (dmAcc, _) = maybe "" (\acc -> "(coin.create-account " <> unAccountName acc <> " %guard)") <$> dmAcc

  rec
    (curSelection, done, _) <- buildDeployTabs Nothing controls

    (conf, result, accDetails) <- elClass "div" "modal__main transaction_details" $ do
      (cfg, cChainId, ttl, gasLimit, mAccountDetails) <- tabPane mempty curSelection DeploymentSettingsView_Cfg $
        -- Is passing around 'Maybe x' everywhere really a good way of doing this ?
        uiCfg Nothing ideL (userChainIdSelect ideL) Nothing (Just defaultTransactionGasLimit) uiAcc

      (mSender, capabilities) <- tabPane mempty curSelection DeploymentSettingsView_Keys $
        uiSenderCapabilities ideL cChainId Nothing $ uiSenderDropdown def ideL cChainId

      -- this seems, fragile
      let code = maybe (constDyn "") mkCode mAccountDetails

      let settings = DeploymentSettingsConfig
            { _deploymentSettingsConfig_chainId = userChainIdSelect
            , _deploymentSettingsConfig_userTab = Nothing
            , _deploymentSettingsConfig_userSection = uiAcc
            , _deploymentSettingsConfig_code = code
            , _deploymentSettingsConfig_sender = uiSenderDropdown def
            , _deploymentSettingsConfig_data = Nothing
            , _deploymentSettingsConfig_nonce = Nothing
            , _deploymentSettingsConfig_ttl = Nothing
            , _deploymentSettingsConfig_gasLimit = Nothing
            , _deploymentSettingsConfig_caps = Nothing
            , _deploymentSettingsConfig_extraSigners = []
            }

      pure
        ( cfg & networkCfg_setSender .~ fmapMaybe (fmap unAccountName) (updated mSender)
        , buildDeploymentSettingsResult ideL mSender cChainId capabilities ttl gasLimit code settings
        , mAccountDetails
        )

    command <- performEvent $ tagMaybe (current result) done
    controls <- modalFooter $ buildDeployTabFooterControls Nothing curSelection progressButtonLabalFn (isNothing <$> result)

  pure
    ( ("Add New Vanity Account", (never, conf))
    , attachWith
        (\ns res -> deploySubmit (_deploymentSettingsResult_chainId res) res ns)
        (current $ ideL ^. network_selectedNodes)
        command
    )
  where
    progressButtonLabalFn DeploymentSettingsView_Keys = "Create Vanity Account"
    progressButtonLabalFn _ = "Next"
