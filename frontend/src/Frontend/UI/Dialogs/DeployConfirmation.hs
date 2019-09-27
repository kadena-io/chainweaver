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
import           Control.Applicative            (liftA2)
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
import           Frontend.Wallet
import           Frontend.JsonData
import           Frontend.UI.DeploymentSettings
import           Reflex.Network.Extended (Flattenable)
import           Frontend.UI.Modal
------------------------------------------------------------------------------
import qualified Pact.Types.Command as Pact
import Common.Foundation
import Frontend.UI.RightPanel
import Frontend.Wallet
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Map (Map)
import Data.Decimal (Decimal)
import Data.Traversable (for)
import Reflex.Extended (tagOnPostBuild)
import Frontend.UI.Widgets

-- | Confirmation dialog for deployments.
--
--   User can make sure to deploy to the right network, has the right keysets,
--   the right keys, ...
uiDeployConfirmation
  :: forall t m model modelCfg.
     ( MonadWidget t m, Monoid modelCfg, Flattenable modelCfg t
     , HasNetwork model t, HasNetworkCfg modelCfg t
     , HasJsonData model t, HasJsonDataCfg modelCfg t
     , HasWallet model t
     )
  => Text
  -> model
  -> Event t () -> m (modelCfg, Event t ())
uiDeployConfirmation code model _onClose = do
  rec
    onClose <- modalHeader $ dynText =<< holdDyn initialTitle updatedTitle
    -- Using runWithReplace directly here instead of workflows so that it is
    -- easier to return modelCfg. Only the first dialog needs to update things
    -- in the model. TODO this is really incorrect, because we also need to be
    -- able to go _back_ to the first dialog.
    ((initialTitle, cfgConfig, result), replaced) <- runWithReplace deployConfig $
      attachWith deployPreview (current $ model ^. wallet_keyAccounts) result
    let (doDeploy', updatedTitle) = splitE replaced
    doDeploy <- switchHold never doDeploy'
    let req = ffor doDeploy $ \(endpoint, chain, cmd) -> [NetworkRequest cmd (ChainRef Nothing chain) (fromMaybe Endpoint_Local endpoint)]
        cfg = mempty & networkCfg_deployCode .~ req
  pure (cfgConfig <> cfg, onClose <> void req)
  where
    deployConfig = do
      -- TODO don't throw away the config here, or data editor won't work
      (settingsCfg, result, _) <- uiDeploymentSettings model $ DeploymentSettingsConfig
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
      pure ("Deployment Settings", settingsCfg, result) -- attachWith deployPreview (current $ model ^. wallet_keyAccounts) result)
    deployPreview keyAccounts (pairs, sender, mEndpoint, chain, cmd) = elClass "div" "modal__main transaction_details" $ do

      transactionInputSection $ pure code
      divClass "title" $ text "Destination"
      divClass "group segment" $ do
        transactionDisplayNetwork model
        predefinedChainIdDisplayed chain model

      let accountsToTrack = Set.insert sender $ getAccounts keyAccounts pairs
      rec
        accountBalances <- trackBalancesFromPostBuild model chain accountsToTrack (void response)
        initialRequestsDone <- holdUniqDyn $ and <$> traverse (fmap isJust . fst) accountBalances
        gotInitialBalances <- tagOnPostBuild initialRequestsDone
        let localReq = [NetworkRequest cmd (ChainRef Nothing chain) Endpoint_Local]
        response <- performLocalRead (model ^. network) $ localReq <$ gotInitialBalances

      divClass "title" $ text "Anticipated Transaction Impact"
      divClass "group segment" $ do
        void $ flip mkLabeledClsInput "Total Gas Cost (KDA)" $ \_ -> do
          text "Not yet implemented" -- TODO
        let tableAttrs = "style" =: "table-layout: fixed; width: 100%" <> "class" =: "table"
        elAttr "table" tableAttrs $ do
          el "thead" $ el "tr" $ do
            let th = elClass "th" "table__heading" . text
            th "Account Name"
            th "Key Name"
            th "Public Key"
            th "Balance Before"
            th "Balance After"
            th "Change in Balance"
          el "tbody" $ flip Map.traverseWithKey accountBalances $ \acc (initialBalance, updatedBalance) -> el "tr" $ do
            let displayBalance = \case
                  Nothing -> "Loading..."
                  Just Nothing -> "Error"
                  Just (Just b) -> tshow b <> " KDA"
            el "td" $ text $ unAccountName acc
            el "td" $ text "Key name" -- TODO
            el "td" $ text "Public key" -- TODO
            el "td" $ dynText $ displayBalance <$> initialBalance
            el "td" $ dynText $ displayBalance <$> updatedBalance
            el "td" $ dynText $ displayBalance <$> (liftA2 . liftA2 . liftA2) subtract initialBalance updatedBalance

      divClass "title" $ text "Raw Response"
      (_, txSuccess) <- divClass "group segment" $ runWithReplace (text "Loading...") $ ffor response $ \case
        (req, Left es) : _ -> do
          text $ prettyPrintNetworkError es
          pure False
        (req, Right v) : _ -> do
          text $ tshow v
          pure True
        [] -> do
          text "No response."
          pure False
      succeeded <- holdDyn False txSuccess

      next <- modalFooter $ do
        --back <- uiButtonDyn backConfig $ text "Back"
        let isDisabled = not <$> succeeded
        uiButtonDyn (def & uiButtonCfg_class .~ "button_type_confirm" & uiButtonCfg_disabled .~ isDisabled) $ text "Create Transaction"

      pure ((mEndpoint, chain, cmd) <$ next, "Deployment Preview")

-- | Track the balances of the given accounts from post build time.
-- Request updated balances on the occurance of the input event.
--
-- Return a pair of (initial balance, most recent balance).
trackBalancesFromPostBuild
  :: (MonadWidget t m, HasNetwork model t)
  => model -> ChainId -> Set AccountName -> Event t () -> m (Map AccountName (Dynamic t (Maybe (Maybe Decimal)), Dynamic t (Maybe (Maybe Decimal))))
trackBalancesFromPostBuild model chain accounts fire = getPostBuild >>= \pb -> sequence $ flip Map.fromSet accounts $ \acc -> do
  initialBalance <- holdDyn Nothing . fmap Just =<< getBalance model chain (acc <$ pb)
  updatedBalance <- holdDyn Nothing . fmap Just =<< getBalance model chain (acc <$ fire)
  pure (initialBalance, updatedBalance)

getAccounts :: KeyAccounts -> [KeyPair] -> Set AccountName
getAccounts keyAccounts pairs = mconcat $ Map.elems $ Map.restrictKeys keyAccounts (Set.fromList $ fmap _keyPair_publicKey pairs)
