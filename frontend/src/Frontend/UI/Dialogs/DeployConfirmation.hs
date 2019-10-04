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
  , fullDeployFlow
  ) where

import Common.Foundation
import Control.Applicative (liftA2)
import Control.Lens
import Control.Monad (void)
import Data.Decimal (Decimal)
import Data.Either (isLeft)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Frontend.Crypto.Ed25519
import Frontend.JsonData
import Frontend.Network
import Frontend.UI.DeploymentSettings
import Frontend.UI.Modal
import Frontend.UI.Wallet
import Frontend.UI.Widgets
import Frontend.Wallet
import Pact.Parse
import Pact.Types.Gas
import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.CssClass (renderClass)
import Reflex.Extended (tagOnPostBuild)
import Reflex.Network.Extended (Flattenable)
import Reflex.Network.Extended (flatten)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import qualified Data.Set as Set

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
uiDeployConfirmation code model = fullDeployFlow "Deployment Settings" model $ do
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
  pure (settingsCfg, result)

-- | Workflow taking the user through Config -> Preview -> Status (eventually)
fullDeployFlow
  :: forall t m model modelCfg.
     ( MonadWidget t m, Monoid modelCfg, Flattenable modelCfg t
     , HasNetwork model t, HasNetworkCfg modelCfg t
     , HasWallet model t
     )
  => Text -- ^ Title of modal
  -> model
  -> m (modelCfg, Event t DeploymentSettingsResult)
  -> Event t () -> m (modelCfg, Event t ())
fullDeployFlow cfgTitle model runner _onClose = do
  rec
    onClose <- modalHeader $ dynText title
    result <- workflow deployConfig
    let (title, (done', conf')) = fmap splitDynPure $ splitDynPure result
  conf <- flatten =<< tagOnPostBuild conf'
  let done = switch $ current done'
  pure (conf, onClose <> done)
  where
    deployConfig = Workflow $ do
      (settingsCfg, result) <- runner
      pure ((cfgTitle, (never, settingsCfg)), attachWith deployPreview (current $ model ^. wallet_keyAccounts) result)
    deployPreview keyAccounts result = Workflow $ elClass "div" "modal__main transaction_details" $ do

      let chain = _deploymentSettingsResult_chainId result
          sender = _deploymentSettingsResult_sender result

      transactionInputSection $ pure $ _deploymentSettingsResult_code result
      divClass "title" $ text "Destination"
      _ <- divClass "group segment" $ do
        transactionDisplayNetwork model
        predefinedChainIdDisplayed chain model

      let accountsToTrack = Set.insert sender $ getAccounts keyAccounts $ _deploymentSettingsResult_signingKeys result
      rec
        accountBalances <- trackBalancesFromPostBuild model chain accountsToTrack (void response)
        initialRequestsDone <- holdUniqDyn $ and <$> traverse (fmap isJust . view _2) accountBalances
        gotInitialBalances <- tagOnPostBuild initialRequestsDone
        let localReq = pure $ NetworkRequest
              { _networkRequest_cmd = _deploymentSettingsResult_command result
              , _networkRequest_chainRef = ChainRef Nothing chain
              , _networkRequest_endpoint = Endpoint_Local
              }
        response <- performLocalRead (model ^. network) $ localReq <$ gotInitialBalances

      divClass "title" $ text "Anticipated Transaction Impact"
      divClass "group segment" $ do
        void $ flip mkLabeledClsInput "Total Gas Cost" $ \c -> do
          let showGasPrice (GasPrice (ParsedDecimal i)) = tshow i
              gasPrice = _deploymentSettingsResult_gasPrice result
          void $ uiInputElement $ def
            & initialAttributes .~ "disabled" =: "" <> "class" =: renderClass c
            & inputElementConfig_initialValue .~ "Loading..."
            & inputElementConfig_setValue .~ ffor response (\case
              (_, Right (Just (Gas gasUnits), _)) : _ -> showGasPrice (fromIntegral gasUnits * gasPrice) <> " KDA"
              _ -> "Error")
        let tableAttrs = "style" =: "table-layout: fixed; width: 100%" <> "class" =: "table"
        elAttr "table" tableAttrs $ do
          el "thead" $ el "tr" $ do
            let th = elClass "th" "table__heading" . text
            th "Account Name"
            th "Key Name"
            th "Public Key"
            th "Change in Balance"
          el "tbody" $ void $ flip Map.traverseWithKey accountBalances $ \acc (publicKeys, initialBalance, updatedBalance) -> el "tr" $ do
            let displayBalance = \case
                  Nothing -> "Loading..."
                  Just Nothing -> "Error"
                  Just (Just b) -> tshow b <> " KDA"
            el "td" $ text $ unAccountName acc
            el "td" $ void $ simpleList publicKeys $ \pks -> do
              let name = fmap snd pks
              el "div" . dynText $ fmap (fromMaybe "") name
            el "td" $ void $ simpleList publicKeys $ \pks -> do
              let key = fmap fst pks
              divClass "wallet__key" . dynText $ fmap keyToText key
            el "td" $ dynText $ displayBalance <$> (liftA2 . liftA2 . liftA2) subtract initialBalance updatedBalance

      divClass "title" $ text "Raw Response"
      (_, txSuccess) <- divClass "group segment" $ runWithReplace (text "Loading...") $ ffor response $ \rs -> do
        traverse_ (text . prettyPrintNetworkErrorResult . snd) rs
        pure $ not $ any (isLeft . snd) rs || null rs
      succeeded <- holdDyn False txSuccess

      (back, next) <- modalFooter $ do
        back <- uiButtonDyn def $ text "Back"
        let isDisabled = not <$> succeeded
        next <- uiButtonDyn (def & uiButtonCfg_class .~ "button_type_confirm" & uiButtonCfg_disabled .~ isDisabled) $ text "Create Transaction"
        pure (back, next)

      let req = ffor (gate (current succeeded) next) $ \_ -> pure $ NetworkRequest
            { _networkRequest_cmd = _deploymentSettingsResult_command result
            , _networkRequest_chainRef = ChainRef Nothing chain
            , _networkRequest_endpoint = fromMaybe Endpoint_Local $ _deploymentSettingsResult_endpoint result
            }
          cfg = mempty & networkCfg_deployCode .~ req

      pure
        ( ("Deployment Preview", (next, cfg))
        , deployConfig <$ back
        )

-- | Track the balances of the given accounts from post build time.
-- Request updated balances on the occurance of the input event.
--
-- Return a tuple of (associated keys/names, initial balance, most recent balance).
trackBalancesFromPostBuild
  :: (MonadWidget t m, HasNetwork model t, HasWallet model t)
  => model -> ChainId -> Set AccountName -> Event t ()
  -> m
    ( Map AccountName (Dynamic t [(PublicKey, Maybe Text)]
    , Dynamic t (Maybe (Maybe Decimal))
    , Dynamic t (Maybe (Maybe Decimal)))
    )
trackBalancesFromPostBuild model chain accounts fire = getPostBuild >>= \pb -> sequence $ flip Map.fromSet accounts $ \acc -> do
  let publicKeys = getKeys <$> model ^. wallet_accountGuards <*> model ^. wallet_keys
      getKeys chains namesToKeyPairs =
        let keysOfAccount = Set.fromList $ maybe [] accountGuardKeys $ Map.lookup acc =<< Map.lookup chain chains
            flipMap f = Map.fromList . fmap (\(k,v) -> (f v, k)) . Map.toList
            keyNames = flipMap _keyPair_publicKey namesToKeyPairs
         in Map.toList $ Map.merge
              Map.dropMissing -- Drop any named keys which aren't associated with this account
              (Map.mapMissing $ \_ () -> Nothing) -- Keep this accounts keys which are not named
              (Map.zipWithMatched $ \_ n _ -> Just n) -- Keep this accounts keys which _are_ named
              keyNames
              (Map.fromSet (const ()) keysOfAccount)
  initialBalance <- holdDyn Nothing . fmap Just =<< getBalance model chain (acc <$ pb)
  updatedBalance <- holdDyn Nothing . fmap Just =<< getBalance model chain (acc <$ fire)
  pure (publicKeys, initialBalance, updatedBalance)

getAccounts :: KeyAccounts -> [KeyPair] -> Set AccountName
getAccounts keyAccounts pairs = mconcat $ Map.elems $ Map.restrictKeys keyAccounts (Set.fromList $ fmap _keyPair_publicKey pairs)
