{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Confirmation dialog for deploying modules and calling functions on the
-- network.
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.DeployConfirmation
  ( DeployConfirmationConfig (..)
  , HasDeployConfirmationConfig (..)

  , CanSubmitTransaction
  , TransactionSubmitFeedback (..)
  , Status (..)
  , statusText

  , submitTransactionWithFeedback

  , uiDeployConfirmation
  , fullDeployFlow
  , fullDeployFlowWithSubmit
  , deploySubmit
  ) where

import Common.Foundation
import Common.Wallet
import Control.Concurrent (newEmptyMVar, tryTakeMVar, putMVar, killThread, forkIO, ThreadId)
import Control.Lens
import Control.Monad (void)
import Control.Monad.Ref (MonadRef, Ref)
import GHC.IORef (IORef)
import Data.Default (Default (..))
import Data.Either (rights)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.These (These(This, That))
import Data.Traversable (for)
import Frontend.Crypto.Class
import Frontend.JsonData
import Frontend.Network
import Frontend.UI.DeploymentSettings
import Frontend.UI.Modal
import Frontend.UI.Widgets
import Frontend.Wallet
import Language.Javascript.JSaddle
import Pact.Types.PactError (PactError)
import Pact.Types.PactValue (PactValue)
import Pact.Types.Pretty
import Reflex
import Reflex.Host.Class (MonadReflexCreateTrigger)
import Reflex.Dom
import Reflex.Extended (tagOnPostBuild)
import Reflex.Network.Extended (Flattenable)
import Reflex.Network.Extended (flatten)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Pact.Server.ApiV1Client as Api
import qualified Pact.Types.API as Api
import qualified Pact.Types.Command as Pact
import qualified Servant.Client.JSaddle as S
import qualified Text.URI as URI

data DeployConfirmationConfig t = DeployConfirmationConfig
  { _deployConfirmationConfig_modalTitle :: Text
  , _deployConfirmationConfig_previewTitle :: Text
  , _deployConfirmationConfig_previewConfirmButtonLabel :: Text
    -- The confirmation button in the preview screen will be disabled if the network
    -- returns an error. Some processes like the signing API need to override this as the
    -- responsibility for the transaction being signed lies with the creator, not the
    -- person doing the signing. This function will be called twice, once with the bool
    -- value of the status of the network call, and again with the setting for the
    -- 'disabled' attribute on the preview confirm button.
    --
    -- A failed transaction would call this function twice like so:
    -- 1) network failure: False
    -- 2) button disabled: True
    --
  , _deployConfirmationConfig_disregardSubmitResponse :: Bool -> Bool
  }

makeClassy ''DeployConfirmationConfig

instance Reflex t => Default (DeployConfirmationConfig t) where
  def = DeployConfirmationConfig "Transaction Details" "Transaction Preview" "Create Transaction" id

type CanSubmitTransaction t m =
  ( DomBuilder t m
  , MonadJSM (Performable m)
  , MonadJSM m
  , MonadRef (Performable m)
  , MonadRef m
  , HasDocument m
  , HasJSContext (Performable m)
  , HasJSContext m
  , PostBuild t m
  , TriggerEvent t m
  , MonadReflexCreateTrigger t m
  , PerformEvent t m
  , MonadHold t m
  , MonadFix m
  , MonadSample t (Performable m)
  , Ref (Performable m) ~ IORef
  , DomBuilderSpace m ~ GhcjsDomSpace
  , Control.Monad.Ref.Ref m ~ IORef
  )

-- We may not need the Status part of the workflow, but we want to be able to reuse as
-- much of this as we can so we have this function type that will give us the event of the
-- next stage or the completion of the workflow.
type DeployPostPreview key t m modelCfg a =
     ChainId
  -> DeploymentSettingsResult key
  -> Event t ()
  -> Event t ()
  -> Behavior t [Either Text NodeInfo]
  -> m (Event t (Either a (Workflow t m (Text, (Event t a, modelCfg)))))

-- | Confirmation dialog for deployments.
--
--   User can make sure to deploy to the right network, has the right keysets,
--   the right keys, ...
uiDeployConfirmation
  :: forall key t m model modelCfg.
     ( MonadWidget t m, Monoid modelCfg, Flattenable modelCfg t
     , HasNetwork model t, HasNetworkCfg modelCfg t
     , HasJsonData model t, HasJsonDataCfg modelCfg t
     , HasWallet model key t, HasCrypto key (Performable m)
     )
  => Text
  -> model
  -> Event t () -> m (modelCfg, Event t ())
uiDeployConfirmation code model = fullDeployFlow def model $ do
  (settingsCfg, result, _) <- uiDeploymentSettings model $ DeploymentSettingsConfig
    { _deploymentSettingsConfig_chainId = userChainIdSelect
    , _deploymentSettingsConfig_userTab = Nothing
    , _deploymentSettingsConfig_userSections = []
    , _deploymentSettingsConfig_code = pure code
    , _deploymentSettingsConfig_sender = uiSenderDropdown def never
    , _deploymentSettingsConfig_data = Nothing
    , _deploymentSettingsConfig_ttl = Nothing
    , _deploymentSettingsConfig_nonce = Nothing
    , _deploymentSettingsConfig_gasLimit = Nothing
    , _deploymentSettingsConfig_caps = Nothing
    , _deploymentSettingsConfig_extraSigners = []
    , _deploymentSettingsConfig_includePreviewTab = True
    }
  pure (settingsCfg, result)

-- | Workflow taking the user through Config -> Preview -> Status
fullDeployFlow
  :: forall key t m model modelCfg.
     ( MonadWidget t m, Monoid modelCfg, Flattenable modelCfg t
     , HasNetwork model t
     , HasWallet model key t
     )
  => DeployConfirmationConfig t
  -> model
  -> m (modelCfg, Event t (DeploymentSettingsResult key))
  -> Event t () -> m (modelCfg, Event t ())
fullDeployFlow deployCfg model runner onClose =
  (fmap . fmap) (fromMaybe ()) <$> fullDeployFlowWithSubmit deployCfg model showSubmitModal runner onClose
  where
    showSubmitModal chain result done _next nodes =
      pure $ Right . deploySubmit chain result <$> nodes <@ done

-- | Workflow taking the user through Config -> Preview -> Status
fullDeployFlowWithSubmit
  :: forall key t m model modelCfg a.
     ( MonadWidget t m, Monoid modelCfg, Flattenable modelCfg t
     , HasNetwork model t
     , HasWallet model key t
     )
  => DeployConfirmationConfig t
  -> model
  -> DeployPostPreview key t m modelCfg a
  -> m (modelCfg, Event t (DeploymentSettingsResult key))
  -> Event t () -> m (modelCfg, Event t (Maybe a))
fullDeployFlowWithSubmit dcfg model onPreviewConfirm runner _onClose = do
  rec
    onClose <- modalHeader $ dynText title
    result <- workflow deployConfig
    let (title, (done', conf')) = fmap splitDynPure $ splitDynPure result
  conf <- flatten =<< tagOnPostBuild conf'
  let done = switch $ current done'
  pure (conf, leftmost [Just <$> done, Nothing <$ onClose])
  where
    deployConfig = Workflow $ do
      (settingsCfg, result) <- runner
      pure (( _deployConfirmationConfig_modalTitle dcfg
            , (never, settingsCfg)
            )
           , attachWith deployPreview (current $ model ^. wallet_accounts) result
           )
    deployPreview accounts result = Workflow $ do

      let chain = _deploymentSettingsResult_chainId result
      succeeded <- elClass "div" "modal__main transaction_details" $ do

        transactionInputSection (pure $ _deploymentSettingsResult_code result) (pure $ _deploymentSettingsResult_command result)
        void $ uiDeployDestination model $ predefinedChainIdDisplayed chain model

        let accountsToTrack = getAccounts accounts
              $ _deploymentSettingsResult_accountsToTrack result
        pb <- getPostBuild
        let localReq = case _deploymentSettingsResult_wrappedCommand result of
              Left _e -> []
              Right cmd -> pure $ NetworkRequest
                { _networkRequest_cmd = cmd
                , _networkRequest_chainRef = ChainRef Nothing chain
                , _networkRequest_endpoint = Endpoint_Local
                }
        responses <- performLocalRead (model ^. network) $ localReq <$ pb
        (errors, resp) <- fmap fanThese $ performEvent $ ffor responses $ \case
          [(_, errorResult)] -> parseNetworkErrorResult parseWrappedBalanceChecks errorResult
          n -> do
            liftIO $ T.putStrLn $ "Expected 1 response, but got " <> tshow (length n)
            pure $ This "Couldn't get a response from the node"

        divClass "title" $ text "Anticipated Transaction Impact (dis)"
        divClass "group segment" $ do
          let tableAttrs = "style" =: "table-layout: fixed; width: 100%" <> "class" =: "table"
          elAttr "table" tableAttrs $ do
            el "thead" $ el "tr" $ do
              let th = elClass "th" "table__heading" . text
              th "Account Name"
              th "Public Key"
              th "Change in Balance"
            accountBalances <- flip Map.traverseWithKey accountsToTrack $ \acc pk -> do
              bal <- holdDyn Nothing $ leftmost [Just Nothing <$ errors, Just . Map.lookup acc . fst <$> resp]
              pure (pk, bal)
            el "tbody" $ void $ flip Map.traverseWithKey accountBalances $ \acc (pk, balance) -> el "tr" $ do
              let displayBalance = \case
                    Nothing -> "Loading..."
                    Just Nothing -> "Error"
                    Just (Just b) -> tshow (unAccountBalance b) <> " KDA"
              el "td" $ text $ unAccountName acc
              el "td" $ divClass "wallet__key" $ text $ keyToText pk
              el "td" $ dynText $ displayBalance <$> balance

        divClass "title" $ text "Raw Response"
        _ <- divClass "group segment transaction_details__raw-response" $ runWithReplace (text "Loading...") $ leftmost
          [ text . renderCompactText . snd <$> resp
          , text <$> errors
          ]
        holdDyn False $ True <$ resp

      let ignoreSuccessStatus = _deployConfirmationConfig_disregardSubmitResponse dcfg

      (back, next) <- modalFooter $ do
        back <- uiButtonDyn def $ text "Back"
        let isDisabled = not . ignoreSuccessStatus <$> succeeded

        next <- uiButtonDyn
          (def & uiButtonCfg_class .~ "button_type_confirm" & uiButtonCfg_disabled .~ isDisabled)
          $ text (_deployConfirmationConfig_previewConfirmButtonLabel dcfg)

        pure (back, next)

      let done = gate (current $ fmap ignoreSuccessStatus succeeded) next

      (eDoneAfterConfirm, eNextWorkflow) <- fanEither
        <$> onPreviewConfirm chain result done next (current $ model ^. network_selectedNodes)

      pure
        ( (_deployConfirmationConfig_previewTitle dcfg, (eDoneAfterConfirm, mempty))
        , leftmost
          [ deployConfig <$ back
          , eNextWorkflow
          ]
        )

deploySubmit
  :: forall key t m a modelCfg.
     ( MonadWidget t m
     , Monoid modelCfg
     )
  => ChainId
  -> DeploymentSettingsResult key
  -> [Either a NodeInfo]
  -> Workflow t m (Text, (Event t (), modelCfg))
deploySubmit chain result nodeInfos = Workflow $ do
  let cmd = _deploymentSettingsResult_command result

  _ <- elClass "div" "modal__main transaction_details" $
    submitTransactionWithFeedback cmd chain nodeInfos

  done <- modalFooter $ uiButtonDyn (def & uiButtonCfg_class .~ "button_type_confirm") $ text "Done"
  pure
    ( ("Transaction Submit", (done, mempty))
    , never
    )

-- | Fork a thread. Here because upstream 'forkJSM' doesn't give us the thread ID
forkJSM' :: JSM () -> JSM ThreadId
forkJSM' a = askJSM >>= \j -> liftIO $ forkIO $ runJSM a j

data Status
  = Status_Waiting
  | Status_Working
  | Status_Failed
  | Status_Done
  deriving Eq

statusText :: Status -> Text
statusText = \case
  Status_Waiting -> "waiting"
  Status_Working -> "working"
  Status_Failed -> "failed"
  Status_Done -> "done"

getAccounts :: Accounts key -> Set AccountName -> Map AccountName PublicKey
getAccounts accounts = Map.restrictKeys (IntMap.foldr f Map.empty accounts)
  where f = \case
          SomeAccount_Deleted -> id
          SomeAccount_Account a -> Map.insert (_account_name a) $ _keyPair_publicKey $ _account_key a

data TransactionSubmitFeedback t = TransactionSubmitFeedback
  { _transactionSubmitFeedback_sendStatus :: Dynamic t Status
  , _transactionSubmitFeedback_listenStatus :: Dynamic t Status
  , _transactionSubmitFeedback_message :: Dynamic t (Maybe (Either PactError PactValue))
  }

submitTransactionWithFeedback
  :: CanSubmitTransaction t m
  => Pact.Command Text
  -> ChainId
  -> [Either a NodeInfo]
  -> m (TransactionSubmitFeedback t)
submitTransactionWithFeedback cmd chain nodeInfos = do
  transactionHashSection cmd
  -- Shove the node infos into servant client envs
  clientEnvs <- fmap catMaybes $ for (rights nodeInfos) $ \nodeInfo -> do
    getChainRefBaseUrl (ChainRef Nothing chain) (Just nodeInfo) >>= \case
      Left e -> do
        liftIO $ putStrLn $ T.unpack $ "deploySubmit: Couldn't get chainUrl: " <> e
        pure Nothing
      Right chainUrl -> case S.parseBaseUrl $ URI.renderStr chainUrl of
        Nothing -> do
          liftIO $ putStrLn $ "deploySubmit: Failed to parse chainUrl: " <> URI.renderStr chainUrl
          pure Nothing
        Just baseUrl -> pure $ Just $ S.mkClientEnv baseUrl
  let doReqFailover [] _ = pure Nothing
      doReqFailover (c:cs) request = S.runClientM request c >>= \case
        Left e -> do
          liftIO $ putStrLn $ "doReqFailover: " <> show e
          doReqFailover cs request
        Right r -> pure $ Just r
      newTriggerHold a = do
        (e, t) <- newTriggerEvent
        s <- holdDyn a e
        pure (s, liftIO . t)
  -- These maintain the UI state for each step and are updated as responses come in
  (sendStatus, send) <- newTriggerHold Status_Waiting
  (listenStatus, listen) <- newTriggerHold Status_Waiting
  --(confirmedStatus, confirm) <- newTriggerHold Status_Waiting
  (message, setMessage) <- newTriggerHold Nothing

  -- Only maintain one thread for getting the status
  thread <- liftIO newEmptyMVar
  let forkListen requestKey = do
        -- Kill the currently running thread and start a new thread
        liftIO (tryTakeMVar thread) >>= \case
          Nothing -> liftIO . putMVar thread =<< forkJSM' (getStatus requestKey)
          Just tid -> do
            liftIO $ killThread tid
            forkListen requestKey

      getStatus requestKey = do
        listen Status_Working
        --confirm Status_Waiting
        -- Wait for the result
        doReqFailover clientEnvs (Api.listen Api.apiV1Client $ Api.ListenerRequest requestKey) >>= \case
          Nothing -> listen Status_Failed
          Just (Api.ListenTimeout _i) -> listen Status_Failed
          Just (Api.ListenResponse commandResult) -> case (Pact._crTxId commandResult, Pact._crResult commandResult) of
            -- We should always have a txId when we have a result
            (Just _txId, Pact.PactResult (Right a)) -> do
              listen Status_Done
              setMessage $ Just $ Right a
              -- TODO wait for confirmation...
            (_, Pact.PactResult (Left err)) -> do
              listen Status_Failed
              setMessage $ Just $ Left err
            -- This case shouldn't happen
            _ -> listen Status_Failed

  -- Send the transaction
  pb <- getPostBuild
  onRequestKey <- performEventAsync $ ffor pb $ \() cb -> liftJSM $ do
    send Status_Working
    doReqFailover clientEnvs (Api.send Api.apiV1Client $ Api.SubmitBatch $ pure cmd) >>= \case
      Nothing -> send Status_Failed
      Just (Api.RequestKeys (requestKey :| _)) -> do
        send Status_Done
        liftIO $ cb requestKey
  performEvent_ $ liftJSM . forkListen <$> onRequestKey
  requestKey <- holdDyn Nothing $ Just <$> onRequestKey

  divClass "title" $ text "Transaction Status"
  divClass "group" $ do
    elClass "ol" "transaction_status" $ do
      let item ds = elDynAttr "li" (ffor ds $ \s -> "class" =: statusText s)
      item sendStatus $ text "Transaction sent to mempool"
      item listenStatus $ text "Transaction successfully mined in block"
      --item confirmedStatus $ text "Transaction confirmed (Depth: >2 blocks)"
    rec
      reloading <- holdDyn False $ leftmost [False <$ canReload, True <$ reload]
      reload <- confirmButton (def & uiButtonCfg_class .~ pure "extra-margin" & uiButtonCfg_disabled .~ reloading) "Reload Status"
      canReload <- delay 2 reload
    throttledReload <- throttle 2 reload
    performEvent_ $ attachWithMaybe (\k _ -> liftJSM . forkListen <$> k) (current requestKey) throttledReload
  divClass "title" $ text "Transaction Result"
  divClass "group" $ el "pre" $ do
    maybeDyn message >>= \md -> dyn_ $ ffor md $ \case
      Nothing -> text "Waiting for response..."
      Just a -> dynText $ prettyPrintNetworkErrorResult . either (This . pure . (Nothing,) .  NetworkError_CommandFailure) (That . (Nothing,)) <$> a

  pure $ TransactionSubmitFeedback sendStatus listenStatus message
