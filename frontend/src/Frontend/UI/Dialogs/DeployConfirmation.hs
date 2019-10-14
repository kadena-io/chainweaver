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
  ( DeployConfirmationConfig (..)
  , HasDeployConfirmationConfig (..)
  , uiDeployConfirmation
  , fullDeployFlow
  , fullDeployFlowWithSubmit
  ) where

import Common.Foundation
import Control.Applicative (liftA2)
import Control.Concurrent (newEmptyMVar, tryTakeMVar, putMVar, killThread, forkIO, ThreadId)
import Control.Lens
import Control.Monad (void)
import Data.Decimal (Decimal)
import Data.Default (Default (..))
import Data.Either (isLeft, rights)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Traversable (for)
import Frontend.Crypto.Ed25519
import Frontend.JsonData
import Frontend.Network
import Frontend.UI.DeploymentSettings
import Frontend.UI.Modal
import Frontend.UI.Wallet
import Frontend.UI.Widgets
import Frontend.Wallet
import Language.Javascript.JSaddle
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
import qualified Data.Text as T
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

-- We may not need the Status part of the workflow, but we want to be able to reuse as
-- much of this as we can so we have this function type that will give us the event of the
-- next stage or the completion of the workflow.
type DeployPostPreview t m modelCfg =
     ChainId
  -> DeploymentSettingsResult
  -> Event t ()
  -> Event t ()
  -> Behavior t [Either Text NodeInfo]
  -> m (Event t (Either () (Workflow t m (Text, (Event t (), modelCfg)))))

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
uiDeployConfirmation code model = fullDeployFlow def model $ do
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

-- | Workflow taking the user through Config -> Preview -> Status
fullDeployFlow
  :: forall t m model modelCfg.
     ( MonadWidget t m, Monoid modelCfg, Flattenable modelCfg t
     , HasNetwork model t
     , HasWallet model t
     )
  => DeployConfirmationConfig t
  -> model
  -> m (modelCfg, Event t DeploymentSettingsResult)
  -> Event t () -> m (modelCfg, Event t ())
fullDeployFlow deployCfg model runner onClose =
  fullDeployFlowWithSubmit deployCfg model showSubmitModal runner onClose
  where
    showSubmitModal chain result done _next nodes =
      pure $ Right . deploySubmit chain result <$> nodes <@ done

-- | Workflow taking the user through Config -> Preview -> Status
fullDeployFlowWithSubmit
  :: forall t m model modelCfg.
     ( MonadWidget t m, Monoid modelCfg, Flattenable modelCfg t
     , HasNetwork model t
     , HasWallet model t
     )
  => DeployConfirmationConfig t
  -> model
  -> DeployPostPreview t m modelCfg
  -> m (modelCfg, Event t DeploymentSettingsResult)
  -> Event t () -> m (modelCfg, Event t ())
fullDeployFlowWithSubmit dcfg model onPreviewConfirm runner _onClose = do
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
      pure (( _deployConfirmationConfig_modalTitle dcfg
            , (never, settingsCfg)
            )
           , attachWith deployPreview (current $ model ^. wallet_keyAccounts) result
           )
    deployPreview keyAccounts result = Workflow $ do

      let chain = _deploymentSettingsResult_chainId result
          sender = _deploymentSettingsResult_sender result
      succeeded <- elClass "div" "modal__main transaction_details" $ do

        transactionInputSection $ pure $ _deploymentSettingsResult_code result
        divClass "title" $ text "Destination"
        _ <- divClass "group segment" $ do
          transactionDisplayNetwork model
          predefinedChainIdDisplayed chain model

        let accountsToTrack = Set.insert sender
              $ getAccounts keyAccounts
              $ _deploymentSettingsResult_signingKeys result
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
        holdDyn False txSuccess

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
  :: forall t m a modelCfg.
     ( MonadWidget t m
     , Monoid modelCfg
     )
  => ChainId
  -> DeploymentSettingsResult
  -> [Either a NodeInfo]
  -> Workflow t m (Text, (Event t (), modelCfg))
deploySubmit chain result nodeInfos = Workflow $ do
      let cmd = _deploymentSettingsResult_command result
          code = _deploymentSettingsResult_code result
      elClass "div" "modal__main transaction_details" $ do
        transactionHashSection $ pure code

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
                  (Just txId, Pact.PactResult (Right a)) -> do
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
            let item ds = elDynAttr "li" (ffor ds $ \s -> "class" =: statusClass s)
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
            Just a -> dynText $ prettyPrintNetworkErrorResult . bimap NetworkError_CommandFailure (Nothing,) <$> a

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

statusClass :: Status -> Text
statusClass = \case
  Status_Waiting -> "waiting"
  Status_Working -> "working"
  Status_Failed -> "failed"
  Status_Done -> "done"

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
