{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Dialog for watching request keys
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.WatchRequest
  ( uiWatchRequestDialog
  ) where

import Control.Lens hiding (failover)
import Control.Error (hush)
import Control.Monad (foldM, unless)

import Data.List.NonEmpty (NonEmpty ((:|)))

import Data.Either (rights)
import qualified Data.HashMap.Strict as HM

import Reflex.Dom

import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Util as Pact
import qualified Pact.Types.API as Pact
import qualified Pact.Types.Pretty as Pact

import qualified Text.URI as URI
import qualified Servant.Client.JSaddle as S
import qualified Pact.Server.ApiClient as Api

import Frontend.Foundation hiding (Arg)
import Frontend.Network
import Frontend.UI.Modal
import Frontend.UI.Widgets
import Frontend.UI.Widgets.Helpers (dialogSectionHeading)

-- | A modal for watching request keys
uiWatchRequestDialog
  :: ( Flattenable mConf t
     , Monoid mConf
     , HasNetwork model t
     , MonadWidget t m
     )
  => model -> Event t () -> m (mConf, Event t ())
uiWatchRequestDialog model onCloseExternal = do
  (conf, closes) <- fmap splitDynPure $ workflow $ inputRequestKey model onCloseExternal
  mConf <- flatten =<< tagOnPostBuild conf
  let close = switch $ current closes
  pure (mConf, close)

-- | Allow the user to input a request key and chain
inputRequestKey
  :: ( Monoid mConf
     , MonadWidget t m
     , HasNetwork model t
     )
  => model
  -> Event t () -- ^ Modal was externally closed
  -> Workflow t m (mConf, Event t ())
inputRequestKey model _ = Workflow $ do
  let
    checkTxBtnLbl = "Check TX Status"
    nodes = fmap rights $ model ^. network_selectedNodes
    noResponseMsg = "Request Key not found"
    showPollResponse (Pact.PollResponses pollMap) = case HM.elems pollMap ^? _head of
      Nothing -> noResponseMsg
      Just commandResult -> case commandResult ^. Pact.crResult of
        Pact.PactResult (Left err) -> "Error: " <> Pact.tShow err
        Pact.PactResult (Right a) -> Pact.renderCompactText a

  close <- modalHeader $ text "Check TX Status"

  modalMain $ do
    dialogSectionHeading mempty "Notice"
    divClass "group" $ do
      text "If you have a request key from a previously submitted transaction, you can use this dialog to wait for and display the results."

    dialogSectionHeading mempty "Required Information"
    dRqKey <- (fmap . fmap) hush $ divClass "group" $ fmap snd $ uiRequestKeyInput False

    rec
      let lockRefresh = (||) <$> fmap isNothing dRqKey <*> blockSpam

      onRequest <- divClass "group check-tx-status__button-wrapper" $
        confirmButton (def & uiButtonCfg_disabled .~ lockRefresh) checkTxBtnLbl

      onTimeoutFinished <- delay 5 onRequest

      blockSpam <- toggle False $ leftmost
        [ onRequest
        , onTimeoutFinished
        ]

    onPollResponse <- performEventAsync $ pollRequestKey
      <$> current nodes
      <@> tagMaybe (current dRqKey) onRequest

    dResp <- holdDyn Nothing $ fmap Just onPollResponse

    dialogSectionHeading mempty "Transaction Result"
    divClass "group" $ do
      maybeDyn dResp >>= \md -> dyn_ $ ffor md $ \case
        Nothing -> text $ "Please enter a valid Request Key and click '" <> checkTxBtnLbl <> "'"
        Just a -> dynText $ maybe noResponseMsg showPollResponse <$> a

  done <- modalFooter $ do
    confirmButton def "Done"

  pure ( (mempty, done <> close)
       , never
       )

buildNodeChainClientEnvs :: MonadJSM m => NodeInfo -> m [(ChainId, S.ClientEnv)]
buildNodeChainClientEnvs node = let chains = getChains node in flip foldMapM chains $ \chain ->
  getChainRefBaseUrl (ChainRef (Just $ nodeInfoRef node) chain) (Just node) <&> \case
    Left _ -> []
    Right chainUrl -> S.parseBaseUrl (URI.renderStr chainUrl) & \case
      Nothing -> []
      Just baseUrl -> [(chain, S.mkClientEnv baseUrl)]

pollRequestKey
  :: MonadJSM m
  => [NodeInfo]
  -> Pact.RequestKey
  -> (Maybe Pact.PollResponses -> IO ())
  -> m ()
pollRequestKey nodes rKey cb = do
  responseReceived <- foldM
    (\respReceived node -> if respReceived then pure respReceived else buildEnvAndPoll node)
    False
    nodes

  unless responseReceived $
    liftIO $ cb Nothing
  where
    doPoll cEnvs = doReqFailover cEnvs (Api.poll Api.apiV1Client $ Pact.Poll (rKey :| [])) >>= \case
      Left _ -> pure False
      Right r -> True <$ liftIO (cb $ Just r)

    buildEnvAndPoll node = do
      envs <- buildNodeChainClientEnvs node
      doPoll $ fmap snd envs
