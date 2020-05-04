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
import Control.Monad

import Data.Aeson.Lens
import Data.List.NonEmpty (NonEmpty ((:|)))

import Data.Either (rights)
import qualified Data.HashMap.Strict as HM
import Data.Map (Map)
import qualified Data.Map as M

import Reflex.Dom
import System.Random (randomRIO)

import qualified Pact.Server.ApiClient as Api
import qualified Pact.Types.API as Pact
import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Hash as Pact
import qualified Pact.Types.Pretty as Pact
import qualified Pact.Types.Util as Pact

import qualified Text.URI as URI
import qualified Servant.Client.JSaddle as S

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

data RequestStatus a = InFlight | ReturnedNothing | ReturnedSomething a
  deriving (Eq,Ord,Show)

instance Semigroup a => Semigroup (RequestStatus a) where
  a@(ReturnedSomething _) <> ReturnedNothing = a
  ReturnedNothing <> b@(ReturnedSomething _) = b
  (ReturnedSomething a) <> (ReturnedSomething b) = ReturnedSomething (a <> b)
  ReturnedNothing <> ReturnedNothing = ReturnedNothing
  InFlight <> b = b
  a <> InFlight = a

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
    checkTxBtnLbl = "Check Tx Status"
    nodes = fmap rights $ model ^. network_selectedNodes

  close <- modalHeader $ text "Check Tx Status"

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

      blockSpam <- holdDyn False $ leftmost
        [ True <$ onRequest
        , False <$ onTimeoutFinished
        ]

    -- This delay gives the Transaction Result section time to update.
    afterRequest <- delay 0 onRequest
    onPollResponse <- performEventAsync $ pollRequestKey
      <$> current nodes
      <@> tagMaybe (current dRqKey) afterRequest

    dResp <- foldDyn ($) ReturnedNothing $ leftmost
      [ const InFlight <$ onRequest
      , accumRequests <$> onPollResponse
      ]

    dialogSectionHeading mempty "Transaction Result"
    divClass "group" $ do
      networkView $ ffor dResp $ \case
        InFlight -> text $ "Querying for transaction..."
        ReturnedNothing -> text $ "Please enter a valid Request Key and click '" <> checkTxBtnLbl <> "'"
        ReturnedSomething m -> showPollResponse m

  done <- modalFooter $ do
    confirmButton def "Done"

  pure ( (mempty, done <> close)
       , never
       )

accumRequests
  :: Map ChainId (RequestStatus [Pact.CommandResult Pact.Hash])
  -> RequestStatus (Map ChainId (RequestStatus [Pact.CommandResult Pact.Hash]))
  -> RequestStatus (Map ChainId (RequestStatus [Pact.CommandResult Pact.Hash]))
accumRequests a InFlight = ReturnedSomething a
accumRequests a ReturnedNothing = ReturnedSomething a
accumRequests a (ReturnedSomething b) = ReturnedSomething $ M.unionWith (<>) a b

showPollResponse :: DomBuilder t m => Map ChainId (RequestStatus [Pact.CommandResult Pact.Hash]) -> m ()
showPollResponse m =
    if M.size m == M.size (M.filter (==ReturnedNothing) m)
      then text "Your request key is not associated with an already processed transaction on any chain."
      else mapM_ showSingleResponse $ M.toList m
  where
    showSingleResponse (chainId, reqStatus) = case reqStatus of
      InFlight -> blank
      ReturnedNothing -> blank
      ReturnedSomething resList -> forM_ resList $ \commandResult ->
        case Pact._crResult commandResult of
          Pact.PactResult (Left err) -> text $ "Error: " <> Pact.tShow err
          Pact.PactResult (Right pv) -> do
            el "div" $ do
              text $ "Chain " <> _chainId chainId
              case Pact._crMetaData commandResult ^? _Just . key "blockHeight" . _Integer of
                Nothing -> blank
                Just h -> text $ ", height " <> tshow h
              text $ ": " <> Pact.renderCompactText pv

buildNodeChainClientEnvs :: NodeInfo -> [(ChainId, S.ClientEnv)]
buildNodeChainClientEnvs node = catMaybes $ map mkPair chains
  where
    chains = getChains node
    mkPair c = case S.parseBaseUrl (URI.renderStr $ getChainBaseUrl c node) of
                 Nothing -> Nothing
                 Just bu -> Just (c, S.mkClientEnv bu)

pollRequestKey
  :: MonadJSM m
  => [NodeInfo]
  -> Pact.RequestKey
  -> (Map ChainId (RequestStatus [Pact.CommandResult Pact.Hash]) -> IO ())
  -> m ()
pollRequestKey nodes rKey cb = do
    i <- liftIO $ randomRIO (0, length nodes - 1)
    let envs = buildNodeChainClientEnvs (nodes !! i)
        m0 = M.fromList $ map ((,InFlight) . fst) envs
        fireResult (c, Pact.PollResponses m) = liftIO $ cb $ case HM.toList m of
          [] -> M.insert c ReturnedNothing m0
          ((_,cr):_) -> M.insert c (ReturnedSomething [cr]) m0
    forM_ envs $ \pair -> do
      res <- pollOne pair
      either (\_ -> pure ()) fireResult res
  where
    pollOne (chain,cEnv) = do
      resp <- liftJSM $ S.runClientM (Api.poll Api.apiV1Client $ Pact.Poll (rKey :| [])) cEnv
      pure $ (chain,) <$> resp
