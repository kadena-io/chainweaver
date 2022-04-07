{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module WalletConnect.Wallet
  ( Session (..)
  , Proposal (..)
  , WalletConnect (..)
  , initWalletConnect
  , doNewPairing
  , module WalletConnect.Common
  )
  where

import Control.Concurrent
import           Control.Monad
import Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Lens                       hiding ((#))
import qualified Data.Aeson as A
import Data.Map (Map)
import qualified Data.Map as M
import           Data.Text                          (Text)
import           Language.Javascript.JSaddle
                                         hiding ( eval
                                                , jsf
                                                , js
                                                , js0
                                                , jss
                                                , js2
                                                , js3
                                                , jsg2
                                                , jsg
                                                , js1
                                                , (<#)
                                                , (!)
                                                )
import Reflex hiding (Request)
import Reflex.Network
import System.Timeout

import WalletConnect.Common
import WalletConnect.Internal

data Session = Session
  { _session_topic :: Topic
  , _session_disconnect :: JSM ()
  , _session_peer :: (PublicKey, Metadata)
  }

data Proposal = Proposal
  { _proposal_topic :: Topic
  , _proposal_ttl :: Int
  , _proposal_proposer :: (PublicKey, Metadata)
  , _proposal_permissions :: Permissions
  , _proposal_approval :: (Either () [Account] -> JSM ())
  }

data WalletConnect t = WalletConnect
  { _walletConnect_pairings :: Dynamic t (Map Topic Pairing)
  , _walletConnect_sessions :: Dynamic t (Map Topic Session)
  , _walletConnect_proposals :: Event t Proposal
  , _walletConnect_requests :: Event t (Topic, Request, Either () A.Value -> JSM ())
  , _walletConnect_client :: MVar JSVal
  }

initWalletConnect ::
  ( TriggerEvent t m
  , PerformEvent t m
  , MonadJSM m
  , MonadJSM (Performable m)
  , Adjustable t m
  , MonadHold t m
  , MonadFix m
  )
  => Maybe Text -- Relay URL
  -> Text       -- Project Id
  -> m (WalletConnect t)
initWalletConnect mRelayUrl projectId = do
  (reqEv, reqAction) <- newTriggerEvent
  (pairingsEv, pairingsAction) <- newTriggerEvent
  (sessionsEv, sessionsAction) <- newTriggerEvent
  (proposalEv, proposalAction) <- newTriggerEvent

  clientMVar <- liftIO $ newEmptyMVar

  liftJSM $ do
    clientPromise <- clientInit mRelayUrl projectId True
    clientPromise ^. js2 "then"
      (subscribeToEvents clientMVar reqAction proposalAction sessionsAction pairingsAction)
      logValueF -- TODO: handle errors

  rec
    pairings <- networkHold (pure mempty) $ ffor (attach (current pairings) pairingsEv) $ \(old, new) -> do
      client <- liftIO $ readMVar clientMVar
      (M.fromList <$>) $ forM new $ \(t, p) ->
        case M.lookup t old of
          Just o -> pure (t,o)
          Nothing -> (t,) <$> makePairing client p

  rec
    sessions <- networkHold (pure mempty) $ ffor (attach (current sessions) sessionsEv) $ \(old, new) -> do
      client <- liftIO $ readMVar clientMVar
      (M.fromList <$>) $ forM new $ \(t, s) ->
        case M.lookup t old of
          Just o -> pure (t,o)
          Nothing -> (t,) <$> makeSession client s

  return $ WalletConnect pairings sessions proposalEv reqEv clientMVar

doNewPairing :: (TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m))
  => WalletConnect t
  -> Event t Text
  -> m (Event t Bool)
doNewPairing walletConnect uriEv = performEventAsync $ ffor uriEv $ \uri -> \res -> liftJSM $ do
  mClient <- liftIO $ timeout fiveSec $ readMVar (_walletConnect_client walletConnect)
  case mClient of
    Nothing -> liftIO $ res False
    Just client -> do
      p <- doPair uri client
      let resp b = fun $ \_ _ v -> do
            -- logValue "Got pairing response"
            -- logValue v
            liftIO $ res b
      void $ p ^. js2 "then" (resp True) (resp False)
  where fiveSec = 5 * 1000 * 1000

makeSession :: (MonadJSM m) => JSVal -> JSVal -> m Session
makeSession client session = do
  -- liftJSM $ do
  --   logValue "makeSession"
  --   logValue session
  topic <- liftJSM $ valToText =<< session ! "topic"
  peer <- liftJSM $ getMetadataPublicKey =<< session ! "peer"
  let
    delete = do
      -- logValue $ "doing disconnect of " <> topic
      args <- do
        o <- create
        (o <# "topic") topic
        (o <# "reason") ("USER_DISCONNECTED" :: Text) -- todo
        pure o
      void $ client ^. js1 "disconnect" args

  return $ Session topic delete peer

subscribeToEvents clientMVar reqAction proposalAction sessionAction pairingsAction = fun $ \_ _ (client:_) -> do
  -- logValue ("subscribeToEvents" :: Text)
  -- logValue client

  liftIO $ putMVar clientMVar client

  wcc <- jsg "WalletConnectClient"
  events <- wcc ! "CLIENT_EVENTS"
  pairing <- events ! "pairing"
  session <- events ! "session"

  let
    onProposal = fun $ \_ _ [proposal] -> do
      -- logValue "onProposal"
      -- logValue proposal
      topic <- valToText =<< proposal ! "topic"
      permissions <- getPermissions proposal
      ttl <- fromJSValUnchecked =<< proposal ! "ttl"
      proposer <- do
        getMetadataPublicKey =<< proposal ! "proposer"
      liftIO $ proposalAction $ Proposal topic ttl proposer permissions (either (doReject proposal) (doApprove proposal))

    doReject proposal _ = do
      args <- do
        o <- create
        (o <# "proposal") proposal
        (o <# "reason") "NOT_APPROVED"
        pure o
      void $ client ^. js1 "reject" args

    doApprove proposal accounts = do
      response <- do
        o <- create
        state <- do
          o <- create
          (o <# "accounts") accounts
          pure o
        (o <# "state") state
        pure o
      args <- do
        o <- create
        (o <# "proposal") proposal
        (o <# "response") response
        pure o
      void $ client ^. js1 "approve" args

  proposal <- session ! "proposal"
  client ^. js2 "on" proposal onProposal

  let
    onRequest = fun $ \_ _ [requestEvent] -> do
      -- logValue "onRequest"
      -- logValue requestEvent
      topic <- valToText =<< requestEvent ! "topic"
      chainId <- valToText =<< requestEvent ! "chainId"
      req <- requestEvent ! "request"
      id' <- req ! "id"
      method <- valToText =<< req ! "method"
      params <- fromJSValUnchecked =<< req ! "params"
      let
        doSend v = do
          result <- mapM toJSVal v
          doRespond client topic id' result

      liftIO $ reqAction
        ( topic
        , Request chainId method params
        , doSend)

  request <- session ! "request"
  client ^. js2 "on" request onRequest

  let onPairingSync = fun $ \_ _ _ -> do
        -- logValue "onSync"
        readPairings

      readPairings = do
        s <- client ! "pairing"
        v <- fromJSValUncheckedListOf =<< s ! "values"
        tp <- forM v $ \pairing -> do
          t <- valToText =<< pairing ! "topic"
          pure (t, pairing)
        liftIO $ pairingsAction tp

  readPairings
  sync <- pairing ! "sync"
  void $ client ^. js2 "on" sync onPairingSync
  deleted <- pairing ! "deleted"
  void $ client ^. js2 "on" deleted onPairingSync

  let onSync = fun $ \_ _ _ -> do
        -- logValue "onSync"
        readSessions

      readSessions = do
        s <- client ! "session"
        v <- fromJSValUncheckedListOf =<< s ! "values"
        tp <- forM v $ \session -> do
          t <- valToText =<< session ! "topic"
          pure (t, session)
        liftIO $ sessionAction tp

  readSessions
  sync <- session ! "sync"
  void $ client ^. js2 "on" sync onSync
