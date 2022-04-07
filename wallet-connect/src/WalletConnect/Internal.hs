{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module WalletConnect.Internal
  where

import           Control.Lens                       hiding ((#))
import           Control.Monad
import qualified Data.Aeson as A
import Data.Maybe (fromMaybe)
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

import qualified Language.Javascript.JSaddle   as JSaddle

import WalletConnect.Common

-- Note on isController
-- https://github.com/WalletConnect/walletconnect-docs/blob/main/docs/protocol/tech-spec.md
-- If you notice the proposer is identified by a controller boolean. When false
-- it means that the proposer will not control the settled session which means
-- that it does not update state, upgrade permissions and is bounded by the
-- permissions.
clientInit :: Maybe Text -> Text -> Bool -> JSM JSVal
clientInit mRelayUrl projectId isController = do
  wcc <- jsg "WalletConnectClient"
  client <- wcc ! "Client"
  args <- do
    o <- create
    (o <# "logger") ("debug" :: Text)
    -- The default specified in client (relay.wallet-connect.com) does not work
    -- so always specify one here.
    (o <# "relayUrl") (fromMaybe "wss://relay.walletconnect.org" mRelayUrl)
    (o <# "projectId") projectId
    (o <# "controller") isController
    pure o
  client ^. js1 "init" args

getMetadataPublicKey :: (MonadJSM m) => JSVal -> m (PublicKey, Metadata)
getMetadataPublicKey v = liftJSM $ do
  pk <- valToText =<< v ! "publicKey"
  (pk,) <$> getMetadata v

getMetadata :: (MonadJSM m) => JSVal -> m Metadata
getMetadata v = liftJSM $ do
  mMetadata <- mapM fromJSVal =<< maybeNullOrUndefined =<< v ! "metadata"
  pure $ case A.fromJSON <$> join mMetadata of
    Just (A.Success m) -> m
    _ -> Metadata "unknown" "" [] ""

getPermissions :: (MonadJSM m) => JSVal -> m Permissions
getPermissions obj = liftJSM $ do
  v <- fromJSValUnchecked =<< obj ! "permissions"
  pure $ case A.fromJSON v of
    A.Error _ -> Permissions [] []
    A.Success p -> p

makePairing :: MonadJSM m => JSVal -> JSVal -> m Pairing
makePairing client pairing = liftJSM $ do
  -- logValue "makePairing"
  -- logValue pairing
  topic <- valToText =<< pairing ! "topic"
  pk <- valToText =<< flip (!) "publicKey" =<< pairing ! "peer"
  metadata <- getMetadata =<< pairing ! "state"
  permissions <- getPermissions pairing
  let
    connect = void . doConnect client (Just topic)
    delete = do
      -- logValue $ "doing disconnect of " <> topic
      args <- do
        o <- create
        (o <# "topic") topic
        (o <# "reason") ("USER_DISCONNECTED" :: Text) -- todo
        pure o
      pairing <- client ! "pairing"
      void $ pairing ^. js1 "delete" args
  pure $ Pairing topic (pk, metadata) permissions connect delete

doConnect :: JSVal -> Maybe Topic -> (Permissions, Metadata) -> JSM JSVal
doConnect client mTopic (permissions, metadata) = do
  -- logValue "doConnect"
  args <- do
    o <- create
    (o <# "permissions") =<< toJSVal (A.toJSON permissions)
    (o <# "metadata") =<< toJSVal (A.toJSON metadata)
    forM mTopic $ \topic -> do
      pairing <- do
        o <- create
        (o <# "topic") topic
        pure o
      (o <# "pairing") pairing
    pure o
  -- logValue args
  client ^. js1 "connect" args

doPair uri client = do
  -- logValue "doPair"
  -- logValue uri
  args <- do
    o <- create
    (o <# "uri") uri
    pure o
  client ^. js1 "pair" args

doRequest :: JSVal -> Topic -> Request -> JSM JSVal
doRequest client topic (Request chainId method params) = do
  -- logValue "doRequest"
  -- logValue topic
  args <- do
    o <- create
    (o <# "topic") topic
    (o <# "chainId") =<< toJSVal chainId
    request <- do
      o <- create
      (o <# "method") =<< toJSVal method
      (o <# "params") =<< toJSVal params
      pure o
    (o <# "request") request
    pure o
  -- logValue args
  client ^. js1 "request" args

doRespond :: JSVal -> Topic -> JSVal -> Either () JSVal -> JSM ()
doRespond client topic id' result = do
  -- logValue "doRespond"
  -- logValue topic
  args <- do
    o <- create
    (o <# "topic") topic
    response <- do
      o <- create
      case result of
        Left _ -> do
          error <- do
            o <- create
            -- JSONRPC_REQUEST_METHOD_REJECTED
            (o <# "message") ("User rejected the request." :: Text)
            (o <# "code") (4001 :: Int) -- 4000 (EIP-1193)
            pure o
          (o <# "error") error
        Right v -> (o <# "result") v
      (o <# "jsonrpc") ("2.0" :: Text)
      (o <# "id") id'
      pure o
    (o <# "response") response
    pure o
  -- logValue args
  void $ client ^. js1 "respond" args

-- JSaddle APIs

-- Specialised for Text
eval t = JSaddle.eval (t :: Text)
jsf t = JSaddle.jsf (t :: Text)
js t = JSaddle.js (t :: Text)
jss t = JSaddle.jss (t :: Text)
js0 t = JSaddle.js0 (t :: Text)
js1 t = JSaddle.js1 (t :: Text)
js2 t = JSaddle.js2 (t :: Text)
js3 t = JSaddle.js3 (t :: Text)
jsg t = JSaddle.jsg (t :: Text)
jsg2 t = JSaddle.jsg2 (t :: Text)
valT t = val (t :: Text)
(<#) o t = (JSaddle.<#) o (t :: Text)
(!) o t = (JSaddle.!) o (t :: Text)

logValueF = fun $ \_ _ [value] -> logValue value

logValue value = do
  w <- jsg "console"
  w ^. js1 "log" value
  pure ()
