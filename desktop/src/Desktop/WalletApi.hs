{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}

module Desktop.WalletApi where

import Control.Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Exception (bracket_, bracket)
import Control.Monad (void, (<=<))
import Control.Monad.IO.Class
import Control.Monad.Except (throwError)
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy (Proxy(..))
import qualified Data.Text.Encoding as T
import Kadena.SigningApi
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Wai
import Servant
import qualified Servant.Server as Servant

import Frontend.AppCfg

walletServer
  :: MonadIO m
  => IO ()
  -> IO ()
  -> Proxy a
  -> m ( MVarHandler (SigningRequest, Maybe a) SigningResponse
       , MVarHandler (QuickSignRequest, Maybe a) QuickSignResponse
       )
  -- Commented much of the old functionality out (vs. removing it) since we intend to revisit it in the near future
  -- -> m ( MVarHandler SigningRequest SigningResponse
  --      , MVarHandler () [PublicKey]
  --      , MVarHandler () (Map NetworkName [AccountName])
  --      )
walletServer moveToForeground moveToBackground _ = do
  signingLock <- liftIO newEmptyMVar -- Only allow one signing request to be served at once
  h@(MVarHandler signingRequestMVar signingResponseMVar) <- liftIO newMVarHandler
  qs@(MVarHandler quickSignRequestMVar quickSignResponseMVar) <- liftIO newMVarHandler
  -- keysHandler <- liftIO newMVarHandler
  -- accountsHandler <- liftIO newMVarHandler
  let
    runSign obj = mkServantHandler <=< liftIO $ bracket_ (putMVar signingLock ()) (takeMVar signingLock) $ do
        putMVar signingRequestMVar (obj, Nothing) -- handoff to app
        bracket moveToForeground (const $ moveToBackground) (\_ -> takeMVar signingResponseMVar)
    runQuickSign obj = mkServantHandler <=< liftIO $ bracket_ (putMVar signingLock ()) (takeMVar signingLock) $ do
        putMVar quickSignRequestMVar (obj, Nothing) -- handoff to app
        bracket moveToForeground (const $ moveToBackground) (\_ -> takeMVar quickSignResponseMVar)
    -- runMVarHandler (MVarHandler req resp) = do
    --   mkServantHandler <=< liftIO $ do
    --     putMVar req ()
    --     takeMVar resp
    mkServantHandler = \case
      Left e -> throwError $ Servant.err409 { Servant.errBody = LBS.fromStrict $ T.encodeUtf8 e }
      Right v -> pure v

    s = Warp.setPort 9467 Warp.defaultSettings
    laxCors _ = Just $ Wai.simpleCorsResourcePolicy
      { Wai.corsRequestHeaders = Wai.simpleHeaders }
    apiServer
      = Warp.runSettings s $ Wai.cors laxCors
      $ Servant.serve walletApi $ (runSign :<|> runQuickSign)  --   :<|> runMVarHandler keysHandler :<|> runMVarHandler accountsHandler

  liftIO $ void $ Async.async $ apiServer
  -- pure (h, keysHandler, accountsHandler)
  pure (h, qs)

type WalletAPI = "v1" :> V1WalletAPI
type V1WalletAPI = V1SigningApi
                   -- :<|> "keys" :> Get '[JSON] [PublicKey]
                   -- :<|> "accounts" :> Get '[JSON] (Map NetworkName [AccountName])

walletApi :: Proxy WalletAPI
walletApi = Proxy
