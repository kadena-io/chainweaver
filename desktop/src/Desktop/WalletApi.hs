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

p = liftIO . putStrLn . ("[MVAR] " <>)

walletServer
  :: MonadIO m
  => IO ()
  -> IO ()
  -> m ( MVarHandler SigningRequest SigningResponse
       , MVarHandler QuickSignRequest QuickSignResponse
       )
  -- Commented much of the old functionality out (vs. removing it) since we intend to revisit it in the near future
  -- -> m ( MVarHandler SigningRequest SigningResponse
  --      , MVarHandler () [PublicKey]
  --      , MVarHandler () (Map NetworkName [AccountName])
  --      )
walletServer moveToForeground moveToBackground = do
  signingLock <- liftIO newEmptyMVar -- Only allow one signing request to be served at once

  p "newMVarHandler for signing req"
  h@(MVarHandler signingRequestMVar signingResponseMVar) <- liftIO newMVarHandler

  p "newMVarHandler for quicksigning req"
  qs@(MVarHandler quickSignRequestMVar quickSignResponseMVar) <- liftIO newMVarHandler
  -- keysHandler <- liftIO newMVarHandler
  -- accountsHandler <- liftIO newMVarHandler
  let
    runSign obj = mkServantHandler <=< liftIO $ bracket_ (p "signing bracket putMVar signingLock" >> putMVar signingLock ()) (p "signing bracket takeMVar signingLock" >> takeMVar signingLock) $ do
        p "runSign putMVar"
        putMVar signingRequestMVar obj -- handoff to app
        p "runSign bracket takeMVar signingResponseMVar"
        bracket moveToForeground (const $ moveToBackground) (\_ -> p "runSign bracket finalizer takeMVar signingResponseMVar" >> takeMVar signingResponseMVar)
    runQuickSign obj = mkServantHandler <=< liftIO $ bracket_ (p "quickSign bracket putMVar signingLock" >> putMVar signingLock ()) (p "quickSign bracket takeVMar signingLock" >> takeMVar signingLock) $ do
        p "runQuickSign putMVar"
        putMVar quickSignRequestMVar obj -- handoff to app
        p "runQuickSign bracket takeMVar quickSignResponseMVar"
        bracket moveToForeground (const $ moveToBackground) (\_ -> takeMVar quickSignResponseMVar)
    -- runMVarHandler (MVarHandler req resp) = do
    --   mkServantHandler <=< liftIO $ do
    --     putMVar req ()
    --     takeMVar resp
    mkServantHandler = \case
      Left e -> do
        p ("mkServantHandler Left: " <> show e)
        throwError $ Servant.err409 { Servant.errBody = LBS.fromStrict $ T.encodeUtf8 e }
      Right v -> do
        p "mkServantHandler Right"
        pure v

    s = Warp.setPort 9467 Warp.defaultSettings
    laxCors _ = Just $ Wai.simpleCorsResourcePolicy
      { Wai.corsRequestHeaders = Wai.simpleHeaders }
    apiServer
      = Warp.runSettings s $ Wai.cors laxCors
      $ Servant.serve walletApi $ (runSign :<|> runQuickSign)  --   :<|> runMVarHandler keysHandler :<|> runMVarHandler accountsHandler

  p "async apiServer"
  liftIO $ void $ Async.async $ apiServer
  -- pure (h, keysHandler, accountsHandler)
  p "return"
  pure (h, qs)

type WalletAPI = "v1" :> V1WalletAPI
type V1WalletAPI = V1SigningApi
                   -- :<|> "keys" :> Get '[JSON] [PublicKey]
                   -- :<|> "accounts" :> Get '[JSON] (Map NetworkName [AccountName])

walletApi :: Proxy WalletAPI
walletApi = Proxy
