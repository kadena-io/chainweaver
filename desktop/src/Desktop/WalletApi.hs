{-# LANGUAGE FlexibleContexts #-}

module Desktop.WalletApi where

import Control.Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Exception (bracket_, bracket)
import Control.Monad.IO.Class
import Control.Monad.Except (throwError)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Kadena.SigningApi (SigningRequest, SigningResponse, signingAPI)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Wai
import qualified Servant.Server as Servant

signingServer
  :: MonadIO m
  => IO ()
  -> IO ()
  -> m (MVar SigningRequest, MVar (Either Text SigningResponse))
signingServer moveToForeground moveToBackground = do
  signingLock <- liftIO newEmptyMVar -- Only allow one signing request to be served at once
  signingRequestMVar <- liftIO newEmptyMVar
  signingResponseMVar <- liftIO newEmptyMVar
  let runSign obj = do
        resp <- liftIO $ bracket_ (putMVar signingLock ()) (takeMVar signingLock) $ do
          putMVar signingRequestMVar obj -- handoff to app
          bracket moveToForeground (const $ moveToBackground) (\_ -> takeMVar signingResponseMVar)
        case resp of
          Left e -> throwError $ Servant.err409
            { Servant.errBody = LBS.fromStrict $ T.encodeUtf8 e }
          Right v -> pure v
      s = Warp.setPort 9467 Warp.defaultSettings
      laxCors _ = Just $ Wai.simpleCorsResourcePolicy
        { Wai.corsRequestHeaders = Wai.simpleHeaders }
      apiServer
        = Warp.runSettings s $ Wai.cors laxCors
        $ Servant.serve signingAPI runSign
  _ <- liftIO $ Async.async $ apiServer
  pure (signingRequestMVar, signingResponseMVar)

signingResponseHandler :: MonadIO m => MVar (Either Text SigningResponse) -> Either Text SigningResponse -> m ()
signingResponseHandler signingResponseMVar = liftIO . putMVar signingResponseMVar
