{-# LANGUAGE OverloadedStrings #-}

module Common.Api where

import Data.Text (Text)
import qualified Data.Text as T
import Obelisk.ExecutableConfig (get)
import Control.Monad.IO.Class (MonadIO, liftIO)



-- | Where to find the network list configuration.
verificationServerPath :: Text
verificationServerPath = "config/common/verification-server"

-- | Get the normalized url of the remote verification server suitable for Pact.
getVerificationServerUrl :: MonadIO m => m (Maybe Text)
getVerificationServerUrl = do
  mUri <- getTextCfg verificationServerPath
  pure $ T.dropWhileEnd (== '/') <$> mUri

-- | Get "config/common/route" normalized.
getConfigRoute :: MonadIO m => m Text
getConfigRoute =
  T.dropWhileEnd (== '/') <$> getMandatoryTextCfg "config/common/route"

getTextCfg :: MonadIO m => Text -> m (Maybe Text)
getTextCfg p = liftIO $ fmap T.strip <$> Obelisk.ExecutableConfig.get p

getMandatoryTextCfg :: MonadIO m => Text -> m Text
getMandatoryTextCfg p = liftIO $ do
  mR <- Obelisk.ExecutableConfig.get p
  case mR of
    Nothing -> fail $ "Obelisk.ExecutableConfig, could not find: '" <> T.unpack p <> "'!"
    Just r -> pure $ T.strip r
