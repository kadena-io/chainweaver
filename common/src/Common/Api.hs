{-# LANGUAGE OverloadedStrings #-}

module Common.Api where

import Data.Text (Text)
import qualified Data.Text as T
import Obelisk.ExecutableConfig.Common


-- | Where to find the network list configuration, under config/common
verificationServerPath :: Text
verificationServerPath = "verification-server"

-- | Get the normalized url of the remote verification server suitable for Pact.
getVerificationServerUrl :: HasCommonConfigs m => m (Maybe Text)
getVerificationServerUrl = do
  mUri <- getCommonTextCfg verificationServerPath
  pure $ T.dropWhileEnd (== '/') <$> mUri

-- | Get "config/common/route" normalized.
getConfigRoute :: HasCommonConfigs m => m Text
getConfigRoute =
  T.dropWhileEnd (== '/') <$> getMandatoryTextCfg getCommonTextCfg "route"

getCommonTextCfg :: HasCommonConfigs m => Text -> m (Maybe Text)
getCommonTextCfg p = fmap T.strip <$> getCommonConfig p

getMandatoryTextCfg
  :: Monad m
  => (Text -> m (Maybe Text))
  -- ^ Function to get the config
  -> Text -> m Text
getMandatoryTextCfg f p = do
  mR <- f p
  case mR of
    Nothing -> fail $ "Obelisk.ExecutableConfig, could not find: '" <> T.unpack p <> "'!"
    Just r -> pure $ T.strip r
