{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Api where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Obelisk.Configs


-- | Where to find the network list configuration, under config/common
verificationServerPath :: Text
verificationServerPath = "common/verification-server"

-- | Get the normalized url of the remote verification server suitable for Pact.
getVerificationServerUrl :: HasConfigs m => m (Maybe Text)
getVerificationServerUrl = do
  mUri <- getConfig verificationServerPath
  pure $ T.dropWhileEnd (== '/') . T.decodeUtf8With T.lenientDecode <$> mUri

-- | Get "config/common/route" normalized.
getConfigRoute :: (HasConfigs m) => m Text
getConfigRoute =
  T.dropWhileEnd (== '/') <$> getMandatoryTextCfg "common/route"

getTextCfg :: (HasConfigs m) => Text -> m (Maybe Text)
getTextCfg p = fmap (T.strip . T.decodeUtf8With T.lenientDecode) <$> getConfig p

getMandatoryTextCfg :: (HasConfigs m) => Text -> m Text
getMandatoryTextCfg p = getTextCfg p >>= \case
  Nothing -> error $ "Obelisk.ExecutableConfig, could not find: '" <> T.unpack p <> "'!"
  Just r -> pure r
