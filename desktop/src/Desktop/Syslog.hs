{-# LANGUAGE OverloadedStrings #-}
module Desktop.Syslog where

import Common.Logger (LogLevel (..), LogStr)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign.C.String (withCStringLen)
import System.Posix.Syslog (Priority (..), Option (..), Facility (User), withSyslog, syslog)

sysloggedMain :: String -> IO a -> IO a
sysloggedMain s = withSyslog s [LogPID, Console] User

logToSyslog :: LogLevel -> LogStr -> IO ()
logToSyslog lvl msg =
  withCStringLen (T.unpack . TE.decodeUtf8 $ fromLogStr msg) $ \cstr ->
    syslog Nothing priorityFromLogLevel cstr
  where
    priorityFromLogLevel = case lvl of
      LevelDebug -> Debug
      LevelInfo  -> Info
      LevelWarn  -> Warning
      LevelError -> Error
      (LevelOther level) -> case level of
        "Emergency" -> Emergency
        "Alert"     -> Alert
        "Critical"  -> Critical
        "Notice"    -> Notice
        _           -> error $ "unknown log level: " ++ T.unpack level

