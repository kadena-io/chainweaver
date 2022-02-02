{-# LANGUAGE TemplateHaskell #-}
module Frontend.Log
  ( LogCfg (..)
  , Logger (..)
  , HasLogCfg (..)
  , HasLogger (..)
  , LogLevel (..)
  , formatLogMessage
  , defaultLogger
  , errorLevelLogger
  , defaultLogger
  , logOn
  , putLog
  , fmtLogTH
  ) where

import Control.Lens (view,(.~))
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (stdout)
import Data.Function ((&))
import Data.ByteString (hPut)
import Data.Text (Text)
import Language.Haskell.TH.Syntax (Q, Exp, qLocation)

import Reflex
import Reflex.Network.Extended (Flattenable (..))

import Common.Logger
import Common.Foundation (makePactLenses)

newtype LogCfg t = LogCfg
  { _logCfg_logMessage :: Event t (LogLevel, Text)
  }

makePactLenses ''LogCfg

data Logger t = Logger
  { _log_formatMessage :: LogLevel -> Text -> LogStr
  , _log_putLog :: LogLevel -> Text -> IO ()
  }

makePactLenses ''Logger

instance Flattenable (LogCfg t) t where
  flattenWith doSwitch ev = LogCfg <$> doSwitch never (_logCfg_logMessage <$> ev)

instance Reflex t => Semigroup (LogCfg t) where
    (<>) (LogCfg a) (LogCfg b) = LogCfg $ leftmost [a,b]

instance Reflex t => Monoid (LogCfg t) where
  mempty = LogCfg never

formatLogMessage :: LogLevel -> Text -> LogStr
formatLogMessage lvl = defaultLogStr defaultLoc "Chainweaver" lvl . toLogStr

errorLevelLogger :: MonadIO m => LogLevel -> LogStr -> m ()
errorLevelLogger LevelError = liftIO . hPut stdout . fromLogStr
errorLevelLogger _ = const $ liftIO $ pure ()

defaultLogger :: MonadIO m => LogLevel -> LogStr -> m ()
defaultLogger _ = liftIO . hPut stdout . fromLogStr

putLog :: (HasLogger model t, MonadIO m) => model -> LogLevel -> Text -> m ()
putLog model lvl msg = liftIO $ view (logger . log_putLog) model lvl msg

logOn :: (HasLogCfg mConf t, Monoid mConf, Reflex t) => LogLevel -> Text -> Event t a -> mConf
logOn lvl msg go = mempty & logCfg_logMessage .~ ((lvl,msg) <$ go)

fmtLogTH :: LogLevel -> Q Exp
fmtLogTH lvl = [| defaultLogStr $(qLocation >>= liftLoc) "Chainweaver" lvl . (toLogStr :: Text -> LogStr) |]

