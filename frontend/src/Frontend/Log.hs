{-# LANGUAGE TemplateHaskell #-}
module Frontend.Log
  ( LogCfg (..)
  , Logger (..)
  , HasLogCfg (..)
  , HasLogger (..)
  , LogLevel (..)
  , formatLogMessage
  , defaultLogger
  , logOn
  , logPromptly
  , logStrPromptly
  , fmtLogTH
  , logDefTH
  , defLogDebug
  ) where

import Control.Lens (view,(.~))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LogLevel (..), LogStr, liftLoc, defaultLogStr, toLogStr, defaultLoc)
import System.Log.FastLogger (fromLogStr)
import System.IO (stdout)
import Data.Function ((&))
import Data.ByteString (hPut)
import Data.Text (Text)

import Language.Haskell.TH.Syntax (Q, Exp, qLocation)

import Reflex
import Reflex.Network.Extended (Flattenable (..))

import Common.Foundation (makePactLenses)

newtype LogCfg t = LogCfg
  { _logCfg_logMessage :: Event t (LogLevel, Text)
  }

makePactLenses ''LogCfg

data Logger t = Logger
  { _log_formatMessage :: LogLevel -> Text -> LogStr
  , _log_logPromptly :: LogLevel -> Text -> IO ()
  , _log_logStr :: LogStr -> IO ()
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

defaultLogger :: MonadIO m => LogStr -> m ()
defaultLogger = liftIO . hPut stdout . fromLogStr

logPromptly :: (HasLogger model t, MonadIO m) => model -> LogLevel -> Text -> m ()
logPromptly model lvl msg = liftIO $ view (logger . log_logPromptly) model lvl msg

logStrPromptly :: (HasLogger model t, MonadIO m) => model -> LogStr -> m ()
logStrPromptly model = liftIO . view (logger . log_logStr) model

logOn :: (HasLogCfg mConf t, Monoid mConf, Reflex t) => LogLevel -> Text -> Event t a -> mConf
logOn lvl msg go = mempty & logCfg_logMessage .~ ((lvl,msg) <$ go)

fmtLogTH :: LogLevel -> Q Exp
fmtLogTH lvl = [| defaultLogStr $(qLocation >>= liftLoc) "Chainweaver" lvl . (toLogStr :: Text -> LogStr) |]

logDefTH :: LogLevel -> Q Exp
logDefTH lvl =
  [| defaultLogger
  . defaultLogStr $(qLocation >>= liftLoc) "Chainweaver" lvl
  . (toLogStr :: Text -> LogStr)
  |]

defLogDebug :: Q Exp
defLogDebug = logDefTH LevelDebug
