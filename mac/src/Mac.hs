{-# LANGUAGE ForeignFunctionInterface #-}

module Mac where

import qualified Control.Concurrent.Async as Async
import Control.Monad ((<=<))
import Control.Monad.Logger (LogLevel (..), LogStr)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Default (Default(..))
import Data.Functor (void)
import Foreign.C.String (CString, peekCString)
import Foreign.StablePtr (StablePtr, newStablePtr)
import Language.Javascript.JSaddle.Types (JSM)
import Language.Javascript.JSaddle.WKWebView (AppDelegateConfig(..), mainBundleResourcePath, runHTMLWithBaseURL)
import System.Log.FastLogger (fromLogStr)
import System.FilePath ((</>))
import System.IO (Handle)
import System.Posix.Syslog (Priority (..), Option (..), Facility (User), withSyslog, syslog)
import qualified System.Process as Process
import Foreign.C.String (withCStringLen)

import Desktop (main', MacFFI(..))

foreign import ccall setupAppMenu :: StablePtr (CString -> IO ()) -> IO ()
foreign import ccall activateWindow :: IO ()
foreign import ccall hideWindow :: IO ()
foreign import ccall moveToForeground :: IO ()
foreign import ccall moveToBackground :: IO ()
foreign import ccall resizeWindow :: Int -> Int -> IO ()
foreign import ccall global_openFileDialog :: IO ()
foreign import ccall global_getHomeDirectory :: IO CString

ffi :: AppFFI
ffi = AppFFI
  { _appFFI_activateWindow = activateWindow
  , _appFFI_moveToBackground = moveToBackground
  , _appFFI_moveToForeground = moveToForeground
  , _appFFI_resizeWindow = uncurry resizeWindow
  , _appFFI_global_openFileDialog = global_openFileDialog
  , _appFFI_global_getStorageDirectory = getStorageDirectory
  , _appFFI_global_logFunction = logToSyslog
  }

logToSyslog :: LogLevel -> LogStr -> IO ()
logToSyslog lvl msg =
  withCStringLen (BS8.unpack $ fromLogStr msg) $ \cstr ->
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
        _           -> error $ "unknown log level: " <> T.unpack level

getStorageDirectory :: IO String
getStorageDirectory = do
  home <- global_getHomeDirectory >>= peekCString
  -- TODO use the bundle identifier directly, don't duplicate it
  pure $ home </> "Library" </> "Application Support" </> "io.kadena.chainweaver"

-- | Redirect the given handles to Console.app
redirectPipes :: [Handle] -> IO a -> IO a
redirectPipes ps m = bracket setup hClose $ \r -> Async.withAsync (go r) $ \_ -> m
  where
    setup = do
      (r, w) <- Process.createPipe
      for_ ps $ \p -> hDuplicateTo w p <> hSetBuffering p LineBuffering
      hClose w
      pure r
      -- TODO figure out how to get the logs to come from the Pact process instead of syslog
    go r = forever $ hGetLine r >>= \l -> do
      Process.callProcess "syslog" ["-s", "-k", "Level", "Notice", "Message", "Pact: " <> l]

main :: IO ()
main = withSyslog "Chainweaver" [LogPID, Console] $
  main' ffi mainBundleResourcePath redirectPipes runMac

runMac
  :: ByteString
  -> ByteString
  -> (String -> IO ())
  -> (FilePath -> IO Bool)
  -> JSM ()
  -> IO ()
runMac url allowing onUniversalLink handleOpen = runHTMLWithBaseURL url allowing $ AppDelegateConfig
  { _appDelegateConfig_willFinishLaunchingWithOptions = do
    putStrLn "will finish launching"
    setupAppMenu <=< newStablePtr $ void . handleOpen <=< peekCString
  , _appDelegateConfig_didFinishLaunchingWithOptions = do
    putStrLn "did finish launching"
    hideWindow
  , _appDelegateConfig_applicationDidBecomeActive = putStrLn "did become active"
  , _appDelegateConfig_applicationWillResignActive = putStrLn "will resign active"
  , _appDelegateConfig_applicationDidEnterBackground = putStrLn "did enter background"
  , _appDelegateConfig_applicationWillEnterForeground = putStrLn "will enter foreground"
  , _appDelegateConfig_applicationWillTerminate = putStrLn "will terminate"
  , _appDelegateConfig_applicationSignificantTimeChange = putStrLn "time change"
  , _appDelegateConfig_applicationUniversalLink = \cs -> do
      s <- peekCString cs
      putStrLn $ "universal link: " <> s
      onUniversalLink s
  , _appDelegateConfig_appDelegateNotificationConfig = def
  , _appDelegateConfig_developerExtrasEnabled = True
  , _appDelegateConfig_applicationOpenFile = \cs -> do
      filePath <- peekCString cs
      putStrLn $ "application open file: " <> filePath
      handleOpen filePath
  }
