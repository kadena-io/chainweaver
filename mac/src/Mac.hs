{-# LANGUAGE ForeignFunctionInterface #-}

module Mac where

import qualified Control.Concurrent.Async as Async
import Control.Monad ((<=<), forever)
import Control.Monad.Logger (LogLevel(LevelInfo), toLogStr)
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.Default (Default(..))
import Data.Functor (void)
import Data.Foldable (for_)
import GHC.IO.Handle (hDuplicateTo, BufferMode(LineBuffering))
import Foreign.C.String (CString, peekCString)
import Foreign.StablePtr (StablePtr, newStablePtr)
import Language.Javascript.JSaddle.Types (JSM)
import Language.Javascript.JSaddle.WKWebView (AppDelegateConfig(..), mainBundleResourcePath, runHTMLWithBaseURL)
import System.FilePath ((</>))
import System.IO
import qualified System.Process as Process

import Backend.App (main', AppFFI(..))
import Desktop.Syslog (sysloggedMain, logToSyslog)

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
    go r = forever $ hGetLine r >>= logToSyslog LevelInfo . toLogStr

main :: IO ()
main = sysloggedMain "Kadena Chainweaver" . redirectPipes [stderr, stdout] $
  main' ffi mainBundleResourcePath runMac

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
