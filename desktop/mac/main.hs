{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..))
import Foreign.StablePtr (StablePtr)
import Language.Javascript.JSaddle.WKWebView (AppDelegateConfig(..))

import Desktop (main', MacFFI(..))

foreign import ccall setupAppMenu :: StablePtr (CString -> IO ()) -> IO ()
foreign import ccall activateWindow :: IO ()
foreign import ccall hideWindow :: IO ()
foreign import ccall resizeWindow :: IO ()
foreign import ccall global_openFileDialog :: IO ()
foreign import ccall global_requestUserAttention :: IO CInt
foreign import ccall global_cancelUserAttentionRequest :: CInt -> IO ()
foreign import ccall global_getHomeDirectory :: IO CString

ffi :: MacFFI
ffi = MacFFI
  { _macFFI_setupAppMenu = setupAppMenu
  , _macFFI_activateWindow = activateWindow
  , _macFFI_hideWindow = hideWindow
  , _macFFI_resizeWindow = resizeWindow
  , _macFFI_global_openFileDialog = global_openFileDialog
  , _macFFI_global_requestUserAttention = global_requestUserAttention
  , _macFFI_global_cancelUserAttentionRequest = global_cancelUserAttentionRequest
  , _macFFI_global_getHomeDirectory = global_getHomeDirectory
  }

main :: IO ()
main = main' ffi runMac

runMac
  :: String
  -> String
  -> (String -> IO ())
  -> (FilePath -> IO Bool)
  -> JSM ()
  -> IO ()
runMac url allowing onUniversalLink handleOpen = runHTMLWithBaseURL url allowing $ AppDelegateConfig
  { _appDelegateConfig_willFinishLaunchingWithOptions = do
    putStrLn "will finish launching"
    (_macFFI_setupAppMenu ffi) <=< newStablePtr $ void . handleOpen <=< peekCString
  , _appDelegateConfig_didFinishLaunchingWithOptions = do
    putStrLn "did finish launching"
    (_macFFI_hideWindow ffi)
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
