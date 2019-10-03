{-# LANGUAGE ForeignFunctionInterface #-}

import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import Data.Default (Default(..))
import Data.Functor (void)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt(..))
import Foreign.StablePtr (StablePtr, newStablePtr)
import Language.Javascript.JSaddle.Types (JSM)

-- import Language.Javascript.JSaddle.WKWebView (AppDelegateConfig(..), mainBundleResourcePath, runHTMLWithBaseURL)
import Language.Javascript.JSaddle.WebKitGTK (run)

import Desktop.Linux (main', LinuxFFI(..))

-- foreign import ccall setupAppMenu :: StablePtr (CString -> IO ()) -> IO ()
-- foreign import ccall activateWindow :: IO ()
-- foreign import ccall hideWindow :: IO ()
-- foreign import ccall moveToForeground :: IO ()
-- foreign import ccall moveToBackground :: IO ()
-- foreign import ccall resizeWindow :: IO ()
-- foreign import ccall global_openFileDialog :: IO ()
-- foreign import ccall global_requestUserAttention :: IO CInt
-- foreign import ccall global_cancelUserAttentionRequest :: CInt -> IO ()
-- foreign import ccall global_getHomeDirectory :: IO CString

ffi :: LinuxFFI
ffi = LinuxFFI
  undefined --  { _linuxFFI_setupAppMenu = setupAppMenu
  undefined --  , _linuxFFI_activateWindow = activateWindow
  undefined --  , _linuxFFI_hideWindow = hideWindow
  undefined --  , _linuxFFI_moveToBackground = moveToBackground
  undefined --  , _linuxFFI_moveToForeground = moveToForeground
  undefined --  , _linuxFFI_resizeWindow = resizeWindow
  undefined --  , _linuxFFI_global_openFileDialog = global_openFileDialog
  undefined --  , _linuxFFI_global_requestUserAttention = global_requestUserAttention
  undefined --  , _linuxFFI_global_cancelUserAttentionRequest = global_cancelUserAttentionRequest
  undefined --  , _linuxFFI_global_getHomeDirectory = global_getHomeDirectory
            --  }

main :: IO ()
main = main' ffi runLinux

runLinux
  :: ByteString
  -> ByteString
  -> (String -> IO ())
  -> (FilePath -> IO Bool)
  -> JSM ()
  -> IO ()
runLinux _url _allowing _onUniversalLink _handleOpen jsm = do
  putStrLn "fantastic"
  run jsm
  -- runHTMLWithBaseURL url allowing $ AppDelegateConfig
  -- { _appDelegateConfig_willFinishLaunchingWithOptions = do
  --   putStrLn "will finish launching"
  --   -- (_linuxFFI_setupAppMenu ffi) <=< newStablePtr $ void . handleOpen <=< peekCString
  -- , _appDelegateConfig_didFinishLaunchingWithOptions = do
  --   putStrLn "did finish launching"
  --   -- (_linuxFFI_hideWindow ffi)
  -- , _appDelegateConfig_applicationDidBecomeActive = putStrLn "did become active"
  -- , _appDelegateConfig_applicationWillResignActive = putStrLn "will resign active"
  -- , _appDelegateConfig_applicationDidEnterBackground = putStrLn "did enter background"
  -- , _appDelegateConfig_applicationWillEnterForeground = putStrLn "will enter foreground"
  -- , _appDelegateConfig_applicationWillTerminate = putStrLn "will terminate"
  -- , _appDelegateConfig_applicationSignificantTimeChange = putStrLn "time change"
  -- , _appDelegateConfig_applicationUniversalLink = \cs -> do
  --     s <- peekCString cs
  --     putStrLn $ "universal link: " <> s
  --     onUniversalLink s
  -- , _appDelegateConfig_appDelegateNotificationConfig = def
  -- , _appDelegateConfig_developerExtrasEnabled = True
  -- , _appDelegateConfig_applicationOpenFile = \cs -> do
  --     filePath <- peekCString cs
  --     putStrLn $ "application open file: " <> filePath
  --     handleOpen filePath
  -- }
