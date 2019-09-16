{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..))
import Foreign.StablePtr (StablePtr)

import Desktop.Mac (main', MacFFI(..))

foreign import ccall setupAppMenu :: StablePtr (CString -> IO ()) -> IO ()
foreign import ccall activateWindow :: IO ()
foreign import ccall hideWindow :: IO ()
foreign import ccall resizeWindow :: IO ()
foreign import ccall global_openFileDialog :: IO ()
foreign import ccall global_requestUserAttention :: IO CInt
foreign import ccall global_cancelUserAttentionRequest :: CInt -> IO ()
foreign import ccall global_getHomeDirectory :: IO CString

main :: IO ()
main = main' $ MacFFI
  { _macFFI_setupAppMenu = setupAppMenu
  , _macFFI_activateWindow = activateWindow
  , _macFFI_hideWindow = hideWindow
  , _macFFI_resizeWindow = resizeWindow
  , _macFFI_global_openFileDialog = global_openFileDialog
  , _macFFI_global_requestUserAttention = global_requestUserAttention
  , _macFFI_global_cancelUserAttentionRequest = global_cancelUserAttentionRequest
  , _macFFI_global_getHomeDirectory = global_getHomeDirectory
  }
