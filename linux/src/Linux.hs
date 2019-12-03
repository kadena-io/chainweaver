module Linux where

import Control.Lens (_head, preview)
import Data.ByteString (ByteString)
import Data.Functor (void)
import Language.Javascript.JSaddle.Types (JSM)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified GI.Gtk as Gtk
import qualified System.Posix.User as PU

import Debug.Todo

import Desktop (main', AppFFI(..))
import WebKitGTK

ffi :: AppFFI
ffi = AppFFI
  { _appFFI_setupAppMenu = todo "setupAppMenu"
  , _appFFI_activateWindow = todo "activateWindow"
  , _appFFI_hideWindow = todo "hideWindow"
  , _appFFI_moveToBackground = todo "moveToBackground"
  , _appFFI_moveToForeground = todo "moveToForeground"
  , _appFFI_resizeWindow = todo "resizeWindow"
  , _appFFI_global_openFileDialog = openFileDialog
  , _appFFI_global_getHomeDirectory = getHomeDirectory
  }

-- TODO: Redirect stderr/out to a system logger
main :: IO ()
main = main' ffi (pure Nothing) runLinux

runLinux
  :: ByteString
  -> ByteString
  -> (String -> IO ())
  -> (FilePath -> IO Bool)
  -> JSM ()
  -> IO ()
runLinux _url allowing _onUniversalLink _handleOpen = customRun (TE.decodeUtf8With TE.lenientDecode allowing)

openFileDialog :: IO ()
openFileDialog = do
  fileFilter <- Gtk.fileFilterNew
  Gtk.fileFilterAddPattern fileFilter (T.pack "*.pact")

  chooser <- Gtk.fileChooserNativeNew
    (Just $ T.pack "Open Pact File")
    Gtk.noWindow
    Gtk.FileChooserActionOpen
    Nothing
    Nothing

  Gtk.fileChooserAddFilter chooser fileFilter
  Gtk.fileChooserSetSelectMultiple chooser False

  res <- Gtk.nativeDialogRun chooser
  pure ()
  --void $ case toEnum (fromIntegral res) of
  --  Gtk.ResponseTypeAccept ->  Gtk.fileChooserGetFilenames chooser)
  --  _ -> pure ()

getHomeDirectory :: IO String
getHomeDirectory = PU.getLoginName >>= fmap PU.homeDirectory . PU.getUserEntryForName


{--
  runHTMLWithBaseURL url allowing $ AppDelegateConfig
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
--}
