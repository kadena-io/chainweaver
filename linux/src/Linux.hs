{-# LANGUAGE OverloadedStrings #-}
module Linux where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, forkIO)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.Functor (void)
import Data.List (lookup)
import Data.Foldable (traverse_)
import Data.GI.Gtk.Threading (postGUIASync)
import Data.Maybe (fromMaybe)
import Language.Javascript.JSaddle.Types (JSM)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified GI.Gtk as Gtk
import Safe (headMay)
import System.Environment (getEnvironment, getExecutablePath)
import System.FilePath ((</>), takeDirectory)
import qualified System.Posix.User as PU

import Backend.App (main', AppFFI(..))
import Frontend.AppCfg (FileType(..), fileTypeExtension)
import Desktop.Syslog (logToSyslog, sysloggedMain)
import WebKitGTK

{-- NOTES

To run: `WEBKIT_DISABLE_COMPOSITING_MODE=1 $(nix-build -A linux)/bin/kadena-chainweaver-rc1` from the project root

BUGS
- Compositing issues with nvidia drivers: See https://bugs.freebsd.org/bugzilla/show_bug.cgi?id=229491
- Getting these warnings in the console. Looks bad:
  - http://localhost:48481/:247:76: CONSOLE WARN ArrayBuffer is deprecated in XMLHttpRequest.send(). Use ArrayBufferView instead.
- I get some errors when launching links with brave. They work, but the error is nasty.
- Needs to log to syslog or something.

NOTES
- Must make sure that no GTK commands escape the UI thread. You'll get errors like these if you mess this up:
  - `Most likely this is a multi-threaded client and XInitThreads has not been called`
  - Get commands into the gui thread withhttps://wiki.haskell.org/Gtk2Hs/Tutorials/ThreadedGUIs
- On NixOS, it's hard for the app to find gnome settings stuff (icons, schema, etc).
  - We use wrapGAppsHook atm, but this wont be great for a non nixos distro because it sets up paths to the nix store! :)
  - See https://nixos.wiki/wiki/Packaging/Quirks_and_Caveats#GLib-GIO-Message:_Using_the_.27memory.27_GSettings_backend._Your_settings_will_not_be_saved_or_shared_with
- Upgrading gtk stack is really hard because reflex-platform uses it, so you are kinda stuck with what we have
unless we bump r-p
- This is a really good article explaining the difference between GTK and GDK: https://stackoverflow.com/questions/27515062/difference-between-gtkwindow-and-gdkwindow
--}

data LinuxMVars = LinuxMVars
  { _linuxMVars_openFileDialog :: MVar FileType
  , _linuxMVars_saveFileDialog :: MVar (FilePath, FilePath -> IO())
  , _linuxMVars_moveToForeground :: MVar ()
  , _linuxMVars_moveToBackground :: MVar ()
  , _linuxMVars_resizeWindow :: MVar (Int,Int)
  , _linuxMVars_activateWindow :: MVar ()
  }

-- We must ensure that we never actually do GTK things in these functions. Otherwise they'll likely be on
-- a thread other than the GUI thread and break!
mkFfi :: IO (AppFFI, LinuxMVars)
mkFfi = do
  activateMV <- newEmptyMVar
  openFileDialogMV <- newEmptyMVar
  moveToForegroundMV <- newEmptyMVar
  moveToBackgroundMV <- newEmptyMVar
  resizeMV <- newEmptyMVar
  saveFileDialogMV <- newEmptyMVar

  let ffi = AppFFI
        { _appFFI_activateWindow = putMVar activateMV ()
        , _appFFI_moveToBackground = putMVar moveToBackgroundMV ()
        , _appFFI_moveToForeground = putMVar moveToForegroundMV ()
        , _appFFI_resizeWindow = putMVar resizeMV
        , _appFFI_global_openFileDialog = putMVar openFileDialogMV
        , _appFFI_global_saveFileDialog = curry (putMVar saveFileDialogMV)
        , _appFFI_global_getStorageDirectory = (\hd -> hd </> ".local" </> "share" </> "chainweaver") <$> getHomeDirectory
        , _appFFI_global_logFunction = logToSyslog
        }
  pure (ffi, LinuxMVars
    { _linuxMVars_openFileDialog = openFileDialogMV
    , _linuxMVars_moveToForeground = moveToForegroundMV
    , _linuxMVars_moveToBackground = moveToBackgroundMV
    , _linuxMVars_resizeWindow = resizeMV
    , _linuxMVars_activateWindow = activateMV
    , _linuxMVars_saveFileDialog = saveFileDialogMV
    })

main :: IO ()
main = sysloggedMain "kadena-chainweaver" $ do
  (ffi, mvars) <- mkFfi
  let getStaticPath = do
        mPath <- lookup "CHAINWEAVER_STATIC_PATH" <$> getEnvironment
        exeDir <- takeDirectory <$> getExecutablePath
        pure . Just . TE.encodeUtf8 . T.pack $ fromMaybe exeDir mPath
  main' ffi getStaticPath (runLinux mvars)

runLinux
  :: LinuxMVars
  -> ByteString
  -> ByteString
  -> (String -> IO ())
  -> (FilePath -> IO Bool)
  -> JSM ()
  -> IO ()
runLinux mvars _url allowing _onUniversalLink handleOpen jsm = do
  customRun (TE.decodeUtf8With TE.lenientDecode allowing) jsm $ \window -> do
    mvarHandler (_linuxMVars_openFileDialog mvars) (openFileDialog handleOpen)
    mvarHandler (_linuxMVars_saveFileDialog mvars) (uncurry saveFileDialog)
    mvarHandler (_linuxMVars_moveToForeground mvars) (const $ moveToForeground window)
    mvarHandler (_linuxMVars_moveToBackground mvars) (const $ moveToBackground window)
    mvarHandler (_linuxMVars_resizeWindow mvars) (resizeWindow window)
    mvarHandler (_linuxMVars_activateWindow mvars) (const $ activateWindow window)

  where
    mvarHandler mvar f = void $ forkIO $ forever $ do
      v <- takeMVar mvar
      postGUIASync $ f v

openFileDialog :: (FilePath -> IO Bool) -> FileType -> IO ()
openFileDialog handleOpen fileType = do
  fileFilter <- Gtk.fileFilterNew
  Gtk.fileFilterAddPattern fileFilter ("*." <> fileTypeExtension fileType)
  Gtk.fileFilterSetName fileFilter (Just $ fileTypeExtension fileType <> " files")

  chooser <- Gtk.fileChooserNativeNew
    (Just $ T.pack "Open File")
    Gtk.noWindow
    Gtk.FileChooserActionOpen
    Nothing
    Nothing

  Gtk.fileChooserAddFilter chooser fileFilter
  Gtk.fileChooserSetSelectMultiple chooser False

  res <- Gtk.nativeDialogRun chooser
  void $ case toEnum (fromIntegral res) of
    Gtk.ResponseTypeAccept ->  Gtk.fileChooserGetFilenames chooser >>= traverse_ handleOpen . headMay
    _ -> pure ()

saveFileDialog :: FilePath -> (FilePath -> IO ()) -> IO ()
saveFileDialog fileName handleSelectDir = do
  chooser <- Gtk.fileChooserNativeNew
    (Just $ T.pack "Choose a directory to save to")
    Gtk.noWindow
    Gtk.FileChooserActionSave
    Nothing
    Nothing

  Gtk.fileChooserSetCreateFolders chooser True
  _ <- Gtk.fileChooserSetCurrentName chooser $ T.pack fileName

  res <- Gtk.nativeDialogRun chooser
  void $ case toEnum (fromIntegral res) of
    Gtk.ResponseTypeAccept ->  Gtk.fileChooserGetFilenames chooser >>= traverse_ handleSelectDir . headMay
    _ -> pure ()

moveToForeground :: Gtk.Window -> IO ()
moveToForeground w = do
  Gtk.windowPresent w

moveToBackground :: Gtk.Window -> IO ()
moveToBackground _w = do
  --Gtk.windowIconify w
  -- This is pretty ugly, so lets stop doing it.
  pure ()

activateWindow :: Gtk.Window -> IO ()
activateWindow w = do
  Gtk.windowMaximize w
  pure ()

resizeWindow :: Gtk.Window -> (Int,Int) -> IO ()
resizeWindow _ _ = do
  pure ()

getHomeDirectory :: IO String
getHomeDirectory = PU.getLoginName >>= fmap PU.homeDirectory . PU.getUserEntryForName
