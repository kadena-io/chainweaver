{-# LANGUAGE OverloadedStrings #-}
module Linux where

-- import Control.Lens (_head, preview)
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, forkIO)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.Functor (void)
import Data.Foldable (traverse_)
import Data.GI.Gtk.Threading (postGUIASync)
import Language.Javascript.JSaddle.Types (JSM)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified GI.Gtk as Gtk
import Safe (headMay)
import System.FilePath ((</>))
import qualified System.Posix.User as PU


import Desktop (main', AppFFI(..))
import WebKitGTK

{-- NOTES

- Compositing issues with nvidia drivers: See https://bugs.freebsd.org/bugzilla/show_bug.cgi?id=229491
- Must make sure that no GTK commands escape the UI thread. You'll get errors like these if you mess this up:
  - `Most likely this is a multi-threaded client and XInitThreads has not been called`
  - Get commands into the gui thread withhttps://wiki.haskell.org/Gtk2Hs/Tutorials/ThreadedGUIs
- On NixOS, it's hard for the app to find gnome settings stuff (icons, schema, etc).
  - We use wrapGAppsHook atm, but this wont be great for a non nixos distro because it sets up paths to the nix store! :)
  - See https://nixos.wiki/wiki/Packaging/Quirks_and_Caveats#GLib-GIO-Message:_Using_the_.27memory.27_GSettings_backend._Your_settings_will_not_be_saved_or_shared_with
- Getting these warnings in the console. Looks bad:
  - http://localhost:48481/:247:76: CONSOLE WARN ArrayBuffer is deprecated in XMLHttpRequest.send(). Use ArrayBufferView instead.
- UI is really slow and gets stuck (maybe loading ace?)
--}

-- We must ensure that we never actually do GTK things in these functions. Otherwise they'll likely be on
-- a thread other than the GUI thread and break!
mkFfi :: IO (AppFFI, MVar ())
mkFfi = do
  -- We probably need a queue rather than an mvar. Or lots of mvars for each action. Think about this
  mvar <- newEmptyMVar
  let ffi = AppFFI
        { _appFFI_activateWindow = pure ()
        -- https://hackage.haskell.org/package/gi-gtk-4.0.1/docs/GI-Gtk-Objects-Window.html#v:windowIconify
        , _appFFI_moveToBackground = pure ()
        -- https://hackage.haskell.org/package/gi-gtk-4.0.1/docs/GI-Gtk-Objects-Window.html#v:windowPresentWithTime
        , _appFFI_moveToForeground = pure ()
        , _appFFI_resizeWindow = (const $ pure ())
        , _appFFI_global_openFileDialog = putMVar mvar ()
        , _appFFI_global_getStorageDirectory = (\hd -> hd </> ".cache" </> "chainweaver") <$> getHomeDirectory
        }
  pure (ffi, mvar)

-- TODO: Redirect stderr/out to a system logger
main :: IO ()
main = do
  (ffi, mvar) <- mkFfi
  main' ffi (pure $ Just ".") (runLinux mvar)

runLinux
  :: MVar ()
  -> ByteString
  -> ByteString
  -> (String -> IO ())
  -> (FilePath -> IO Bool)
  -> JSM ()
  -> IO ()
runLinux mvar _url allowing _onUniversalLink _handleOpen jsm = do
  void $ forkIO $ forever $ do
    _ <- takeMVar mvar
    postGUIASync $ openFileDialog (const $ pure True)
  customRun (TE.decodeUtf8With TE.lenientDecode allowing) jsm

openFileDialog :: (FilePath -> IO Bool) -> IO ()
openFileDialog handleOpen = do
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
  void $ case toEnum (fromIntegral res) of
    Gtk.ResponseTypeAccept ->  Gtk.fileChooserGetFilenames chooser >>= traverse_ handleOpen . headMay
    _ -> pure ()

getHomeDirectory :: IO String
getHomeDirectory = PU.getLoginName >>= fmap PU.homeDirectory . PU.getUserEntryForName
