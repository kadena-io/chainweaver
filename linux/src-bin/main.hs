import Control.Lens (_head, preview)
import Control.Monad ((<=<), (>=>))
import Data.ByteString (ByteString)
import Data.Default (Default(..))
import Data.Functor (void)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt(..))
import Foreign.StablePtr (StablePtr, newStablePtr)
import Language.Javascript.JSaddle.Types (JSM)

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE

import qualified System.Posix.User as PU

import qualified GI.Gtk as Gtk

import Desktop.Linux (main', LinuxFFI(..))

import CustomRun (customRun)

ffi :: LinuxFFI
ffi = LinuxFFI
  undefined --  { _linuxFFI_setupAppMenu = setupAppMenu
  undefined --  , _linuxFFI_activateWindow = activateWindow
  undefined --  , _linuxFFI_hideWindow = hideWindow
  undefined --  , _linuxFFI_moveToBackground = moveToBackground
  undefined --  , _linuxFFI_moveToForeground = moveToForeground
  undefined --  , _linuxFFI_resizeWindow = resizeWindow
  openFileDialog
  undefined --  , _linuxFFI_global_requestUserAttention = global_requestUserAttention
  undefined --  , _linuxFFI_global_cancelUserAttentionRequest = global_cancelUserAttentionRequest
  getHomeDirectory

openFileDialog :: IO (Maybe String)
openFileDialog = do
  filter <- Gtk.fileFilterNew
  Gtk.fileFilterAddPattern filter (T.pack "*.pact")

  chooser <- Gtk.fileChooserNativeNew
    (Just $ T.pack "Open Pact File")
    Gtk.noWindow
    Gtk.FileChooserActionOpen
    Nothing
    Nothing

  Gtk.fileChooserAddFilter chooser filter
  Gtk.fileChooserSetSelectMultiple chooser False

  res <- Gtk.nativeDialogRun chooser
  case toEnum (fromIntegral res) of
    Gtk.ResponseTypeAccept -> preview _head <$> Gtk.fileChooserGetFilenames chooser
    _ -> pure Nothing
  where
    floop :: (Gtk.IsGValue x, Gtk.IsGValue y) => x -> IO y
    floop = Gtk.toGValue >=> Gtk.fromGValue

getHomeDirectory :: IO String
getHomeDirectory = PU.getLoginName >>= fmap PU.homeDirectory . PU.getUserEntryForName

main :: IO ()
main = main' ffi runLinux

runLinux
  :: ByteString
  -> ByteString
  -> (String -> IO ())
  -> (FilePath -> IO Bool)
  -> JSM ()
  -> IO ()
runLinux _url allowing _onUniversalLink _handleOpen jsm =
  customRun (TE.decodeUtf8With TE.lenientDecode allowing) jsm
