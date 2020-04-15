-- | AppCfg is used to configure the app and pass things in and out of reflex
module Frontend.AppCfg where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever)
import Control.Monad.Logger (LogLevel, LogStr)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Language.Javascript.JSaddle (JSM, MonadJSM, liftJSM)

import Kadena.SigningApi
import Reflex (PerformEvent(..), TriggerEvent, Event, newTriggerEvent)

import Common.Wallet (Key)

data ChangePassword key t m = ChangePassword
  { _changePassword_requestChange :: Event t (Text, Text, Text) -> m (Event t (Either Text ()))
  -- ^ Request to change the password. Performs validation and changes passwords
  -- if validation passes. (Old pass, new pass, repeat pass).
  , _changePassword_updateKeys :: Event t (Int -> Key key -> Key key)
  -- ^ Update all keys using the given function
  }

data ExportWalletError
  = ExportWalletError_PasswordIncorrect
  | ExportWalletError_NoKeys
  | ExportWalletError_FileNotWritable Text
  | ExportWalletError_CommandLogExport
  | ExportWalletError_UpgradeFailed

data ExportWallet t m = ExportWallet
  { _exportWallet_requestExport :: Event t Text -> m (Event t (Either ExportWalletError FilePath))
  -- ^ Request to export the wallet to the user as a file. Password must match storage.
  }

data EnabledSettings key t m = EnabledSettings
  { _enabledSettings_changePassword :: Maybe (ChangePassword key t m)
  , _enabledSettings_exportWallet :: Maybe (ExportWallet t m)
  , _enabledSettings_transactionLog :: Bool
  }

-- The types of files targeted by the open file dialog. Used for filtering in the dialog
data FileType = FileType_Pact | FileType_Import

fileTypeExtension :: FileType -> Text
fileTypeExtension FileType_Import = "chainweaver"
fileTypeExtension FileType_Pact = "pact"

data FileFFI t m = FileFFI
  { _fileFFI_openFileDialog :: FileType -> JSM ()
  -- ^ Trigger an "open file" dialog
  , _fileFFI_externalFileOpened :: Event t (FilePath, Text)
  -- ^ File contents from file chosen in "open file" dialog
  , _fileFFI_deliverFile :: Event t (FilePath, Text) -> m (Event t (Either Text FilePath))
  -- ^ Delivers a file to an appropriate place to the user
  }

liftFileFFI :: (forall a. m a -> n a) -> FileFFI t m -> FileFFI t n
liftFileFFI natTransform oldFFI = oldFFI { _fileFFI_deliverFile = natTransform . (_fileFFI_deliverFile oldFFI) }

data AppCfg key t m = AppCfg
  { _appCfg_gistEnabled :: Bool
  , _appCfg_loadEditor :: m (Maybe Text)
  -- ^ Initial code to load into editor
  , _appCfg_editorReadOnly :: Bool
  -- ^ Is the editor read only?
  , _appCfg_signingHandler :: FRPHandler SigningRequest SigningResponse t m
  , _appCfg_enabledSettings :: EnabledSettings key t m
  , _appCfg_logMessage :: LogLevel -> LogStr -> IO ()
  -- ^ Logging Function
  }

data MVarHandler req res = MVarHandler
  { _mvarHandler_readRequest :: MVar req
  , _mvarHandler_writeResponse :: MVar (Either Text res)
  }

data FRPHandler req res t m = FRPHandler
  { _frpHandler_readRequests :: Event t req
  , _frpHandler_writeResponses :: Event t (Either Text res) -> m (Event t ())
  }

newMVarHandler :: IO (MVarHandler req res)
newMVarHandler = MVarHandler <$> newEmptyMVar <*> newEmptyMVar

mkFRPHandler
  :: (PerformEvent t m, TriggerEvent t m, MonadIO m)
  => (PerformEvent t n, MonadJSM (Performable n))
  => MVarHandler req res -> m (FRPHandler req res t n)
mkFRPHandler (MVarHandler reqMVar resMVar) = do
  reqs <- mvarTriggerEvent reqMVar
  let resps = performEvent . fmap (liftJSM . liftIO . putMVar resMVar)
  pure $ FRPHandler reqs resps

-- | Push writes to the given 'MVar' into an 'Event'.
mvarTriggerEvent
  :: (PerformEvent t m, TriggerEvent t m, MonadIO m)
  => MVar a -> m (Event t a)
mvarTriggerEvent mvar = do
  (e, trigger) <- newTriggerEvent
  _ <- liftIO $ forkIO $ forever $ trigger =<< takeMVar mvar
  pure e
