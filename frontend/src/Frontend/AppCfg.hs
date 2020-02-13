-- | AppCfg is used to configure the app and pass things in and out of reflex
module Frontend.AppCfg where

import Control.Monad.Logger (LogLevel, LogStr)
import Data.Text (Text)
import Language.Javascript.JSaddle (JSM)
import Reflex.Dom hiding (Key)

import Common.Wallet (Key)
import Kadena.SigningApi

data ChangePassword key t m = ChangePassword
  { _changePassword_requestChange :: Event t (Text, Text, Text) -> m (Event t (Either Text ()))
  -- ^ Request to change the password. Performs validation and changes passwords
  -- if validation passes. (Old pass, new pass, repeat pass).
  , _changePassword_updateKeys :: Event t (Int -> Key key -> Key key)
  -- ^ Update all keys using the given function
  }

data ExportWalletError =  ExportWalletError_PasswordIncorrect | ExportWalletError_NoKeys

data ExportWallet t m = ExportWallet
  { _exportWallet_requestExport :: Event t Text -> m (Event t (Either ExportWalletError ()))
  -- ^ Request to export the wallet to the user as a file. Password must match storage.
  }

data EnabledSettings key t m = EnabledSettings
  { _enabledSettings_changePassword :: Maybe (ChangePassword key t m)
  , _enabledSettings_exportWallet :: Maybe (ExportWallet t m)
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
  , _fileFFI_deliverFile :: Event t (FilePath, Text) -> m (Event t ())
  -- ^ Delivers a file to an appropriate place to the user
  }

-- TODO: Tidy this up
liftFileFFI :: (forall a. m a -> n a) -> FileFFI t m -> FileFFI t n
liftFileFFI natTransform oldFFI = oldFFI { _fileFFI_deliverFile = natTransform . (_fileFFI_deliverFile oldFFI) }

data AppCfg key t m = AppCfg
  { _appCfg_gistEnabled :: Bool
  , _appCfg_loadEditor :: m (Maybe Text)
  -- ^ Initial code to load into editor
  , _appCfg_editorReadOnly :: Bool
  -- ^ Is the editor read only?
  , _appCfg_signingRequest :: Event t SigningRequest
  -- ^ Requests to sign this object
  , _appCfg_signingResponse :: Either Text SigningResponse -> JSM ()
  -- ^ Responses to signings
  , _appCfg_enabledSettings :: EnabledSettings key t m
  , _appCfg_logMessage :: LogLevel -> LogStr -> IO ()
  -- ^ Logging Function
  }


