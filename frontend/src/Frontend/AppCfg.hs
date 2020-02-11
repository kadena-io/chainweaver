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

data EnabledSettings key t m = EnabledSettings
  { _enabledSettings_changePassword :: Maybe (ChangePassword key t m)
  }

data AppCfg key t m = AppCfg
  { _appCfg_gistEnabled :: Bool
  , _appCfg_externalFileOpened :: Event t Text
  -- ^ File contents from file chosen in "open file" dialog
  , _appCfg_openFileDialog :: JSM ()
  -- ^ Trigger an "open file" dialog
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


