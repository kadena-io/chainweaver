-- | AppCfg is used to configure the app and pass things in and out of reflex
module Frontend.AppCfg where

import Data.Text (Text)

import Control.Monad.Logger (LogLevel, LogStr)
import Language.Javascript.JSaddle (JSM)

import Reflex.Dom

import Kadena.SigningApi

data EnabledSettings = EnabledSettings
  { -- Eventually, our settings page will want different settings per app type
    -- but right now we only have the network settings, and all apps want that
    -- so this is empty for now but wired between the bits that need it
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
  , _appCfg_enabledSettings :: EnabledSettings
  -- ^ Logging Function
  , _appCfg_logMessage :: LogLevel -> LogStr -> IO ()
  }


