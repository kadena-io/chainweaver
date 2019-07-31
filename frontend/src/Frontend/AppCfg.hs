-- | AppCfg is used to configure the app and pass things in and out of reflex
module Frontend.AppCfg where

import Common.Foundation as Common
import Data.ByteString (ByteString)
import Data.Text (Text)
import Frontend.Crypto.Ed25519
import Language.Javascript.JSaddle (JSM)
import Reflex.Dom
import qualified Data.Aeson as Aeson

data SigningResult = SigningResult
  { _signingResult_publicKeys :: [PublicKey]
  , _signingResult_signatures :: [Signature]
  } deriving Generic

instance Aeson.ToJSON SigningResult where
  toJSON = Aeson.genericToJSON compactEncoding
  toEncoding = Aeson.genericToEncoding compactEncoding

instance Aeson.FromJSON SigningResult where
  parseJSON = Aeson.genericParseJSON compactEncoding

data AppCfg t m = AppCfg
  { _appCfg_gistEnabled :: Bool
  , _appCfg_externalFileOpened :: Event t Text
  -- ^ File contents from file chosen in "open file" dialog
  , _appCfg_openFileDialog :: JSM ()
  -- ^ Trigger an "open file" dialog
  , _appCfg_loadEditor :: m (Maybe Text)
  -- ^ Initial code to load into editor
  , _appCfg_editorReadOnly :: Bool
  -- ^ Is the editor read only?
  , _appCfg_signingRequest :: Event t ByteString
  -- ^ Requests to sign this object
  , _appCfg_signingResponse :: Either Text SigningResult -> JSM ()
  -- ^ Responses to signings
  }
