-- | AppCfg is used to configure the app and pass things in and out of reflex
module Frontend.AppCfg where

import Common.Foundation as Common
import Data.Text (Text)
import Language.Javascript.JSaddle (JSM)
import Reflex.Dom
import qualified Data.Aeson as Aeson
import Pact.Types.ChainMeta (TTLSeconds(..))
import Pact.Types.Runtime (GasLimit(..), ChainId)
import Pact.Types.Command (Command)
import Frontend.Wallet (AccountName, Wallet, WalletCfg)
import Frontend.UI.Wallet

data SigningRequest = SigningRequest
  { _signingRequest_code :: Text
  , _signingRequest_data :: Maybe Aeson.Object
  , _signingRequest_nonce :: Maybe Text
  , _signingRequest_chainId :: Maybe ChainId
  , _signingRequest_gasLimit :: Maybe GasLimit
  , _signingRequest_ttl :: Maybe TTLSeconds
  , _signingRequest_sender :: Maybe AccountName
  } deriving (Show, Generic)

instance Aeson.ToJSON SigningRequest where
  toJSON = Aeson.genericToJSON compactEncoding
  toEncoding = Aeson.genericToEncoding compactEncoding

instance Aeson.FromJSON SigningRequest where
  parseJSON = Aeson.genericParseJSON compactEncoding

data SigningResponse = SigningResponse
  { _signingResponse_body :: Command Text
  , _signingResponse_chainId :: ChainId
  } deriving (Eq, Show, Generic)

instance Aeson.ToJSON SigningResponse where
  toJSON = Aeson.genericToJSON compactEncoding
  toEncoding = Aeson.genericToEncoding compactEncoding

instance Aeson.FromJSON SigningResponse where
  parseJSON = Aeson.genericParseJSON compactEncoding

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
  , _appCfg_makeWallet :: WalletCfg key t -> m (Wallet key t)
  -- ^ Wallet manager
  , _appCfg_displayWallet :: forall mConf. (HasUiWalletModelCfg mConf key m t) => Wallet key t -> m mConf
  -- ^ Wallet widget
  , _appCfg_sidebarExtra :: m ()
  -- ^ Extra widget to display at the bottom of the sidebar
  }

-- Are we running the Kadena Chainweaver ALPHA edition
isChainweaverAlpha :: Bool
isChainweaverAlpha = True
