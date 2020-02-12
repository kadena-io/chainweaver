module Desktop.ImportExport where

import qualified Cardano.Crypto.Wallet as Crypto
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Trans (lift)
import Language.Javascript.JSaddle (MonadJSM)
import Data.Text (Text)

import Desktop.Orphans ()
import Desktop.Crypto.BIP (BIPStorage, bipMetaPrefix)
import Frontend.AppCfg (ExportWalletError(..))
import Frontend.Crypto.Class (HasCrypto)
import Frontend.Storage (HasStorage, dumpLocalStorage, _storageVersioner_metaPrefix)
import Frontend.Store (StoreFrontend(..))
import qualified Frontend.Store as FrontendStore

newtype Password = Password { unPassword :: Text } deriving (Eq)

data ImportWalletError = ImportWalletError_PasswordIncorrect

doImport
  :: forall m
  .  ( MonadIO m
     , MonadJSM m
     , HasStorage m
     )
  => Password -- Password
  -> Text -- Backup data
  -> m (Either ImportWalletError (Crypto.XPrv, Password))
doImport pw contents = pure $ Left ImportWalletError_PasswordIncorrect

doExport
  :: forall m
  .  (HasCrypto Crypto.XPrv m
     , MonadJSM m
     , HasStorage m
     )
  => Password
  -> Password
  -> m (Either ExportWalletError (FilePath, Text))
doExport oldPw pw = runExceptT $ do
  unless (oldPw == pw) $ throwError ExportWalletError_PasswordIncorrect
  bipBackup <- lift $ dumpLocalStorage @BIPStorage bipMetaPrefix
  feBackup <- lift $ dumpLocalStorage
    @(StoreFrontend Crypto.XPrv)
    (_storageVersioner_metaPrefix (FrontendStore.versioner @Crypto.XPrv @m))
  pure ("butts.json","butts")
