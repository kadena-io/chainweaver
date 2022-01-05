{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Desktop.ImportExport where

import qualified Cardano.Crypto.Wallet as Crypto
import Control.Lens (over, mapped, _Left)
import Control.Error (hoistEither, failWith)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT, throwError)
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON, Value, eitherDecode, object, (.=), (.:), withObject)
import Data.Aeson.Types (Parser, parseEither)
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Time (getZonedTime, zonedTimeToLocalTime, iso8601DateFormat, formatTime)
import Data.Functor.Identity (Identity, runIdentity)
import Language.Javascript.JSaddle (MonadJSM)
import Data.Time (getCurrentTime)
import System.Locale.Read (getCurrentLocale)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Encoding as TL
import Reflex

import Desktop.Orphans ()
import Desktop.Crypto.BIP (BIPStorage(..), bipMetaPrefix, runBIPCryptoT, passwordRoundTripTest)
import Frontend.AppCfg (ExportWalletError(..), FileType(FileType_Import), fileTypeExtension)
import Pact.Server.ApiClient (WalletEvent (..), TransactionLogger (..))
import Frontend.Crypto.Class (HasCrypto)
import Frontend.Wallet (PublicKeyPrefix (..), genZeroKeyPrefix)
import Frontend.Storage (HasStorage, dumpLocalStorage)
import Frontend.VersionedStore (VersionedStorage(..), StorageVersion, VersioningDecodeJsonError(..))
import qualified Frontend.VersionedStore as FrontendStore
import Frontend.Crypto.Password

data ImportWalletError
  = ImportWalletError_PasswordIncorrect
  | ImportWalletError_NoRootKey
  | ImportWalletError_NotJson Text
  | ImportWalletError_UnknownVersion Text StorageVersion
  | ImportWalletError_DecodeError Text StorageVersion Text
  | ImportWalletError_InvalidCommandLogDestination
  | ImportWalletError_CommandLogWriteError
  deriving (Eq, Show)

bipStorageVersionKey :: Text
bipStorageVersionKey = "BIPStorage_Version"

bipStorageDataKey :: Text
bipStorageDataKey = "BIPStorage_Data"

storeFrontendVersionKey :: Text
storeFrontendVersionKey = "StoreFrontend_Version"

storeFrontendDataKey :: Text
storeFrontendDataKey = "StoreFrontend_Data"

chainweaverImportObj :: String
chainweaverImportObj = "ChainweaverImport"

hoistParser
  :: Monad m
  => Text
  -> StorageVersion
  -> (Value -> Parser a)
  -> Value
  -> ExceptT ImportWalletError m a
hoistParser errLabel ver p  =
  hoistEither
  . first (ImportWalletError_DecodeError errLabel ver . T.pack)
  . parseEither p

extractImportDataField
  :: forall a m
  . ( Monad m
    , FromJSON a
    )
  => Text
  -> StorageVersion
  -> Value
  -> ExceptT ImportWalletError m a
extractImportDataField key ver =
  hoistParser key ver (withObject chainweaverImportObj (.: key))

extractImportVersionField
  :: (Monad m)
  => Text
  -> StorageVersion
  -> Value
  -> ExceptT ImportWalletError m StorageVersion
extractImportVersionField =
  extractImportDataField

doImport
  :: forall t m
  .  ( MonadIO m
     , MonadJSM m
     , HasStorage m
     , MonadSample t m
     , Reflex t
     )
  => TransactionLogger
  -> Password
  -> Text -- Backup data
  -> m (Either ImportWalletError (Crypto.XPrv, Password))
doImport txLogger pw contents = runExceptT $ do
  jVal <- hoistEither . first (ImportWalletError_NotJson . T.pack) $
    eitherDecode @Value (TL.encodeUtf8 . TL.fromStrict $ contents)

  bVer <- extractImportVersionField bipStorageVersionKey 0 jVal
  unless (bVer == 0) $ throwError $ ImportWalletError_UnknownVersion "BIPStorage" bVer
  bipCrypto <- extractImportDataField @(DMap BIPStorage Identity) bipStorageDataKey 0 jVal
  rootKey <- failWith ImportWalletError_NoRootKey (runIdentity <$> DMap.lookup BIPStorage_RootKey bipCrypto)

  let pwOk = passwordRoundTripTest rootKey pw

  unless pwOk $ throwError ImportWalletError_PasswordIncorrect

  feVer <- extractImportVersionField storeFrontendVersionKey 0 jVal
  feData <- extractImportDataField @Value storeFrontendDataKey feVer jVal

  _ <- ExceptT $ runBIPCryptoT (constant (rootKey, unPassword pw)) $ do
    let vStore = FrontendStore.versionedStorage
    feLatestEither <- first (expandDecodeVersionJsonError storeFrontendDataKey feVer)
      <$> (_versionedStorage_decodeVersionedJson vStore feVer feData)

    traverse_
      (\dmap -> liftIO (TL.putStrLn $ encodeToLazyText dmap) >> _versionedStorage_restoreBackup vStore dmap)
      feLatestEither

    ts <- liftIO getCurrentTime
    sender <- genZeroKeyPrefix
    liftIO $ _transactionLogger_walletEvent txLogger WalletEvent_Import (_unPublicKeyPrefix sender) ts
    pure $ Right ()

  pure (rootKey, pw)

  where
    expandDecodeVersionJsonError section expectedVer (VersioningDecodeJsonError_DecodeFailure err) =
      ImportWalletError_DecodeError section expectedVer err
    expandDecodeVersionJsonError section _ (VersioningDecodeJsonError_UnknownVersion ver) =
      ImportWalletError_UnknownVersion section ver

doExport
  :: forall m
  .  ( HasCrypto Crypto.XPrv m
     , MonadJSM m
     , HasStorage m
     )
  => TransactionLogger
  -> PublicKeyPrefix
  -> Password
  -> Password
  -> m (Either ExportWalletError (FilePath, Text))
doExport txLogger keyPfx oldPw pw = runExceptT $ do
  unless (oldPw == pw) $ throwError ExportWalletError_PasswordIncorrect
  let store = FrontendStore.versionedStorage @Crypto.XPrv @m

  -- Trigger an upgrade of the storage to ensure we're exporting the latest version.
  _ <- ExceptT $ over (mapped . _Left) (const ExportWalletError_UpgradeFailed)
    $ _versionedStorage_upgradeStorage store txLogger

  (bipVer,bipData) <- lift $ dumpLocalStorage @BIPStorage bipMetaPrefix
  (feVer, feData) <- lift $ _versionedStorage_dumpLocalStorage store

  tl <- liftIO getCurrentLocale
  lt <- zonedTimeToLocalTime <$> liftIO getZonedTime

  pure $
    ( intercalate "."
      [ T.unpack $ _unPublicKeyPrefix keyPfx
      -- Mac does something weird with colons in the name and converts them to subdirs...
      , formatTime tl (iso8601DateFormat (Just "%H-%M-%S")) lt
      , T.unpack (fileTypeExtension FileType_Import)
      ]
    , TL.toStrict $ encodeToLazyText $ object $
      [ "BIPStorage_Version" .= bipVer, "BIPStorage_Data" .= bipData
      , "StoreFrontend_Version" .= feVer, "StoreFrontend_Data" .= feData
      ]
    )
