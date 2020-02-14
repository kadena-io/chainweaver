{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Desktop.ImportExport where

import qualified Cardano.Crypto.Wallet as Crypto
import Control.Error (hoistEither, failWith)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT, throwError)
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON, Value, eitherDecode, object, (.=), (.!=), (.:?), (.:), withObject)
import Data.Aeson.Types (Parser, parseEither)
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (first)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Foldable (fold)
import qualified Data.IntMap as IntMap
import Data.Time (getZonedTime, zonedTimeToLocalTime, iso8601DateFormat, formatTime, defaultTimeLocale)
import Data.Functor.Identity (Identity(Identity), runIdentity)
import Language.Javascript.JSaddle (MonadJSM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Encoding as TL
import Reflex

import Common.Wallet (_keyPair_publicKey, _key_pair, keyToText)
import Desktop.Orphans ()
import Desktop.Crypto.BIP (BIPStorage(..), bipMetaPrefix, runBIPCryptoT)
import Frontend.AppCfg (ExportWalletError(..), FileType(FileType_Import), fileTypeExtension)
import Frontend.Crypto.Class (HasCrypto)
import Frontend.Storage (HasStorage, dumpLocalStorage)
import Frontend.VersionedStore (StoreFrontend(..), VersionedStorage(..), StorageVersion, VersioningDecodeJsonError(..))
import qualified Frontend.VersionedStore as FrontendStore

newtype Password = Password { unPassword :: Text } deriving (Eq)

data ImportWalletError
  = ImportWalletError_PasswordIncorrect
  | ImportWalletError_NoRootKey
  | ImportWalletError_NotJson Text
  | ImportWalletError_UnknownVersion Text StorageVersion
  | ImportWalletError_DecodeError Text StorageVersion Text
  deriving (Eq, Show)

bipStorageVersionKey :: Text
bipStorageVersionKey = "BIPStorage_Version"
bipStorageDataKey :: Text
bipStorageDataKey = "BIPStorage_Data"
storeFrontendVersionKey :: Text
storeFrontendVersionKey = "StoreFrontend_Version"
storeFrontendDataKey :: Text
storeFrontendDataKey = "StoreFrontend_Data"

hoistParser :: Monad m => Text -> StorageVersion -> (Value -> Parser a) -> Value -> ExceptT ImportWalletError m a
hoistParser errLabel ver p  =
  hoistEither
  . first (ImportWalletError_DecodeError errLabel ver . T.pack)
  . parseEither p

extractImportVersionField :: (Monad m) => Text -> StorageVersion -> Value -> ExceptT ImportWalletError m StorageVersion
extractImportVersionField key ver = hoistParser key ver (withObject "ChainWeaverImport" (\o -> o .:? key .!= 0 ))

extractImportDataField :: forall a m. (Monad m, FromJSON a) => Text -> StorageVersion -> Value -> ExceptT ImportWalletError m a
extractImportDataField key ver = hoistParser key ver (withObject "ChainWeaverImport" (\o -> o .: key))

doImport
  :: forall t m
  .  ( MonadIO m
     , MonadJSM m
     , HasStorage m
     , MonadSample t m
     , Reflex t
     )
  => Password -- Password
  -> Text -- Backup data
  -> m (Either ImportWalletError (Crypto.XPrv, Password))
doImport pw contents = runExceptT $ do
  jVal <- hoistEither . first (ImportWalletError_NotJson . T.pack) $
    eitherDecode @Value (TL.encodeUtf8 . TL.fromStrict $ contents)

  bVer <- extractImportVersionField bipStorageVersionKey 0 jVal
  unless (bVer == 0) $ throwError $ ImportWalletError_UnknownVersion "BIPStorage" bVer
  bipCrypto <- extractImportDataField @(DMap BIPStorage Identity) bipStorageDataKey 0 jVal
  rootKey <- failWith ImportWalletError_NoRootKey (runIdentity <$> DMap.lookup BIPStorage_RootKey bipCrypto)

  -- TODO: Test that the password is good for the XPrv.Crypto

  feVer <- extractImportVersionField storeFrontendVersionKey 0 jVal
  feData <- extractImportDataField @Value storeFrontendDataKey feVer jVal

  _ <- ExceptT $ runBIPCryptoT (constant (rootKey, unPassword pw)) $ do
    let vStore = FrontendStore.versionedStorage
    feLatestEither <- first (expandDecodeVersionJsonError storeFrontendDataKey feVer)
      <$> (_versionedStorage_decodeVersionedJson vStore feVer feData)
    -- For some silly reason, this has the right data, but the import doesn't stick
    -- We end up with no keys or accounts
    traverse (\dmap -> liftIO (TL.putStrLn $ encodeToLazyText dmap) >> _versionedStorage_restoreBackup vStore dmap) feLatestEither

  pure (rootKey, pw)

  where
    expandDecodeVersionJsonError section expectedVer (VersioningDecodeJsonError_DecodeFailure err) =
      ImportWalletError_DecodeError section expectedVer err
    expandDecodeVersionJsonError section _ (VersioningDecodeJsonError_UnknownVersion ver) =
      ImportWalletError_UnknownVersion section ver

-- This is running on the assumption that the storage has been upgraded already and isn't on
-- an old version.
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
  (bipVer,bipData) <- lift $ dumpLocalStorage @BIPStorage bipMetaPrefix
  (feVer, feData) <- lift $ _versionedStorage_dumpLocalStorage (FrontendStore.versionedStorage @Crypto.XPrv @m)

  keyPair <- failWith ExportWalletError_NoKeys $ do
    (Identity keyMap) <- DMap.lookup StoreFrontend_Wallet_Keys feData
    IntMap.lookup 0 keyMap

  lt <- zonedTimeToLocalTime <$> liftIO getZonedTime

  pure $
    ( fold
      [ (T.unpack . (\t -> T.take 4 t <> T.takeEnd 4 t) . keyToText . _keyPair_publicKey . _key_pair $ keyPair)
      , "."
      , formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) lt
      , "."
      , T.unpack (fileTypeExtension FileType_Import)
      ]
    , TL.toStrict $ encodeToLazyText $ object
      [ "BIPStorage_Version" .= bipVer, "BIPStorage_Data" .= bipData
      , "StoreFrontend_Version" .= feVer, "StoreFrontend_Data" .= feData
      ]
    )
