{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Desktop.ImportExport where

import qualified System.Directory as Dir
import qualified Cardano.Crypto.Wallet as Crypto
import Control.Lens (over, mapped, _Left)
import Control.Error (hoistEither, failWith)
import Control.Exception (catch, displayException)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT, throwError, catchError)
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON, Value, eitherDecode, object, (.=), (.:), withObject)
import Data.Aeson.Types (Parser, parseEither)
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (first)
import Data.Functor (($>))
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import qualified Data.IntMap as IntMap
import Data.Time (getZonedTime, zonedTimeToLocalTime, iso8601DateFormat, formatTime, defaultTimeLocale)
import Data.Functor.Identity (Identity(Identity), runIdentity)
import Language.Javascript.JSaddle (MonadJSM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Encoding as TL
import Reflex

import Common.Wallet (_keyPair_publicKey, _key_pair, keyToText)
import Desktop.Orphans ()
import Desktop.Crypto.BIP (BIPStorage(..), bipMetaPrefix, runBIPCryptoT, passwordRoundTripTest)
import Frontend.AppCfg (ExportWalletError(..), FileType(FileType_Import), fileTypeExtension)
import Pact.Server.ApiClient (TransactionLogger (..), CommandLog, commandLogCurrentVersion)
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

commandLogDataKey :: Text
commandLogDataKey = "CommandLogs_Data"

commandLogVersionKey :: Text
commandLogVersionKey  = "CommandLogs_Version"

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
  hoistParser key ver (withObject chainweaverImportObj (\o -> o .: key))

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

  let pwOk = passwordRoundTripTest rootKey (unPassword pw)

  unless pwOk $ throwError ImportWalletError_PasswordIncorrect

  feVer <- extractImportVersionField storeFrontendVersionKey 0 jVal
  feData <- extractImportDataField @Value storeFrontendDataKey feVer jVal

  -- We may not have any commandlogs included in the import so only fail if there is
  -- something to try to import.
  _ <- attemptImportCommandLogs jVal

  _ <- ExceptT $ runBIPCryptoT (constant (rootKey, unPassword pw)) $ do
    let vStore = FrontendStore.versionedStorage
    feLatestEither <- first (expandDecodeVersionJsonError storeFrontendDataKey feVer)
      <$> (_versionedStorage_decodeVersionedJson vStore feVer feData)

    traverse
      (\dmap -> liftIO (TL.putStrLn $ encodeToLazyText dmap) >> _versionedStorage_restoreBackup vStore dmap)
      feLatestEither

  pure (rootKey, pw)

  where
    expandDecodeVersionJsonError section expectedVer (VersioningDecodeJsonError_DecodeFailure err) =
      ImportWalletError_DecodeError section expectedVer err
    expandDecodeVersionJsonError section _ (VersioningDecodeJsonError_UnknownVersion ver) =
      ImportWalletError_UnknownVersion section ver

    attemptImportCommandLogs jVal = do
      let natVer = fromIntegral commandLogCurrentVersion
      mGetLogVer <- catchError
        (Just <$> extractImportVersionField commandLogVersionKey natVer jVal)
        (const $ pure Nothing)

      case mGetLogVer of
        -- No CommandLogs included in wallet export
        Nothing -> pure ()
        Just cmdLogVer -> do
          rawCmdLogData <- extractImportDataField @Text commandLogDataKey natVer jVal
          -- Empty logs are possible and acceptable, so don't fail if there isn't anything to parse.
          unless (T.null rawCmdLogData) $ hoistEither
            $ first (ImportWalletError_DecodeError commandLogDataKey cmdLogVer . T.pack)
            $ traverse_ (eitherDecode @CommandLog . TL.encodeUtf8 . TL.fromStrict)
            $ T.lines rawCmdLogData

          logFilePath <- failWith ImportWalletError_InvalidCommandLogDestination $
            _transactionLogger_destination txLogger

          ExceptT $ liftIO $ catch (Right <$> T.writeFile logFilePath rawCmdLogData) $ \(_ :: IOError) ->
            pure $ Left ImportWalletError_CommandLogWriteError

doExport
  :: forall m
  .  ( HasCrypto Crypto.XPrv m
     , MonadJSM m
     , HasStorage m
     )
  => TransactionLogger
  -> Password
  -> Password
  -> m (Either ExportWalletError (FilePath, Text))
doExport txLogger oldPw pw = runExceptT $ do
  unless (oldPw == pw) $ throwError ExportWalletError_PasswordIncorrect
  let store = FrontendStore.versionedStorage @Crypto.XPrv @m

  -- Trigger an upgrade of the storage to ensure we're exporting the latest version.
  _ <- ExceptT $ over (mapped . _Left) (const ExportWalletError_UpgradeFailed)
    $ _versionedStorage_upgradeStorage store

  (bipVer,bipData) <- lift $ dumpLocalStorage @BIPStorage bipMetaPrefix
  (feVer, feData) <- lift $ _versionedStorage_dumpLocalStorage store

  keyPair <- failWith ExportWalletError_NoKeys $ do
    (Identity keyMap) <- DMap.lookup StoreFrontend_Wallet_Keys feData
    IntMap.lookup 0 keyMap

  cmdLogFile <- failWith ExportWalletError_CommandLogExport $
    _transactionLogger_destination txLogger

  cmdLogsExist <- liftIO $ catch (Dir.doesFileExist cmdLogFile) $ \(_ :: IOError) ->
    pure False

  cmdLogs <-
    if cmdLogsExist then do
      logs <- ExceptT $ liftIO $ catch (Right <$> TL.readFile cmdLogFile) $ \(e :: IOError) -> do
        liftIO (putStrLn $ displayException e) $> Left ExportWalletError_CommandLogExport
      pure [ commandLogVersionKey .= commandLogCurrentVersion
           , commandLogDataKey .= logs
           ]
    else
      pure mempty

  lt <- zonedTimeToLocalTime <$> liftIO getZonedTime

  pure $
    ( intercalate "."
      [ (T.unpack . (T.take 8) . keyToText . _keyPair_publicKey . _key_pair $ keyPair)
      -- Mac does something weird with colons in the name and converts them to subdirs...
      , formatTime defaultTimeLocale (iso8601DateFormat (Just "%H-%M-%S")) lt
      , T.unpack (fileTypeExtension FileType_Import)
      ]
    , TL.toStrict $ encodeToLazyText $ object $
      [ "BIPStorage_Version" .= bipVer, "BIPStorage_Data" .= bipData
      , "StoreFrontend_Version" .= feVer, "StoreFrontend_Data" .= feData
      ] <> cmdLogs
    )
