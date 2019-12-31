{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Store
  ( module V1
  , versioner
  , versionedUi
  ) where

import Frontend.Storage
import Data.Aeson (FromJSON, ToJSON)
import Data.Dependent.Sum.Orphans ()
import Data.Proxy (Proxy(Proxy))
import Language.Javascript.JSaddle (MonadJSM, JSM)
import Reflex
import Reflex.Dom

import qualified Frontend.Store.V0 as V0
import qualified Frontend.Store.V1 as V1
import Frontend.Store.V1 as Latest

-- Doesn't execute the widget until we've checked the current version and upgraded if necessary
-- TODO: This should have a better home.
versionedUi
  :: forall t m key
   . ( DomBuilder t m, HasStorage (Performable m), PostBuild t m, MonadHold t m
     , PerformEvent t m , MonadJSM (Performable m), StorageM (Performable m) ~ JSM
     )
  => StorageVersioner key
  -> m ()
  -> m ()
versionedUi v widget = do
  pbE <- getPostBuild
  migratedE <- performEvent $ performUpdate <$ pbE
  widgetHold_ blank $ widget <$ migratedE
  where
    performUpdate = do
      _ <- runStorageJSM $ storageVersioner_upgrade v
      pure ()

versioner :: forall key. (ToJSON key, FromJSON key) => StorageVersioner (Latest.StoreFrontend key)
versioner = StorageVersioner
  { storageVersion_metaPrefix = prefix
  , storageVersioner_backupVersion = backup
  , storageVersioner_upgrade = upgrade
  }
  where
    prefix :: StoreKeyMetaPrefix
    prefix = StoreKeyMetaPrefix "StoreFrontend_Meta"
    backup :: Storage (Maybe VersioningError)
    backup = do
      ver <- getCurrentVersion prefix
      case ver of
        0 -> do
          _ok <- backupLocalStorage prefix (Proxy @(V0.StoreFrontend key)) 0
          pure Nothing
        1 -> do
          _ok <- backupLocalStorage prefix (Proxy @(V1.StoreFrontend key)) 0
          pure Nothing
        v -> pure $ Just $ VersioningError_UnknownVersion v

    upgrade :: Storage (Maybe VersioningError)
    upgrade = do
      ver <- getCurrentVersion prefix
      case ver of
        0 -> do
          mDump <- backupLocalStorage prefix (Proxy @(V0.StoreFrontend key)) 0
          case mDump of
            Nothing -> error "TODO Add a version error case for this"
            Just dump -> do
              let v1Dump = (V1.upgradeFromV0 dump)
              removeKeyUniverse (Proxy @(V0.StoreFrontend key)) localStorage
              removeKeyUniverse (Proxy @(V0.StoreFrontend key)) sessionStorage
              restoreLocalStorageDump prefix v1Dump 1
              pure Nothing
        1 -> pure Nothing
        v -> pure $ Just $ VersioningError_UnknownVersion v
