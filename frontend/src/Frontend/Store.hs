{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Store
  ( module V1
  , versioner
  ) where

import Frontend.Storage
import Data.Aeson (FromJSON, ToJSON)
import Data.Dependent.Sum.Orphans ()
import Data.Proxy (Proxy(Proxy))

import qualified Frontend.Store.V0 as V0
import Frontend.Store.V1 as V1

type Latest key = V1.StoreFrontend key

versioner :: forall key. (ToJSON key, FromJSON key) => StorageVersioner (Latest key)
versioner = StorageVersioner
  { storageVersioner_backupVersion = backup
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
              restoreLocalStorageDump (V1.upgradeFromV0 dump)
              pure Nothing
        1 -> pure Nothing
        v -> pure $ Just $ VersioningError_UnknownVersion v
