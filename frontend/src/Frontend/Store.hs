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

versioner :: forall key. (ToJSON key, FromJSON key) => StorageVersioner (StoreFrontend key)
versioner = StorageVersioner
  { storageVersioner_backupVersion = backup
  , storageVersioner_upgrade = upgrade
  }
  where
    prefix :: StoreKeyMetaPrefix
    prefix = StoreKeyMetaPrefix "StoreFrontend_Meta"
    backup :: StorageVersion -> Storage (Maybe VersioningError)
    backup = \case
      0 -> do
        _ok <- backupLocalStorage prefix (Proxy @(V0.StoreFrontend key)) 0
        pure Nothing
      1 -> undefined
      v -> pure $ Just $ VersioningError_UnknownVersion v

    upgrade :: StorageVersion -> Storage (Maybe VersioningError)
    upgrade = \case
      0 -> undefined
      1 -> pure Nothing
      v -> pure $ Just $ VersioningError_UnknownVersion v
