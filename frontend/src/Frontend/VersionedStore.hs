{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.VersionedStore
  ( module V1
  , VersionedStorage(..)
  , StorageVersion
  , VersioningDecodeJsonError(..)
  , VersioningUpgradeError(..)
  , versionedStorage
  , versionedFrontend
  ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans (lift)
import Control.Error ((!?), hoistEither)
import Data.Aeson (FromJSON, ToJSON, Value, parseJSON)
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (first)
import Data.Constraint.Extras (Has')
import Data.Dependent.Map (DMap, DSum(..), Some(Some), GCompare)
import Data.Functor.Identity (Identity)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import Numeric.Natural (Natural)
import Reflex
import Reflex.Dom (DomBuilder, widgetHold_, blank, text)

import Frontend.Storage.Class
import qualified Frontend.Storage.Class as Storage
import qualified Frontend.VersionedStore.V0 as V0
import qualified Frontend.VersionedStore.V1 as V1
import Frontend.VersionedStore.V1 as Latest
import Frontend.Crypto.Class

type StorageVersion = Natural

data VersioningUpgradeError
  = VersioningUpgradeError_UnknownVersion StorageVersion
  | VersioningUpgradeError_CouldNotBackup StorageVersion
  deriving (Show)

data VersioningDecodeJsonError
  = VersioningDecodeJsonError_UnknownVersion StorageVersion
  | VersioningDecodeJsonError_DecodeFailure Text
  deriving Show

data StoreFrontendVersion key k where
  StoreFrontendVersion_0 :: StoreFrontendVersion key (V0.StoreFrontend key)
  StoreFrontendVersion_1 :: StoreFrontendVersion key (V1.StoreFrontend key)

parseVersion :: forall key. StorageVersion -> Maybe (DSum (StoreFrontendVersion key) Proxy)
parseVersion 0 = Just $ StoreFrontendVersion_0 :=> (Proxy @(V0.StoreFrontend key))
parseVersion 1 = Just $ StoreFrontendVersion_1 :=> (Proxy @(V1.StoreFrontend key))
parseVersion _ = Nothing

_nextVersion :: Some (StoreFrontendVersion key) -> Maybe (Some (StoreFrontendVersion key))
_nextVersion (Some StoreFrontendVersion_0) = Just (Some StoreFrontendVersion_1)
_nextVersion (Some StoreFrontendVersion_1) = Nothing

versionedFrontend
  :: forall t m key
   . ( DomBuilder t m, PostBuild t m, MonadHold t m, PerformEvent t m
     )
  => VersionedStorage (Performable m) (Latest.StoreFrontend key)
  -> m ()
  -> m ()
versionedFrontend v widget = do
  pbE <- getPostBuild
  migratedE <- performEvent $ (_versionedStorage_upgradeStorage v) <$ pbE
  widgetHold_ blank (displayResult <$> migratedE)
  where
    -- Should we do this, or should we just push on? The backup functionality really
    -- depends on the storage being properly latest, but I think that our failthrough
    -- to nothing behaviour in storage will just zero everything out to latest version...
    displayResult (Left e) = text $ "Version upgrade failed. Chainweaver cannot start. Error is: " <> (T.pack . show $ e)
    displayResult (Right ()) = widget

data VersionedStorage m k = VersionedStorage
  { _versionedStorage_metaPrefix :: StoreKeyMetaPrefix
  , _versionedStorage_upgradeStorage :: m (Either VersioningUpgradeError ())
  , _versionedStorage_decodeVersionedJson :: StorageVersion -> Value -> m (Either VersioningDecodeJsonError (DMap k Identity))
  , _versionedStorage_dumpLocalStorage :: m (StorageVersion, DMap k Identity)
  , _versionedStorage_restoreBackup :: DMap k Identity -> m ()
  }

-- Notes
-- -- This kinda means that we can't change the key representation ever.
--     But this will be hard to fix because the frontend doesn't know anything about this
--     key other than it can go to/from json
-- -- We don't do any looping of upgrades, so when we do a new version we'll have to think about that
versionedStorage
  :: forall key m.
     ( ToJSON key
     , FromJSON key
     , Monad m
     , HasStorage m
     , HasCrypto key m
     )
  => VersionedStorage m (Latest.StoreFrontend key)
versionedStorage = VersionedStorage
  { _versionedStorage_metaPrefix = prefix
  , _versionedStorage_upgradeStorage = upgradeStorage
  , _versionedStorage_decodeVersionedJson = decodeVersionedJson
  , _versionedStorage_dumpLocalStorage = dumpLocalStorage'
  , _versionedStorage_restoreBackup = restoreBackup
  }
  where
    prefix :: StoreKeyMetaPrefix
    prefix = StoreKeyMetaPrefix "StoreFrontend_Meta"

    restoreBackup :: DMap (Latest.StoreFrontend key) Identity -> m ()
    restoreBackup dm = restoreLocalStorageDump prefix dm 1

    -- Takes a json blob and upgrades it to the latest DMap structure
    decodeVersionedJson
      :: StorageVersion
      -> Value
      -> m (Either VersioningDecodeJsonError (DMap (Latest.StoreFrontend key) Identity))
    decodeVersionedJson ver jval = runExceptT $ case parseVersion @key ver of
      Nothing -> throwError $ VersioningDecodeJsonError_UnknownVersion ver
      Just (StoreFrontendVersion_0 :=> p) -> do
        v0map <- decodeDMap p jval
        lift $ V1.upgradeFromV0 v0map
      Just (StoreFrontendVersion_1 :=> p) -> do
        decodeDMap p jval

    decodeDMap
      :: (GCompare k, FromJSON (Some k), Has' FromJSON k Identity)
      => Proxy k -> Value -> ExceptT VersioningDecodeJsonError m (DMap k Identity)
    decodeDMap _ = hoistEither . first (VersioningDecodeJsonError_DecodeFailure . T.pack) . parseEither parseJSON

    dumpLocalStorage' :: m (StorageVersion, DMap (Latest.StoreFrontend key) Identity)
    dumpLocalStorage' = Storage.dumpLocalStorage prefix

    upgradeStorage :: m (Either VersioningUpgradeError ())
    upgradeStorage = runExceptT $ do
      ver <- lift $ getCurrentVersion prefix
      case parseVersion @key ver of
        Nothing -> throwError $ VersioningUpgradeError_UnknownVersion ver
        Just (StoreFrontendVersion_0 :=> p) -> do
          dump <- (backupLocalStorage prefix p ver) !? VersioningUpgradeError_CouldNotBackup ver
          lift $ do
            v1Dump <- V1.upgradeFromV0 dump
            removeKeyUniverse p localStorage
            removeKeyUniverse p sessionStorage
            restoreLocalStorageDump prefix v1Dump 1
          pure ()
        Just (StoreFrontendVersion_1 :=> _) -> pure ()
