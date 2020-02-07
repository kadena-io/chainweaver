{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Desktop.Storage.SQLite where

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Control.Monad.Primitive (PrimMonad (PrimState, primitive))
import Control.Monad.Reader
import Control.Monad.Ref (MonadRef, MonadAtomicRef)

import Data.Coerce (coerce)

import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple.QQ (sql)
import qualified Database.SQLite.Simple as Sql

import Database.SQLite.SimpleErrors (runDBAction)
import Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import Data.Pool (Pool)
import qualified Data.Pool as Pool

import Language.Javascript.JSaddle (MonadJSM)

import Obelisk.Configs
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Reflex.Host.Class (MonadReflexCreateTrigger)

import Pact.Server.ApiV1Client (HasTransactionLogger)

newtype SQLiteStorageT m a = SQLiteStorageT
  { unSQLiteStorageT :: ReaderT (Pool Connection) m a
  } deriving
    ( Functor, Applicative, Monad
    , MonadFix, MonadIO, MonadRef, MonadAtomicRef
    , DomBuilder t, NotReady t, MonadHold t, MonadSample t
    , TriggerEvent t, PostBuild t, HasJS x
    , MonadReflexCreateTrigger t, MonadQuery t q, Requester t
    , HasTransactionLogger
    )

class HasSQLiteStorage (m :: * -> *) where
  askSQLiteStoragePool :: m (Pool Connection)

instance Monad m => HasSQLiteStorage (SQLiteStorageT m) where
  askSQLiteStoragePool = SQLiteStorageT ask

instance PerformEvent t m => PerformEvent t (SQLiteStorageT m) where
  type Performable (SQLiteStorageT m) = SQLiteStorageT (Performable m)
  performEvent_ e = SQLiteStorageT $ do
    f <- ask
    lift $ performEvent_ $ runSQLiteStorageT f <$> e
  performEvent e = SQLiteStorageT $ do
    f <- ask
    lift $ performEvent $ runSQLiteStorageT f <$> e

instance PrimMonad m => PrimMonad (SQLiteStorageT m) where
  type PrimState (SQLiteStorageT m) = PrimState m
  primitive = lift . primitive

instance MonadReader r m => MonadReader r (SQLiteStorageT m) where
  ask = lift ask
  local f m = SQLiteStorageT $ ask >>= lift . local f . flip runSQLiteStorageT m

instance HasDocument m => HasDocument (SQLiteStorageT m)
instance HasJSContext m => HasJSContext (SQLiteStorageT m) where
  type JSContextPhantom (SQLiteStorageT m) = JSContextPhantom m
  askJSContext = SQLiteStorageT askJSContext
#if !defined(ghcjs_HOST_OS)
instance MonadJSM m => MonadJSM (SQLiteStorageT m)
#endif

instance (Monad m, Routed t r m) => Routed t r (SQLiteStorageT m) where
  askRoute = lift askRoute

instance (Monad m, RouteToUrl r m) => RouteToUrl r (SQLiteStorageT m) where
  askRouteToUrl = lift askRouteToUrl

instance (Reflex t, Monad m, SetRoute t r m) => SetRoute t r (SQLiteStorageT m) where
  modifyRoute = lift . modifyRoute

instance EventWriter t w m => EventWriter t w (SQLiteStorageT m) where
  tellEvent = lift . tellEvent

instance MonadTrans (SQLiteStorageT) where
  lift = SQLiteStorageT . lift

instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (SQLiteStorageT m) where
  runWithReplace a0 a' = SQLiteStorageT $ runWithReplace (unSQLiteStorageT a0) (fmapCheap unSQLiteStorageT a')
  traverseDMapWithKeyWithAdjust f dm0 dm' = SQLiteStorageT $ traverseDMapWithKeyWithAdjust (coerce . f) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = SQLiteStorageT $ traverseDMapWithKeyWithAdjustWithMove (coerce . f) dm0 dm'
  traverseIntMapWithKeyWithAdjust f im0 im' = SQLiteStorageT $ traverseIntMapWithKeyWithAdjust (coerce f) im0 im'

deriving instance DomRenderHook t m => DomRenderHook t (SQLiteStorageT m)

instance (Prerender js t m, Monad m, Reflex t) => Prerender js t (SQLiteStorageT m) where
  type Client (SQLiteStorageT m) = SQLiteStorageT (Client m)
  prerender a b = SQLiteStorageT $ prerender (unSQLiteStorageT a) (unSQLiteStorageT b)

instance HasConfigs m => HasConfigs (SQLiteStorageT m) where
  getConfigs = lift getConfigs

runSQLiteStorageT :: Pool Connection -> SQLiteStorageT m a -> m a
runSQLiteStorageT connPool = flip runReaderT connPool . unSQLiteStorageT

initialiseInMemorySQLiteLogStorage :: MonadIO m => FilePath -> m (Pool Connection)
initialiseInMemorySQLiteLogStorage db =
  let
    stripes = 1
    connections = 5
  in
    liftIO $ Pool.createPool (Sql.open db) Sql.close stripes 1 connections

withSQLitePool
  :: ( MonadIO m
     , HasSQLiteStorage m
     )
  => (Connection -> IO a)
  -> m (Either SQLiteResponse a)
withSQLitePool ma = do
  pool <- askSQLiteStoragePool
  liftIO $ Pool.withResource pool (runDBAction . ma)

sqliteQuery
  :: ( MonadIO m
     , HasSQLiteStorage m
     , Sql.ToRow r
     , Sql.FromRow a
     )
  => Sql.Query
  -> r
  -> m (Either SQLiteResponse [a])
sqliteQuery q r = withSQLitePool $ \conn ->
  Sql.query conn q r

sqliteExecute
  :: ( MonadIO m
     , HasSQLiteStorage m
     , Sql.ToRow r
     )
  => Sql.Query
  -> r
  -> m (Either SQLiteResponse ())
sqliteExecute q r = withSQLitePool $ \conn ->
  Sql.execute conn q r

sqliteExecute_
  :: ( MonadIO m
     , HasSQLiteStorage m
     )
  => Sql.Query
  -> m (Either SQLiteResponse ())
sqliteExecute_ q = withSQLitePool $ \conn ->
  Sql.execute_ conn q
