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

module Desktop.Storage.File
  ( FileStorageT
  , runFileStorageT
  ) where

import Control.Exception (try, catch)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Control.Monad.Primitive (PrimMonad (PrimState, primitive))
import Control.Monad.Reader
import Control.Monad.Ref (MonadRef, MonadAtomicRef)
import Data.Coerce (coerce)
import Data.Text (Text)
import Language.Javascript.JSaddle (MonadJSM)
import Obelisk.Configs
import Obelisk.Route.Frontend
import Pact.Server.ApiClient (HasTransactionLogger)
import Reflex.Dom.Core
import Reflex.Host.Class (MonadReflexCreateTrigger)
import System.FilePath ((</>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

import Frontend.Storage.Class

-- | Store items as files in the given directory, using the key as the file name
instance MonadJSM m => HasStorage (FileStorageT m) where
  getItemStorage' _ k = FileStorageT $ do
    dir <- ask
    liftIO $ try (T.readFile $ path dir k) >>= \case
      Left (e :: IOError) -> do
        putStrLn $ "Error reading storage: " <> show e <> " : " <> path dir k
        pure Nothing
      Right v -> pure $ Just v
  setItemStorage' _ k a = FileStorageT $ do
    dir <- ask
    liftIO $ catch (T.writeFile (path dir k) a) $ \(e :: IOError) -> do
      putStrLn $ "Error writing storage: " <> show e <> " : " <> path dir k
  removeItemStorage' _ k = FileStorageT $ do
    dir <- ask
    liftIO $ catch (Directory.removeFile (path dir k)) $ \(e :: IOError) -> do
      putStrLn $ "Error removing storage: " <> show e <> " : " <> path dir k

path :: FilePath -> Text -> FilePath
path dir k = dir </> FilePath.makeValid (T.unpack k)

newtype FileStorageT m a = FileStorageT
  { unFileStorageT :: ReaderT FilePath m a
  } deriving
    ( Functor, Applicative, Monad
    , MonadFix, MonadIO, MonadRef, MonadAtomicRef
    , DomBuilder t, NotReady t, MonadHold t, MonadSample t
    , TriggerEvent t, PostBuild t
    , MonadReflexCreateTrigger t, MonadQuery t q, Requester t
    , HasTransactionLogger
    )

instance PerformEvent t m => PerformEvent t (FileStorageT m) where
  type Performable (FileStorageT m) = FileStorageT (Performable m)
  performEvent_ e = FileStorageT $ do
    f <- ask
    lift $ performEvent_ $ runFileStorageT f <$> e
  performEvent e = FileStorageT $ do
    f <- ask
    lift $ performEvent $ runFileStorageT f <$> e

instance PrimMonad m => PrimMonad (FileStorageT m) where
  type PrimState (FileStorageT m) = PrimState m
  primitive = lift . primitive

instance MonadReader r m => MonadReader r (FileStorageT m) where
  ask = lift ask
  local f m = FileStorageT $ ask >>= lift . local f . flip runFileStorageT m

instance HasDocument m => HasDocument (FileStorageT m)
#if !defined(ghcjs_HOST_OS)
instance MonadJSM m => MonadJSM (FileStorageT m)
#endif

instance (Monad m, Routed t r m) => Routed t r (FileStorageT m) where
  askRoute = lift askRoute

instance (Monad m, RouteToUrl r m) => RouteToUrl r (FileStorageT m) where
  askRouteToUrl = lift askRouteToUrl

instance (Reflex t, Monad m, SetRoute t r m) => SetRoute t r (FileStorageT m) where
  modifyRoute = lift . modifyRoute

instance EventWriter t w m => EventWriter t w (FileStorageT m) where
  tellEvent = lift . tellEvent

instance MonadTrans (FileStorageT) where
  lift = FileStorageT . lift

instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (FileStorageT m) where
  runWithReplace a0 a' = FileStorageT $ runWithReplace (unFileStorageT a0) (fmapCheap unFileStorageT a')
  traverseDMapWithKeyWithAdjust f dm0 dm' = FileStorageT $ traverseDMapWithKeyWithAdjust (coerce . f) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = FileStorageT $ traverseDMapWithKeyWithAdjustWithMove (coerce . f) dm0 dm'
  traverseIntMapWithKeyWithAdjust f im0 im' = FileStorageT $ traverseIntMapWithKeyWithAdjust (coerce f) im0 im'

deriving instance DomRenderHook t m => DomRenderHook t (FileStorageT m)

instance (Prerender t m, Monad m, Reflex t) => Prerender t (FileStorageT m) where
  type Client (FileStorageT m) = FileStorageT (Client m)
  prerender a b = FileStorageT $ prerender (unFileStorageT a) (unFileStorageT b)

instance HasConfigs m => HasConfigs (FileStorageT m) where
  getConfigs = lift getConfigs

runFileStorageT :: FilePath -> FileStorageT m a -> m a
runFileStorageT dir = flip runReaderT dir . unFileStorageT
