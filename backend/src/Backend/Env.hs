{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Environment backend operations are run in.
module Backend.Env
  ( Env (..)
  , env_random
  , ServerM
  , MonadServer
  , genRandomBytes
  ) where


-- Don't really know why this package import is necessary, as we only have
-- crypto-api in the cabal file ...
import           Control.Concurrent.STM      (STM, atomically, retry)
import           Control.Concurrent.STM.TVar
import           Control.Lens
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (MonadReader, asks)
import           Control.Monad.Trans.Reader  (ReaderT, runReaderT)
import           Crypto.Classes.Exceptions   (genBytes)
import           "crypto-api" Crypto.Random  (SystemRandom)
import           Data.ByteString             (ByteString)
import           Snap.Core                   (MonadSnap, Snap)


data Env = Env
  { _env_random :: !(TVar SystemRandom)

  }

makeLenses ''Env

-- | Monad transformer stack for running server operations.
type ServerM a = ReaderT Env Snap a

-- | Monad for running server operations.
type MonadServer m = (MonadReader Env m, MonadSnap m, MonadIO m)



genRandomBytes :: MonadServer m => Int -> m ByteString
genRandomBytes l = do
    randRef <- asks _env_random
    liftIO $ genBytesSecure randRef
  where
    -- STM necessary so multiple threads won't return the same secret!
    genBytesSecure randRef = atomically $ do
      oldGen <- readTVar randRef
      let (r, newGen) = genBytes l oldGen
      writeTVar randRef newGen
      pure r
