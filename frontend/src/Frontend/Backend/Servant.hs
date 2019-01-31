{-# LANGUAGE CPP                        #-}

-- | Module abstracting away the differences between servant-client-ghcjs and
-- servant-client.
module Frontend.Backend.Servant
  ( -- * Types & Classes
    HttpManager
  , S.ClientM
  , S.ClientEnv
  , S.BaseUrl
  , S.ServantError (..)
    -- * Functions
  , makeHttpManager
  , mkClientEnv
  , runClientM
  , S.parseBaseUrl
  , S.responseStatusCode
  ) where

import qualified Network.HTTP.Client               as HTTP
import Control.Monad.IO.Class (MonadIO)


#ifdef ghcjs_HOST_OS

import qualified Servant.Client.Ghcjs              as S
import qualified Servant.Client.Internal.XhrClient as S

#else

import           Network.HTTP.Client.TLS           (newTlsManager)
import qualified Servant.Client                    as S
import qualified Network.HTTP.Client as Client

#endif


#ifdef ghcjs_HOST_OS

type HttpManager = ()

makeHttpManager :: MonadIO m => m HttpManager
makeHttpManager = pure ()

mkClientEnv :: HttpManager -> S.BaseUrl -> S.ClientEnv
mkClientEnv = const S.ClientEnv

runClientM :: S.ClientM a -> S.ClientEnv -> IO (Either S.ServantError a)
runClientM = S.runClientMOrigin

#else

type HttpManager = Client.Manager

makeHttpManager :: MonadIO m => m HttpManager
makeHttpManager = newTlsManager

mkClientEnv :: HttpManager -> S.BaseUrl -> S.ClientEnv
mkClientEnv = S.mkClientEnv

runClientM :: S.ClientM a -> S.ClientEnv -> IO (Either S.ServantError a)
runClientM = S.runClientM

#endif


