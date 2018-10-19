{-# LANGUAGE ScopedTypeVariables #-}
module Backend where

import           Control.Exception               (catchJust)
import           Control.Monad                   (void)
import           Control.Monad.IO.Class          (liftIO)
import           Data.Foldable                   (traverse_)
import           Data.Monoid                     ((<>))
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import           Obelisk.ExecutableConfig.Inject (injectPure)
import           System.IO.Error                 (isDoesNotExistError)

import           Common.Api
import           Common.Route
import           Frontend
import qualified Obelisk.Backend                 as Ob

backend :: Ob.Backend BackendRoute FrontendRoute
backend = Ob.Backend
  { Ob._backend_run = \serve -> serve $ const $ return ()
  , Ob._backend_routeEncoder = backendRouteEncoder
  }
