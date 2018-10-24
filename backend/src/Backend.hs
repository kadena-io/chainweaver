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
import           Frontend
import qualified Obelisk.Backend                 as Ob

backend :: IO ()
backend = Ob.backend Ob.def
  { Ob._backendConfig_head = do
      fst frontend
      -- TODO: When upgrading Obelisk, we should use that:
      -- injectExecutableConfigs
      let
        k1 = "common/pact-port"
        k2 = "common/route"
        doesNotExist = \e -> if isDoesNotExistError e then Just () else Nothing
      mUrl <- liftIO $ catchJust doesNotExist
	(fmap Just . T.readFile $ "config/" <> k2) (const $ pure Nothing)
      traverse_ (injectPure k2) mUrl
      mPort <- liftIO $ catchJust doesNotExist
	(fmap Just . T.readFile $ "config/" <> k1) (const $ pure Nothing)
      traverse_ (injectPure k1) mPort
  }
