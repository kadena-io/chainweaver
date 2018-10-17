module Backend where

import           Control.Monad                   (void)
import           Control.Monad.IO.Class          (liftIO)
import           Data.Monoid                     ((<>))
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import           Obelisk.ExecutableConfig.Inject (injectPure)

import           Common.Api
import           Frontend
import qualified Obelisk.Backend                 as Ob

backend :: IO ()
backend = Ob.backend Ob.def
  { Ob._backendConfig_head = do
      fst frontend
      -- TODO: When upgrading Obelisk, we should use that:
      -- injectExecutableConfigs
      let k = "common/server-url"
      url <- liftIO $ T.readFile ("config/" <> k)
      injectPure k url
  }
