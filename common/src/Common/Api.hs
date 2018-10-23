module Common.Api where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Semigroup ((<>))



-- | How many Pact instances to launch in development mode.
numPactInstances :: Int
numPactInstances = 4

-- | Get the port number a given pact instance is launched on.
--
--   Numeration starts at 1. So for `numPactInstances` == 2 we would have
--   instances with numbers 1 and 2.
getPactInstancePort :: Int -> Text
getPactInstancePort num = T.pack . show $ 7020 + num
