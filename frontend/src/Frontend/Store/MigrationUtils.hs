module Frontend.Store.MigrationUtils where

import qualified Data.Dependent.Map as DMap
import Data.Dependent.Map (DMap, DSum(..))
import Data.Functor.Identity (Identity)
import Data.GADT.Compare (GCompare)

copyKeyDSum :: (GCompare old) => old a -> new a -> DMap old Identity -> Maybe (DSum new Identity)
copyKeyDSum oldK newK oldDMap = optionalKey newK (DMap.lookup oldK oldDMap)

optionalKey :: k a -> Maybe (Identity a) -> Maybe (DSum k Identity)
optionalKey k = fmap (k :=>)
