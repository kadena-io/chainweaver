{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Reflex.Class.Extended
Description : Some convenience functions missing from Reflex.
Copyright   : (c) Robert Klotzner, 2017
-}
module Reflex.Class.Extended ( -- * Re-exported modules
                               module Reflex
                               -- * Functions
                             , leftmostList
                             , mergeAsList
                             ) where


import           Control.Monad          ((<=<))
import           Data.Default
import           Reflex.Class           as Reflex
import           Reflex.NotReady.Class
import           Reflex.PostBuild.Class






-- | Uses the leftmost event in case of coincidence, but wraps it in a list for
--   use in an API that expects a list of events.
leftmostList :: Reflex t => [Event t a] -> Event t [a]
leftmostList = fmap (:[]) . leftmost


-- | Merge a list of events into a single list event.
--
mergeAsList :: (Functor f, Monoid (f [a]))
            => [f a] -> f [a]
mergeAsList = mconcat . map (fmap (:[]))
