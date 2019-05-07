{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Useful functions for Reflex that could probably be upstreamed at some point.
module Reflex.Extended
  ( module Reflex
    -- * Useful functions for MVC based code:
  , tagOnPostBuild
  , waitForEvents
  , getListUpdates
  ) where

import Reflex
import Control.Applicative
import Control.Monad.Fix
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Control.Monad

-- | Safely initialize UI state from model state.
--
--   This function produces an event that fires when the given `Dynamic` fires
--   and on post build, where the `current` value of the `Dynamic` gets tagged.
--
--   By means of this function one can safely keep some UI in sync with the
--   application model.
tagOnPostBuild :: PostBuild t m => Dynamic t a -> m (Event t a)
tagOnPostBuild v = do
  onPostBuild <- getPostBuild
  pure $ leftmost [ tagPromptlyDyn v onPostBuild
                  , updated v
                  ]



-- | Once the first event occurred, wait until both of the other events occurred.
--
--   Then return a resulting event by using the given combining function.
waitForEvents :: (Reflex t, MonadHold t m )
              => (a -> b -> c) -> Event t ignored -> Event t a -> Event t b
              -> m (Event t c)
waitForEvents combine trigger evA evB = do
  a <- holdDyn Nothing $ leftmost [ Just <$> evA
                                  , Nothing <$ trigger
                                  ]
  b <- holdDyn Nothing $ leftmost [ Just <$> evB
                                  , Nothing <$ trigger
                                  ]
  let combined = zipDynWith (liftA2 combine) a b
  pure $ fmapMaybe id $ updated combined


getListUpdates
  :: forall t m a.
     (Reflex t, MonadHold t m, MonadFix m)
  => Dynamic t [a]
  -> m (IntMap (Dynamic t a), Event t (PatchIntMap (Dynamic t a)))
getListUpdates l = do
    let
      withIndex = IntMap.fromList . zip [0..] <$> l
      byIndex = fanInt $ updated withIndex
    lengthChanges :: Dynamic t (IntMap a) <- holdUniqDynBy (\xs ys -> IntMap.size xs == IntMap.size ys) withIndex

    let
      onNewMap = pushAlways (IntMap.traverseWithKey (holdIndexValue byIndex)) (updated lengthChanges)
    cInitMap <- IntMap.traverseWithKey (holdIndexValue byIndex) <=< sample $ current lengthChanges

    m <- holdDyn cInitMap onNewMap

    pure (cInitMap, PatchIntMap <$> getPatch (current m) (updated m))

  where
    holdIndexValue :: forall m1. MonadHold t m1 => EventSelectorInt t a -> Int -> a -> m1 (Dynamic t a)
    holdIndexValue (EventSelectorInt selectInt) i initial = holdDyn initial $ selectInt i

    -- Note: Due to the IntMap being built from a list, any changes will always
    -- be at the highest index. There will be no changes in the middle of the
    -- IntMap.  (Elements change their idendity, which is exactly what we want
    -- (Edit of new node, morphes seemlessly into edit of existing node).)
    getPatch currentMap =
      pushAlways
        ( \newNodes -> do
            oldNodes <- sample currentMap
            let
              insertions = Just <$> IntMap.difference newNodes oldNodes
              deletions =  Nothing <$ IntMap.difference oldNodes newNodes
              -- There are only insertions and deletions, as updates are
              -- handled implicitely by the Dynamics created by means of
              -- fanInt.
            pure $ insertions <> deletions
        )
