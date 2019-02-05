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
  ) where

import Reflex
import Control.Applicative

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


