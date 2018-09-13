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
  ) where

import Reflex

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
  pure $ leftmost [ tag (current v) onPostBuild
                  , updated v
                  ]
