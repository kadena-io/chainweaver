{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : Reflex.Network.Extended
Description : Some convenience functions missing from Reflex.
Copyright   : (c) Robert Klotzner, 2017
-}
module Reflex.Network.Extended ( -- * Re-exported modules
                               module Reflex.Network
                               -- * Functions
                             , Flattenable(..)
                             , SwitchHold
                             , flatten
                             , flattenDynamic
                             , flattenDynamicDef
                             , networkViewFlatten
                             ) where


import           Control.Monad          ((<=<))
import           Data.Default
import           Reflex.Class
import           Reflex.Network
import           Reflex.NotReady.Class
import           Reflex.PostBuild.Class
import           Reflex.Adjustable.Class


-- | Can be either 'switchHold never' or 'switchHoldPromptly never'
type SwitchHold t = forall a m. (Reflex t, MonadHold t m) => Event t a -> Event t (Event t a) -> m (Event t a)

class Flattenable a t where
  -- | The first parameter is either switchHold or switchHoldPromptly.
  --
  -- So we get both variants for free, if implementors use this parameter for
  -- implementing flattenWith.
  flattenWith :: forall m. (Reflex t, MonadHold t m)
                  => SwitchHold t -> Event t a -> m a

-- | Extract a type from an event, with the given initial value.
flatten :: forall a t m. (Flattenable a t, Reflex t, MonadHold t m)
              => Event t a -> m a
flatten = flattenWith switchHold

-- | Flatten a Dynamic of `Default` a.
flattenDynamicDef :: forall a t m. (Reflex t, MonadHold t m, Default a)
               => SwitchHold t -> Event t (Dynamic t a) -> m (Dynamic t a)
flattenDynamicDef doSwitch ev = do
  let
    initVal = pushAlways (sample . current) ev
  updateVal <- doSwitch initVal (updated <$> ev)
  holdDyn def updateVal

-- | Flatten a Dynamic given a default value that it should have before the
-- event occurs.
flattenDynamic :: forall a t m. (Reflex t, MonadHold t m)
               => a -> SwitchHold t -> Event t (Dynamic t a) -> m (Dynamic t a)
flattenDynamic defVal doSwitch ev = do
  let
    initVal = pushAlways (sample . current) ev
  updateVal <- doSwitch initVal (updated <$> ev)
  holdDyn defVal updateVal

-- | networkView combined with flattenDef
networkViewFlatten
  :: ( NotReady t m, Adjustable t m, PostBuild t m
     , Flattenable a t, MonadHold t m
     )
  => Dynamic t (m a)
  -> m a
networkViewFlatten = flatten <=< networkView
