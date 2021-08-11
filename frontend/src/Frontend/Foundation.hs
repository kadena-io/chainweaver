{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Definitions common to the whole frontend.
--
--   And commonly used imports.

module Frontend.Foundation
  ( -- * Useful types
    ReflexValue
  , MDynamic
  , LeftmostEv (..)
    -- * Helpers that should really not be here
  , getBrowserProperty
  , showWithDecimal
  , appendDecimalToText
    -- * Common Foundation
  , module Common
    -- * Re-exports
  , module Reflex.Extended
  , module Reflex.Network.Extended
  , module Language.Javascript.JSaddle
  , module Obelisk.Configs
  , module Reflex.Dom.Contrib.CssClass
  , forkJSM
  ) where

import           Control.Lens
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Coerce                       (coerce)
import           Data.Decimal
import           Data.Foldable
import           Data.Semigroup
import           Data.Scientific
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           GHC.Generics                      (Generic)
import           Language.Javascript.JSaddle       (JSM, MonadJSM, askJSM,
                                                    liftJSM, runJSM)
import qualified Language.Javascript.JSaddle       as JS
import           Language.Javascript.JSaddle.Monad (JSContextRef)
import           Obelisk.Configs
import           Reflex.Dom.Class                  (HasJSContext (..),
                                                    JSContextSingleton (..))
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Dom.WebSocket              (forkJSM)
import           Reflex.Extended
import           Reflex.Network.Extended

import           Text.Read

import           Data.Maybe

import           Common.Foundation                 as Common

-- | Shorthand for Dynamic t (Maybe a).
--
--   Usually used for things that might not be loaded yet.
type MDynamic t a = Dynamic t (Maybe a)

-- | Wrapper around Event with a Monoid instance based on `leftmost`.
newtype LeftmostEv t a = LeftmostEv
  { unLeftmostEv :: Event t a
  }

instance Reflex t => Semigroup (LeftmostEv t a) where
  (LeftmostEv a) <> (LeftmostEv b) = LeftmostEv $ leftmost [a, b]

instance Reflex t => Monoid (LeftmostEv t a) where
  mempty = LeftmostEv never
  mappend = (<>)
  mconcat = LeftmostEv . leftmost . coerce

getBrowserProperty :: forall m. MonadJSM m => Text -> m Bool
getBrowserProperty property = liftJSM $ fromMaybe False <$> JS.catch (JS.fromJSVal =<< JS.eval ("bowser." <> property)) (\(_ :: JS.JSException) -> pure Nothing)

-- TODO: upstream this?
instance HasJSContext JSM where
  type JSContextPhantom JSM = JSContextRef
  askJSContext = JSContextSingleton <$> askJSM


-- | Re-use data constructors more flexibly.
type family ReflexValue (f :: * -> *) x where
    ReflexValue (Dynamic t) x = Dynamic t x

    ReflexValue Identity x = x

    ReflexValue (Behavior t) x = Behavior t x

    ReflexValue (Event t) x = Event t x

appendDecimalToText :: Text -> Text
appendDecimalToText t =
    case readMaybe $ T.unpack t of
      Nothing -> t
      Just d -> showWithDecimal d

showWithDecimal :: Decimal -> Text
showWithDecimal d =
    if decimalPlaces d == 0
      then t <> ".0"
      else t
  where
    t = tshow d
