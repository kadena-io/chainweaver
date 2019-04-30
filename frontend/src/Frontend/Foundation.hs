{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE ExtendedDefaultRules   #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Definitions common to the whole frontend.
--
--   And commonly used imports.

module Frontend.Foundation
  ( -- * Useful types
    ReflexValue
  , MDynamic
  , LeftmostEv (..)
    -- * Lenses and Prisms
  , makePactLenses
  , makePactLensesNonClassy
  , makePactPrisms
    -- * Helpers that should really not be here
  , forkJSM
    -- * Common Foundation
  , module Common
    -- * Re-exports
  , module Reflex.Extended
  , module Reflex.Network.Extended
  , module Language.Javascript.JSaddle
  , module Reflex.Dom.Contrib.CssClass
  ) where

import           Control.Concurrent                    (ThreadId, forkIO)
import           Control.Lens
import           Control.Monad.Except                  (MonadError, throwError)
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Aeson                            as A
import           Data.ByteString                       (ByteString)
import           Data.Coerce                           (coerce)
import           Data.Foldable
import qualified Data.List.Split                       as L
import           Data.Semigroup
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as T
import qualified Data.Text.Encoding.Error              as T
import           GHC.Generics                          (Generic)
import           Language.Haskell.TH                   (DecsQ)
import           Language.Haskell.TH.Syntax            (Name)
import           Language.Javascript.JSaddle           (JSM, MonadJSM, askJSM,
                                                        liftJSM, runJSM)
import           Language.Javascript.JSaddle.Monad     (JSContextRef)
import           Reflex.Dom.Class                      (HasJSContext (..),
                                                        JSContextSingleton (..))
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Extended
import           Reflex.Network.Extended

import           Data.Maybe

import qualified Data.Text.Prettyprint.Doc             as Pretty (defaultLayoutOptions,
                                                                  layoutCompact,
                                                                  layoutPretty)
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Pact.Types.Pretty                     as Pretty

import Common.Foundation as Common

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

-- | Lenses in this project should be generated by means of this function.
--
--   We generate lazy classy lenses. Classes make the export lists less tedious
--   and allows for generic code, which will come in handy when the project
--   grows.
--
--   We want lazy lenses so we can uses lenses also in recursive definitions.

makePactLenses :: Name -> DecsQ
makePactLenses =
  makeLensesWith
    ( classyRules
        & generateLazyPatterns .~ True
        & createClass .~ True
    )

-- | Non classy non simple lenses.
--
--   For some reason ( I have not investigated yet ), classy non simple lenses
--   don't seem to work properly, at least not if the type has parameters.
--   Therefore if you have a type where you need non simple lenses (lenses that
--   can change the type), you have to use this function instead of
--   `makePactLenses`.
makePactLensesNonClassy :: Name -> DecsQ
makePactLensesNonClassy =
  makeLensesWith
    ( lensRules
        & simpleLenses .~ False
        & generateLazyPatterns .~ True
    )

-- | Make Prisms in "pact style".
--
--   Currently this is just standard `makePrisms`
makePactPrisms :: Name -> DecsQ
makePactPrisms = makePrisms


-- | Aeson encoding options for compact encoding.
--
--   We pass on the most compact sumEncoding as it could be unsound for certain types.
--
--   But we assume the following naming of constructor names (sum typs) and
--   field names (records): _TypeName_Blah and _typename_blah.
--
--   In particular we assume that only the string after the last underscore is
--   significant for distinguishing field names/constructor names. If this
--   assumption is not met this encoding might not result in the same decoding.
compactEncoding :: A.Options
compactEncoding = defaultOptions
    { fieldLabelModifier = shortener
    , allNullaryToStringTag = True
    , constructorTagModifier = shortener
    , omitNothingFields = True
    , sumEncoding = ObjectWithSingleField
    , unwrapUnaryRecords = True
    , tagSingleConstructors = False
    }
  where
    -- As long as names are not empty or just underscores this head should be fine:
    shortener = head . reverse . filter (/= "") . L.splitOn "_"

tshow :: Show a => a -> Text
tshow = T.pack . show

prettyTextCompact :: Pretty.Pretty a => a -> Text
prettyTextCompact = Pretty.renderStrict . Pretty.layoutCompact . Pretty.pretty

prettyTextPretty :: Pretty.Pretty a => a -> Text
prettyTextPretty = Pretty.renderStrict . Pretty.layoutPretty Pretty.defaultLayoutOptions . Pretty.pretty

note :: MonadError e m => e -> Maybe a -> m a
note e = maybe (throwError e) pure

safeDecodeUtf8 :: ByteString -> Text
safeDecodeUtf8 = T.decodeUtf8With T.lenientDecode

forkJSM :: (MonadJSM m) => JSM () -> m ThreadId
forkJSM t = do
  jsm <- askJSM
  let ioT = runJSM t jsm
  liftIO $ forkIO ioT

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
