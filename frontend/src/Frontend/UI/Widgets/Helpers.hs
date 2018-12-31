{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TupleSections         #-}

-- | Little helpers useful for implementing widgets.
module Frontend.UI.Widgets.Helpers
  ( imgWithAlt
  , imgWithAltCls
  , tabPane
  , tabPane'
  , makeClickable
  , setFocus
  , setFocusOn
  , setFocusOnSelected
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Map.Strict             (Map)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Language.Javascript.JSaddle (PToJSVal, call, eval, js0,
                                              liftJSM, obj, pToJSVal)
import           Obelisk.Generated.Static
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Dom.Core
import Data.Set (Set)
import qualified Data.Map as Map
------------------------------------------------------------------------------
import           Frontend.Foundation
------------------------------------------------------------------------------


imgWithAlt :: DomBuilder t m => Text -> Text -> m a -> m a
imgWithAlt = imgWithAltCls mempty

imgWithAltCls :: DomBuilder t m => CssClass -> Text -> Text -> m a -> m a
imgWithAltCls cls loc alt child =
  elAttr "img" ("src" =: loc <> "alt" =: alt <> "class" =: renderClass cls) child

makeClickable :: DomBuilder t m => m (Element EventResult (DomBuilderSpace m) t, ()) -> m (Event t ())
makeClickable item = do
  (e, _) <- item
  return $ domEvent Click e

-- Shamelessly stolen (and adjusted) from reflex-dom-contrib:
tabPane'
    :: (Eq tab, DomBuilder t m, PostBuild t m)
    => Map Text Text
    -> Dynamic t tab
    -> tab
    -> m a
    -> m (Element EventResult (DomBuilderSpace m) t, a)
tabPane' staticAttrs currentTab t child = do
    let
      mkAttrs ct =
        if ct == t
           then addToClassAttr ("tab-set__content" <> "tab-set__content_active") staticAttrs
           else addToClassAttr "tab-set__content" staticAttrs
    elDynAttr' "div" (mkAttrs <$> currentTab) child

tabPane
    :: (Eq tab, DomBuilder t m, PostBuild t m)
    => Map Text Text
    -> Dynamic t tab
    -> tab
    -> m a
    -> m a
tabPane staticAttrs currentTab t = fmap snd . tabPane' staticAttrs currentTab t

------------------------------------------------------------------------------


setFocus :: (MonadJSM m, PToJSVal a) => a -> m ()
setFocus e =  void . liftJSM $ pToJSVal e ^. js0 ("focus" :: Text)

-- | Set focus on a given child element in case the given Event occurs.
setFocusOn
  :: MonadWidget t m
  => Element EventResult (DomBuilderSpace m) t -- ^ The root element.
  -> Text -- ^ A css selector to select an ancestor.
  -> Event t a -- ^ The triggering event.
  -> m ()
setFocusOn e cssSel onEv = do
  myEl <- liftJSM $ do
    getEl <- eval $ "(function(e) { return e.querySelector(\"" <> cssSel <> "\");})"
    call getEl obj [_element_raw e]
  onSetFocus <- delay 0.1 $ onEv
  performEvent_ $ setFocus myEl <$ onSetFocus

-- | Set focus on a given child element in case a matching event occurs.
--
--   Same as `setFocusOn`, but filters the given `Event` by comparing its value
--   to the given filter.
setFocusOnSelected
  :: (MonadWidget t m, Eq a)
  => Element EventResult (DomBuilderSpace m) t -- ^ The root element.
  -> Text -- ^ A css selector to select an ancestor.
  -> a -- ^ Filter the event for matching this value.
  -> Event t a -- ^ The triggering event.
  -> m ()
setFocusOnSelected e cssSel p onPred = setFocusOn e cssSel $ ffilter (== p) onPred
