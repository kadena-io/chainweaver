{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}

-- | Widgets collection
-- Based on semui, currently being deprecated in favour of Frontend.UI.Widgets
-- which is building custom widgets.
module Frontend.Widgets
  ( imgWithAlt
  , showLoading
  , paginationWidget
  , tabPane
  , tabPane'
  , makeClickable
  , accordionItem
  , accordionItem'
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Control.Monad.Fix           (MonadFix)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Language.Javascript.JSaddle (js0, liftJSM, pToJSVal)
import           Obelisk.Generated.Static
import           Reflex.Dom.Core             (keypress, _textInput_element)
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Dom.SemanticUI       hiding (mainWidget)
import           Reflex.Network.Extended
-- import Reflex.Dom.Prerender (Prerender, prerender)
------------------------------------------------------------------------------
import           Frontend.Foundation
import           Frontend.UI.Button
import           Frontend.UI.Icon
------------------------------------------------------------------------------

imgWithAlt :: MonadWidget t m => Text -> Text -> m a -> m a
imgWithAlt loc alt child = elAttr "img" ("src" =: loc <> "alt" =: alt) child

showLoading
  :: (NotReady t m, Adjustable t m, PostBuild t m, DomBuilder t m, Monoid b)
  => Dynamic t (Maybe a)
  -> (a -> m b)
  -> m (Event t b)
showLoading i w = do
    networkView $ maybe loadingWidget w <$> i
  where
    loadingWidget = do
      text "Loading ..."
      pure mempty

accordionItem'
  :: MonadWidget t m
  => Bool
  -> CssClass
  -> m a
  -> m b
  -> m (a,b)
accordionItem' initActive contentClass title inner = mdo
    isActive <- foldDyn (const not) initActive $ domEvent Click e
    let mkClass a = singleClass "control-block" <> contentClass <> activeClass a
    (e, pair) <- elDynKlass "div" (mkClass <$> isActive) $ do
      (e1,a1) <- el' "h2" $ do
        el "button" $ imgWithAlt (static @"img/arrow-down.svg") "Expand" blank
        title
      b1 <- inner
      return (e1,(a1,b1))
    return pair
  where
    activeClass = \case
      False -> singleClass "collapsed"
      True -> mempty

accordionItem :: MonadWidget t m => Bool -> CssClass -> Text -> m a -> m a
accordionItem initActive contentClass title inner =
  snd <$> accordionItem' initActive contentClass (text title) inner

makeClickable :: DomBuilder t m => m (Element EventResult (DomBuilderSpace m) t, ()) -> m (Event t ())
makeClickable item = do
  (e, _) <- item
  return $ domEvent Click e

-- | An HTML element that delivers an element on `Enter` press.
enterEl
  :: ( DomBuilder t m
     , Reflex t
     )
  => Text -> Map Text Text -> m a -> m (Event t (), a)
enterEl name mAttrs child = do
  (e, r) <- elAttr' name mAttrs child
  let enterPressed = keypress Enter e
  pure (enterPressed, r)



-- Shamelessly stolen (and adjusted) from reflex-dom-contrib:

tabPane'
    :: (Eq tab, DomBuilder t m, PostBuild t m)
    => Map Text Text
    -> Dynamic t tab
    -> tab
    -> m a
    -> m (Element EventResult (DomBuilderSpace m) t, a)
tabPane' staticAttrs currentTab t child = do
    let mkAttrs ct = if ct == t
                       then addToClassAttr "active" staticAttrs
                       else staticAttrs
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

paginationWidget
  :: MonadWidget t m
  => Dynamic t Int  -- ^ Current page
  -> Dynamic t Int  -- ^ Total number of pages
  -> m (Event t Int)
paginationWidget currentPage totalPages = do
    let pageButton i = do
          (e,_) <- el' "button" $ uiIcon i def
          return $ domEvent Click e
    let canGoFirst = (> 1) <$> currentPage
    first <- filteredButton canGoFirst $ pageButton "fa-angle-double-left"
    prev <- filteredButton canGoFirst $ pageButton "fa-angle-left"
    void $ elClass "span" "page-count" $ do
      display currentPage
      text " of "
      display totalPages
    let canGoLast = (<) <$> currentPage <*> totalPages
    nextL <- filteredButton canGoLast $ pageButton "fa-angle-right"
    lastL <- filteredButton canGoLast $ pageButton "fa-angle-double-right"
    pure $ leftmost
      [ attachWith (\x _ -> pred x) (current currentPage) prev
      , 1 <$ first
      , attachWith (\x _ -> succ x) (current currentPage) nextL
      , tag (current totalPages) lastL
      ]

filteredButton
  :: MonadWidget t m
  => Dynamic t Bool
  -> m (Event t a)
  -> m (Event t a)
filteredButton okay content = do
  e <- content
  pure $ gate (current okay) e
