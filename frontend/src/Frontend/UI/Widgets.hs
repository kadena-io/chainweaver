{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}

-- | Widgets collection
-- Was based on semui, but now transitioning to custom widgets
module Frontend.UI.Widgets
  ( imgWithAlt
  , showLoading
  , paginationWidget
  , validatedInputWithButton
  , tabPane
  , tabPane'
  , makeClickable
  , accordionItem
  , accordionItem'
  , setFocus
  , setFocusOn
  , setFocusOnSelected
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Language.Javascript.JSaddle (js0, liftJSM, pToJSVal, PToJSVal, eval, call, obj)
import           Reflex.Dom.Core
import           Reflex.Dom.Contrib.CssClass
import           Data.Map.Strict             (Map)
import           Obelisk.Generated.Static
------------------------------------------------------------------------------
import           Frontend.Foundation
import           Frontend.UI.Button
------------------------------------------------------------------------------


-- | Validated input with button
validatedInputWithButton
  :: MonadWidget t m
  => (Text -> PushM t (Maybe Text))
  -- ^ Validation function returning `Just error message` on error.
  -> Text -- ^ Placeholder
  -> Text -- ^ Button text
  -> m (Event t Text)
validatedInputWithButton check placeholder buttonText = do
    (update, checked) :: (Event t Text, Dynamic t (Maybe Text)) <- elClass "div" "fieldset" $ mdo
      name <- inputElement $ def
          & inputElementConfig_setValue .~ (T.empty <$ confirmed)
          & initialAttributes .~ ("placeholder" =: placeholder <> "type" =: "text")
      let
        nameVal = T.strip <$> _inputElement_value name
        onEnter = keypress Enter name
        nameEmpty = (== "") <$> nameVal

      checkedL <- holdDyn Nothing $ pushAlways check $ updated nameVal
      let
        checkFailed = isJust <$> checkedL
        btnCfg = def & uiButtonCfg_disabled .~ liftA2 (||) nameEmpty checkFailed


      (clicked, _) <- uiButton btnCfg $ text buttonText

      let
        filterValid = fmap (const ()) . ffilter not . tag (current checkFailed)
        confirmed = filterValid $ leftmost [ onEnter, clicked ]
      void $ performEvent (liftJSM (pToJSVal (_inputElement_raw name) ^.  js0 ("focus" :: String)) <$ confirmed)
      pure $ (tag (current nameVal) confirmed, checkedL)

    elClass "span" "error" $ dynText $ fromMaybe "" <$> checked

    pure update

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
    isActive <- foldDyn (const not) initActive onClick
    let mkClass a = singleClass "control-block" <> contentClass <> activeClass a
    (onClick, pair) <- elDynKlass "div" (mkClass <$> isActive) $ do
      (onClick,a1) <- el "h2" $ do
        (b, _) <- el' "button" $ imgWithAlt (static @"img/arrow-down.svg") "Expand" blank
        r <- title
        pure (domEvent Click b, r)
      b1 <- inner
      return (onClick, (a1, b1))
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
    let pageButton okay i = filteredButton okay $ elClass "i" ("fa " <> i) blank
    let canGoFirst = (> 1) <$> currentPage
    first <- pageButton canGoFirst "fa-angle-double-left"
    prev <-  pageButton canGoFirst "fa-angle-left"
    void $ elClass "div" "page-count" $ elClass "span" "page-count-text" $ do
      display currentPage
      text " of "
      display totalPages
    let canGoLast = (<) <$> currentPage <*> totalPages
    nextL <- pageButton canGoLast "fa-angle-right"
    lastL <- pageButton canGoLast "fa-angle-double-right"
    pure $ leftmost
      [ attachWith (\x _ -> pred x) (current currentPage) prev
      , 1 <$ first
      , attachWith (\x _ -> succ x) (current currentPage) nextL
      , tag (current totalPages) lastL
      ]

filteredButton
  :: MonadWidget t m
  => Dynamic t Bool
  -> m ()
  -> m (Event t ())
filteredButton okay = fmap fst . uiButton (def & uiButtonCfg_disabled .~ fmap not okay)


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
