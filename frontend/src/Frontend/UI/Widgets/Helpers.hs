{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}

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
  , preventScrollWheelAndUpDownArrow
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Data.Proxy                  (Proxy (..))
import           Data.Map.Strict             (Map)
import           Data.Text                   (Text)
import           Language.Javascript.JSaddle (PToJSVal, call, eval, js0, obj,
                                              pToJSVal)
import qualified Web.KeyCode                 as Keys
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Dom.Core
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

preventScrollWheelAndUpDownArrow
  :: forall m
     . DomSpace (DomBuilderSpace m)
  => EventSpec (DomBuilderSpace m) EventResult
  -> EventSpec (DomBuilderSpace m) EventResult
preventScrollWheelAndUpDownArrow = 
  preventMouseWheel . preventUpDownArrow
  where
    preventMouseWheel = 
      addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Mousewheel (const preventDefault)

    preventUpDownArrow = 
      addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Keydown
      (maybe mempty
        (\c ->
           let
             kc = fromIntegral (unEventResult c)
           in
            if Keys.isKeyCode Keys.ArrowUp kc || Keys.isKeyCode Keys.ArrowDown kc
            then preventDefault
            else mempty
        ))
