{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
-- | Semui based widgets collection
module Frontend.Widgets where

import           Control.Applicative
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Monoid
import           Data.Text           (Text)
import           Reflex.Dom.Core

{- data AccordionItemConf t = -}
{-   AccordionItemConf -}
{-     { _accordionItemConf_title :: Text -}
{-     {- , _accordionItemConf_attrs :: Dynamic t (Map Text Text) -} -}
{-     , _accordionItemConf_initActive :: Bool -}
{-     } -}

{- instance Default (AccordionItemConf t) where -}
{-   def = AccordionItemConf "" True -}


{- accordionItem :: MonadWidget t m => AccordionItemConf t -> m a -> m a -}
accordionItem' :: MonadWidget t m
              => Bool -> Text -> Text -> m a
              -> m (Element EventResult (DomBuilderSpace m) t, a)
accordionItem' initActive contentClass title inner = mdo
  isActive <- foldDyn (const not) initActive $ domEvent Click e
  (e, _) <- elDynClass' "div" ("title " <> fmap activeClass isActive) $ do
    elClass "i" "dropdown icon" blank
    text title
  elDynClass' "div" ("content " <> pure contentClass <> fmap activeClass isActive) inner
  where
    activeClass = \case
      False -> ""
      True -> " active"

accordionItem :: MonadWidget t m => Bool -> Text -> Text -> m a -> m a
accordionItem initActive contentClass title inner =
  snd <$> accordionItem' initActive contentClass title inner

makeClickable :: DomBuilder t m => m (Element EventResult (DomBuilderSpace m) t, ()) -> m (Event t ())
makeClickable item = do
  (e, _) <- item
  return $ domEvent Click e

-- Shamelessly stolen (and adjusted) from reflex-dom-contrib:

tabPane'
    :: (MonadWidget t m, Eq tab)
    => Map Text Text
    -> Dynamic t tab
    -> tab
    -> m a
    -> m (Element EventResult (DomBuilderSpace m) t, a)
tabPane' staticAttrs currentTab t child = do
    let attrs = addDisplayNone (constDyn staticAttrs) ((==t) <$> currentTab)
    elDynAttr' "div" attrs child

tabPane
    :: (MonadWidget t m, Eq tab)
    => Map Text Text
    -> Dynamic t tab
    -> tab
    -> m a
    -> m a
tabPane staticAttrs currentTab t = fmap snd . tabPane' staticAttrs currentTab t

------------------------------------------------------------------------------
-- | Helper function for hiding your tabs with display none.
addDisplayNone
    :: Reflex t
    => Dynamic t (Map Text Text)
    -> Dynamic t Bool
    -> Dynamic t (Map Text Text)
addDisplayNone attrs isActive = zipDynWith f isActive attrs
  where
    f True as  = as
    f False as = Map.insert "style" "display: none" as

