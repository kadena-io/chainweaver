{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
-- | Semui based widgets collection
module Widgets where

import Reflex.Dom.Core
import Data.Text (Text)
import Data.Monoid
import Data.Map.Strict (Map)

{- data AccordionItemConf t = -}
{-   AccordionItemConf -}
{-     { _accordionItemConf_title :: Text -}
{-     {- , _accordionItemConf_attrs :: Dynamic t (Map Text Text) -} -}
{-     , _accordionItemConf_initActive :: Bool -}
{-     } -}

{- instance Default (AccordionItemConf t) where -}
{-   def = AccordionItemConf "" True -}


{- accordionItem :: MonadWidget t m => AccordionItemConf t -> m a -> m a -}
accordionItem :: MonadWidget t m => Bool -> Text -> Text -> m a -> m a
accordionItem initActive contentClass title inner = mdo
  isActive <- foldDyn (const not) initActive $ domEvent Click e
  (e, _) <- elDynClass' "div" ("title " <> fmap activeClass isActive) $ do
    elClass "i" "dropdown icon" blank
    text title
  elDynClass "div" ("content " <> pure contentClass <> fmap activeClass isActive) inner
  where
    activeClass = \case
      False -> ""
      True -> " active"

