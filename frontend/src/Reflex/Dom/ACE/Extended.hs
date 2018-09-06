{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Reflex.Dom.ACE.Extended ( module ACE
                               , resizableAceWidget
                               ) where

import           Control.Lens
import           Data.Foldable
import           Data.Map                          (Map)
import           Data.Monoid
import           Data.Text                         (Text)
import           Language.Javascript.JSaddle       (MonadJSM, js0, liftJSM)
import           Reflex
import           Reflex.Dom.Core
import           Reflex.Dom.Widget.Resize.Extended (resizeDetectorWithAttrsAbsolute)

import           Reflex.Dom.ACE                    as ACE


-- | ACE editor widget that handles resizes of the containing DOM element properly.
--
--   For resize support the ace editor will be packed in an additonal div, the
--   `attrs` parameter are attrs for that div. The passed attrs should not
--   modify the CSS position attribute.
resizableAceWidget
    :: forall t m. MonadWidget t m
    => Map Text Text -> AceConfig -> AceDynConfig -> Text -> m (ACE t)
resizableAceWidget attrs ac adc initContents = do
  -- let fullAttrs = attrs <> "style" =: "top:0px;bottom:0px;left:0px;right:0px;"
  let fullAttrs = attrs <> "style" =: "top:0px;bottom:0px;left:0px;right:0px;"
  (onResize, editor) <- resizeDetectorWithAttrsAbsolute fullAttrs $ aceWidgetStatic ac adc initContents
  resizeEditor onResize editor
  pure editor

-- | Call resize() on the editor when the given event occurs.
resizeEditor
  :: (Reflex t, PerformEvent t m, MonadJSM (Performable m))
  => Event t () -> ACE t
  -> m ()
resizeEditor onResize ace =
  let
    curAce = current $ aceRef ace
    doResize (AceInstance ai) = liftJSM $ ai ^. js0 ("resize" :: Text)
  in
    performEvent_ $ traverse_ doResize <$> tag curAce onResize

