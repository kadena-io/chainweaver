{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
-- So jsaddle stuff works a little nicer:
{-# LANGUAGE NoOverloadedStrings #-}
module Reflex.Dom.ACE.Extended 
  ( 
    -- module ACE -- This does break deployments, because for ACE no haddocks can be generated.
   resizableAceWidget
  ) where

import           Language.Javascript.JSaddle (js0, MonadJSM, liftJSM, jsg)
import qualified Language.Javascript.JSaddle as JS
import Data.Text (Text)
import Control.Lens
import Reflex
import Data.Foldable
import Reflex.Dom.Core
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad

import Reflex.Dom.ACE as ACE


-- | ACE editor widget that handles resizes of the containing DOM element properly.
--
-- Requires  https://github.com/marcj/css-element-queries
resizableAceWidget
    :: forall t m. MonadWidget t m
    => AceConfig -> AceDynConfig -> Element EventResult (DomBuilderSpace m) t -> Text -> m (ACE t)
resizableAceWidget ac adc parent initContents = do
  (onResize, triggerResize) <- newTriggerEvent
  editor <- aceWidgetStatic ac adc initContents
  pb <- delay 0 =<< getPostBuild -- Be a bit more lazy when accessing the parent.
  performEvent_ $ getResizeEvent triggerResize parent <$ pb

  resizeEditor onResize editor
  pure editor
  where
    getResizeEvent :: (() -> IO ()) -> Element EventResult (DomBuilderSpace m) t -> Performable m ()
    getResizeEvent triggerResize p = do
      let rawP = JS.toJSVal $ _element_raw p
      jsTriggerResize <- liftJSM $ JS.function $ \ _ _ [] -> liftIO (triggerResize ())

      void . liftJSM $ JS.new (jsg "ResizeSensor") (JS.toJSVal rawP, jsTriggerResize)


-- | Call resize() on the editor when the given event occurs.
resizeEditor 
  :: (Reflex t, PerformEvent t m, MonadJSM (Performable m)) 
  => Event t () -> ACE t 
  -> m ()
resizeEditor onResize ace =
  let
    curAce = current $ aceRef ace
    doResize (AceInstance ai) = liftJSM $ ai ^. js0 "resize"
  in
    performEvent_ $ traverse_ doResize <$> tag curAce onResize

