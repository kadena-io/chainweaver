{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Reflex.Dom.ACE.Extended ( module ACE
                               , ExtendedACE (..)
                               , resizableAceWidget
                               ) where

import           Control.Lens
import           Control.Monad
import           Data.Foldable
import           Data.Map                          (Map)
import           Data.Monoid
import           Data.Text                         (Text)
import           Language.Javascript.JSaddle       (MonadJSM, js0, liftJSM)
import           Reflex
import           Reflex.Dom.Core
import           Reflex.Dom.Widget.Resize.Extended (resizeDetectorWithAttrsAbsolute)
import           Reflex.Dom.ACE                    as ACE


data ExtendedACE t = ExtendedACE
  { _extendedACE_baseACE      :: ACE t
  , _extendedACE_onUserChange :: Event t Text
    -- ^ Fires only if the change was not coming from reflex input.
  }

-- | ACE editor widget that handles resizes of the containing DOM element properly.
--
--   For resize support the ace editor will be packed in an additonal div, the
--   `attrs` parameter are attrs for that div. The passed attrs should not
--   modify the CSS position attribute.
resizableAceWidget
    :: forall t m. MonadWidget t m
    => Map Text Text 
    -> AceConfig 
    -> AceDynConfig 
    -> Text 
    -> Event t Text 
    -> m (ExtendedACE t)
resizableAceWidget attrs ac adc initContents onNewContent = do
  -- let fullAttrs = attrs <> "style" =: "top:0px;bottom:0px;left:0px;right:0px;"
  let fullAttrs = attrs <> "style" =: "top:0px;bottom:0px;left:0px;right:0px;"
  {- (onResize, editor) <- resizeDetectorWithAttrsAbsolute fullAttrs $ aceWidgetStatic ac adc initContents -}
  editor <- 
    elAttr "div" fullAttrs $ extendedAceWidget ac adc initContents onNewContent
  let onResize = never
  resizeEditor onResize (_extendedACE_baseACE editor)
  pure editor

extendedAceWidget
  :: forall t m. MonadWidget t m
  => AceConfig
  -> AceDynConfig
  -> Text
  -> Event t Text
  -> m (ExtendedACE t)
extendedAceWidget ac adc initContents onSetContent = do
    ace <- aceWidgetStatic ac adc initContents
    void $ withAceInstance ace (setValueACE <$> onSetContent)
    let
      aceVal = aceValue ace
      onNewAceVal = traceEvent "Got new Value from editor" $ updated aceVal

    setPlainContent <- hold Nothing $ Just <$> onSetContent
    let
      onUserEdit
        = ffilter id
        $ attachWith (\c u -> c /= Just u) setPlainContent onNewAceVal

    isUserUpdate <- holdDyn True $ leftmost [ False <$ onSetContent
                                            , True <$ traceEvent "User edited" onUserEdit
                                            ]
    let 
      onUserUpdate = fmapMaybe id . updated $ do
        c <- aceVal
        fromUser <- isUserUpdate
        if fromUser
           then pure $ Just c
           else pure $ Nothing
    pure $ ExtendedACE ace onUserUpdate




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

