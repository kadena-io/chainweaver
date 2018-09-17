{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
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
import           Reflex.Dom.ACE                    as ACE
import           Reflex.Dom.Core
import           Reflex.Dom.Widget.Resize.Extended (resizeDetectorWithAttrsAbsolute)


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
  let fullAttrs = attrs <> "style" =: "top:0px;bottom:0px;left:0px;right:0px;"
  (onResize, editor) <- resizeDetectorWithAttrsAbsolute fullAttrs $ extendedAceWidget ac adc initContents onNewContent
  resizeEditor onResize (_extendedACE_baseACE editor)
  pure editor

extendedAceWidget
  :: forall t m. MonadWidget t m
  => AceConfig
  -> AceDynConfig
  -> Text
  -> Event t Text
  -> m (ExtendedACE t)
extendedAceWidget ac adc initContents onNewContent = do
    newContent <- holdDyn Nothing $ Just <$> onNewContent
    ace <- aceWidgetStatic ac adc initContents
    let
      aceVal = aceValue ace
      onAceUpdate = updated aceVal

      -- For ignoring changes made by us (editor is already up2date):
      onExternalSet = fmapMaybe id . updated $ do
        nC <- newContent
        aC <- aceVal
        if Just aC == nC
           then pure Nothing
           else pure nC

    void $ withAceInstance ace (setValueACE <$> onExternalSet)
    cached <- hold initContents $ leftmost [ onExternalSet
                                           , onAceUpdate
                                           ]
    -- Only send out changes that come from the user.
    -- This relies on the fact that onAceUpdate is delayed, compared to
    -- onNewContent:
    let
      onUserChange =
        fmap snd . ffilter (uncurry (/=)) $ attach cached onAceUpdate

    pure $ ExtendedACE ace onUserChange




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

