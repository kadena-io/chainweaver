{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Reflex.Dom.ACE.Extended ( module ACE
                               , AceAnnotation (..)
                               , ExtendedACE (..)
                               , resizableAceWidget
                               ) where

import           Control.Lens
import           Control.Monad
import           Data.Foldable
import           Data.Map                          (Map)
import           Data.Text                         (Text)
import           Language.Javascript.JSaddle       (MonadJSM, js0, js1, liftJSM, ToJSVal(..), obj, (<#))
import           Reflex
import           Reflex.Dom.ACE                    as ACE
import           Reflex.Dom.Core
import           Reflex.Dom.Widget.Resize.Extended (resizeDetectorWithAttrsAbsolute)


data ExtendedACE t = ExtendedACE
  { _extendedACE_baseACE      :: ACE t
  , _extendedACE_onUserChange :: Event t Text
    -- ^ Fires only if the change was not coming from reflex input.
  }

-- | Send an annotation to the ace editor, e.g. for displaying an error/warning.
data AceAnnotation = AceAnnotation
  { _aceAnnotation_row    :: Int
  , _aceAnnotation_column :: Int
  , _aceAnnotation_text   :: Text -- ^ Message that is shown to the user.
  , _aceAnnotation_type   :: Text -- ^ E.g. "error"
  }

instance ToJSVal AceAnnotation where
  toJSVal a = do
    o <- obj
    o <# ("row" :: Text) $ _aceAnnotation_row a
    o <# ("column" :: Text) $ _aceAnnotation_column a
    o <# ("text" :: Text) $ _aceAnnotation_text a
    o <# ("type" :: Text) $ _aceAnnotation_type a
    toJSVal o


-- | ACE editor widget that handles resizes of the containing DOM element properly.
--
--   For resize support the ace editor will be packed in an additonal div, the
--   `attrs` parameter are attrs for that div. The passed attrs should not
--   modify the CSS position attribute.
resizableAceWidget
    :: forall t m. MonadWidget t m
    => Event t ()
    -> Map Text Text
    -> AceConfig
    -> AceDynConfig
    -> Event t [AceAnnotation]
    -> Text
    -> Event t Text
    -> m (ExtendedACE t)
resizableAceWidget externalResize attrs ac adc onAnnotations initContents onNewContent = do
  let fullAttrs = attrs <> "class" =: "ace_container"
  (onResize, editor) <- resizeDetectorWithAttrsAbsolute fullAttrs $ extendedAceWidget ac adc onAnnotations initContents onNewContent
  resizeEditor (externalResize <> onResize) (_extendedACE_baseACE editor)
  pure editor

extendedAceWidget
  :: forall t m. MonadWidget t m
  => AceConfig
  -> AceDynConfig
  -> Event t [AceAnnotation]
  -> Text
  -> Event t Text
  -> m (ExtendedACE t)
extendedAceWidget ac adc onAnnotations initContents onNewContent = do
    newContent <- holdDyn Nothing $ Just <$> onNewContent
    ace <- aceWidgetStatic ac adc initContents

    -- The delay is a hack, without it the warning disappears immediately
    -- after being shown. Proper fix would be to track ace annotation changes
    -- and make sure the ones set by us are present.
    delayedAnnotations <- delay 1 onAnnotations
    handleAnnotations ace delayedAnnotations
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
  where
    handleAnnotations ace annotations =
      let
        curAce = current $ aceRef ace
        -- TODO: That functions seems to be present in ACE already!
        setAnnotationsL (mAi, annot) =
          case mAi of
            Nothing -> pure ()
            Just (AceInstance ai) -> liftJSM $ do
              session <- ai ^. js0 ("getSession" :: Text)
              void $ session ^. js1 ("setAnnotations" :: Text) annot
      in
        performEvent_ $ setAnnotationsL <$> attach curAce annotations





-- | Call resize() on the editor when the given event occurs.
resizeEditor
  :: (PerformEvent t m, MonadJSM (Performable m))
  => Event t () -> ACE t
  -> m ()
resizeEditor onResize ace =
  let
    curAce = current $ aceRef ace
    doResize (AceInstance ai) = liftJSM $ ai ^. js0 ("resize" :: Text)
  in
    performEvent_ $ traverse_ doResize <$> tag curAce onResize
