{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
-- | Semui based widgets collection
module Frontend.Widgets where

import           Control.Lens
import           Control.Monad
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (isJust)
import           Data.Monoid
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Language.Javascript.JSaddle (js0, liftJSM, pToJSVal)
import           Reflex.Dom.Core             (keypress, _textInput_element)
import           Reflex.Dom.SemanticUI       hiding (mainWidget)

accordionItem'
  :: MonadWidget t m
  => Bool
  -> Text
  -> Text
  -> m a
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

-- | An HTML element that delivers an element on `Enter` press.
enterEl
  :: MonadWidget t m
  => Text -> Map Text Text -> m a -> m (Event t (), a)
enterEl name mAttrs child = do
  (e, r) <- elAttr' name mAttrs child
  let enterPressed = keypress Enter e
  pure (enterPressed, r)



-- | Combine a `TextInput` and a `Button`
--
--   to a single widget with the following properties:
--
--   - Enter press confirms just as button click
--   - TextInput will be cleared on button click or on Enter press.
--   - TextInput will get focus on button click.
--
--   The resulting Event contains the current content of the InputWidget at
--   Enter press or button click.
confirmTextInput
  :: MonadWidget t m
  => m (TextInput t)
  -> m (Event t ())
  -> m (TextInput t, Event t Text)
confirmTextInput i b =
  elClass "div" "ui fluid action input" $ mdo
      ti <- i
      clicked <- b

      let
        onEnter = keypress Enter ti
        confirmed = leftmost [ onEnter, clicked ]
        setFocus =
          liftJSM $ pToJSVal (_textInput_element ti) ^. js0 ("focus" :: Text)
      void $ performEvent (setFocus <$ confirmed)

      let
        onReq = tag (current $ _textInput_value ti) confirmed
      pure (ti, onReq)



-- Shamelessly stolen (and adjusted) from reflex-dom-contrib:

tabPane'
    :: (MonadWidget t m, Eq tab)
    => Map Text Text
    -> Dynamic t tab
    -> tab
    -> m a
    -> m (Element EventResult (DomBuilderSpace m) t, a)
tabPane' staticAttrs currentTab t child = do
    let mAttrs = addDisplayNone (constDyn staticAttrs) ((==t) <$> currentTab)
    elDynAttr' "div" mAttrs child

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
addDisplayNone mAttrs isActive = zipDynWith f isActive mAttrs
  where
    f True as  = as
    f False as = Map.insert "style" "display: none" as

------------------------------------------------------------------------------
-- | Validated input with button
validatedInputWithButton
  :: MonadWidget t m
  => (Text -> Performable m (Either Text Text))
  -- ^ Validation function returning 'Left' an error message or 'Right' the value
  -> Text -- ^ Placeholder
  -> Text -- ^ Button text
  -> m (Event t Text)
validatedInputWithButton check placeholder buttonText = mdo
    update <- elClass "div" "ui fluid action input" $ mdo
      name <- textInput $ def
          & textInputConfig_value .~ SetValue "" (Just $ "" <$ values)
          & textInputConfig_placeholder .~ pure placeholder
      let
        nameVal = T.strip <$> value name
        onEnter = keypress Enter name
        nameEmpty = (== "") <$> nameVal

      clicked <- flip button (text buttonText) $ def
        & buttonConfig_emphasis .~ Static (Just Secondary)
        & buttonConfig_disabled .~ Dyn nameEmpty

      let confirmed = leftmost [ onEnter, clicked ]
      void $ performEvent (liftJSM (pToJSVal (_textInput_element name) ^. js0 ("focus" :: String)) <$ confirmed)
      pure $ tag (current nameVal) confirmed

    checked <- performEvent $ check <$> update
    let (errors, values) = fanEither checked
    hasError <- holdUniqDyn <=< holdDyn False $ leftmost
      [ ffor checked $ \case
          Left _ -> True
          Right _ -> False
      , False <$ close
      ]

    let trans dir = Transition Fade $ def
          & transitionConfig_duration .~ 0.2
          & transitionConfig_direction .~ Just dir
        config = def
          & messageConfig_type .~ Static (Just $ MessageType Negative)
          & action ?~ (def
            & action_event ?~ ffor (updated hasError) (\e -> trans $ if e then In else Out)
            & action_initialDirection .~ Out)
    close <- message config $ do
      e <- domEvent Click <$> icon' "close" (def & style .~ "position: absolute; top: 0; right: 0")
      widgetHold (pure ()) $ text <$> errors
      pure e
    pure values

