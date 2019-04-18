{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}

-- | Widgets collection
-- Was based on semui, but now transitioning to custom widgets
module Frontend.UI.Widgets
  ( -- * Standard widgets for pact-web
    -- ** Buttons
    module Frontend.UI.Button
  -- ** Other widgets
  , uiSegment
  , uiGroup
  , uiCodeFont
  , uiInputElement
  , uiRealInputElement
  , uiIntInputElement
  , uiInputView
  , mkLabeledInput
  , uiCheckbox
  , uiDropdown
  , uiSelectElement
  , validatedInputWithButton
    -- ** Helper widgets
  , imgWithAlt
  , showLoading
  , paginationWidget
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
import           Data.Map.Strict             (Map)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Language.Javascript.JSaddle (js0, liftJSM, pToJSVal)
import           Obelisk.Generated.Static
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Dom.Core
import           Reflex.Extended             (tagOnPostBuild)
------------------------------------------------------------------------------
import           Frontend.Foundation
import           Frontend.UI.Button
import           Frontend.UI.Widgets.Helpers (imgWithAlt, makeClickable,
                                              setFocus, setFocusOn,
                                              setFocusOnSelected, tabPane,
                                              tabPane')
------------------------------------------------------------------------------

-- | A styled checkbox.
--
--   In contrast to `checkbox` this is not only the actual checkbox but also a
--   containing label, as this is the only way to have a styled checkbox.
--
--   Note that this is actually a secondary type checkbox, I have to
--   generalize this at some point.
uiCheckbox
  :: (DomBuilder t m, PostBuild t m)
  => CssClass
  -> Bool
  -> CheckboxConfig t
  -> m () -- ^ Some label to display alongside the checkbox. (Usually `text`.)
  -> m (Checkbox t)
uiCheckbox cls b cfg c =
  elKlass "label" (cls <> "label checkbox checkbox_type_secondary") $ do
    cb <- checkbox b $ cfg
    elClass "span" "checkbox__checkmark checkbox__checkmark_type_secondary" blank
    c
    pure cb

-- | A segment.
--
--   Segments are divs with separated by a dashed line (standard).
uiSegment :: DomBuilder t m => CssClass -> m a -> m a
uiSegment cls = elKlass "div" (cls <> "segment")

-- | A group.
--
--   A group is a div with a darker grey background.
uiGroup :: DomBuilder t m => CssClass -> m a -> m a
uiGroup cls = elKlass "div" (cls <> "group")

-- | Span rendered in code-font.
uiCodeFont :: DomBuilder t m => CssClass -> Text -> m ()
uiCodeFont cls = elKlass "span" ("code-font" <> cls) . text

uiDropdown
  :: forall k t m
  . ( DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m, Ord k
    )
  => k -> Dynamic t (Map k Text) -> DropdownConfig t k -> m (Dropdown t k)
uiDropdown k0 options (DropdownConfig setK uAttrs) = do
  let attrs = addToClassAttr "select" <$> uAttrs
  dropdown k0 options (DropdownConfig setK attrs)

uiSelectElement
  :: DomBuilder t m
  => SelectElementConfig er t (DomBuilderSpace m)
  -> m a
  -> m (SelectElement er (DomBuilderSpace m) t, a)
uiSelectElement uCfg child = do
  let cfg = uCfg & initialAttributes %~ addToClassAttr "select"
  selectElement cfg child

-- | Factored out input class modifier, so we can keep it in sync.
addInputElementCls :: Map AttributeName Text -> Map AttributeName Text
addInputElementCls = addToClassAttr "input"


-- | reflex-dom `inputElement` with pact-web default styling:
uiInputElement
  :: DomBuilder t m
  => InputElementConfig er t (DomBuilderSpace m)
  -> m (InputElement er (DomBuilderSpace m) t)
uiInputElement cfg = inputElement $ cfg & initialAttributes %~ addInputElementCls

-- | uiInputElement which should always provide a proper real number.
--
--   In particular it will always has a decimal point in it.
uiRealInputElement
  :: DomBuilder t m
  => InputElementConfig er t (DomBuilderSpace m)
  -> m (InputElement er (DomBuilderSpace m) t)
uiRealInputElement cfg = do
    inputElement $ cfg & initialAttributes %~
        (<> ("type" =: "number")) . addInputElementCls

uiIntInputElement
  :: DomBuilder t m
  => InputElementConfig er t (DomBuilderSpace m)
  -> m (InputElement er (DomBuilderSpace m) t)
uiIntInputElement cfg = do
    r <- inputElement $ cfg & initialAttributes %~
            (<> ("type" =: "number")) . addInputElementCls
    pure $ r
      { _inputElement_value = fmap fixNum $ _inputElement_value r
      , _inputElement_input = fmap fixNum $ _inputElement_input r
      }
  where
    fixNum = T.takeWhile (/='.')

-- | Take an `uiInputElement` like thing and make it a view with change events
-- of your model.
uiInputView
  :: (DomBuilder t m, er ~ EventResult, PostBuild t m, MonadFix m, MonadHold t m)
  => (InputElementConfig er t (DomBuilderSpace m) -> m (InputElement er (DomBuilderSpace m) t))
  -> InputElementConfig er t (DomBuilderSpace m)
  -> Dynamic t Text
  -> m (Event t Text)
uiInputView mkInput cfg mVal = mdo
  onSet <- tagOnPostBuild mVal
  let
    isValid = (==) <$> mVal <*> _inputElement_value i
    validCls = (\v -> if v then "" else "input_invalid") <$> isValid
    dynAttrs = do
      let baseAttrs = cfg ^. initialAttributes
      cValid <- validCls
      pure $ (addToClassAttr cValid . addInputElementCls) baseAttrs
  -- Short delay to avoid initial red state on load:
  modifyAttrs <- tailE =<< dynamicAttributesToModifyAttributes dynAttrs

  i <- mkInput $ cfg
    & inputElementConfig_setValue .~ onSet
    & modifyAttributes .~ modifyAttrs
  pure $ _inputElement_input i


-- | Make labeled and segmented input.
mkLabeledInput
  :: (DomBuilder t m, er ~ EventResult, PostBuild t m, MonadFix m
     , MonadHold t m
     )
  => (InputElementConfig er t (DomBuilderSpace m) -> m (InputElement er (DomBuilderSpace m) t))
  -> Text -> Dynamic t Text -> m (Event t Text)
mkLabeledInput mkInput n v = elClass "div" "segment segment_type_tertiary labeled-input" $ do
  divClass "label labeled-input__label" $ text n
  uiInputView mkInput (def & initialAttributes %~ addToClassAttr "labeled-input__input") v

-- | Validated input with button
validatedInputWithButton
  :: MonadWidget t m
  => CssClass
  -> (Text -> PushM t (Maybe Text))
  -- ^ Validation function returning `Just error message` on error.
  -> Text -- ^ Placeholder
  -> Text -- ^ Button text
  -> m (Event t Text)
validatedInputWithButton uCls check placeholder buttonText = do
    let cls = uCls <> "new-by-name"
    elKlass "div" cls $ do
      (update, checked) <- elClass "div" "new-by-name_inputs" $ mdo
        name <- uiInputElement $ def
            & inputElementConfig_setValue .~ (T.empty <$ confirmed)
            & initialAttributes .~ ("placeholder" =: placeholder <> "type" =: "text" <> "class" =: "new-by-name__input")
        let
          nameVal = T.strip <$> _inputElement_value name
          onEnter = keypress Enter name
          nameEmpty = (== "") <$> nameVal

        checkedL <- holdDyn Nothing $ pushAlways check $ updated nameVal

        let
          checkFailed = isJust <$> checkedL
          btnCfg = def & uiButtonCfg_disabled .~ liftA2 (||) nameEmpty checkFailed
                       & uiButtonCfg_class .~ "button_type_primary" <> "new-by-name__button"
        clicked <- uiButtonDyn btnCfg $ text buttonText

        let
          filterValid = fmap (const ()) . ffilter not . tag (current checkFailed)
          confirmed = filterValid $ leftmost [ onEnter, clicked ]
        void $ performEvent (liftJSM (pToJSVal (_inputElement_raw name) ^.  js0 ("focus" :: String)) <$ confirmed)
        pure $ (tag (current nameVal) confirmed, checkedL)

      elClass "div" "new-by-name_error" $
        elClass "span" "error_inline" $ dynText $ fromMaybe "" <$> checked

      pure update


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
    let mkClass a = singleClass "accordion" <> contentClass <> activeClass a
    (onClick, pair) <- elDynKlass "div" (mkClass <$> isActive) $ do
      (onClickL,a1) <- elClass "h2" "accordion__header" $ do
        b <- uiButton (def & uiButtonCfg_class .~ "accordion__toggle-button button_type_secondary") $
          imgWithAlt (static @"img/arrow-down.svg") "Expand" blank
        r <- title
        pure (b, r)
      b1 <- divClass "accordion__content" inner
      return (onClickL, (a1, b1))
    return pair
  where
    activeClass = \case
      False -> singleClass "accordion-collapsed"
      True -> mempty

accordionItem :: MonadWidget t m => Bool -> CssClass -> Text -> m a -> m a
accordionItem initActive contentClass title inner =
  snd <$> accordionItem' initActive contentClass (text title) inner

------------------------------------------------------------------------------

paginationWidget
  :: MonadWidget t m
  => CssClass
  -> Dynamic t Word  -- ^ Current page
  -> Dynamic t Word  -- ^ Total number of pages
  -> m (Event t Word)
paginationWidget cls currentPage totalPages = elKlass "div" (cls <> "pagination") $ do
    let
      pageButton okay i = do
        let
          cfg = btnCfgTertiary
            & uiButtonCfg_disabled .~ fmap not okay
            & uiButtonCfg_class %~ fmap (<> "pagination__button")
        uiButtonDyn cfg $ elClass "i" ("fa " <> i) blank

      canGoFirst = (> 1) <$> currentPage
    first <- pageButton canGoFirst "fa-angle-double-left"
    prev <-  pageButton canGoFirst "fa-angle-left"
    void $ elClass "div" "pagination__page-count" $
      elClass "span" "pagination__page-count-text" $ do
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
----------------------------------------------------------------------------------
