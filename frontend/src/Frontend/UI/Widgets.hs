{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Widgets collection
-- Was based on semui, but now transitioning to custom widgets
module Frontend.UI.Widgets
  ( -- * Standard widgets for chainweaver
    -- ** Buttons
    module Frontend.UI.Button
  -- ** Single Purpose Widgets
  , uiGasPriceInputField
  , uiDetailsCopyButton
  , uiDisplayKadenaAddressWithCopy
    -- * Values for _deploymentSettingsConfig_chainId:
  , predefinedChainIdSelect
  , predefinedChainIdDisplayed
  , userChainIdSelect
  , userChainIdSelectWithPreselect
  , uiChainSelection
  -- ** Other widgets
  , uiInputWithInlineFeedback
  , uiSegment
  , uiGroup
  , uiGroupHeader
  , uiCodeFont
  , uiInputElement
  , uiTextAreaElement
  , uiCorrectingInputElement
  , uiRealInputElement
  , uiNonnegativeRealWithPrecisionInputElement
  , uiIntInputElement
  , uiSlider
  , uiSliderInputElement
  , uiInputView
  , mkLabeledInputView
  , mkLabeledInput
  , mkLabeledView
  , uiLabeledRadioView
  , uiRadioElementView
  , mkLabeledClsInput
  , uiCheckbox
  , uiDropdown
  , uiSelectElement
  , uiPassword
  , validatedInputWithButton
  , uiAccountBalance
  , uiAccountBalance'
  , uiPublicKeyShrunk
    -- ** Helper widgets
  , imgWithAlt
  , showLoading
  , paginationWidget
  , tabPane
  , tabPane'
  , makeClickable
  , accordionItem
  , accordionItem'
  , accordionItemWithClick
  , controlledAccordionItem
  , accordionHeaderBtn
  , setFocus
  , setFocusOn
  , setFocusOnSelected
  , noAutofillAttrs
  , addNoAutofillAttrs
  , horizontalDashedSeparator
  , dimensionalInputFeedbackWrapper
  , uiSidebarIcon
  ) where


------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Arrow (first, (&&&))
import           Control.Lens
import           Control.Monad
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import           Data.Either (isLeft, rights)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict as Map
import           Data.String                 (IsString)
import           Data.Proxy                  (Proxy(..))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import           GHC.Word                    (Word8)
import           Data.Decimal                (Decimal)
import qualified Data.Decimal                as D
import           Language.Javascript.JSaddle (js0, pToJSVal)
import           Obelisk.Generated.Static
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Dom.Core
import           Reflex.Extended             (tagOnPostBuild)
------------------------------------------------------------------------------
import Pact.Types.ChainId (ChainId (..))
import Pact.Types.Runtime (GasPrice (..))
import Pact.Parse (ParsedDecimal (..))
------------------------------------------------------------------------------
import           Common.Wallet
import           Frontend.Network (HasNetwork(..), NodeInfo, getChains, maxCoinPrecision)
import           Frontend.Foundation
import           Frontend.UI.Button
import           Frontend.Wallet
import           Frontend.UI.Widgets.Helpers (imgWithAlt, imgWithAltCls, makeClickable,
                                              setFocus, setFocusOn,
                                              setFocusOnSelected, tabPane,
                                              preventScrollWheelAndUpDownArrow,
                                              tabPane')
import           Frontend.KadenaAddress (KadenaAddress)
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
    cb <- checkbox' b $ cfg
    elClass "span" "checkbox__checkmark checkbox__checkmark_type_secondary" blank
    c
    pure cb

-- Copied from reflex-dom because CheckboxConfig doesn't expose ElementConfig, and without stopPropagation odd things happen
-- Several modals which use uiCheckbox strangely trigger `domEvent Click` twice when we click the toggle
-- and it looks like the second one is propagated all the way to the modal backdrop, dismissing it.
--
-- | Create an editable checkbox
--   Note: if the "type" or "checked" attributes are provided as attributes, they will be ignored
{-# INLINABLE checkbox' #-}
checkbox' :: forall t m. (DomBuilder t m, PostBuild t m) => Bool -> CheckboxConfig t -> m (Checkbox t)
checkbox' checked config = do
  let permanentAttrs = "type" =: "checkbox"
      dAttrs = Map.delete "checked" . Map.union permanentAttrs <$> _checkboxConfig_attributes config
  modifyAttrs <- dynamicAttributesToModifyAttributes dAttrs
  i <- inputElement $ (def :: InputElementConfig EventResult t (DomBuilderSpace m))
    & inputElementConfig_initialChecked .~ checked
    & inputElementConfig_setChecked .~ _checkboxConfig_setValue config
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ Map.mapKeys (AttributeName Nothing) permanentAttrs
    & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs
    & inputElementConfig_elementConfig . elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (const stopPropagation)
  return $ Checkbox
    { _checkbox_value = _inputElement_checked i
    , _checkbox_change = _inputElement_checkedChange i
    }

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

-- | The header element of a group
uiGroupHeader :: DomBuilder t m => CssClass -> m a -> m a
uiGroupHeader cls = elKlass "div" (cls <> "group__header")

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

uiPassword :: DomBuilder t m => Text -> Text -> Text -> m (InputElement EventResult (DomBuilderSpace m) t)
uiPassword wrapperCls inputCls ph = elClass "span" wrapperCls $ do
  imgWithAltCls "setup__password-wrapper-lock" (static @"img/lock-dark.svg") "Password" blank
  uiInputElement $ def & initialAttributes .~ mconcat
    [ "type" =: "password"
    , "placeholder" =: ph
    , "class" =: inputCls
    ]

-- | Factored out input class modifier, so we can keep it in sync.
addInputElementCls :: (Ord attr, IsString attr) => Map attr Text -> Map attr Text
addInputElementCls = addToClassAttr "input"

addNoAutofillAttrs :: (Ord attr, IsString attr) => Map attr Text -> Map attr Text
addNoAutofillAttrs = (noAutofillAttrs <>)


-- | reflex-dom `inputElement` with chainweaver default styling:
uiInputElement
  :: DomBuilder t m
  => InputElementConfig er t (DomBuilderSpace m)
  -> m (InputElement er (DomBuilderSpace m) t)
uiInputElement cfg = inputElement $ cfg & initialAttributes %~ (addInputElementCls . addNoAutofillAttrs)

-- | uiInputElement which should always provide a proper real number.
--
--   In particular it will always have a decimal point in it.
uiRealInputElement
  :: DomBuilder t m
  => InputElementConfig er t (DomBuilderSpace m)
  -> m (InputElement er (DomBuilderSpace m) t)
uiRealInputElement cfg = do
    inputElement $ cfg & initialAttributes %~
        (<> ("type" =: "number")) . addInputElementCls . addNoAutofillAttrs

uiCorrectingInputElement
  :: forall t m a explanation. DomBuilder t m
  => MonadFix m
  => (Text -> Maybe a)
  -> (a -> Maybe (a, explanation))
  -> (a -> Maybe (a, explanation))
  -> (a -> Text)
  -> InputElementConfig EventResult t (DomBuilderSpace m)
  -> m (InputElement EventResult (DomBuilderSpace m) t, Dynamic t (Maybe a), Event t (a, Maybe explanation))
uiCorrectingInputElement parse inputSanitize blurSanitize render cfg = mdo
  ie <- inputElement $ cfg & inputElementConfig_setValue %~ (\e -> leftmost
      [ attemptCorrection e
      , forceCorrection inp
      , blurAttemptCorrection eBlurredVal
      , blurForceCorrection eBlurredVal
      ]
    )
  let
    inp = _inputElement_input ie
    val = value ie
    eBlurredVal = current val <@ domEvent Blur ie

    sanitization :: Functor f => (a -> Maybe (a, explanation)) -> f Text -> f (Maybe (a, Maybe explanation))
    sanitization f = fmap $ \i -> ffor (parse i) $ \p ->
      case f p of
        Nothing -> (p, Nothing)
        Just (a, x) -> (a, Just x)

    inputSanitization :: Functor f => f Text -> f (Maybe (a, Maybe explanation))
    inputSanitization = sanitization inputSanitize

    blurSanitization :: Functor f => f Text -> f (Maybe (a, Maybe explanation))
    blurSanitization = sanitization blurSanitize

    intercept :: ((a, Maybe explanation) -> Bool) -> (Event t Text -> Event t Text)
    intercept p = fmap render . fmap fst . ffilter p . fmapMaybe id . inputSanitization

    attemptCorrection :: Event t Text -> Event t Text
    attemptCorrection = intercept (const True)

    forceCorrection :: Event t Text -> Event t Text
    forceCorrection = intercept (isJust . snd)

    blurIntercept :: ((a, Maybe explanation) -> Bool) -> (Event t Text -> Event t Text)
    blurIntercept p = fmap render . fmap fst . ffilter p . fmapMaybe id . blurSanitization

    blurAttemptCorrection :: Event t Text -> Event t Text
    blurAttemptCorrection = blurIntercept (const True)

    blurForceCorrection :: Event t Text -> Event t Text
    blurForceCorrection = blurIntercept (isJust . snd)

    -- type inference goes crazy if we inline this - ghc bug?
    inp' = fmapMaybe id $ inputSanitization inp

  pure (ie, (fmap . fmap) fst $ inputSanitization val, inp')

uiInputWithInlineFeedback
  :: ( DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     )
  => (a -> Dynamic t (Either e b))
  -> (a -> Dynamic t Bool)
  -> (e -> Text)
  -> Maybe Text
  -> (InputElementConfig EventResult t (DomBuilderSpace m) -> m a)
  -> InputElementConfig EventResult t (DomBuilderSpace m)
  -> m (a, Dynamic t (Either e b))
uiInputWithInlineFeedback parse isDirty renderFeedback mUnits mkInp cfg =
  dimensionalInputFeedbackWrapper mUnits $ do
    inp <- mkInp cfg

    let dParsed = parse inp
    dIsDirty <- holdUniqDyn $ isDirty inp

    dyn_ $ ffor2 dParsed dIsDirty $ curry $ \case
      (Left e, True) -> elClass "span" "dimensional-input__feedback" $ text $ renderFeedback e
      _ -> blank

    pure (inp, dParsed)

-- | Decimal input to the given precision. Returns the element, the value, and
-- the user input events
uiNonnegativeRealWithPrecisionInputElement
  :: forall t m a. (DomBuilder t m, MonadFix m, MonadHold t m)
  => Word8
  -> (Decimal -> a)
  -> InputElementConfig EventResult t (DomBuilderSpace m)
  -> m (InputElement EventResult (DomBuilderSpace m) t, Dynamic t (Maybe a), Event t a)
uiNonnegativeRealWithPrecisionInputElement prec fromDecimal cfg = do
  rec
    (ie, val, input) <- uiCorrectingInputElement parse inputSanitize blurSanitize tshow $ cfg
      & initialAttributes %~ addInputElementCls . addNoAutofillAttrs
        . (<> ("type" =: "number" <> "step" =: stepSize <> "min" =: stepSize))
    widgetHold_ blank $ ffor (fmap snd input) $ traverse_ $
      elClass "span" "dimensional-input__feedback" . text

  pure (ie, (fmap . fmap) fromDecimal val, fmap (fromDecimal . fst) input)

  where
    stepSize = "0." <> T.replicate (fromIntegral prec - 1) "0" <> "1"
    parse t = tread $
      if "." `T.isPrefixOf` t && not (T.any (== '.') $ T.tail t) then
        T.cons '0' t
      else
        t

    blurSanitize :: Decimal -> Maybe (Decimal, Text)
    blurSanitize decimal = asum
      [ (D.roundTo 1 decimal, "")
        <$ guard (D.decimalPlaces decimal == 0) -- To avoid `: Failure: Type error: expected decimal, found integer`
      ]

    inputSanitize :: Decimal -> Maybe (Decimal, Text)
    inputSanitize decimal = asum
      [ (0, "Cannot be negative")
        <$ guard (decimal < 0)
      , (D.roundTo prec decimal, ("Rounded to " <> tshow prec <> " places"))
        <$ guard (D.decimalPlaces decimal > prec)
      ]

-- TODO: correct floating point decimals
uiIntInputElement
  :: DomBuilder t m
  => MonadFix m
  => Maybe Integer
  -> Maybe Integer
  -> InputElementConfig EventResult t (DomBuilderSpace m)
  -> m (InputElement EventResult (DomBuilderSpace m) t, Event t Integer)
uiIntInputElement mmin mmax cfg = do
    (r, _, input) <- uiCorrectingInputElement (tread . fixNum) sanitize (const Nothing) tshow $ cfg & initialAttributes %~
      ((<> numberAttrs) . addInputElementCls . addNoAutofillAttrs)

    pure (r
          { _inputElement_value = fmap fixNum $ _inputElement_value r
          , _inputElement_input = fmap fixNum $ _inputElement_input r
          }
         , fst <$> input)

  where
    fixNum = T.takeWhile (/='.')
    numberAttrs = mconcat $ fmapMaybe id
      [ Just $ "type" =: "number"
      , ffor mmin $ ("min" =:) . tshow
      , ffor mmax $ ("max" =:) . tshow
      ]
    clamp = appEndo $ foldMap Endo $ fmapMaybe id
      [ ffor mmin $ max
      , ffor mmax $ min
      ]

    sanitize :: Integer -> Maybe (Integer, Maybe ())
    sanitize num = let s = clamp num
                   in guard (s /= num) *> Just (s, Just ())

uiSlider
  :: DomBuilder t m
  => CssClass
  -> m ()
  -> m ()
  -> InputElementConfig er t (DomBuilderSpace m)
  -> m (InputElement er (DomBuilderSpace m) t)
uiSlider cls minLabel maxLabel conf = elKlass "div" ("slider" <> cls) $ do
  s <- uiSliderInputElement conf
  divClass "slider_min" minLabel
  divClass "slider_max" maxLabel
  divClass "clear" $ pure ()
  pure s

uiSliderInputElement
  :: DomBuilder t m
  => InputElementConfig er t (DomBuilderSpace m)
  -> m (InputElement er (DomBuilderSpace m) t)
uiSliderInputElement conf = inputElement $ conf
    & initialAttributes %~ Map.insert "type" "range" . addToClassAttr "slider"

-- | Take an `uiInputElement` like thing and make it a view with change events
-- of your model. It also takes care of input validation.
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


uiLabeledRadioView
  :: (MonadWidget t m, Eq a)
  => (CssClass -> m (Element EventResult (DomBuilderSpace m) t))
  -> Dynamic t a
  -> a
  -> m (Event t a)
uiLabeledRadioView mkLabel val self  = do
    onRadioChange <- uiRadioElementView val self
    l <- mkLabel $ "label" <> "label_for_radio"
    let onLabelClick = domEvent Click l
    pure $ leftmost
      [ onRadioChange
      , fmapMaybe selectUnselected . tag (current val) $ onLabelClick
      ]
  where
    selectUnselected v = if v /= self then Just self else Nothing


uiRadioElementView :: (MonadWidget t m, Eq a) => Dynamic t a -> a -> m (Event t a)
uiRadioElementView val self = do
  v <- tagOnPostBuild val
  let
    cfg = def
      & initialAttributes .~ ("type" =: "radio" <> "class" =: "input input_type_radio")
      & inputElementConfig_setChecked .~ fmap (== self) v
  e <- uiInputElement cfg
  pure $ fmap (const self) . ffilter id $ _inputElement_checkedChange e


uiTextAreaElement
  :: DomBuilder t m
  => TextAreaElementConfig r t (DomBuilderSpace m)
  -> m (TextAreaElement r (DomBuilderSpace m) t)
uiTextAreaElement uCfg =
  let
    cfg = uCfg & initialAttributes %~ (addToClassAttr  "input input_type_textarea" . addNoAutofillAttrs)
  in
    textAreaElement cfg


-- | Make labeled and segmented input view.
mkLabeledInputView
  :: (DomBuilder t m, er ~ EventResult, PostBuild t m, MonadFix m
     , MonadHold t m
     )
  => Bool
  -> Text
  -> (InputElementConfig er t (DomBuilderSpace m) -> m (InputElement er (DomBuilderSpace m) t))
  -> Dynamic t Text
  -> m (Event t Text)
mkLabeledInputView inlineLabel n mkInput v = elClass "div" ("segment segment_type_tertiary labeled-input" <> inlineState) $ do
  divClass ("label labeled-input__label" <> inlineState) $ text n
  uiInputView mkInput (def & initialAttributes %~ addToClassAttr "labeled-input__input") v
  where
    inlineState = bool "" "-inline" inlineLabel

-- | Make labeled and segmented input.
mkLabeledInput
  :: (DomBuilder t m , InitialAttributes cfg)
  => Bool
  -> Text
  -> (cfg -> m element)
  -> cfg
  -> m element
mkLabeledInput inlineLabel n mkInput cfg = elClass "div" ("segment segment_type_tertiary labeled-input" <> inlineState) $ do
  divClass ("label labeled-input__label" <> inlineState) $ text n
  mkInput (cfg & initialAttributes %~ addToClassAttr "labeled-input__input")
  where
    inlineState = bool "" "-inline" inlineLabel

-- | Make labeled and segmented display for some element
--
mkLabeledView
  :: DomBuilder t m
  => Bool
  -> Text
  -> m a
  -> m a
mkLabeledView inlineLabel n body = elClass "div" ("segment segment_type_tertiary labeled-input" <> inlineState) $ do
  divClass ("label labeled-input__label" <> inlineState) $ text n
  divClass "labeled-input__input" body
  where
    inlineState = bool "" "-inline" inlineLabel

-- | Make some input a labeled input.
--
--   Any widget creating function that can be called with additional classes will do.
--   TODO: This function can probably replace `mkLabeledInput`.
mkLabeledClsInput
  :: (DomBuilder t m, PostBuild t m)
  => Bool
  -> Dynamic t Text
  -> (CssClass -> m element)
  -> m element
mkLabeledClsInput inlineLabel name mkInput = elClass "div" ("segment segment_type_tertiary labeled-input" <> inlineState) $ do
  divClass ("label labeled-input__label" <> inlineState) $ dynText name
  mkInput "labeled-input__input"
  where
    inlineState = bool "" "-inline" inlineLabel

-- | Attributes which will turn off all autocomplete/autofill/autocorrect
-- functions, including the OS-level suggestions on macOS.
noAutofillAttrs :: (Ord attr, IsString attr) => Map attr Text
noAutofillAttrs = Map.fromList
  [ ("autocomplete", "off")
  , ("autocorrect", "off")
  , ("autocapitalize", "off")
  , ("spellcheck", "false")
  ]

-- | Validated input with button
validatedInputWithButton
  :: MonadWidget t m
  => CssClass
  -> (Dynamic t (Text -> Either Text a))
  -- ^ Validation function returning `Just error message` on error.
  -> Text -- ^ Placeholder
  -> Text -- ^ Button text
  -> m (Event t a)
validatedInputWithButton uCls check placeholder buttonText = do
    let cls = uCls <> "new-by-name"
    elKlass "div" cls $ do
      (update, checked, rawIn) <- elClass "div" "new-by-name_inputs" $ mdo
        name <- uiInputElement $ def
            & inputElementConfig_setValue .~ (T.empty <$ onConfirmed)
            & initialAttributes .~ ("placeholder" =: placeholder <> "type" =: "text" <> "class" =: "new-by-name__input")
        let
          dInputIsInvalid = liftA2 (||) nameEmpty checkFailed
          nameVal = T.strip <$> _inputElement_value name
          onEnter = gate (current (not <$> dInputIsInvalid)) $ keypress Enter name
          nameEmpty = (== "") <$> nameVal

          checkedL = check <*> nameVal

        let
          checkFailed = isLeft <$> checkedL
          btnCfg = def & uiButtonCfg_disabled .~ dInputIsInvalid
                       & uiButtonCfg_class .~ "button_type_primary" <> "new-by-name__button"
        clicked <- uiButtonDyn btnCfg $ text buttonText

        let
          filterValid = fmap (const ()) . ffilter not . tag (current checkFailed)
          onConfirmed = filterValid $ leftmost [ onEnter, clicked ]
        void $ performEvent (liftJSM (pToJSVal (_inputElement_raw name) ^.  js0 ("focus" :: String)) <$ onConfirmed)
        pure $ (attachWithMaybe (\e _ -> either (const Nothing) Just e) (current checkedL) onConfirmed, checkedL, nameVal)

      elClass "div" "new-by-name_error" $ do
        confirmed <- holdUniqDyn <=< holdDyn False $
          leftmost [ False <$ updated rawIn, True <$ update ]
        let
          -- Avoid error msg flickr on confirmation (e.g. duplicates):
          checkedMsg = do
            c <- confirmed
            if c then pure "" else either id (const "") <$> checked
        elClass "span" "error_inline" $ dynText checkedMsg

      pure update


uiAccountBalance' :: Bool -> Account -> Text
uiAccountBalance' showUnits acc = case _account_status acc of
  AccountStatus_Unknown -> "Unknown"
  AccountStatus_DoesNotExist -> "Account not present"
  AccountStatus_Exists d -> mconcat $ catMaybes
    [ Just $ tshow $ unAccountBalance $ _accountDetails_balance d
    , " KDA" <$ guard showUnits
    ,  "*" <$ _vanityAccount_unfinishedCrossChainTransfer (_account_storage acc)
    ]

uiAccountBalance :: Bool -> Maybe AccountBalance -> Text
uiAccountBalance showUnits = \case
  Nothing -> "Account not present"
  Just b -> mconcat $ catMaybes
    [ Just $ tshow $ unAccountBalance b
    , " KDA" <$ guard showUnits
    ]

uiPublicKeyShrunk :: (DomBuilder t m, PostBuild t m) => Dynamic t PublicKey -> m ()
uiPublicKeyShrunk pk = do
  divClass "wallet__public-key" $ do
    elClass "span" "wallet__public-key__prefix" $ dynText $ T.dropEnd 6 <$> ktxt
    elClass "span" "wallet__public-key__suffix" $ dynText $ T.takeEnd 6 <$> ktxt
  where
    ktxt = keyToText <$> pk

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

controlledAccordionItem
  :: MonadWidget t m
  => Dynamic t Bool
  -> CssClass
  -> m a
  -> m b
  -> m (Event t (), (a,b))
controlledAccordionItem dActive contentClass title inner = do
    let mkClass a = singleClass "accordion" <> contentClass <> activeClass a
    (onClick, pair) <- elDynKlass "div" (mkClass <$> dActive) $ do
      (onClickL,a1) <- elClass "h2" "accordion__header" $ do
        b <- accordionButton def
        r <- title
        pure (b, r)
      b1 <- divClass "accordion__content" inner
      return (onClickL, (a1, b1))
    return (onClick, pair)
  where
    activeClass = \case
      False -> singleClass "accordion-collapsed"
      True -> singleClass "accordion-revealed"

accordionItemWithClick
  :: MonadWidget t m
  => Bool
  -> CssClass
  -> m a
  -> m b
  -> m (Event t (), (a,b))
accordionItemWithClick initActive contentClass title inner = mdo
    isActive <- foldDyn (const not) initActive onClick
    (onClick, pair) <- controlledAccordionItem isActive contentClass title inner
    return (onClick, pair)

accordionItem'
  :: MonadWidget t m
  => Bool
  -> CssClass
  -> m a
  -> m b
  -> m (a,b)
accordionItem' initActive contentClass title inner =
  snd <$> accordionItemWithClick initActive contentClass title inner

accordionItem :: MonadWidget t m => Bool -> CssClass -> Text -> m a -> m a
accordionItem initActive contentClass title inner =
  snd . snd <$> accordionItemWithClick initActive contentClass (divClass "accordion__header-btn-text" $ text title) inner

accordionHeaderBtn :: DomBuilder t m => Text -> m ()
accordionHeaderBtn = divClass "accordion__header-btn-text" . text

----------------------------------------------------------------------------------

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
    first' <- pageButton canGoFirst "fa-angle-double-left"
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
      , 1 <$ first'
      , attachWith (\x _ -> succ x) (current currentPage) nextL
      , tag (current totalPages) lastL
      ]
----------------------------------------------------------------------------------

horizontalDashedSeparator :: DomBuilder t m => m ()
horizontalDashedSeparator = divClass "horizontal-dashed-separator" blank

dimensionalInputFeedbackWrapper :: DomBuilder t m => Maybe Text -> m a -> m a
dimensionalInputFeedbackWrapper units inp = divClass "dimensional-input-wrapper" $ do
  traverse_ (divClass "dimensional-input-wrapper__units" . text) units
  inp

uiDetailsCopyButton
  :: (DomBuilder t m, MonadFix m, MonadHold t m, MonadJSM (Performable m), PerformEvent t m, PostBuild t m)
  => Behavior t Text -> m ()
uiDetailsCopyButton txt = do
  let cfg = def
        & uiButtonCfg_class .~ constDyn "button_type_confirm"
        & uiButtonCfg_title .~ constDyn (Just "Copy")
  divClass "details__copy-btn-wrapper" $ copyButton cfg False txt

uiDisplayKadenaAddressWithCopy
  :: ( MonadJSM (Performable m)
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     , PerformEvent t m
     )
  => KadenaAddress
  -> m ()
uiDisplayKadenaAddressWithCopy address = void $ do
  let txtAddr = LT.toStrict $ LTB.toLazyText $ AesonPretty.encodePrettyToTextBuilder address
  -- Kadena Address
  elClass "div" "segment segment_type_tertiary labeled-input" $ do
    divClass "label labeled-input__label" $ text "[Kadena Address]"
    void $ uiTextAreaElement $ def
      & initialAttributes <>~ ("disabled" =: "true" <> "class" =: " labeled-input__input labeled-input__kadena-address")
      & textAreaElementConfig_initialValue .~ txtAddr
  uiDetailsCopyButton $ pure txtAddr

uiGasPriceInputField
  :: forall m t.
     ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     )
  => InputElementConfig EventResult t (DomBuilderSpace m)
  -> m ( InputElement EventResult (DomBuilderSpace m) t
       , Dynamic t (Maybe GasPrice)
       , Event t GasPrice
       )
uiGasPriceInputField conf = dimensionalInputFeedbackWrapper (Just "KDA") $
 uiNonnegativeRealWithPrecisionInputElement maxCoinPrecision (GasPrice . ParsedDecimal) $ conf
  & initialAttributes %~ addToClassAttr "input-units"
  & inputElementConfig_elementConfig . elementConfig_eventSpec %~ preventScrollWheelAndUpDownArrow @m


-- | Let the user pick a chain id.
userChainIdSelect
  :: (MonadWidget t m, HasNetwork model t
     )
  => model
  -> m (MDynamic t ChainId)
userChainIdSelect m =
  userChainIdSelectWithPreselect m (constDyn Nothing)

-- | Let the user pick a chain id but preselect a value
userChainIdSelectWithPreselect
  :: (MonadWidget t m, HasNetwork model t
     )
  => model
  -> Dynamic t (Maybe ChainId)
  -> m (MDynamic t ChainId)
userChainIdSelectWithPreselect m mChainId = mkLabeledClsInput True "Chain ID" (uiChainSelection mNodeInfo mChainId)
  where mNodeInfo = (^? to rights . _head) <$> m ^. network_selectedNodes

uiChainSelection
  :: MonadWidget t m
  => Dynamic t (Maybe NodeInfo)
  -> Dynamic t (Maybe ChainId)
  -> CssClass
  -> m (Dynamic t (Maybe ChainId))
uiChainSelection info mPreselected cls = do
  pb <- getPostBuild

  let
    chains = map (id &&& _chainId) . maybe [] getChains <$> info
    mkPlaceHolder cChains = if null cChains then "No chains available" else "Select chain"
    mkOptions cs = Map.fromList $ (Nothing, mkPlaceHolder cs) : map (first Just) cs

    staticCls = cls <> "select"
    mkDynCls v = if isNothing v then "select_mandatory_missing" else mempty

  rec
    let allCls = renderClass <$> fmap mkDynCls d <> pure staticCls
        cfg = def
          & dropdownConfig_attributes .~ (("class" =:) <$> allCls)
          & dropdownConfig_setValue .~ (current mPreselected <@ pb)

    d <- _dropdown_value <$> dropdown Nothing (mkOptions <$> chains) cfg
  pure d

-- | Use a predefined chain id, don't let the user pick one.
predefinedChainIdSelect
  :: (Reflex t, Monad m)
  => ChainId
  -> model
  -> m (Dynamic t (Maybe ChainId))
predefinedChainIdSelect chanId _ = pure . pure . pure $ chanId

-- | Use a predefined immutable chain id, but display it too.
predefinedChainIdDisplayed
  :: DomBuilder t m
  => ChainId
  -> model
  -> m (Dynamic t (Maybe ChainId))
predefinedChainIdDisplayed cid _ = do
  _ <- mkLabeledInput True "Chain ID" uiInputElement $ def
    & initialAttributes %~ Map.insert "disabled" ""
    & inputElementConfig_initialValue .~ _chainId cid
  pure $ pure $ pure cid

uiSidebarIcon :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> Text -> Text -> m (Event t ())
uiSidebarIcon selected src label = do
  let preventTwitching = el "div" -- questionable hack - somehow prevents images from twitching when container is resized
      cls = ffor selected $ bool "normal" "highlighted"
      mkAttrs sel = "class" =: ("sidebar__link" <> if sel then " selected" else "")
  (e, _) <- elDynAttr' "div" (mkAttrs <$> selected) $ do
    preventTwitching $ elDynAttr "img" (ffor cls $ \c -> "class" =: c <> "src" =: src) blank
    elAttr "span" ("class" =: "sidebar__link-label") $ text label
  pure $ domEvent Click e
