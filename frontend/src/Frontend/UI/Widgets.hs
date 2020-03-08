{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Widgets collection
-- Was based on semui, but now transitioning to custom widgets
module Frontend.UI.Widgets
  ( -- * Standard widgets for chainweaver
    -- ** Buttons
    module Frontend.UI.Button
  , noValidation
  -- ** Single Purpose Widgets
  , uiGasPriceInputField
  , uiDetailsCopyButton

  , uiTxBuilder
  , uiDisplayTxBuilderWithCopy
    -- * Values for _deploymentSettingsConfig_chainId:
  , predefinedChainIdSelect
  , predefinedChainIdDisplayed
  , userChainIdSelect
  , userChainIdSelectWithPreselect
  , uiChainSelection

  , mkChainTextAccounts
  , accountListId
  , accountDatalist
  , uiAccountNameInput
  , uiAccountFixed
  , uiAccountDropdown
  , uiAccountDropdown'
  , uiKeyPairDropdown

  -- ** Other widgets
  , PopoverState (..)
  , uiInputWithPopover
  , uiSegment
  , uiGroup
  , uiGroupHeader
  , uiCodeFont
  , uiInputElement
  , uiTextAreaElement
  , uiCorrectingInputElement
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
  , uiForm
  , uiForm'
  , uiPublicKeyShrunkDyn
    -- ** Helper types to avoid boolean blindness for additive input
  , AllowAddNewRow (..)
  , AllowDeleteRow (..)
  , uiAdditiveInput
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
  , uiEmptyState
  ) where


------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Arrow (first, (&&&))
import           Control.Lens hiding (element)
import           Control.Error (hush)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Maybe
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import           Data.Either (isLeft, rights)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict as Map
import qualified Data.IntMap as IntMap
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
import qualified GHCJS.DOM.Element as JS
import qualified GHCJS.DOM.DOMTokenList as JS
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
import           Frontend.Foundation
import           Frontend.Ide
import           Frontend.Network (HasNetwork(..), NodeInfo, getChains, maxCoinPrecision)
import           Frontend.TxBuilder (TxBuilder)
import           Frontend.UI.Button
import           Frontend.UI.Modal.Impl
import           Frontend.UI.Widgets.Helpers (imgWithAlt, imgWithAltCls, makeClickable,
                                              setFocus, setFocusOn,
                                              setFocusOnSelected, tabPane,
                                              preventUpAndDownArrow,
                                              preventScrollWheel,
                                              tabPane')
import           Frontend.Wallet

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
  imgWithAltCls "password-input__lock" (static @"img/lock-dark.svg") "Password" blank
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

-- | Produces a form wrapper given a suitable submit button so that the enter key is correctly handled
uiForm' :: forall t m a b. DomBuilder t m => ElementConfig EventResult t (DomBuilderSpace m) -> m b -> m a -> m (Event t (), (a,b))
uiForm' cfg btn fields = do
  let mkCfg = elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Submit (\_ -> preventDefault)
  (elt, a) <- element "form" (mkCfg cfg) $ (,) <$> fields <*> btn
  pure (domEvent Submit elt, a)

-- | Produces a form wrapper given a suitable submit button so that the enter key is correctly handled
uiForm :: forall t m a. DomBuilder t m => ElementConfig EventResult t (DomBuilderSpace m) -> m () -> m a -> m (Event t (), a)
uiForm cfg btn fields = (_2 %~ fst) <$> uiForm' cfg btn fields

-- | reflex-dom `inputElement` with chainweaver default styling:
uiInputElement
  :: DomBuilder t m
  => InputElementConfig er t (DomBuilderSpace m)
  -> m (InputElement er (DomBuilderSpace m) t)
uiInputElement cfg = inputElement $ cfg & initialAttributes %~ (addInputElementCls . addNoAutofillAttrs)

uiCorrectingInputElement
  :: forall t m a explanation
     . ( DomBuilder t m
       , MonadFix m
       , MonadHold t m
       )
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
      , purgeInvalidInputs
      ]
    )

  dLastGoodValue <- holdUniqDyn =<< holdDyn
    (cfg ^. inputElementConfig_initialValue)
    (leftmost
     [ cfg ^. inputElementConfig_setValue
     , fmapMaybe (fmap render) $ fmap parse inp
     ])

  let
    inp = _inputElement_input ie
    val = value ie
    eBlurredVal = current val <@ domEvent Blur ie

    sanitization :: Functor f => (a -> Maybe (a, explanation)) -> f Text -> f (Maybe (a, Maybe explanation))
    sanitization f = fmap $ \i -> ffor (parse i) $ \p ->
      case f p of
        Nothing -> (p, Nothing)
        Just (a, x) -> (a, Just x)

    purgeInvalidInputs :: Event t Text
    purgeInvalidInputs = attachWithMaybe
      (\old -> maybe (Just old) (const Nothing) . parse)
      (current dLastGoodValue)
      inp

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

-- | Decimal input to the given precision. Returns the element, the value, and
-- the user input events
uiNonnegativeRealWithPrecisionInputElement
  :: forall t m a
     . ( DomBuilder t m
       , MonadFix m
       , MonadHold t m
       , PostBuild t m
       , PerformEvent t m
       , MonadJSM m
       , MonadJSM (Performable m)
       , DomBuilderSpace m ~ GhcjsDomSpace
       )
  => Word8
  -> (Decimal -> a)
  -> InputElementConfig EventResult t (DomBuilderSpace m)
  -> m (InputElement EventResult (DomBuilderSpace m) t, Dynamic t (Maybe a), Event t a)
uiNonnegativeRealWithPrecisionInputElement prec fromDecimal cfg = do
  let
    uiCorrecting cfg0 = do
      (ie, val, input) <- uiCorrectingInputElement parse inputSanitize blurSanitize tshow $ cfg0
      pure (ie, (input, val))

    showPopover (_, (onInput, _)) = pure $ ffor onInput $
      maybe PopoverState_Disabled PopoverState_Error . snd

  (ie, (input, val)) <- uiInputWithPopover uiCorrecting (_inputElement_raw . fst) showPopover $ cfg
    & initialAttributes %~ addInputElementCls . addNoAutofillAttrs . (<> ("type" =: "number" <> "step" =: stepSize <> "min" =: stepSize))
    & inputElementConfig_elementConfig . elementConfig_eventSpec %~ preventUpAndDownArrow @m
  preventScrollWheel $ _inputElement_raw ie

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
  :: forall t m
     . ( DomBuilder t m
       , MonadJSM m
       , GhcjsDomSpace ~ DomBuilderSpace m
       , MonadHold t m
       , MonadFix m
       )
  => Maybe Integer
  -> Maybe Integer
  -> InputElementConfig EventResult t (DomBuilderSpace m)
  -> m (InputElement EventResult (DomBuilderSpace m) t, Event t Integer)
uiIntInputElement mmin mmax cfg = do
    (r, _, input) <- uiCorrectingInputElement (tread . fixNum) sanitize (const Nothing) tshow $ cfg
      & initialAttributes %~ ((<> numberAttrs) . addInputElementCls . addNoAutofillAttrs)
      & inputElementConfig_elementConfig . elementConfig_eventSpec %~ preventUpAndDownArrow @m
    preventScrollWheel $ _inputElement_raw r

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
  AccountStatus_DoesNotExist -> "Does not exist"
  AccountStatus_Exists d -> mconcat $ catMaybes
    [ Just $ tshow $ unAccountBalance $ _accountDetails_balance d
    , " KDA" <$ guard showUnits
    ,  "*" <$ _vanityAccount_unfinishedCrossChainTransfer (_account_storage acc)
    ]

uiAccountBalance :: Bool -> Maybe AccountBalance -> Text
uiAccountBalance showUnits = \case
  Nothing -> "Does not exist"
  Just b -> mconcat $ catMaybes
    [ Just $ tshow $ unAccountBalance b
    , " KDA" <$ guard showUnits
    ]

uiPublicKeyShrunkDOM :: DomBuilder t m => m () -> m () -> m ()
uiPublicKeyShrunkDOM f6 l6 = divClass "wallet__public-key" $ do
  elClass "span" "wallet__public-key__prefix" f6
  elClass "span" "wallet__public-key__suffix" l6

uiPublicKeyShrunk :: DomBuilder t m => PublicKey -> m ()
uiPublicKeyShrunk pk = uiPublicKeyShrunkDOM
  (text $ T.dropEnd 6 ktxt)
  (text $ T.takeEnd 6 ktxt)
  where
    ktxt = keyToText pk

uiPublicKeyShrunkDyn :: (DomBuilder t m, PostBuild t m) => Dynamic t PublicKey -> m ()
uiPublicKeyShrunkDyn pk = uiPublicKeyShrunkDOM
  (dynText $ T.dropEnd 6 <$> ktxt)
  (dynText $ T.takeEnd 6 <$> ktxt)
  where
    ktxt = keyToText <$> pk

newtype AllowAddNewRow t out = AllowAddNewRow (out -> Event t Bool)
newtype AllowDeleteRow t out = AllowDeleteRow (out -> Event t Bool)

uiAdditiveInput
  :: forall t m out particular
     . ( MonadWidget t m
       )
  => (IntMap.Key -> particular -> m out)
  -> AllowAddNewRow t out
  -> AllowDeleteRow t out
  -> particular
  -> Event t (PatchIntMap particular)
  -> m (Dynamic t (IntMap.IntMap out))
uiAdditiveInput mkIndividualInput (AllowAddNewRow newRow) (AllowDeleteRow deleteRow) initialSelection onExternal = do
  let
    minRowIx = 0

    decideAddNewRow :: (IntMap.Key, out) -> Event t (IntMap.IntMap (Maybe particular))
    decideAddNewRow (i, out) = IntMap.singleton (succ i) (Just initialSelection) <$ ffilter id (newRow out)

    decideDeletion :: IntMap.Key -> out -> Event t (IntMap.IntMap (Maybe particular))
    decideDeletion i out = IntMap.singleton i Nothing <$ ffilter id (deleteRow out)

  rec
    (keys, newSelection) <- traverseIntMapWithKeyWithAdjust mkIndividualInput (IntMap.singleton minRowIx initialSelection) $
      leftmost
      [ -- Delete rows when 'select' is chosen
        fmap PatchIntMap $ switchDyn $ IntMap.foldMapWithKey decideDeletion <$> dInputKeys
        -- Add a new row when all rows have a selection and there are more keys to choose from
      , fmap PatchIntMap $ switchDyn $ maybe never decideAddNewRow . IntMap.lookupMax <$> dInputKeys
        -- Set the values of the rows from an external event.
      , onExternal
      ]
    dInputKeys <- foldDyn applyAlways keys newSelection

  pure dInputKeys

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

prettyTxBuilder :: TxBuilder -> Text
prettyTxBuilder = LT.toStrict . LTB.toLazyText . AesonPretty.encodePrettyToTextBuilder

uiTxBuilder
  :: DomBuilder t m
  => Maybe TxBuilder
  -> TextAreaElementConfig r t (DomBuilderSpace m)
  -> m (TextAreaElement r (DomBuilderSpace m) t)
uiTxBuilder txBuilder cfg = do
  let txt = prettyTxBuilder <$> txBuilder
  uiTextAreaElement $ cfg
    & maybe id (textAreaElementConfig_initialValue .~) txt
    & initialAttributes <>~ fold
      [ "rows" =: tshow (max 13 {- for good luck -} $ maybe 0 (length . T.lines) txt)
      , "class" =: " labeled-input__input labeled-input__tx-builder"
      ]

uiDisplayTxBuilderWithCopy
  :: ( MonadJSM (Performable m)
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     , PerformEvent t m
     )
  => Bool
  -> TxBuilder
  -> m ()
uiDisplayTxBuilderWithCopy withLabel txBuilder = do
  elClass "div" "segment segment_type_tertiary labeled-input" $ do
    when withLabel $ divClass "label labeled-input__label" $ text "Tx Builder"
    void $ uiTxBuilder (Just txBuilder) $ def
      & initialAttributes <>~ "disabled" =: "true"
  uiDetailsCopyButton $ pure $ prettyTxBuilder txBuilder

uiGasPriceInputField
  :: forall m t.
     ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , MonadJSM m, MonadJSM (Performable m)
     , PostBuild t m
     , PerformEvent t m
     )
  => InputElementConfig EventResult t (DomBuilderSpace m)
  -> m ( InputElement EventResult (DomBuilderSpace m) t
       , Dynamic t (Maybe GasPrice)
       , Event t GasPrice
       )
uiGasPriceInputField conf = dimensionalInputFeedbackWrapper (Just "KDA") $ do
  uiNonnegativeRealWithPrecisionInputElement maxCoinPrecision (GasPrice . ParsedDecimal) $ conf
    & initialAttributes %~ addToClassAttr "input-units"

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

noValidation :: Applicative f => f (a -> Either err a)
noValidation = pure Right

-- | Global dom representation of the account list for use in an HTML5 combo
-- box. This uses and is addressed by an id, so there should only ever be one of
-- them in the DOM. Currently it's at the very top of the <body> tag.
accountDatalist
  :: ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m
     , HasNetwork ide t
     , HasWallet ide key t
     )
  => ide
  -> m ()
accountDatalist ideL = elAttr "datalist" ("id" =: accountListId) $ do
  let net = ideL ^. network_selectedNetwork
      networks = ideL ^. wallet_accounts
  mAccounts <- maybeDyn $ ffor2 net networks $ \n (AccountData m) -> case Map.lookup n m of
    Just accs | not (Map.null accs) -> Just accs
    _ -> Nothing
  dyn $ ffor mAccounts $ \case
    Nothing -> blank
    Just am -> void $ listWithKey am $ \k _ -> do
      el "option" $ text $ unAccountName k
  pure ()

accountMapNames :: Map AccountName (AccountInfo Account) -> [Text]
accountMapNames = map unAccountName . Map.keys

accountListId :: Text
accountListId = "account-list"

uiAccountNameInput
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , PerformEvent t m
     , MonadJSM (Performable m)
     )
  => Maybe AccountName
  -> Dynamic t (AccountName -> Either Text AccountName)
  -> m (Dynamic t (Maybe AccountName))
uiAccountNameInput initval validateName = do
  let
    mkMsg True (Left e) = PopoverState_Error e
    mkMsg _    _ = PopoverState_Disabled

    validate = ffor validateName $ (<=< mkAccountName)

    showPopover (ie, _) = pure $ (\v t -> mkMsg (not $ T.null t) (v t))
      <$> current validate
      <@> fmap T.strip (_inputElement_input ie)

    uiNameInput cfg = do
      inp <- uiInputElement $ cfg & initialAttributes %~ (<> "list" =: accountListId)
      pure (inp, _inputElement_raw inp)

  (inputE, _) <- mkLabeledInput True "Account Name" (uiInputWithPopover uiNameInput snd showPopover)
    $ def & inputElementConfig_initialValue .~ fold (fmap unAccountName initval)

  pure $ hush <$> (validate <*> fmap T.strip (value inputE))

-- | Set the account to a fixed value
uiAccountFixed
  :: DomBuilder t m
  => AccountName
  -> m (Dynamic t (Maybe (AccountName, Account)))
uiAccountFixed sender = do
  _ <- uiInputElement $ def
    & initialAttributes %~ Map.insert "disabled" ""
    & inputElementConfig_initialValue .~ unAccountName sender
  pure $ pure $ pure (sender, Account AccountStatus_Unknown blankVanityAccount)

mkChainTextAccounts
  :: (Reflex t, HasWallet model key t, HasNetwork model t)
  => model
  -> Dynamic t (AccountName -> Account -> Bool)
  -> Dynamic t (Maybe ChainId)
  -> Dynamic t (Either Text (Map AccountName Text))
mkChainTextAccounts m allowAccount mChainId = runExceptT $ do
  allowAcc <- lift allowAccount
  netId <- lift $ m ^. network_selectedNetwork

  keys <- lift $ m ^. wallet_keys
  chain <- ExceptT $ note "You must select a chain ID before choosing an account" <$> mChainId
  accountsOnNetwork <- ExceptT $ note "No accounts on current network" . Map.lookup netId . unAccountData <$> m ^. wallet_accounts
  let mkVanity n (AccountInfo _ chainMap)
        | Just a <- Map.lookup chain chainMap
        , allowAcc n a
        -- Only select _our_ accounts. TODO: run pact code to ensure keyset predicate(s)
        -- are satisfied so we're able to handle user created predicates correctly.
        , accountSatisfiesKeysetPredicate keys a
        = Map.singleton n (unAccountName n)
        | otherwise = mempty
      vanityAccounts = Map.foldMapWithKey mkVanity accountsOnNetwork
      accountsOnChain = vanityAccounts
  when (Map.null accountsOnChain) $ throwError "No accounts on current chain"
  pure accountsOnChain

-- | Let the user pick an account
uiAccountDropdown'
  :: ( PostBuild t m, DomBuilder t m
     , MonadHold t m, MonadFix m
     , HasWallet model key t
     , HasNetwork model t
     )
  => DropdownConfig t (Maybe AccountName)
  -> Dynamic t (AccountName -> Account -> Bool)
  -> Dynamic t (Text -> Text)
  -> model
  -> Maybe AccountName
  -> Dynamic t (Maybe ChainId)
  -> Event t (Maybe AccountName)
  -> m (Dynamic t (Maybe (AccountName, Account)))
uiAccountDropdown' uCfg allowAccount mkPlaceholder m initVal chainId setSender = do
  let
    textAccounts = mkChainTextAccounts m allowAccount chainId
    dropdownItems = ffor2 mkPlaceholder textAccounts $ \mk ->
      either
          (Map.singleton Nothing)
          (Map.insert Nothing (mk "Choose an Account") . Map.mapKeys Just)

  choice <- dropdown initVal dropdownItems $ uCfg
    & dropdownConfig_setValue .~ leftmost [Nothing <$ updated chainId, setSender]
    & dropdownConfig_attributes <>~ pure ("class" =: "labeled-input__input select select_mandatory_missing")
  let result = runMaybeT $ do
        net <- lift $ m ^. network_selectedNetwork
        accs <- lift $ m ^. wallet_accounts
        chain <- MaybeT chainId
        name <- MaybeT $ value choice
        acc <- MaybeT $ pure $ accs ^? _AccountData . ix net . ix name . accountInfo_chains . ix chain
        pure (name, acc)
  pure result

-- | Let the user pick an account
uiAccountDropdown
  :: ( PostBuild t m, DomBuilder t m
     , MonadHold t m, MonadFix m
     , HasWallet model key t
     , HasNetwork model t
     )
  => DropdownConfig t (Maybe AccountName)
  -> Dynamic t (AccountName -> Account -> Bool)
  -> Dynamic t (Text -> Text)
  -> model
  -> Dynamic t (Maybe ChainId)
  -> Event t (Maybe AccountName)
  -> m (Dynamic t (Maybe (AccountName, Account)))
uiAccountDropdown uCfg allowAccount mkPlaceholder m = uiAccountDropdown' uCfg allowAccount mkPlaceholder m Nothing

uiKeyPairDropdown
  :: forall t m key model
   . ( HasWallet model key t
     , DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m
     )
  => model
  -> DropdownConfig t (Maybe (KeyPair key))
  -> m (Dynamic t (Maybe (KeyPair key)))
uiKeyPairDropdown m cfg = fmap _dropdown_value $ uiDropdown Nothing options $ cfg
  & dropdownConfig_attributes <>~ pure ("class" =: "labeled-input__input select select_mandatory_missing")
  where
    options = ffor (m ^. wallet_keys)
      $ Map.fromList
      . fmap (\k -> (Just k, keyToText (_keyPair_publicKey k)))
      . fmap _key_pair
      . IntMap.elems

uiSidebarIcon :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> Text -> Text -> m (Event t ())
uiSidebarIcon selected src label = do
  let preventTwitching = el "div" -- questionable hack - somehow prevents images from twitching when container is resized
      cls = ffor selected $ bool "normal" "highlighted"
      mkAttrs sel = "class" =: ("sidebar__link" <> if sel then " selected" else "")
  (e, _) <- elDynAttr' "div" (mkAttrs <$> selected) $ do
    preventTwitching $ elDynAttr "img" (ffor cls $ \c -> "class" =: c <> "src" =: src) blank
    elAttr "span" ("class" =: "sidebar__link-label") $ text label
  pure $ domEvent Click e

data PopoverState
  = PopoverState_Error Text
  | PopoverState_Warning Text
  | PopoverState_Disabled
  deriving (Eq, Show)

uiInputWithPopover
  :: forall t m cfg el rawEl a
  .  ( DomBuilder t m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , HasDomEvent t el 'BlurTag
     , HasDomEvent t el 'FocusTag
     , JS.IsElement rawEl
     )
  -- Return a tuple here so there is a bit more flexibility for what
  -- can be returned from your input widget.
  => (cfg -> m (el,a))
  -> ((el,a) -> rawEl)
  -> ((el,a) -> m (Event t PopoverState))
  -> cfg
  -> m (el,a)
uiInputWithPopover body getStateBorderTarget mkMsg cfg = divClass "popover" $ do
  let
    popoverBlurCls = \case
      PopoverState_Error _ -> Just "popover__error-state"
      PopoverState_Warning _ -> Just "popover__warning-state"
      PopoverState_Disabled -> Nothing

    popoverDiv :: Map Text Text -> m ()
    popoverDiv attrs = elAttr "div" attrs blank

    popoverHiddenAttrs = "class" =: "popover__message"

    popoverAttrs cls d =
      "class" =: ("popover__message popover__display " <> cls) <>
      "data-tip" =: d

    popoverToAttrs = \case
      PopoverState_Error m -> popoverAttrs "popover__error" m
      PopoverState_Warning m -> popoverAttrs "popover__warning" m
      PopoverState_Disabled -> popoverHiddenAttrs

    pushClass :: JS.IsElement e => Text -> e -> JSM ()
    pushClass cls = JS.getClassList >=> flip JS.add [cls]

    dropClass :: JS.IsElement e => Text -> e -> JSM ()
    dropClass cls = JS.getClassList >=> flip JS.remove [cls]

    onShift f e (popoverBlurCls -> Just cls) = liftJSM $ f cls e
    onShift _ _ _ = pure ()

    popoverIcon cls =
      elClass "i" ("fa fa-warning popover__icon " <> cls) blank

  a <- body cfg

  onMsg <- mkMsg a
  dPopState <- holdDyn PopoverState_Disabled onMsg

  _ <- dyn_ $ ffor dPopState $ \case
    PopoverState_Disabled -> blank
    PopoverState_Error _ -> popoverIcon "popover__icon-error"
    PopoverState_Warning _ -> popoverIcon "popover__icon-warning"

  let
    borderTargetEl = getStateBorderTarget a
    onFocus = domEvent Focus $ fst a
    onBlur = domEvent Blur $ fst a

  _ <- performEvent_ $ leftmost
    [ onShift pushClass borderTargetEl <$> current dPopState <@ onBlur
    , onShift dropClass borderTargetEl <$> current dPopState <@ onFocus
    ]

  _ <- runWithReplace (divClass "popover__message" blank) $ leftmost
    [ popoverDiv . popoverToAttrs <$> onMsg
    , popoverDiv popoverHiddenAttrs <$ onBlur
    , popoverDiv . popoverToAttrs <$> current dPopState <@ onFocus
    ]

  pure a

uiEmptyState :: DomBuilder t m => Text -> Text -> m a -> m a
uiEmptyState icon title content = divClass "empty-state" $ do
  let iconAttrs = Map.fromList
        [ ("class", "empty-state__icon")
        , ("style", "background-image: url(" <> icon <>")")
        ]
  divClass "empty-state__icon-circle" $ elAttr "div" iconAttrs blank
  elClass "h1" "empty-state__title" $ text title
  divClass "empty-state__content" content
