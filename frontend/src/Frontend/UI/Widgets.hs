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
    backButton
  , refreshButton
  , confirmButton
  , cancelButton
  , openButton
  , viewButton
  , callButton
  , deleteButton
  , module Frontend.UI.Button
  -- * Other widgets
  , uiInputElement
  , uiDropdown
  , uiSelectElement
  , validatedInputWithButton
  , signingKeysWidget
    -- * Helper widgets
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
import qualified Data.Map                    as Map
import           Data.Map.Strict             (Map)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Language.Javascript.JSaddle (PToJSVal, call, eval, js0,
                                              liftJSM, obj, pToJSVal)
import           Obelisk.Generated.Static
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Dom.Core
------------------------------------------------------------------------------
import           Frontend.Foundation
import           Frontend.UI.Button
import           Frontend.UI.Icon
import           Frontend.UI.Widgets.Helpers (imgWithAlt, setFocus, setFocusOn,
                                              setFocusOnSelected, tabPane, tabPane', makeClickable)
import           Frontend.Wallet             (HasWallet (..), KeyName, KeyPair,
                                              Wallet)
------------------------------------------------------------------------------



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
  
-- | reflex-dom `inputElement` with pact-web default styling:
uiInputElement
  :: DomBuilder t m
  => InputElementConfig er t (DomBuilderSpace m)
  -> m (InputElement er (DomBuilderSpace m) t)
uiInputElement cfg = inputElement $ cfg & initialAttributes %~ addToClassAttr "input"


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

-- | Widget for selection of signing keys.
signingKeysWidget
  :: forall t m. MonadWidget t m
  => Wallet t
  -> m (Dynamic t (Set KeyName))
signingKeysWidget aWallet = do
  let keyMap = aWallet ^. wallet_keys
      tableAttrs = 
        "style" =: "table-layout: fixed; width: 100%" <> "class" =: "table"
  boxValues <- elAttr "table" tableAttrs $ do
    el "thead" $ elClass "tr" "table__row" $ do
      elClass "th" "table__heading" $ text "Key Name"
      elClass "th" "table__heading" $ text ""
    el "tbody" $ listWithKey keyMap $ \name key -> signingItem (name, key)
  dyn_ $ ffor keyMap $ \keys -> when (Map.null keys) $ text "No keys ..."
  return $ do -- The Dynamic monad
    m :: Map KeyName (Dynamic t Bool) <- boxValues
    ps <- traverse (\(k,v) -> (k,) <$> v) $ Map.toList m
    return $ Set.fromList $ map fst $ filter snd ps


------------------------------------------------------------------------------
-- | Display a key as list item together with it's name.
signingItem
  :: MonadWidget t m
  => (Text, Dynamic t KeyPair)
  -> m (Dynamic t Bool)
signingItem (n, _) = do
    elClass "tr" "table__row" $ do
      el "td" $ text n
      box <- elClass "td" "signing-selector__check-box" $ checkbox False $ def
      pure (value box)

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
      (onClick,a1) <- elClass "h2" "accordion__header" $ do
        b <- uiButton (def & uiButtonCfg_class .~ "accordion__toggle-button button_type_secondary") $
          imgWithAlt (static @"img/arrow-down.svg") "Expand" blank
        r <- title
        pure (b, r)
      b1 <- divClass "accordion__content" inner
      return (onClick, (a1, b1))
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
  => Dynamic t Int  -- ^ Current page
  -> Dynamic t Int  -- ^ Total number of pages
  -> m (Event t Int)
paginationWidget currentPage totalPages = do
    let
      pageButton okay i =
        uiButtonDyn (btnCfgTertiary & uiButtonCfg_disabled .~ fmap not okay) $
          elClass "i" ("fa " <> i) blank

      canGoFirst = (> 1) <$> currentPage
    first <- pageButton canGoFirst "fa-angle-double-left"
    prev <-  pageButton canGoFirst "fa-angle-left"
    void $ elClass "div" "page-count" $ elClass "span" "page-count-text" $ do
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
