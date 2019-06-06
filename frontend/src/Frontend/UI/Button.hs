{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

{-| pact-web buttons.

-}

module Frontend.UI.Button
  ( -- * Config
    -- ** Types
    UiButtonCfgRep (..)
  , UiButtonCfg
  , UiButtonDynCfg
  , HasUiButtonCfgRep (..)
    -- ** Predefined Configs
  , btnCfgPrimary
  , btnCfgSecondary
  , btnCfgTertiary
    -- * Primitives
  , uiButton
  , uiButtonDyn
    -- ** Specialized buttons
  , homeButton
  , backButton
  , copyButton
  , deleteButton
  , addButton
  , signoutButton
  , deleteButtonNaked
  , openButton
  , cogButton
  , viewButton
  , callButton
  , refreshButton
  , confirmButton
  , cancelButton
  -- ** Images in buttons
  , btnTextIcon
  , btnIcon
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Default
import           Data.Default                (def)
import           Control.Applicative         ((<|>))
import           Data.Map                    (Map)
import           Data.String                 (IsString)
import           Data.Text                   (Text)
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Dom.Core
import           Language.Javascript.JSaddle (MonadJSM, liftJSM, JSVal)
import qualified Language.Javascript.JSaddle as JS
import qualified Data.Text as T
import           Control.Monad (void)
import qualified GHCJS.DOM.Types                   as DOM
------------------------------------------------------------------------------
import           Obelisk.Generated.Static
------------------------------------------------------------------------------
import           Frontend.Foundation         (ReflexValue, makePactLenses)
import           Frontend.UI.Widgets.Helpers (imgWithAltCls)


-- | Configuration for uiButton.
data UiButtonCfgRep f = UiButtonCfg
  { _uiButtonCfg_disabled :: ReflexValue f Bool
    -- ^ Whether or not the button should be clickable by the user.
  , _uiButtonCfg_class    :: ReflexValue f CssClass
  , _uiButtonCfg_title    :: ReflexValue f (Maybe Text)
  }

$(makePactLenses ''UiButtonCfgRep)

-- | Static button configuration.
type UiButtonCfg = UiButtonCfgRep Identity

-- | Dynamic button configuration.
type UiButtonDynCfg t = UiButtonCfgRep (Dynamic t)


instance Reflex t => Default (UiButtonCfgRep (Dynamic t)) where
  def = UiButtonCfg (pure False) (pure mempty) (pure Nothing)

instance Default (UiButtonCfgRep Identity) where
  def = UiButtonCfg False mempty Nothing

-- Primitives:

btnCfgPrimary
  :: (Default (UiButtonCfgRep f), IsString (ReflexValue f CssClass))
  => UiButtonCfgRep f
btnCfgPrimary = def & uiButtonCfg_class .~ "button_type_primary"

btnCfgSecondary
  :: (Default (UiButtonCfgRep f), IsString (ReflexValue f CssClass))
  => UiButtonCfgRep f
btnCfgSecondary = def & uiButtonCfg_class .~ "button_type_secondary"

btnCfgTertiary
  :: (Default (UiButtonCfgRep f), IsString (ReflexValue f CssClass))
  => UiButtonCfgRep f
btnCfgTertiary = def & uiButtonCfg_class .~ "button_type_tertiary"

-- | Constraints needed for a static button
type StaticButtonConstraints t m =
  ( DomBuilder t m
  , HasDomEvent t (Element EventResult (DomBuilderSpace m) t) 'ClickTag
  )

-- | Constraints needed for a button with dynamic attributes.
type DynamicButtonConstraints t m =
  ( DomBuilder t m
  , HasDomEvent t (Element EventResult (DomBuilderSpace m) t) 'ClickTag
  , PostBuild t m
  )

uiButton
  :: StaticButtonConstraints t m
  => UiButtonCfg -> m () -> m (Event t ())
uiButton cfg body = do
    (e, _) <- elAttr' "button" (toBtnAttrs cfg) body
    pure $ domEvent Click e

uiButtonDyn
  :: DynamicButtonConstraints t m
  => UiButtonDynCfg t -> m () -> m (Event t ())
uiButtonDyn cfg body = do
    (e, _) <- elDynAttr' "button" (toBtnAttrs <$> toStatic cfg) body
    pure $ domEvent Click e


-- Specialized buttons:


-- | Button for "going back" action.
backButton :: StaticButtonConstraints t m => m (Event t ())
backButton = -- uiIcon "fas fa-chevron-left" $ def & iconConfig_size .~ Just IconLG
  uiButton def $ btnIcon (static @"img/left_arrow.svg") "Go back" blank

-- | Button for "going home" action.
homeButton :: StaticButtonConstraints t m => CssClass -> m (Event t ())
homeButton cls = -- uiIcon "fas fa-chevron-left" $ def & iconConfig_size .~ Just IconLG
  uiButton (def & uiButtonCfg_class .~ cls) $ btnIcon (static @"img/double_left_arrow.svg") "Go back" blank

-- | Copies the text content of a given node.
--
--   Probably won't work for input elements.
copyButton
  :: forall t m
  . (DynamicButtonConstraints t m, MonadJSM (Performable m), PerformEvent t m
    , RawElement (DomBuilderSpace m) ~ DOM.Element
    )
  => UiButtonDynCfg t
  -> RawElement (DomBuilderSpace m)
  -> m (Event t ())
copyButton cfg e = do
    onClick <- uiButtonDyn cfg $ text "copy"
    performEvent_ $ jsCopy e <$ onClick
    pure onClick
  where

    jsCopy :: forall m1. MonadJSM m1 => RawElement (DomBuilderSpace m) -> m1 ()
    jsCopy eL = do
      jsCopyFunc <- jsCopyVal
      void $ liftJSM $ JS.call jsCopyFunc JS.obj [DOM.unElement eL]

    jsCopyVal :: forall m1. MonadJSM m1 => m1 JSVal
    jsCopyVal = liftJSM $ JS.eval $ T.unlines
      [ "(function(e) {"
      , " try {"
      , "    var range = document.createRange();"
      , "    range.selectNodeContents(e);"
      , "    var selection = window.getSelection();"
      , "    selection.removeAllRanges();"
      , "    selection.addRange(range);"
      , "    document.execCommand('copy');"
      , " } catch(e) { console.log('Copy failed!'); return false; }"
      , "})"
      ]

addButton :: StaticButtonConstraints t m => UiButtonCfg -> m (Event t ())
addButton uCfg =
  let
    cfg = uCfg & uiButtonCfg_class %~ (<> "button_type_secondary" <> "button_size_tiny")
  in
    uiButton cfg $ imgWithAltCls "button__icon" (static @"img/plus.svg") "Add" blank

deleteButton :: StaticButtonConstraints t m => UiButtonCfg -> m (Event t ())
deleteButton uCfg =
  let
    cfg = uCfg & uiButtonCfg_class %~ (<> "button_type_secondary" <> "button_size_tiny")
  in
    uiButton cfg $ imgWithAltCls "button__icon" (static @"img/bin.svg") "Delete" blank

deleteButtonNaked :: StaticButtonConstraints t m => UiButtonCfg -> m (Event t ())
deleteButtonNaked cfg =
  {- let -}
  {-   cfg = uCfg & uiButtonCfg_class %~ (<> "button_type_secondary" <> "button_size_tiny") -}
  {- in -}
    uiButton cfg $ imgWithAltCls "button__icon" (static @"img/bin.svg") "Delete" blank

cogButton :: StaticButtonConstraints t m => UiButtonCfg -> m (Event t ())
cogButton uCfg =
  uiButton
      ( uCfg
          & uiButtonCfg_title .~ Just "Settings"
          {- & uiButtonCfg_class %~ (<> "main-header__text-icon-button") -}
      ) $ do
    elClass "span" "fa fa-lg fa-cog" blank

signoutButton :: StaticButtonConstraints t m => UiButtonCfg -> m (Event t ())
signoutButton uCfg =
  uiButton
      ( uCfg
          & uiButtonCfg_title %~ \old -> old <|> Just "Sign out"
      ) $ do
    elClass "span" "fa fa-lg fa-sign-out" blank

-- | Button that loads something into the Editor.
openButton :: StaticButtonConstraints t m => CssClass -> m (Event t ())
openButton cls =
  uiButton (def & uiButtonCfg_class .~ "button_type_secondary" <> cls) $
    btnTextIcon (static @"img/open.svg") "Open" blank >> text "Open"

-- | Button that loads something into the Editor.
viewButton :: StaticButtonConstraints t m => CssClass -> m (Event t ())
viewButton cls =
  uiButton (def & uiButtonCfg_class .~ cls <> "button_type_secondary") $
    btnTextIcon (static @"img/view.svg") "View" blank >> text "View"

callButton :: StaticButtonConstraints t m => CssClass -> m (Event t ())
callButton cls =
  uiButton (def & uiButtonCfg_class .~ "button_type_secondary" <> cls) $
    btnTextIcon (static @"img/call.svg") "Call" blank >> text "Call"

-- | Button that triggers a refresh/reload of something.
refreshButton :: StaticButtonConstraints t m => CssClass -> m (Event t ())
refreshButton cls =
  uiButton (btnCfgTertiary & uiButtonCfg_class %~ (<> cls)) $
    elClass "i" "fa fa-lg fa-refresh" blank

confirmButton :: DynamicButtonConstraints t m => UiButtonDynCfg t -> Text -> m (Event t ())
confirmButton cfg msg =
    uiButtonDyn (cfg & uiButtonCfg_class .~ "button_type_confirm") $ text msg

cancelButton :: StaticButtonConstraints t m => UiButtonCfg -> Text -> m (Event t ())
cancelButton cfg msg =
    uiButton (cfg & uiButtonCfg_class .~ "button_type_tertiary") $ text msg


-- | Create HTML element attributes from config.
toBtnAttrs :: UiButtonCfg -> Map Text Text
toBtnAttrs uCfg =
  let
    cfg = uCfg & uiButtonCfg_class %~ (<> "button")
    titleAttr = maybe mempty ("title" =:) $ _uiButtonCfg_title cfg
    disabledAttr = case _uiButtonCfg_disabled cfg of
      False -> mempty
      True  -> "disabled" =: "true"
  in
    addToClassAttr (_uiButtonCfg_class cfg) $ titleAttr <> disabledAttr


-- | Combine a Dynamic config into a single dynamic value.
toStatic :: Reflex t => UiButtonDynCfg t -> Dynamic t UiButtonCfg
toStatic s =
  UiButtonCfg
    <$> _uiButtonCfg_disabled s
    <*> _uiButtonCfg_class s
    <*> _uiButtonCfg_title s

btnTextIcon :: DomBuilder t m => Text -> Text -> m a -> m a
btnTextIcon = imgWithAltCls "button__text-icon"

btnIcon :: DomBuilder t m => Text -> Text -> m a -> m a
btnIcon = imgWithAltCls "button__icon"
