{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-| chainweaver buttons.

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
  , uiButtonWithOnClick
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
  , receiveButton
  , sendButton
  , detailsButton
  , copyToClipboard
  -- ** Images in buttons
  , btnTextIcon
  , btnIcon
  ) where

------------------------------------------------------------------------------
import           Control.Monad.Trans         (lift)
import           Control.Lens
import           Data.Default
import           Data.Default                (def)
import           Control.Applicative         ((<|>))
import           Data.Map                    (Map)
import           Data.String                 (IsString)
import           Data.Text                   (Text)
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Dom.Core
import           Language.Javascript.JSaddle (MonadJSM)
import qualified Language.Javascript.JSaddle as JS
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as GlobalEventHandlers
import qualified GHCJS.DOM.HTMLElement as HTMLElement
import qualified GHCJS.DOM.HTMLTextAreaElement as TextArea
import qualified GHCJS.DOM.Node as Node
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
  , _uiButtonCfg_type     :: Maybe Text
  }

$(makePactLenses ''UiButtonCfgRep)

-- | Static button configuration.
type UiButtonCfg = UiButtonCfgRep Identity

-- | Dynamic button configuration.
type UiButtonDynCfg t = UiButtonCfgRep (Dynamic t)


instance Reflex t => Default (UiButtonCfgRep (Dynamic t)) where
  def = UiButtonCfg (pure False) (pure mempty) (pure Nothing) Nothing

instance Default (UiButtonCfgRep Identity) where
  def = UiButtonCfg False mempty Nothing Nothing

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

-- | Uses ghcjs-dom event handler to ensure the action runs immediately as a
-- result of the user's click. Using performEvent_ on domEvent Click can cause
-- certain actions to be unreliable - e.g. opening file dialogs programatically
uiButtonWithOnClick
  :: (StaticButtonConstraints t m, RawElement (DomBuilderSpace m) ~ DOM.Element, MonadJSM m)
  => DOM.JSM () -> UiButtonCfg -> m () -> m (Event t ())
uiButtonWithOnClick action cfg body = do
    (e, _) <- elAttr' "button" (toBtnAttrs cfg) body
    let htmlElement = DOM.HTMLElement . DOM.unElement $ _element_raw e
    _ <- DOM.liftJSM $ htmlElement `EventM.on` GlobalEventHandlers.click $ lift action
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
  . (DynamicButtonConstraints t m, MonadJSM (Performable m), PerformEvent t m)
  => UiButtonDynCfg t
  -> Behavior t Text
  -> m (Event t ())
copyButton cfg t = do
    onClick <- uiButtonDyn (cfg & uiButtonCfg_class <>~ "button_border_none") $ do
      elClass "i" "fa fa-lg fa-copy fa-fw" blank
      dyn_ $ ffor (cfg ^. uiButtonCfg_title) $ \case
        Nothing -> blank
        Just title -> text title
    _ <- copyToClipboard $ tag t onClick
    pure onClick

-- | Copy the given text to the clipboard
copyToClipboard
  :: (MonadJSM (Performable m), PerformEvent t m)
  => Event t Text
  -- ^ Text to copy to clipboard. Event must come directly from user
  -- interaction (e.g. domEvent Click), or the copy will not take place.
  -> m (Event t Bool)
  -- ^ Did the copy take place successfully?
copyToClipboard copy = performEvent $ ffor copy $ \t -> do
  doc <- DOM.currentDocumentUnchecked
  ta <- DOM.uncheckedCastTo TextArea.HTMLTextAreaElement <$> Document.createElement doc ("textarea" :: Text)
  TextArea.setValue ta t
  body <- Document.getBodyUnchecked doc
  _ <- Node.appendChild body ta
  HTMLElement.focus ta
  TextArea.select ta
  success <- Document.execCommand doc ("copy" :: Text) False (Nothing :: Maybe Text)
  _ <- Node.removeChild body ta
  pure success

addButton :: StaticButtonConstraints t m => UiButtonCfg -> m (Event t ())
addButton uCfg =
  let
    cfg = uCfg & uiButtonCfg_class %~ (<> "button_type_secondary" <> "button_size_tiny")
  in
    uiButton cfg $ imgWithAltCls "button__icon" (static @"img/plus.svg") "Add" blank

deleteButton :: StaticButtonConstraints t m => UiButtonCfg -> m (Event t ())
deleteButton uCfg =
  let
    cfg = uCfg
      & uiButtonCfg_class %~ (<> "button_type_secondary" <> "button_size_tiny" <> "button_border_none")
  in
    uiButton cfg $ imgWithAltCls "button__icon" (static @"img/bin.svg") "Delete" blank

deleteButtonNaked :: StaticButtonConstraints t m => UiButtonCfg -> m (Event t ())
deleteButtonNaked cfg =
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
    uiButtonDyn (cfg & uiButtonCfg_class <>~ "button_type_confirm") $ text msg

cancelButton :: StaticButtonConstraints t m => UiButtonCfg -> Text -> m (Event t ())
cancelButton cfg msg =
    uiButton (cfg & uiButtonCfg_class <>~ "button_type_tertiary") $ text msg

receiveButton :: StaticButtonConstraints t m => UiButtonCfg -> m (Event t ())
receiveButton cfg =
  uiButton (cfg & uiButtonCfg_class <>~ "button_type_secondary" <> "button_type_secondary") $
    btnTextIcon (static @"img/receive.svg") "Receive" blank >> text "Receive"

detailsButton :: StaticButtonConstraints t m => UiButtonCfg -> m (Event t ())
detailsButton cfg =
  uiButton (cfg & uiButtonCfg_class <>~ "button_type_secondary" <> "button_type_secondary") $
    btnIcon (static @"img/ellipsis.svg") "Details" blank

sendButton :: StaticButtonConstraints t m => UiButtonCfg -> m (Event t ())
sendButton cfg =
  uiButton (cfg & uiButtonCfg_class <>~ "button_type_secondary" <> "button_type_secondary") $
    btnTextIcon (static @"img/send.svg") "Send" blank >> text "Send"

-- | Create HTML element attributes from config.
toBtnAttrs :: UiButtonCfg -> Map Text Text
toBtnAttrs uCfg =
  let
    cfg = uCfg & uiButtonCfg_class %~ (<> "button")
    titleAttr = maybe mempty ("title" =:) $ _uiButtonCfg_title cfg
    disabledAttr = case _uiButtonCfg_disabled cfg of
      False -> mempty
      True  -> "disabled" =: "true"
    typeAttr = maybe mempty ("type" =:) $ _uiButtonCfg_type cfg
  in
    addToClassAttr (_uiButtonCfg_class cfg) $ titleAttr <> disabledAttr <> typeAttr


-- | Combine a Dynamic config into a single dynamic value.
toStatic :: Reflex t => UiButtonDynCfg t -> Dynamic t UiButtonCfg
toStatic s =
  UiButtonCfg
    <$> _uiButtonCfg_disabled s
    <*> _uiButtonCfg_class s
    <*> _uiButtonCfg_title s
    <*> pure (_uiButtonCfg_type s)

btnTextIcon :: DomBuilder t m => Text -> Text -> m a -> m a
btnTextIcon = imgWithAltCls "button__text-icon"

btnIcon :: DomBuilder t m => Text -> Text -> m a -> m a
btnIcon = imgWithAltCls "button__icon"
