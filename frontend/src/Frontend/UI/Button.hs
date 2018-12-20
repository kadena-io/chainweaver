{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds       #-}

{-|

Collection of widgets custom to this project that have all the project-specific
styling.

TODO In the logical conclusion this library will need whole lot more flexibility
and sophistication. Keeping things really simple for now for the sake of rapid
development.

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
  , btnCfgTertiary
    -- * Primitives
  , uiButton
  , uiButtonDyn
    -- ** Specialized buttons 
  , backButton
  , deleteButton
  , openButton
  , viewButton
  , callButton
  , refreshButton
  , confirmButton
  , cancelButton
  ) where

------------------------------------------------------------------------------
import           Data.Default
import           Data.Default        (def)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Reflex.Dom.Core
import           Control.Lens
import           Reflex.Dom.Contrib.CssClass
import           Data.String (IsString)
------------------------------------------------------------------------------
import           Obelisk.Generated.Static
------------------------------------------------------------------------------
import           Frontend.Foundation (makePactLenses, ReflexValue (..))
import           Frontend.UI.Widgets.Helpers (imgWithAlt)

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

btnCfgTertiary
  :: (Default (UiButtonCfgRep f), IsString (ReflexValue f CssClass))
  => UiButtonCfgRep f
btnCfgTertiary = def & uiButtonCfg_class .~ "button_type_tertiary"

uiButton :: MonadWidget t m => UiButtonCfg -> m () -> m (Event t ())
uiButton cfg body = do
    (e, _) <- elAttr' "button" (toBtnAttrs cfg) body
    pure $ domEvent Click e

uiButtonDyn :: MonadWidget t m => UiButtonDynCfg t -> m () -> m (Event t ())
uiButtonDyn cfg body = do
    (e, _) <- elDynAttr' "button" (toBtnAttrs <$> toStatic cfg) body
    pure $ domEvent Click e


-- Specialized buttons:


-- | Button for "going back" action.
backButton :: MonadWidget t m => m (Event t ())
backButton = -- uiIcon "fas fa-chevron-left" $ def & iconConfig_size .~ Just IconLG
  uiButton def $ imgWithAlt (static @"img/left_arrow.svg") "Go back" blank

deleteButton :: MonadWidget t m => m (Event t ())
deleteButton = -- uiIcon "fas fa-chevron-left" $ def & iconConfig_size .~ Just IconLG
  uiButton def $ imgWithAlt (static @"img/X.svg") "Delete" blank

-- | Button that loads something into the Editor.
openButton :: MonadWidget t m => m (Event t ())
openButton =
  uiButton def $ imgWithAlt (static @"img/open.svg") "Open" blank >> text "Open"

-- | Button that loads something into the Editor.
viewButton :: MonadWidget t m => m (Event t ())
viewButton =
  uiButton def $ imgWithAlt (static @"img/view.svg") "View" blank >> text "View"

callButton :: MonadWidget t m => m (Event t ())
callButton =
  uiButton def $ imgWithAlt (static @"img/call.svg") "Call" blank >> text "Call"

-- | Button that triggers a refresh/reload of something.
refreshButton :: MonadWidget t m => m (Event t ())
refreshButton =
  uiButton btnCfgTertiary $ elClass "i" "fa fa-lg fa-refresh" blank

confirmButton :: MonadWidget t m => UiButtonDynCfg t -> Text -> m (Event t ())
confirmButton cfg msg =
    uiButtonDyn (cfg & uiButtonCfg_class .~ "confirm-button") $ text msg

cancelButton :: MonadWidget t m => UiButtonCfg -> Text -> m (Event t ())
cancelButton cfg msg =
    uiButton (cfg & uiButtonCfg_class .~ "cancel-button") $ text msg


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

