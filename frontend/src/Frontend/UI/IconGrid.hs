{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.UI.IconGrid where

import Control.Lens ((<>~))
import Control.Monad (when, void)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Reflex.Dom.Core
import Obelisk.Generated.Static

import Frontend.UI.Button (uiButtonCfg_class, uiButton)

data IconGridCellConfig = IconGridCellConfig
  { _iconGridCellConfig_title :: Text
  , _iconGridCellConfig_iconUrl :: Text
  , _iconGridCellConfig_desc :: Maybe Text
  }

iconGridLaunchLink' :: DomBuilder t m => Text -> Bool -> IconGridCellConfig -> m ()
iconGridLaunchLink' href inNewWindow config =
  elAttr "a" (withTarget inNewWindow $ "class" =: "icon-grid__cell" <> "href" =: href) $
    void $ iconGridCell' config True
  where
    withTarget True = mappend ("target" =: "_blank")
    withTarget _ = id

iconGridLaunchLink :: DomBuilder t m => Text -> IconGridCellConfig -> m ()
iconGridLaunchLink href = iconGridLaunchLink' href True

iconGridCell :: DomBuilder t m => IconGridCellConfig -> m (Event t ())
iconGridCell config = do
  (cellEl, onBtnClick) <- elAttr' "div" ("class" =: "icon-grid__cell") $
    iconGridCell' config False
  pure $ onBtnClick <> domEvent Click cellEl

iconGridCell' :: DomBuilder t m => IconGridCellConfig -> Bool -> m (Event t ())
iconGridCell' config hasLaunch = do
  elAttr "div"
    (  "class" =: "icon-grid__cell-icon"
    <> "style" =: ("background-image: url(" <> _iconGridCellConfig_iconUrl config <>")")
    )
    blank

  evt <- uiButton (def & uiButtonCfg_class <>~ "button_type_tertiary" <> "icon-grid__cell-header") $ do
    when hasLaunch $
      elAttr "img" ("src" =: $(static "img/launch.svg") <> "class" =: "icon-grid__cell-launch") blank
    elClass "span" "icon-grid__cell-title" $ text (_iconGridCellConfig_title config)

  traverse_ (elClass "div" "icon-grid__cell-desc" . text) (_iconGridCellConfig_desc config)
  pure evt
