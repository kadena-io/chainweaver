{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.UI.IconGrid where

import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Reflex.Dom.Core
import Obelisk.Generated.Static

data IconGridCellConfig = IconGridCellConfig
  { _iconGridCellConfig_title :: Text
  , _iconGridCellConfig_iconUrl :: Text
  , _iconGridCellConfig_desc :: Maybe Text
  }

iconGridLaunchLink :: (DomBuilder t m) => Text -> IconGridCellConfig -> m ()
iconGridLaunchLink href config =
  elAttr "a" ("class" =: "icon-grid__cell" <> "href" =: href <> "target" =: "_blank") $
    iconGridCell' config True

iconGridCell :: (DomBuilder t m) => IconGridCellConfig -> m (Event t ())
iconGridCell config = do
  (elt, _) <- elAttr' "div" ("class" =: "icon-grid__cell") $ iconGridCell' config False
  pure $ domEvent Click elt

iconGridCell' :: (DomBuilder t m) => IconGridCellConfig -> Bool -> m ()
iconGridCell' config hasLaunch = do
  elAttr "div"
    (  "class" =: "icon-grid__cell-icon"
    <> "style" =: ("background-image: url(" <> _iconGridCellConfig_iconUrl config <>")")
    )
    blank
  elClass "div" "icon-grid__cell-header" $ do
    when hasLaunch $
      elAttr "img" ("src" =: (static @"img/launch.svg") <> "class" =: "icon-grid__cell-launch") blank
    elClass "span" "icon-grid__cell-title" $ text (_iconGridCellConfig_title config)

  traverse_ (elClass "div" "icon-grid__cell-desc" . text) (_iconGridCellConfig_desc config)
