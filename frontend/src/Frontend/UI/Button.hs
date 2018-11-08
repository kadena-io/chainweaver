{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TemplateHaskell       #-}

{-|

Collection of widgets custom to this project that have all the project-specific
styling.

TODO In the logical conclusion this library will need whole lot more flexibility
and sophistication. Keeping things really simple for now for the sake of rapid
development.

-}

module Frontend.UI.Button where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Default
import           Data.Map                    (Map)
import           Data.Text                   (Text)
import           Reflex.Dom.Core
import           Reflex.Dom.Contrib.CssClass
import           Data.Default (def)
------------------------------------------------------------------------------
import Frontend.Foundation (makePactLenses)

-- | Configuration for uiButton.
data UiButtonCfg t = UiButtonCfg
  { _uiButtonCfg_disabled :: Dynamic t Bool
    -- ^ Whether or not the button should be clickable by the user.
  }

$(makePactLenses ''UiButtonCfg)

instance Reflex t => Default (UiButtonCfg t) where
  def = UiButtonCfg (pure False)


uiButtonSimple :: MonadWidget t m => Text -> m (Event t ())
uiButtonSimple msg = do
    (e, _) <- el' "button" $ text msg
    return $ domEvent Click e

uiButton :: MonadWidget t m => UiButtonCfg t -> m a -> m (Event t (), a)
uiButton cfg body = do
    let
      attrs = mkDisabledAttr <$> _uiButtonCfg_disabled cfg
      mkDisabledAttr = \case
        False -> mempty
        True  -> "disabled" =: "true"
    (e, a) <- elDynAttr' "button" attrs body
    return (domEvent Click e, a)
