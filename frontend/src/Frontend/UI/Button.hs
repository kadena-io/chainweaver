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
import           Data.Default
import           Data.Default        (def)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Reflex.Dom.Core
------------------------------------------------------------------------------
import           Frontend.Foundation (makePactLenses)

-- | Configuration for uiButton.
data UiButtonCfg t = UiButtonCfg
  { _uiButtonCfg_disabled :: Dynamic t Bool
    -- ^ Whether or not the button should be clickable by the user.
  , _uiButtonCfg_class    :: Set Text
  }

$(makePactLenses ''UiButtonCfg)


instance Reflex t => Default (UiButtonCfg t) where
  def = UiButtonCfg (pure False) mempty


uiButtonSimple :: MonadWidget t m => Text -> m (Event t ())
uiButtonSimple msg = do
    (e, _) <- el' "button" $ text msg
    return $ domEvent Click e

uiButton :: MonadWidget t m => UiButtonCfg t -> m a -> m (Event t (), a)
uiButton cfg body = do
    let
      attrs = mkDisabledAttr <$> _uiButtonCfg_disabled cfg
      getCls = T.intercalate " " . Set.toList . _uiButtonCfg_class
      baseAttrs = "class" =: getCls cfg
      mkDisabledAttr = \case
        False -> baseAttrs
        True  -> baseAttrs <> "disabled" =: "true"
    (e, a) <- elDynAttr' "button" attrs body
    return (domEvent Click e, a)
