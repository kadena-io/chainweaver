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
------------------------------------------------------------------------------

uiButtonSimple :: MonadWidget t m => Text -> m (Event t ())
uiButtonSimple msg = do
    (e, _) <- el' "button" $ text msg
    return $ domEvent Click e

uiButton :: MonadWidget t m => m a -> m (Event t (), a)
uiButton body = do
    (e, a) <- el' "button" body
    return (domEvent Click e, a)

-- TODO Here for the fontawesome 5.5, but had problems getting it working
--data IconStyle = IconRegular | IconSolid | IconLight
--  deriving (Eq,Ord,Show,Read,Enum,Bounded)
--
--styleFaText :: IconStyle -> Text
--styleFaText IconRegular = "far"
--styleFaText IconSolid = "fas"
--styleFaText IconLight = "fal"

data IconSize
    = IconXS
    | IconSM
    | IconLG
    | Icon2x
    | Icon3x
    | Icon5x
    | Icon7x
    | Icon10x
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

sizeFaText :: IconSize -> Text
sizeFaText IconXS = "fa-xs"
sizeFaText IconSM = "fa-sm"
sizeFaText IconLG = "fa-lg"
sizeFaText Icon2x = "fa-2x"
sizeFaText Icon3x = "fa-3x"
sizeFaText Icon5x = "fa-5x"
sizeFaText Icon7x = "fa-7x"
sizeFaText Icon10x = "fa-10x"

data IconConfig = IconConfig
    { _iconConfig_size       :: Maybe IconSize
--    , _iconConfig_style      :: IconStyle
    , _iconConfig_attrs :: Map Text Text
    } deriving (Eq,Ord,Show,Read)

makeLenses ''IconConfig

instance Default IconConfig where
  def = IconConfig Nothing mempty

------------------------------------------------------------------------------
uiIcon
  :: MonadWidget t m
  => Text
  -- ^ The icon name, fa-key, fa-trash, etc
  -> IconConfig
  -- ^ Other icon config options
  -> m (Event t ())
uiIcon i cfg = do
    let klass = singleClass "fa" -- (styleFaText $ _iconConfig_style cfg)
             <> singleClass i
             <> maybe mempty (singleClass . sizeFaText) (_iconConfig_size cfg)
    let attrs = addToClassAttr klass (_iconConfig_attrs cfg)
    (e,_) <- elAttr' "i" attrs blank
    return $ domEvent Click e
