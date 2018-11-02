{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}

{-|

Collection of widgets custom to this project that have all the project-specific
styling.

TODO In the logical conclusion this library will need whole lot more flexibility
and sophistication. Keeping things really simple for now for the sake of rapid
development.

-}

module Frontend.UI.Button where

import           Data.Text                   (Text)
import           Reflex.Dom.Core

uiButtonSimple :: MonadWidget t m => Text -> m (Event t ())
uiButtonSimple msg = do
    (e, _) <- elAttr' "button" ("class" =: "button") $ text msg
    return $ domEvent Click e
