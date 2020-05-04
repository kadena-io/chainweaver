module Frontend.UI.Common where

------------------------------------------------------------------------------
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict as Map
import           Data.String
import           Data.Text                   (Text)
import           Reflex.Dom.Contrib.CssClass
------------------------------------------------------------------------------

-- | Attributes which will turn off all autocomplete/autofill/autocorrect
-- functions, including the OS-level suggestions on macOS.
noAutofillAttrs :: (Ord attr, IsString attr) => Map attr Text
noAutofillAttrs = Map.fromList
  [ ("autocomplete", "off")
  , ("autocorrect", "off")
  , ("autocapitalize", "off")
  , ("spellcheck", "false")
  ]

-- | Factored out input class modifier, so we can keep it in sync.
addInputElementCls :: (Ord attr, IsString attr) => Map attr Text -> Map attr Text
addInputElementCls = addToClassAttr "input"

addNoAutofillAttrs :: (Ord attr, IsString attr) => Map attr Text -> Map attr Text
addNoAutofillAttrs = (noAutofillAttrs <>)
