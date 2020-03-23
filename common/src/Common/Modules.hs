module Common.Modules where

import Data.Text         (Text)
import Pact.Types.Pretty (renderCompactText)
import Pact.Types.Term   (ModuleName)

quotedFullName :: ModuleName -> Text
quotedFullName m = "\"" <> renderCompactText m <> "\""
