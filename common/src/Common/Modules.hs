{-# LANGUAGE OverloadedStrings #-}
module Common.Modules where

import qualified Data.Attoparsec.Text as AP
import Data.Text               (Text)
import Pact.Types.Parser       (style)
import Pact.Types.Pretty       (renderCompactText)
import Pact.Types.Names        (ModuleName(..), NamespaceName(..))
import Text.Parser.Combinators (eof, optional, (<?>))
import Text.Parser.Token       (TokenParsing, dot, ident)

moduleNameParser :: (TokenParsing m, Monad m) => m ModuleName
moduleNameParser = do
  a <- ident style
  b <- optional (dot *> ident style)
  case b of
    Nothing -> return (ModuleName a Nothing) <?> "module name"
    Just b' -> return (ModuleName b' (Just . NamespaceName $ a)) <?> "namespaced module name"

parseModuleName :: Text -> Either String ModuleName
parseModuleName = AP.parseOnly (moduleNameParser <* eof)

quotedFullName :: ModuleName -> Text
quotedFullName m = "\"" <> renderCompactText m <> "\""
