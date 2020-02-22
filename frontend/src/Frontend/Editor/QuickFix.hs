{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Provide QuickFixes for certain kinds of errors.
--
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.Editor.QuickFix
  ( QuickFix (..)
  , _QuickFix_UnreadableKeyset
  , _QuickFix_UndefinedKeyset
  , makeQuickFixes
  , makeQuickFix
  ) where

------------------------------------------------------------------------------
import           Control.Applicative        ((<|>))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MP
import           Control.Monad        (void)
------------------------------------------------------------------------------
import           Frontend.Editor.Annotation
import           Frontend.Foundation


-- | Errors that can be quickly fixed by the user by pressing a button.
data QuickFix
  = QuickFix_UnreadableKeyset Text
    -- ^ Keyset was not available in message - add it.
  | QuickFix_UndefinedKeyset Text
    -- ^ Keyset was used without being defined
  deriving (Generic, Show, Read, Eq, Ord)

makePactPrisms ''QuickFix

-- | Create `QuickFix`s from `Annotation`s.
makeQuickFixes :: [Annotation] -> [QuickFix]
makeQuickFixes = mapMaybe makeQuickFix

-- | Make a quick fix from an annotation, if possible.
makeQuickFix :: Annotation -> Maybe QuickFix
makeQuickFix =
  MP.parseMaybe (MP.try unreadableKeysetParser <|> undefinedKeysetParser)
  . _annotation_msg

-- | Parser for `QuickFix_UnreadableKeyset`.
unreadableKeysetParser :: MP.Parsec Void Text QuickFix
unreadableKeysetParser = do
    void $ MP.string "No such key in message: "
    QuickFix_UnreadableKeyset <$> parseString

-- | Parser for `QuickFix_UndefinedKeyset`.
undefinedKeysetParser :: MP.Parsec Void Text QuickFix
undefinedKeysetParser = do
    void $ MP.string "No such keyset: "
    QuickFix_UndefinedKeyset <$> parseString



-- | Parse a string, that is some text enclosed in quotes.
--
--   The returned text will be without the enclosing quotes.
parseString :: MP.Parsec Void Text Text
parseString = do
    optionalQuote
    -- This is new too, parsing error messages really sucks, where is my type safety? *sad*
    optionalChar '\''
    str <- parseInnerString
    optionalQuote
    pure str
  where
    optionalChar c = (void $ MP.try $ MP.char c) <|> pure ()
    -- This used to be quoted, it is not anymore. So let's accept both for maximum robustness.
    optionalQuote = optionalChar '\"'


parseInnerString :: MP.Parsec Void Text Text
parseInnerString = do
  r <- MP.takeWhile1P Nothing (/= '"')
  -- T.last is fine here because of takeWhile1P!
  if T.last r == '\\' -- Ok that wasn't really the end of it ...
  then do
    void $ MP.char '\"'
    ((r <> "\"") <>) <$> parseInnerString
  else
    pure r

-- Instances

instance Semigroup QuickFix where
   a <> _ = a
