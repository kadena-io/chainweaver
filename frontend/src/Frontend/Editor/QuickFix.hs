{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE ExtendedDefaultRules   #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DoAndIfThenElse        #-}

-- | Provide QuickFixes for certain kinds of errors.
--
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.Editor.QuickFix
  ( QuickFix (..)
  , _QuickFix_MissingEnvKeyset
  , _QuickFix_MissingKeyset
  , makeQuickFixes
  , makeQuickFix
  ) where

------------------------------------------------------------------------------
import           Control.Applicative        ((<|>))
import           Data.Maybe                 (mapMaybe)
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
  = QuickFix_MissingEnvKeyset Text
    -- ^ Keyset was not available in message - add it.
  | QuickFix_MissingKeyset Text
    -- ^ Keyset was used without being defined
    --
makePactPrisms ''QuickFix

-- | Create `QuickFix`s from `Annotation`s.
makeQuickFixes :: [Annotation] -> [QuickFix]
makeQuickFixes = mapMaybe makeQuickFix

-- | Make a quick fix from an annotation, if possible.
makeQuickFix :: Annotation -> Maybe QuickFix
makeQuickFix =
  MP.parseMaybe (missingEnvKeysetParser <|> missingKeysetParser)
  . _annotation_msg

-- | Parser for `QuickFix_MissingEnvKeyset`.
missingEnvKeysetParser :: MP.Parsec Void Text QuickFix
missingEnvKeysetParser = do
    void $ MP.string "No such key in message: "
    QuickFix_MissingEnvKeyset <$> parseString

-- | Parser for `QuickFix_MissingEnvKeyset`.
missingKeysetParser :: MP.Parsec Void Text QuickFix
missingKeysetParser = do
    void $ MP.string "No such keyset: "
    QuickFix_MissingKeyset <$> parseString



-- | Parse a string, that is some text enclosed in quotes.
--
--   The returned text will be without the enclosing quotes.
parseString :: MP.Parsec Void Text Text
parseString = do
    void $ MP.char '"'
    str <- parseInnerString
    void $ MP.char '"'
    pure str


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

