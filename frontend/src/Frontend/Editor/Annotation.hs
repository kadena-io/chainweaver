{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Annotations for our `Editor`.
--
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.Editor.Annotation
  ( -- * Types and Classes
    AnnoType (..)
  , Annotation (..)
  , AnnotationSource (..)
    -- * Parsers
  , annoParser
  , annoFallbackParser
  , annoJsonParser
  ) where

------------------------------------------------------------------------------
import           Control.Applicative  ((<|>))
import           Control.Monad        (void)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Void            (Void)
import           Text.Read            (readMaybe)
import qualified Text.Megaparsec      as MP
import qualified Text.Megaparsec.Char as MP
------------------------------------------------------------------------------

-- | Annotation type.
data AnnoType = AnnoType_Warning | AnnoType_Error
  deriving (Eq, Ord)

data AnnotationSource = AnnotationSource_Pact | AnnotationSource_Json
  deriving (Eq, Ord, Show)

instance Show AnnoType where
  show = \case
    AnnoType_Warning -> "warning"
    AnnoType_Error   -> "error"

-- | Annotation to report warning/errors to the user.
data Annotation = Annotation
  { _annotation_type   :: AnnoType -- ^ Is it a warning or an error?
  , _annotation_msg    :: Text -- ^ The message to report.
  , _annotation_source :: AnnotationSource
  , _annotation_pos    :: Maybe (Int, Int) -- row, column
  }
  deriving (Show, Eq, Ord)

annoParser :: Text -> Maybe [Annotation]
annoParser = MP.parseMaybe pactErrorParser

-- | Some errors have no line number for some reason: fallback with Nothing.
annoFallbackParser :: Text -> [Annotation]
annoFallbackParser msg =
  case annoParser msg of
    Nothing ->
      [ Annotation
        { _annotation_type = AnnoType_Error
        , _annotation_msg = msg
        , _annotation_source = AnnotationSource_Pact
        , _annotation_pos = Nothing
        }
      ]
    Just a -> a

--TODO: fix line numbers
--TODO: collapse with json warnings
annoJsonParser :: Text -> Annotation
annoJsonParser msg = Annotation
  { _annotation_type = AnnoType_Error
  , _annotation_msg = msg
  , _annotation_source = AnnotationSource_Json
  , _annotation_pos = Nothing
  }

pactErrorParser :: MP.Parsec Void Text [Annotation]
pactErrorParser = MP.many $ do
    dropOptionalQuote
    startErrorParser
    line <- digitsP
    colonP
    column <- digitsP
    colonP
    MP.space
    annoType <- MP.withRecovery (const $ pure AnnoType_Error) $ do
      void $ MP.string' "warning:"
      pure AnnoType_Warning

    -- Get rid of trailing quote as well:
    msg <- T.dropWhileEnd (== '"') <$> msgParser

    pure $ Annotation
      { _annotation_type = annoType
      , _annotation_msg = msg
      , _annotation_source = AnnotationSource_Pact
      , _annotation_pos = Just (max line 1, max column 1) -- Some errors have linenumber 0 which is invalid.
      }
  where
    digitsP :: MP.Parsec Void Text Int
    digitsP = maybe (fail "pactErrorParser: digitsP: no parse") pure . readMaybe =<< MP.some MP.digitChar

    dropOptionalQuote = MP.withRecovery (const $ pure ()) (void $ MP.char '\"')

-- | Parse the actual error message.
msgParser :: MP.Parsec Void Text Text
msgParser = linesParser <|> restParser
  where
    restParser = do
      MP.notFollowedBy startErrorParser
      MP.takeRest

    linesParser = fmap T.unlines . MP.try . MP.some $ lineParser

    lineParser = do
      MP.notFollowedBy startErrorParser
      l <- MP.takeWhileP Nothing (/= '\n')
      void $ MP.newline
      pure l

-- | Error/warning messages start this way:
startErrorParser :: MP.Parsec Void Text ()
startErrorParser = do
    MP.space
    void $ MP.many (MP.string "Property proven valid" >> MP.space)
    void $ MP.oneOf ['<', '('] -- Until now we found messages with '<' and some with '('.
    void $ MP.string "interactive"
    void $ MP.oneOf ['>', ')']
    colonP
    pure ()

colonP :: MP.Parsec Void Text ()
colonP = void $ MP.char ':'
