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

-- | Annotations for our `Editor`.
--
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.Editor.Annotation
  ( -- * Types and Classes
    AnnoType (..)
  , Annotation (..)
    -- * Parsers
  , annoParser
  , annoFallbackParser
  ) where

------------------------------------------------------------------------------
import           Control.Applicative  ((<|>))
import           Control.Monad        (void)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Void            (Void)
import qualified Text.Megaparsec      as MP
import qualified Text.Megaparsec.Char as MP

------------------------------------------------------------------------------

-- | Annotation type.
data AnnoType = AnnoType_Warning | AnnoType_Error

instance Show AnnoType where
  show = \case
    AnnoType_Warning -> "warning"
    AnnoType_Error   -> "error"

-- | Annotation to report warning/errors to the user.
data Annotation = Annotation
  { _annotation_type   :: AnnoType -- ^ Is it a warning or an error?
  , _annotation_msg    :: Text -- ^ The message to report.
  , _annotation_line   :: Int -- ^ What line to put the annotation to.
  , _annotation_column :: Int -- ^ What column.
  }
  deriving Show

annoParser :: Text -> Maybe [Annotation]
annoParser = MP.parseMaybe pactErrorParser

-- | Some errors have no line number for some reason: fallback with dummy line number.
annoFallbackParser :: Text -> [Annotation]
annoFallbackParser msg =
  case annoParser msg of
    Nothing ->
      [ Annotation
        { _annotation_type = AnnoType_Error
        , _annotation_msg = msg
        , _annotation_line = 1
        , _annotation_column = 0
        }
      ]
    Just a -> a

pactErrorParser :: MP.Parsec Void Text [Annotation]
pactErrorParser = MP.many $ do
    startErrorParser
    line <- digitsP
    colonP
    column <- digitsP
    colonP
    MP.space
    annoType <- MP.withRecovery (const $ pure AnnoType_Error) $ do
      void $ MP.string' "warning:"
      pure AnnoType_Warning

    msg <- msgParser

    pure $ Annotation
      { _annotation_type = annoType
      , _annotation_msg = msg
      , _annotation_line = max line 1 -- Some errors have linenumber 0 which is invalid.
      , _annotation_column = max column 1
      }
  where
    digitsP :: MP.Parsec Void Text Int
    digitsP = read <$> MP.some MP.digitChar

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
