{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Monad.Logger
  ( 
    LogSource
  , LogLevel (..)
  , LogStr(..)  -- Re-export from fast-logger
  , toLogStr    -- Re-export from fast-logger
  , fromLogStr  -- Re-export from fast-logger
  , liftLoc
  , defaultLoc
  , defaultLogStr
  ) where

import qualified Data.ByteString.Char8 as S8
import Data.Monoid (mappend, mempty)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH.Syntax (Lift (lift), Q, Exp, Loc (..), qLocation)
import System.Log.FastLogger

type LogSource = Text

data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError | LevelOther Text
    deriving (Eq, Prelude.Show, Prelude.Read, Ord)

instance Lift LogLevel where
    lift LevelDebug = [|LevelDebug|]
    lift LevelInfo  = [|LevelInfo|]
    lift LevelWarn  = [|LevelWarn|]
    lift LevelError = [|LevelError|]
    lift (LevelOther x) = [|LevelOther $ T.pack $(lift $ T.unpack x)|]

liftLoc :: Loc -> Q Exp
liftLoc (Loc a b c (d1, d2) (e1, e2)) = [|Loc
    $(lift a)
    $(lift b)
    $(lift c)
    ($(lift d1), $(lift d2))
    ($(lift e1), $(lift e2))
    |]

defaultLoc :: Loc
defaultLoc = Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)

defaultLogLevelStr :: LogLevel -> LogStr
defaultLogLevelStr level = case level of
    LevelOther t -> toLogStr t
    _            -> toLogStr $ S8.pack $ drop 5 $ show level

defaultLogStr :: Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> LogStr
defaultLogStr loc src level msg =
    "[" `mappend` defaultLogLevelStr level `mappend`
    (if T.null src
        then mempty
        else "#" `mappend` toLogStr src) `mappend`
    "] " `mappend`
    msg `mappend`
    (if isDefaultLoc loc
        then "\n"
        else
            " @(" `mappend`
            toLogStr (S8.pack fileLocStr) `mappend`
            ")\n")
  where
    -- taken from file-location package
    -- turn the TH Loc loaction information into a human readable string
    -- leaving out the loc_end parameter
    fileLocStr = (loc_package loc) ++ ':' : (loc_module loc) ++
      ' ' : (loc_filename loc) ++ ':' : (line loc) ++ ':' : (char loc)
      where
        line = show . fst . loc_start
        char = show . snd . loc_start

isDefaultLoc :: Loc -> Bool
isDefaultLoc (Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)) = True
isDefaultLoc _ = False
