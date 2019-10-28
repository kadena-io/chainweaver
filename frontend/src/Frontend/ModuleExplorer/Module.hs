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
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Module, ModuleRef related functions and types.
--
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.ModuleExplorer.Module
  ( -- * Re-exported Module and ModuleName
    module PactTerm
    -- * Abbreviations
  , ModDef
    -- * Retrieve information about a `Module`
  , PactFunction (..)
  , HasPactFunction (..)
  , nameOfModule
  , codeOfModule
  , functionsOfModule
  , importsOfModule
  , importNamesOfModule
  , interfacesOfModule
  , isModule
  , functionsByDefType
  , functionIsCallable
  ) where

------------------------------------------------------------------------------
import qualified Bound
import           Control.Lens
import           Data.Text           (Text)
import qualified Pact.Compile        as Pact
import qualified Pact.Parse          as Pact
import           Pact.Types.Term     as PactTerm
------------------------------------------------------------------------------
import           Pact.Types.Lang     (Code (..), Def (..), DefName (..),
                                      DefType, FunType, Meta (_mDocs), Name,
                                      Term (TDef, TList, TModule))
------------------------------------------------------------------------------
import           Frontend.Foundation

-- Module utility functions:
--

-- | ModuleDef (Term Name) is quite a mouth full.
type ModDef = ModuleDef (Term Name)

-- | Get the `ModuleName` of a `Module`.
nameOfModule :: Lens' (ModuleDef g) ModuleName
nameOfModule f = \case
  MDInterface m -> MDInterface <$> interfaceName f m
  MDModule m    -> MDModule <$> mName f m

-- | Get the source code of a `Module`.
codeOfModule :: Lens' (ModuleDef g) Code
codeOfModule f = \case
  MDInterface m -> MDInterface <$> interfaceCode f m
  MDModule m    -> MDModule <$> mCode f m


-- | Get the used modules of a `Module`.
importsOfModule :: Lens' (ModuleDef g) [ Use ]
importsOfModule f = \case
  MDInterface m -> MDInterface <$> interfaceImports f m
  MDModule m    -> MDModule <$> mImports f m


-- | Get the imported `ModuleName`s directly.
--
--   `_uModuleName` gets unwrapped, in contrast to `importsOfModule` which gets
--   you the full `Use`.
importNamesOfModule :: Lens' (ModuleDef g) [ ModuleName ]
importNamesOfModule f modL = setImports <$> f imports
  where
    setImports c = case modL of
      MDInterface (m@Interface {..}) -> MDInterface $ m { _interfaceImports = zipWith setName _interfaceImports c }
      MDModule (m@Module {..})    -> MDModule $ m { _mImports = zipWith setName _mImports c }
    imports = map _uModuleName $ case modL of
      MDInterface (Interface {..}) -> _interfaceImports
      MDModule (Module {..})    -> _mImports
    setName u n = u { _uModuleName = n }

-- | Get the `ModuleName` of a `Module`.
interfacesOfModule :: Functor f => ([ModuleName] -> f b) -> ModuleDef a -> f (ModuleDef a)
interfacesOfModule f modL = modL <$ f interfaces
  where
    interfaces = case modL of
      MDInterface (Interface {..}) -> []
      MDModule (Module {..})    -> _mInterfaces

-- | Module is really a `Module` as opposed to an interface?
isModule :: (ModuleDef g) -> Bool
isModule = \case
  MDModule {} -> True
  _         -> False

-- Get hold of modules and functions:

-- | Useful data about a pact function.
data PactFunction = PactFunction
  { _pactFunction_module        :: ModuleName
  , _pactFunction_name          :: Text
  , _pactFunction_defType       :: DefType
  , _pactFunction_documentation :: Maybe Text
  , _pactFunction_type          :: FunType (Term Name)
  }
  deriving Show

makePactLenses ''PactFunction

-- | Functions of a `Module`
functionsOfModule :: ModuleDef g -> [PactFunction]
functionsOfModule m =
  case Pact.compileExps Pact.mkEmptyInfo <$> Pact.parseExprs (_unCode $ m ^. codeOfModule) of
    Right (Right terms) -> concatMap getFunctions terms
    _                   -> []


-- | Separate functions by their `DefType`.
functionsByDefType :: [PactFunction] -> [(DefType, [PactFunction])]
functionsByDefType fs = getFiltered <$> [minBound .. maxBound] <*> pure fs
  where
    getFiltered what xs = (what, filter (is what) xs)
    is what f = _pactFunction_defType f == what

-- | Whether the current "function" is actually some kind of function that can be called.
functionIsCallable :: PactFunction -> Bool
functionIsCallable f =
  case _pactFunction_defType f of
    Defcap -> False
    Defpact -> False
    Defun  -> True


-- | Get the top level functions from a 'Term'.
getFunctions :: Term Name -> [PactFunction]
getFunctions (TModule _ body _) = getFunctions $ Bound.instantiate undefined body
getFunctions (TDef (Def (DefName name) moduleName defType funType _body meta _defMeta _info) _) =
  [PactFunction moduleName name defType (_mDocs meta) funType]
getFunctions (TList list1 _ _) = getFunctions =<< toList list1
getFunctions _ = []
