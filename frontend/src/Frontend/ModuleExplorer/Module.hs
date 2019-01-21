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
    -- * Retrieve information about a `Module`
  , PactFunction (..)
  , HasPactFunction (..)
  , nameOfModule
  , codeOfModule
  , functionsOfModule
  , interfacesOfModule
  ) where

------------------------------------------------------------------------------
import qualified Bound
import           Control.Lens
import           Data.Text           (Text)
import qualified Pact.Compile        as Pact
import qualified Pact.Parse          as Pact
import           Pact.Types.Term     as PactTerm (Module (..), ModuleName (..))
------------------------------------------------------------------------------
import           Pact.Types.Lang     (Code (..), Def (..), DefName (..),
                                      DefType, FunType, Meta (_mDocs), Name,
                                      Term (TDef, TList, TModule))
------------------------------------------------------------------------------
import           Frontend.Foundation

-- Module utility functions:

-- | Get the `ModuleName` of a `Module`.
nameOfModule :: Lens' Module ModuleName
nameOfModule f modL = setModName <$> f modName
  where
    setModName n = case modL of
      (Interface {}) -> modL { _interfaceName = n }
      (Module {})    -> modL { _mName = n }
    modName = case modL of
      Interface {..} -> _interfaceName
      Module {..}    -> _mName

-- | Get the `ModuleName` of a `Module`.
codeOfModule :: Lens' Module Code
codeOfModule f modL = setCode <$> f code
  where
    setCode c = case modL of
      (Interface {}) -> modL { _interfaceCode = c }
      (Module {})    -> modL { _mCode = c }
    code = case modL of
      Interface {..} -> _interfaceCode
      Module {..}    -> _mCode

-- | Get the `ModuleName` of a `Module`.
interfacesOfModule :: Fold Module [ModuleName]
interfacesOfModule f modL = modL <$ f interfaces
  where
    interfaces = case modL of
      Interface {..} -> []
      Module {..}    -> _mInterfaces

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
functionsOfModule :: Module -> [PactFunction]
functionsOfModule m =
  case Pact.compileExps Pact.mkEmptyInfo <$> Pact.parseExprs (_unCode $ m ^. codeOfModule) of
    Right (Right terms) -> concatMap getFunctions terms
    _                   -> []

-- | Get the top level functions from a 'Term'.
getFunctions :: Term Name -> [PactFunction]
getFunctions (TModule _ body _) = getFunctions $ Bound.instantiate undefined body
getFunctions (TDef (Def (DefName name) moduleName defType funType _body meta _) _) =
  [PactFunction moduleName name defType (_mDocs meta) funType]
getFunctions (TList list1 _ _) = getFunctions =<< list1
getFunctions _ = []
