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
  ) where

------------------------------------------------------------------------------
------------------------------------------------------------------------------
import qualified Bound
import           Data.Text                (Text)
import           Obelisk.Generated.Static
import qualified Pact.Compile             as Pact
import qualified Pact.Parse               as Pact
import           Pact.Types.Lang          (Code (..), DefType, FunType,
                                           ModuleName, Name, Term)
import           Pact.Types.Term          as PactTerm (Module (..),
                                                       ModuleName (..))
import           Reflex.Dom.Core          (HasJSContext, MonadHold, PostBuild,
                                           XhrResponse (..), newXMLHttpRequest,
                                           xhrRequest)
------------------------------------------------------------------------------
import           Pact.Types.Lang          (Code (..), Def (..), DefName (..),
                                           DefType, FunType, Meta (_mDocs),
                                           Module (..), ModuleName, Name,
                                           Term (TDef, TList, TModule))
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Foundation
import           Frontend.Wallet

-- Module utility functions:

-- | Get the `ModuleName` of a `Module`.
nameOfModule :: Module -> ModuleName
nameOfModule = \case
  Interface {..} -> _interfaceName
  Module {..}    -> _mName

-- | Get the `ModuleName` of a `Module`.
codeOfModule :: Module -> Code
codeOfModule = \case
  Interface {..} -> _interfaceCode
  Module {..}    -> _mCode

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
  case Pact.compileExps Pact.mkEmptyInfo <$> Pact.parseExprs (_unCode $ codeOfModule m) of
    Right (Right terms) -> concatMap getFunctions terms
    _                   -> []

-- | Get the top level functions from a 'Term'.
getFunctions :: Term Name -> [PactFunction]
getFunctions (TModule _ body _) = getFunctions $ Bound.instantiate undefined body
getFunctions (TDef (Def (DefName name) moduleName defType funType _body meta _) _) =
  [PactFunction moduleName name defType (_mDocs meta) funType]
getFunctions (TList list1 _ _) = getFunctions =<< list1
getFunctions _ = []
