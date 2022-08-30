{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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

-- | Refererencing `Module`s.
--
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.ModuleExplorer.ModuleRef
  ( -- * Re-exported Module and ModuleName
    module PactTerm
    -- * ModuleRefs
  , ModuleSource (..)
  , _ModuleSource_Deployed
  , _ModuleSource_File
  , ModuleRefV (..)
  , moduleRef_source
  , moduleRef_name
  , ModuleRef
  , DeployedModuleRef
  , FileModuleRef
    -- * Get more specialized references:
  , getDeployedModuleRef
  , getFileModuleRef
    -- * Get hold of a `Module`
  , fetchModule
    -- * Pretty printing
  , textModuleRefSource
  , textModuleRefName
  ) where

------------------------------------------------------------------------------
import           Control.Applicative             ((<|>))
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except            (throwError)
import           Data.Bifunctor                  (first)
import           Data.List.NonEmpty              (NonEmpty ((:|)))
import qualified Data.Map                        as Map
import qualified Data.Set                        as Set
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.These                      (These(This,That))
import           Data.Traversable                (for)
import           Reflex.Dom.Core                 (MonadHold)
import qualified Text.Megaparsec                 as MP
------------------------------------------------------------------------------
import qualified Data.Aeson                      as A
import           Pact.Types.Exp                  (Literal (LString))
import           Pact.Types.Info                 (Code (..))
import           Pact.Types.Lang                 (ModuleName)
import           Pact.Types.PactValue
import           Pact.Types.Pretty               (renderCompactText)
import           Pact.Types.Term                 as PactTerm (FieldKey,
                                                              Module (..),
                                                              ModuleDef (..),
                                                              ModuleName (..),
                                                              Name,
                                                              -- Namespace (..),
                                                              NamespaceName (..),
                                                              Object (..),
                                                              ObjectMap (..),
                                                              Term (TList, TLiteral, TModule, TObject),
                                                              moduleDefName, mnNamespace, tStr)
------------------------------------------------------------------------------
import           Common.Modules
import           Common.RefPath                  as MP
import           Common.Wallet
import           Frontend.Foundation
import           Frontend.ModuleExplorer.Example
import           Frontend.ModuleExplorer.File
import           Frontend.ModuleExplorer.Module (nameOfModule)
import           Frontend.Network
import           Frontend.Log
import           Frontend.Crypto.Class
import           Frontend.UI.Common

-- | A `Module` can come from a number of sources.
--
--   A `Module` either comes from some kind of a file or a network (blockchain).
--
--   We don't treet networks as file, because they can't contain arbitrary code
--   and data as files. So while they appear to be quite similar conceptually
--   to files, they should in practice be treated differently:
--
--   - Networks can only contain modules and keysets, not arbitrary Pact code
--     and even less random arbitrary data.
--   - Networks will usually hold much more data than typical files, so we need
--     things like search/filter and maybe later on some proper pagination and
--     stuff.
--   - We combine several networks in a view and let the user filter by
--     network, we don't do that with files.
data ModuleSource
  = ModuleSource_Deployed ChainRef
    -- ^ Reference a deployed module.
  | ModuleSource_File FileRef
    -- ^ Reference a module living in a file.
  deriving (Eq, Ord, Show, Generic)

makePactPrisms ''ModuleSource

instance A.ToJSON ModuleSource where
  toJSON = A.genericToJSON compactEncoding
  toEncoding = A.genericToEncoding compactEncoding

instance A.FromJSON ModuleSource where
  parseJSON = A.genericParseJSON compactEncoding

-- | A Module is uniquely identified by its name and its origin.
data ModuleRefV s = ModuleRef
  { _moduleRef_source :: s -- ^ Where does the module come from.
  , _moduleRef_name   :: ModuleName   -- ^ Fully qualified name of the module.
  }
  deriving (Eq, Ord, Show, Generic)

makePactLensesNonClassy ''ModuleRefV

instance A.ToJSON s => A.ToJSON (ModuleRefV s) where
  toJSON = A.genericToJSON compactEncoding
  toEncoding = A.genericToEncoding compactEncoding

instance A.FromJSON s => A.FromJSON (ModuleRefV s) where
  parseJSON = A.genericParseJSON compactEncoding


-- | Most general `ModuleRef`.
--
--   Can refer to a deployed module or to a file module.
type ModuleRef = ModuleRefV ModuleSource

-- | `ModuleRefV` that refers to some deployed module.
type DeployedModuleRef = ModuleRefV ChainRef

-- | `ModuleRefV` that refers to a module coming from some file.
type FileModuleRef = ModuleRefV FileRef


-- | Parse and render a `ModuleRef`.
instance IsRefPath ModuleRef where
  renderRef r =
    renderRef (_moduleRef_source r) <> renderRef (_moduleRef_name r)

  parseRef = ModuleRef <$> parseRef <*> parseRef


instance IsRefPath ModuleSource where
  renderRef = \case
    ModuleSource_Deployed bs -> "deployed" <> renderRef bs
    ModuleSource_File f -> renderRef f

  parseRef =
      fmap ModuleSource_File tryParseRef
      <|> fmap ModuleSource_Deployed parseDeployed
    where
      parseDeployed :: forall a. IsRefPath a => RefParser a
      parseDeployed = do
        r <- MP.anySingle
        case r of
          "deployed" -> parseRef
          _          -> MP.unexpected $ MP.Tokens (r :| [])

instance IsRefPath ModuleName where
  renderRef = mkRefPath . renderCompactText
  parseRef = do
    fn <- MP.anySingle
    case reverse $ T.splitOn "." fn of
      []   ->
        MP.failure
          (Just . MP.Tokens $ "" :| [])
          (Set.singleton $ MP.Tokens $ "module name" :| [])
      n:[] -> pure $ ModuleName n Nothing
      n:ns -> pure $ ModuleName n (Just . NamespaceName . T.intercalate "." . reverse $ ns)


-- | Get a `DeployedModuleRef` if it is one.
getDeployedModuleRef :: ModuleRef -> Maybe DeployedModuleRef
getDeployedModuleRef = traverse (^? _ModuleSource_Deployed)

-- | Get a `FileModuleRef` if it is one.
getFileModuleRef :: ModuleRef -> Maybe FileModuleRef
getFileModuleRef = traverse (^? _ModuleSource_File)

-- | Get the module name of a `ModuleRefV` as `Text`.
--
--   Same as:
--
-- @
--   textModuleName . _moduleRef_name
-- @
textModuleRefName :: ModuleRefV a -> Text
textModuleRefName = renderCompactText . _moduleRef_name

-- | Show the type of the selected module.
--
--   It is either "Example Module" or "Deployed Module"
--   The first parameter tells whether we are really dealing with a `Module` or with an `Interface`. It is `True` for modules, `False` otherwise.
textModuleRefSource :: Bool -> ModuleRef -> Text
textModuleRefSource isModule m =
    case _moduleRef_source m of
      ModuleSource_File (FileRef_Example n)
        -> printPretty "Example" (exampleName n)
      ModuleSource_File (FileRef_Stored n)
        -> printPretty "Stored" (textFileName n)
      ModuleSource_File (FileRef_Gist _)
        -> printPretty "Gist" "GitHub"
      ModuleSource_Deployed c
        -> printPretty "Deployed" (tshow $ _chainRef_chain c)
  where
    printPretty n d = mconcat [ n, " ", moduleText, " [ " , d , " ]" ]
    moduleText = if isModule then "Module" else "Interface"

-- Get hold of a deployed module:

-- | Fetch a `Module` from a network where it has been deployed.
--
--   Resulting Event is either an error msg or the loaded module.
fetchModule
  :: forall key m t model
  . ( MonadHold t m, PerformEvent t m, MonadJSM (Performable m)
    , TriggerEvent t m, MonadIO m
    , MonadSample t (Performable m)
    , HasNetwork model t
    , HasLogger model t
    , HasCrypto key (Performable m)
    , HasTransactionLogger m
    )
  => model
  -> Event t DeployedModuleRef
  -> m (Event t (DeployedModuleRef, These Text (ModuleDef (Term Name))))
fetchModule model onReq = do
    onReq' :: Event t (DeployedModuleRef, NetworkRequest)
      <- performEvent $ attachWith mkReq (current $ getNetworkNameAndMeta model) onReq
    deployedResults :: Event t ((DeployedModuleRef, NetworkRequest), [NetworkErrorResult])
      <- performLocalReadCustom (model ^. logger) (model ^. network) (pure . snd) onReq'
    let
      deployedResultsZipped = first fst <$> deployedResults

    pure $ fmapMaybe listToMaybe . ffor deployedResultsZipped $ \(dmr, errs) ->
      map ((dmr,) . (either This That . getModule . snd <=< first prettyPrintNetworkErrors)) errs

  where
    mkReq (networkName, pm) mRef = (mRef,) <$> mkSimpleReadReq code networkName pm (_moduleRef_source mRef)
      where code = mconcat
              [ "(describe-module "
              , quotedFullName $ _moduleRef_name mRef
              , ")"
              ]

    getModule :: PactValue -> Either Text (ModuleDef (Term Name))
    getModule  = \case
      PObject (ObjectMap props) -> do
        --Compatibility hack: pact has this wrapped, chainweb not yet - so:
        (ObjectMap objMod) <- case  Map.lookup "v" props of
          Just (PObject obj) -> pure obj
          Nothing -> pure $ ObjectMap props
          _ -> throwError "Expected object describing the module."

        codeLit <- note "No code property in module description object:\n" $
          Map.lookup "code" objMod

        c <- case codeLit of
          PLiteral (LString c) -> pure $ Code c
          _ -> throwError "No code found, but something else!"

        mn <- for (Map.lookup "name" objMod) $ \case
          PLiteral (LString mn) -> first T.pack $ parseModuleName mn
          _ -> throwError "Expected module name"

        mods <- codeModules c
        case Map.elems mods of
          []   -> throwError "No module in response"
          m:[] -> pure $ m & maybe id (nameOfModule .~) mn
          _    -> throwError "More than one module in response?"
      _ -> throwError "Server response did not contain a PObject module description."

-- Instances:

instance Functor ModuleRefV where
  fmap f (ModuleRef s n) = ModuleRef (f s) n

instance Foldable ModuleRefV where
  foldMap f (ModuleRef s _) = f s

instance Traversable ModuleRefV where
  traverse f (ModuleRef s n)  = flip ModuleRef n <$> f s

instance Semigroup (ModuleRefV a) where
  a <> _ = a
