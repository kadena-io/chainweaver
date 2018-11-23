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
{-# LANGUAGE ConstraintKinds           #-}

-- | Implementation of the Frontend.ModuleExplorer interface.
--
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.ModuleExplorer.Impl
  ( -- * Interface
    module API
    -- * Types
  , HasModuleExplorerModelCfg
    -- * Creation
  , makeModuleExplorer
  ) where

------------------------------------------------------------------------------
import qualified Bound
import           Control.Lens
import           Data.Aeson               as Aeson (Result (..), fromJSON)
import qualified Data.Set as Set
import           Data.Default
import qualified Data.Map                 as Map
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Reflex
import           Reflex.Dom.Core          (HasJSContext, MonadHold,
                                           PostBuild, XhrResponse (..),
                                           performRequestAsync, xhrRequest)
------------------------------------------------------------------------------
import qualified Pact.Compile             as Pact
import qualified Pact.Parse               as Pact
import           Pact.Types.Lang
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Foundation
import           Frontend.JsonData
import           Frontend.Wallet
import           Frontend.Messages
import           Frontend.Editor
import           Frontend.Repl
import           Frontend.ModuleExplorer as API

{- -- | Our dependencies. -}
{- type HasModuleExplorerModel model t = HasBackend model t -}

type HasModuleExplorerModelCfg mConf t =
  ( Monoid mConf
  , HasEditorCfg mConf t
  , HasMessagesCfg mConf t
  , HasJsonDataCfg mConf t
  , HasReplCfg mConf t
  )


-- | Constraints needed by functions in this module.
type ReflexConstraints t m =
  ( MonadHold t m, TriggerEvent t m, Reflex t, PerformEvent t m
  , HasJSContext (Performable m) , MonadJSM (Performable m)
  , PostBuild t m
  )


makeModuleExplorer
  :: forall t m cfg mConf
  . ( ReflexConstraints t m
    , HasModuleExplorerCfg cfg t
    {- , HasModuleExplorerModel model t -}
    , HasModuleExplorerModelCfg mConf t
    )
  => cfg
  -> m (mConf, ModuleExplorer t)
makeModuleExplorer cfg = do
    (loadedCfg, loaded)     <- loadModule   $ cfg ^. moduleExplorerCfg_loadModule
    (selectedCfg, selected) <- selectModule $ cfg ^. moduleExplorerCfg_selModule

    pure
      ( mconcat [ loadedCfg, selectedCfg ]
      , ModuleExplorer
          { _moduleExplorer_loadedModule = loaded
          , _moduleExplorer_selectedModule = selected
          }
      )

-- | Takes care of loading a contract as requested by the user.
loadModule
  :: forall m t mConf
  . ( ReflexConstraints t m
    , HasModuleExplorerModelCfg  mConf t
    )
  => Event t ModuleSel
  -> m (mConf, MDynamic t ModuleSel)
loadModule onNewContractReq = do
  let initContract = ModuleSel_Example initialDemoModule

  -- Holds the currently requested contract, might not yet be loaded:
  requestedContract <- holdDyn initContract onNewContractReq
  onNewContractName <- tagOnPostBuild requestedContract

  let
    onExampleModule  = fmapMaybe (^? _ModuleSel_Example)  onNewContractName
    onDeployedModule = fmapMaybe (^? _ModuleSel_Deployed) onNewContractName

  (eCfg, onExampleLoad)  <- loadExampleModule onExampleModule
  (dCfg, onDeployedLoad) <- loadDeployedModule onDeployedModule

  let onContract = leftmost [ onExampleLoad , onDeployedLoad ]
      onLoad = () <$ onContract

  contract <- holdDyn Nothing $ Just <$> tagPromptlyDyn requestedContract onContract

  pure ( mconcat
          [ eCfg
          , dCfg
          , mempty
              & messagesCfg_clear .~ onLoad
              & replCfg_reset .~ onLoad
          ]
       , contract
       )

-- | Load a given `ExampleModule` from the server.
--
--   The returned Event will fire once the data is available.
loadExampleModule
  :: forall t m mConf
  . ( ReflexConstraints t m
    , HasModuleExplorerModelCfg  mConf t
    )
  => Event t ExampleModule
  -> m (mConf, Event t ())
loadExampleModule onExampleModule = do
  code <- loadContractData $ fmap _exampleModule_code onExampleModule
  json <- loadContractData $ fmap _exampleModule_data onExampleModule
  onCodeJson <- waitForEvents (,) onExampleModule code json

  pure
    ( mempty
        & jsonDataCfg_setRawInput .~ fmap snd onCodeJson
        & editorCfg_setCode .~ fmap fst onCodeJson
    , () <$ onCodeJson
    )
  where
    loadContractData onNewContractName =
      fmap (fmap codeFromResponse)
      . performRequestAsync $ ffor onNewContractName
      $ \example -> xhrRequest "GET" example def
--
-- | Load a deployed contract into the editor.
--
--   The returned Event fires once the loading is complete.
loadDeployedModule
  :: forall m t mConf
  . ( ReflexConstraints t m
    , HasModuleExplorerModelCfg  mConf t
    )
  => Event t DeployedModule
  -> m (mConf, Event t ())
loadDeployedModule onNewContractReq = do
  onErrModule <- fetchDeployedModule onNewContractReq
  let
    (onErr, onModule) = fanEither onErrModule
    onCode = _unCode . _mCode <$> onModule
  pure
    ( mempty
        & editorCfg_setCode .~ onCode
        & messagesCfg_send .~ onErr
    , () <$ onModule
    )
--
-- | Select a contract.
--
--   For deployed contracts this loads its code & functions.
--
--   The returned Event fires once the loading is complete.
selectModule
  :: forall m t mConf
  . ( MonadHold t m, Reflex t, PerformEvent t m, MonadJSM (Performable m)
    , HasJSContext (Performable m), TriggerEvent t m
    , HasMessagesCfg  mConf t, Monoid mConf
    )
  => Event t DeployedModule
  -> m (mConf, MDynamic t SelectedModule)
selectModule onSelReq = do
  onErrModule <- fetchDeployedModule onSelReq
  let
    (onErr, onModule) = fanEither onErrModule
  selReq <- holdDyn Nothing $ Just <$> onSelReq
  let
    buildSelected :: DeployedModule -> Module -> SelectedModule
    buildSelected depl m =
      SelectedModule depl (_unCode $ _mCode m) (listPactFunctions (_mCode m))

    mayBuildSelected :: Maybe DeployedModule -> Module -> Maybe SelectedModule
    mayBuildSelected mDepl m = buildSelected <$> mDepl <*> pure m

    onSelected = attachPromptlyDynWith mayBuildSelected selReq onModule
  selected <- holdDyn Nothing $ onSelected
  pure
    ( mempty
        & messagesCfg_send .~ fmap ("Loading functions failed: " <>) onErr
    , selected
    )


-- | Fetch source code of a deployed module.
--
--   Resulting Event is either an error msg or the loaded module.
fetchDeployedModule
  :: forall m t
  . ( MonadHold t m, PerformEvent t m, MonadJSM (Performable m)
    , HasJSContext (Performable m), TriggerEvent t m
    )
  => Event t DeployedModule
  -> m (Event t (Either Text Module))
fetchDeployedModule onReq = do
  deployedResult <- backendRequest emptyWallet $
    ffor onReq $ \c -> BackendRequest
      { _backendRequest_code = mconcat
        [ "(describe-module '"
        , _deployedModule_name c
        , ")"
        ]
      , _backendRequest_data = mempty
      , _backendRequest_backend = _deployedModule_backendUri c
      , _backendRequest_signing = Set.empty
      }
  let
    (deployedResultError, deployedValue) = fanEither deployedResult
    (deployedDecodeError, deployedModule) = fanEither $ ffor deployedValue $ \v ->
      case fromJSON v of
        Aeson.Error e   -> Left e
        Aeson.Success a -> Right a
  pure $ leftmost [ Left . T.pack <$> deployedDecodeError
                  , Left . T.pack . show <$> deployedResultError
                  , Right <$> deployedModule
                  ]

-- | Get the top level functions from a 'Term'
getFunctions :: Term Name -> [PactFunction]
getFunctions (TModule _ body _) = getFunctions $ Bound.instantiate undefined body
getFunctions (TDef name moduleName defType funType _ docs _) = [PactFunction moduleName name defType (_mDocs docs) funType]
getFunctions (TList list1 _ _) = getFunctions =<< list1
getFunctions _ = []

-- | Parse and compile the code to list the top level function data
listPactFunctions :: Code -> Maybe [PactFunction]
listPactFunctions (Code code) = case Pact.compileExps Pact.mkEmptyInfo <$> Pact.parseExprs code of
  Right (Right terms) -> Just $ concatMap getFunctions terms
  _                   -> Nothing

codeFromResponse :: XhrResponse -> Text
codeFromResponse =
    fromMaybe "error: could not connect to server" . _xhrResponse_responseText

-- | What demo do we load on startup:
initialDemo :: Int
initialDemo = 0

initialDemoModule :: ExampleModule
initialDemoModule = fromJust $ Map.lookup initialDemo demos



