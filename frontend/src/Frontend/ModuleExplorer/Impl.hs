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
import Control.Monad.Except (throwError)
import Control.Monad (void, (<=<))
import           Control.Arrow               ((***), left)
import           Data.Bifunctor (second)
import           Control.Lens
import           Data.Aeson                  as Aeson (Result (..), fromJSON, FromJSON, Value)
import           Data.Default
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Reflex
import           Reflex.Dom.Core             (HasJSContext, MonadHold,
                                              PostBuild, XhrResponse (..),
                                              newXMLHttpRequest, xhrRequest)
------------------------------------------------------------------------------
import qualified Pact.Compile                as Pact
import qualified Pact.Parse                  as Pact
import           Pact.Types.Lang
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Editor
import           Frontend.Foundation
import           Frontend.JsonData
import           Frontend.Messages
import           Frontend.ModuleExplorer     as API
import           Frontend.Repl
import           Frontend.Wallet

type HasModuleExplorerModelCfg mConf t =
  ( Monoid mConf
  , HasEditorCfg mConf t
  , HasMessagesCfg mConf t
  , HasJsonDataCfg mConf t
  , HasReplCfg mConf t
  , HasBackendCfg mConf t
  )

type HasModuleExplorerModel model t =
  ( HasEditor model t
  , HasJsonData model t
  , HasBackend model t
  )


-- | Constraints needed by functions in this module.
type ReflexConstraints t m =
  ( MonadHold t m, TriggerEvent t m, Reflex t, PerformEvent t m
  , HasJSContext (Performable m) , MonadJSM (Performable m)
  , PostBuild t m, MonadFix m
  )


makeModuleExplorer
  :: forall t m cfg mConf model
  . ( ReflexConstraints t m
    , HasModuleExplorerCfg cfg t
    {- , HasModuleExplorerModel model t -}
    , HasModuleExplorerModelCfg mConf t
    , HasModuleExplorerModel model t
    )
  => model
  -> cfg
  -> m (mConf, ModuleExplorer t)
makeModuleExplorer m cfg = do
    (loadedCfg, loaded)     <- loadModule $ cfg ^. moduleExplorerCfg_loadModule
    (selectedCfg, selected) <- selectModule m $ cfg ^. moduleExplorerCfg_selModule
    let
      deployEdCfg = deployEditor m $ cfg ^. moduleExplorerCfg_deployEditor
      deployCodeCfg = deployCode m $ cfg ^. moduleExplorerCfg_deployCode

    pure
      ( mconcat [ loadedCfg, selectedCfg, deployEdCfg, deployCodeCfg ]
      , ModuleExplorer
          { _moduleExplorer_loadedModule = loaded
          , _moduleExplorer_selectedModule = selected
          }
      )

deployEditor
  :: forall t mConf model
  . ( Reflex t
    , HasModuleExplorerModelCfg  mConf t
    , HasModuleExplorerModel model t
    )
  => model
  -> Event t TransactionInfo
  -> mConf
deployEditor m = deployCode m . attach (current $ m ^. editor_code)

deployCode
  :: forall t mConf model
  . ( Reflex t
    , HasModuleExplorerModelCfg  mConf t
    , HasModuleExplorerModel model t
    )
  => model
  -> Event t (Text, TransactionInfo)
  -> mConf
deployCode m onDeploy =
  let
    mkReq :: Dynamic t ((Text, TransactionInfo) -> Maybe BackendRequest)
    mkReq = do
      ed      <- m ^. jsonData_data
      mbs     <- m ^. backend_backends
      pure $ \(code, info) -> do
        bs <- mbs
        b <- Map.lookup (_transactionInfo_backend info) bs
        d <- ed ^? _Right
        pure $ BackendRequest code d b (_transactionInfo_keys info)

    jsonError :: Dynamic t (Maybe Text)
    jsonError = do
      ed <- m ^. jsonData_data
      pure $ case ed of
        Left _  -> Just $ "Deploy not possible: JSON data was invalid!"
        Right _ -> Nothing

  in
    mempty
      & backendCfg_deployCode .~ attachWithMaybe ($) (current mkReq) onDeploy
      & messagesCfg_send .~ tagMaybe (current jsonError) onDeploy


-- | Takes care of loading a contract into the editor as requested by the user.
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

  let
    onContract = leftmost
      [ ModuleSel_Example  <$> onExampleLoad
      , ModuleSel_Deployed <$> onDeployedLoad
      ]
    onLoad = () <$ onContract

  contract <- holdDyn Nothing $ Just <$> onContract

  pure ( mconcat
          [ eCfg
          , dCfg
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
  -> m (mConf, Event t ExampleModule)
loadExampleModule onExampleModule = do
  onResp <- fetchExample onExampleModule
  let onCodeJson = snd <$> onResp
  pure
    ( mempty
        & jsonDataCfg_setRawInput .~ fmap snd onCodeJson
        & editorCfg_setCode .~ fmap fst onCodeJson
    , fst <$> onResp
    )

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
  -> m (mConf, Event t DeployedModule)
loadDeployedModule onNewContractReq = do
  onErrModule <- fetchDeployedModule onNewContractReq
  let
    (onErr, onModule) = fanEither . fmap snd $ onErrModule
    onCode = _unCode . _mCode <$> onModule
    onLoaded = fmap fst $ onErrModule
  pure
    ( mempty
        & editorCfg_setCode .~ onCode
        & messagesCfg_send .~ onErr
    , onLoaded
    )

-- | Select a contract.
--
--   For deployed contracts this loads its code & functions.
--
--   The returned Event fires once the loading is complete.
selectModule
  :: forall m t mConf model
  . ( MonadHold t m, PerformEvent t m, MonadJSM (Performable m)
    , HasJSContext (Performable m), TriggerEvent t m, MonadFix m
    , HasMessagesCfg  mConf t, Monoid mConf
    , HasBackend model t
    )
  => model
  -> Event t (Maybe ModuleSel)
  -> m (mConf, MDynamic t SelectedModule)
selectModule m onMaySelReq = mdo
  let
    onSelReq = fmapMaybe id onMaySelReq
    onExampleModule  = fmapMaybe (^? _ModuleSel_Example)  onSelReq
    onDeployedModuleNew = fmapMaybe (^? _ModuleSel_Deployed) onSelReq

    selectedDeployed = (^? _Just . selectedModule_module . _ModuleSel_Deployed) <$> selected
    onDeployedModule = leftmost
      [ onDeployedModuleNew
        -- Reload when something got deployed:
      , tagMaybe (current selectedDeployed) $ m ^. backend_deployed
      ]

  onExSel <- selectExample onExampleModule
  (deCfg, onDeSel) <- selectDeployed onDeployedModule
  selected <- holdDyn Nothing $ leftmost
    [ Just <$> onExSel
    , Just <$> onDeSel
    , Nothing <$ ffilter isNothing onMaySelReq
    ]
  pure
    ( deCfg
    , selected
    )

-- | Select Example contract.
selectExample
  :: forall m t
  . ( MonadHold t m, PerformEvent t m, MonadJSM (Performable m)
    , HasJSContext (Performable m), TriggerEvent t m
    )
  => Event t ExampleModule
  -> m (Event t SelectedModule)
selectExample onSelReq = do
  onExampleRec <- fetchExample onSelReq
  let
    buildSelected :: ExampleModule -> Text -> SelectedModule
    buildSelected depl code =
      SelectedModule (ModuleSel_Example depl) code (listPactFunctions (Code code))

  pure $ uncurry buildSelected . second fst <$> onExampleRec


-- | Select a deployed contract.
--
--   For deployed contracts this loads its code & functions.
--
--   The returned Event fires once the loading is complete.
selectDeployed
  :: forall m t mConf
  . ( MonadHold t m, PerformEvent t m, MonadJSM (Performable m)
    , HasJSContext (Performable m), TriggerEvent t m
    , HasMessagesCfg  mConf t, Monoid mConf
    )
  => Event t DeployedModule
  -> m (mConf, Event t SelectedModule)
selectDeployed onSelReq = do
  onErrModule <- fetchDeployedModule onSelReq
  let
    onErr = fmapMaybe (^? _2 . _Left) onErrModule

    onRes :: Event t (DeployedModule, Module)
    onRes = fmapMaybe (traverse (^? _Right)) onErrModule

    buildSelected :: DeployedModule -> Module -> SelectedModule
    buildSelected depl m =
      SelectedModule
        (ModuleSel_Deployed depl)
        (_unCode $ _mCode m)
        (listPactFunctions (_mCode m))

  pure
    ( mempty
        & messagesCfg_send .~ fmap ("Loading functions failed: " <>) onErr
    , uncurry buildSelected <$> onRes
    )

-- | Fetch a given given example contract data, given the URL.
fetchExample
  :: ( PerformEvent t m, TriggerEvent t m, MonadJSM (Performable m)
     , HasJSContext (Performable m)
     )
  => Event t ExampleModule -> m (Event t (ExampleModule, (Text, Text)))
fetchExample onExampleModule =
  performEventAsync $ ffor onExampleModule $ \example cb -> void . forkJSM $ do
    let
      callback = liftIO . cb . (example,) . (codeFromResponse *** codeFromResponse)

    let codeReq = xhrRequest "GET" (_exampleModule_code example) def
    void $ newXMLHttpRequest codeReq $ \codeRes -> do
      let jsonReq = xhrRequest "GET" (_exampleModule_data example) def
      void $ newXMLHttpRequest jsonReq $ \jsonRes ->
        callback (codeRes, jsonRes)

-- | Fetch source code of a deployed module.
--
--   Resulting Event is either an error msg or the loaded module.
fetchDeployedModule
  :: forall m t
  . ( MonadHold t m, PerformEvent t m, MonadJSM (Performable m)
    , HasJSContext (Performable m), TriggerEvent t m
    )
  => Event t DeployedModule
  -> m (Event t (DeployedModule, Either Text Module))
fetchDeployedModule onReq = do
    deployedResult :: Event t (DeployedModule, BackendErrorResult)
      <- performBackendRequestCustom emptyWallet mkReq onReq

    pure $ ffor deployedResult $
      id *** (fromJsonEither <=< left (T.pack . show))

  where
    mkReq dm = BackendRequest
      { _backendRequest_code = mconcat
        [ "(describe-module '"
        , _deployedModule_name dm
        , ")"
        ]
      , _backendRequest_data = mempty
      , _backendRequest_backend = _deployedModule_backend dm
      , _backendRequest_signing = Set.empty
      }

    fromJsonEither :: FromJSON a => Value -> Either Text a
    fromJsonEither v = case fromJSON v of
        Aeson.Error e -> throwError . T.pack $ e
        Aeson.Success a -> pure a


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



