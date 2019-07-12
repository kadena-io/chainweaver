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
  , loadEditorFromLocalStorage
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad             (guard, (<=<))
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.Either               (rights)
import qualified Data.Map                  as Map
import           Data.Text                 (Text)
import           Data.Tuple                (swap)
import           Reflex
import           Reflex.Dom.Core           (HasJSContext, MonadHold, PostBuild)
import           Safe                      (tailSafe)
------------------------------------------------------------------------------
import           Pact.Types.Lang
------------------------------------------------------------------------------
import           Frontend.Editor
import           Frontend.Foundation
import           Frontend.GistStore
import           Frontend.JsonData
import           Frontend.Messages
import           Frontend.ModuleExplorer   as API
import           Frontend.Network
import           Frontend.Repl
import           Frontend.Storage

type HasModuleExplorerModelCfg mConf t =
  ( Monoid mConf
  , HasEditorCfg mConf t
  , HasMessagesCfg mConf t
  , HasJsonDataCfg mConf t
  , HasReplCfg mConf t
  , HasNetworkCfg mConf t
  , HasGistStoreCfg mConf t
  )

type HasModuleExplorerModel model t =
  ( HasEditor model t
  , HasJsonData model t
  , HasNetwork model t
  , HasGistStore model t
  )


-- | Constraints needed by functions in this module.
type ReflexConstraints t m =
  ( MonadHold t m, TriggerEvent t m, Reflex t, PerformEvent t m
  , HasJSContext (Performable m) , MonadJSM (Performable m)
  , PostBuild t m, MonadFix m
  )

-- Storing data:

-- | Storage keys for referencing data to be stored/retrieved.
data StoreModuleExplorer a where
  StoreModuleExplorer_SessionFile  :: StoreModuleExplorer Text -- Current editor contents
  -- TODO: Store `moduleExplorer_loaded` too with this key:
  {- StoreModuleExplorer_SessionFileRef :: StoreModuleExplorer ModuleSource -}

deriving instance Show (StoreModuleExplorer a)

-- | Write text to localstorage.
storeEditor :: MonadJSM m => Text -> m ()
storeEditor ks = setItemStorage localStorage StoreModuleExplorer_SessionFile ks

-- | Load text from localstorage.
loadEditorFromLocalStorage :: MonadJSM m => m (Maybe Text)
loadEditorFromLocalStorage = getItemStorage localStorage StoreModuleExplorer_SessionFile


makeModuleExplorer
  :: forall t m cfg mConf model
  . ( ReflexConstraints t m
    , HasModuleExplorerCfg cfg t
    {- , HasModuleExplorerModel model t -}
    , HasModuleExplorerModelCfg mConf t
    , HasModuleExplorerModel model t
    , MonadSample t (Performable m)
    )
  => AppCfg t m
  -> model
  -> cfg
  -> m (mConf, ModuleExplorer t)
makeModuleExplorer appCfg m cfg = mfix $ \ ~(_, explr) -> do

    (selCfg, selectedFile) <- selectFile m
      (fmapMaybe getFileModuleRef $ cfg ^. moduleExplorerCfg_pushModule)
      (leftmost
        [ cfg ^. moduleExplorerCfg_selectFile
        , Nothing <$ cfg ^. moduleExplorerCfg_goHome
        ]
      )

    onPostBuild <- getPostBuild
    mInitFile <- _appCfg_loadEditor appCfg
    let
      onInitFile =
        if isNothing mInitFile
           then (const $ FileRef_Example ExampleRef_HelloWorld) <$> onPostBuild
           else never
      editorInitCfg = mempty
        & editorCfg_loadCode .~ fmapMaybe (const mInitFile) onPostBuild
    -- Store to disk max every 2 seconds:
    onAutoStore <- throttle 2 $ updated $ m ^. editor_code
    performEvent_ $ storeEditor <$> onAutoStore
    let
      onCreateGistUnsafe = fmap swap
          -- TODO: We should save whenever we are about to leave the page, no matter the cause:
        . attach (current (m ^. editor_code)) $ cfg ^. moduleExplorerCfg_createGist

    onCreateGist <- performEvent $ ffor onCreateGistUnsafe $ \metaCode@(_, cCode) -> do
      storeEditor cCode
      pure metaCode
    let
      gistCfg = mempty & gistStoreCfg_create .~ onCreateGist
      onNewGistRef = FileRef_Gist <$> m ^. gistStore_created

    (lFileCfg, loadedSource) <- loadToEditor m
      (leftmost [cfg ^. moduleExplorerCfg_loadFile, onInitFile, onNewGistRef])
      (cfg ^. moduleExplorerCfg_loadModule)

    (stckCfg, stack) <- pushPopModule m explr
      (leftmost -- Reset module stack.
        [cfg ^. moduleExplorerCfg_goHome -- on go home
        , () <$ updated (m ^. network_selectedNetwork) -- and on network switch.
        ]
      )
      (cfg ^. moduleExplorerCfg_pushModule)
      (cfg ^. moduleExplorerCfg_popModule)

    let
      deployEdCfg = deployEditor m $ cfg ^. moduleExplorerCfg_deployEditor
      deployCodeCfg = deployCode m $ cfg ^. moduleExplorerCfg_deployCode

    growth <- mkSelectionGrowth explr

    modules <- makeModuleList m (cfg ^. moduleExplorerCfg_modules)

    pure
      ( mconcat [ editorInitCfg, lFileCfg, stckCfg, deployEdCfg, deployCodeCfg, gistCfg, selCfg ]
      , ModuleExplorer
        { _moduleExplorer_moduleStack = stack
        , _moduleExplorer_selectedFile = selectedFile
        , _moduleExplorer_loaded = loadedSource
        , _moduleExplorer_selectionGrowth = growth
        , _moduleExplorer_modules = modules
        }
      )



-- | Check whether we are going deeper with selections or not.
mkSelectionGrowth
  :: (Reflex t, MonadHold t m, PerformEvent t m, TriggerEvent t m
     , MonadIO (Performable m)
     , HasModuleExplorer explr t
     )
  => explr
  -> m (Dynamic t Ordering)
mkSelectionGrowth explr = do
    let
      stk = explr ^. moduleExplorer_moduleStack
      stkLen = length <$> stk

    let
      sel = explr ^. moduleExplorer_selectedFile
      stkLenSel = zipDyn stkLen sel

    let
      onGrowth = pushAlways (\(newLen, newSel) -> do
          oldLen <- sample $ current stkLen
          oldSel <- sample $ current sel
          pure $ case (newLen `compare` oldLen, newSel `compareSel` oldSel) of
            (EQ, a) -> a
            (a, EQ) -> a
            (_, a)  -> a -- File wins.
        )
        (updated stkLenSel)
    -- Reset is necessary, otherwise we get animations everytime the module
    -- explorer tab gets selected:
    onReset <- fmap (const EQ) <$> delay 0.6 onGrowth
    holdDyn EQ $ leftmost [onGrowth, onReset]

  where
    compareSel oldSelected newSelected =
      case (oldSelected, newSelected) of
        (Nothing, Nothing) -> EQ
        (Nothing, Just _)  -> LT
        (Just _, Nothing)  -> GT
        (Just _, Just _)   -> EQ


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
    mkReq :: Dynamic t ((Text, TransactionInfo) -> Maybe NetworkRequest)
    mkReq = do
      ed      <- m ^. jsonData_data
      pure $ \(code, info) -> do
        let c = ChainRef Nothing $ _transactionInfo_chainId info
        d <- ed ^? _Right
        pure $ NetworkRequest code d c (_transactionInfo_endpoint info) (_transactionInfo_keys info)

    jsonError :: Dynamic t (Maybe Text)
    jsonError = do
      ed <- m ^. jsonData_data
      pure $ case ed of
        Left _  -> Just $ "Deploy not possible: JSON data was invalid!"
        Right _ -> Nothing
  in
    mempty
      & networkCfg_deployCode .~ fmap pure (attachWithMaybe ($) (current mkReq) onDeploy)
      & messagesCfg_send .~ tagMaybe (fmap pure <$> current jsonError) onDeploy


-- | Takes care of loading a file/module into the editor.
loadToEditor
  :: forall m t mConf model
  . ( ReflexConstraints t m
    , HasModuleExplorerModelCfg  mConf t
    , HasModuleExplorerModel  model t
    , MonadSample t (Performable m)
    )
  => model
  -> Event t FileRef
  -> Event t ModuleRef
  -> m (mConf, MDynamic t LoadedRef)
loadToEditor m onFileRef onModRef = do
    let onFileModRef = fmapMaybe getFileModuleRef onModRef

    (fCfg, onFile) <- fetchFile m $ leftmost
      [ onFileRef
      , _moduleRef_source <$> onFileModRef
      ]

    (modCfg, onMod)  <- loadModule m $ fmapMaybe getDeployedModuleRef onModRef

    fileModRequested <- holdDyn Nothing $ leftmost
      [ Just <$> onFileModRef
      , Nothing <$ onFileRef -- Order important.
      ]
    let
      onFileMod :: Event t (FileModuleRef, Code)
      onFileMod = fmapMaybe id $
        attachPromptlyDynWith getFileModuleCode fileModRequested onFile

      onGlobalModRef =
        attachWith makeGlobalLoadedRef (current selectedInfos) . fmap fst $ onMod
      selectedInfos = rights <$> m ^. network_selectedNodes

    loaded <- holdDyn Nothing $ leftmost
      [ Just . LoadedRef_Module . (moduleRef_source %~ ModuleSource_File) . fst <$> onFileMod
      , Just . LoadedRef_File . fst <$> onFile -- Order important we prefer `onFileMod` over `onFile`.
        -- Use globally working references for URL:
      , Just <$> onGlobalModRef
        -- For now, until we have file saving support:
      , fmap (const Nothing) . ffilter id . updated $ m ^.editor_modified
      ]

    let
      onCode = fmap _unCode $ leftmost
        [ snd <$> onFileMod
        , snd <$> onFile
        , view codeOfModule . snd <$> onMod
        ]

    pure ( mconcat [modCfg, fCfg, mempty & editorCfg_loadCode .~ onCode]
         , loaded
         )
  where
    makeGlobalLoadedRef :: [NodeInfo] -> DeployedModuleRef -> LoadedRef
    makeGlobalLoadedRef infos =
      LoadedRef_Module . (moduleRef_source %~ ModuleSource_Deployed . makeGlobal infos)

    -- Make a ChainRef work globally (also on different machines), by adding a `NodeRef`.
    -- TODO: At the moment this is actually mandatory even for sharing with
    -- machines with the same configured network, because at startup we don't
    -- yet have loaded the nodeinfos of the selected network, so loading the
    -- URL of a locally referenced module (without a noderef) will fail. To
    -- make this work (if needed for some reason), we would need to delay
    -- loading of the references until `network_selectedNodes` is fully loaded.
    makeGlobal :: [NodeInfo] -> ChainRef -> ChainRef
    makeGlobal infos ref =
      case (infos, ref) of
        (i:_, ChainRef Nothing c)  -> ChainRef (Just $ nodeInfoRef i) c
        _ -> ref

    getFileModuleCode :: Maybe FileModuleRef -> (FileRef, PactFile) -> Maybe (FileModuleRef, Code)
    getFileModuleCode = \case
      Nothing -> const Nothing
      Just r@(ModuleRef _ n) ->
        fmap ((r,) . view codeOfModule)
        . Map.lookup n
        . fileModules
        . snd


-- | Select a `PactFile`, note that a file gets also implicitely selected when
--   a module of a given file gets selected.
selectFile
  :: forall m t model mConf
  . ( MonadHold t m, PerformEvent t m, MonadJSM (Performable m)
    , TriggerEvent t m, MonadFix m
    , HasModuleExplorerModel model t, HasModuleExplorerModelCfg mConf t
    )
  => model
  -> Event t FileModuleRef
  -> Event t (Maybe FileRef)
  -> m (mConf, MDynamic t (FileRef, PactFile))
selectFile m onModRef onMayFileRef = mdo

    (fCfg, onFileSelect) <- fetchFile m . push (filterNewFileRef selected) $ leftmost
      [ _moduleRef_source <$> onModRef
      , fmapMaybe id onMayFileRef
      ]

    selected <- holdDyn Nothing $ leftmost
      [ Just    <$> onFileSelect
      , Nothing <$  ffilter isNothing onMayFileRef
      ]
    pure (fCfg, selected)
  where
    filterNewFileRef oldFile newFileRef = do
      cOld <- sample . current $ oldFile
      pure $ if fmap fst cOld /= Just newFileRef
         then Just newFileRef
         else Nothing


-- | Push/pop a module on the `_moduleExplorer_moduleStack`.
--
--   The deployed module on the top of the stack will always be kept up2date on
--   `_network_deployed` fires.
pushPopModule
  :: forall m t mConf model
  . ( MonadHold t m, PerformEvent t m, MonadJSM (Performable m)
    , HasJSContext (Performable m), TriggerEvent t m, MonadFix m, PostBuild t m
    , MonadSample t (Performable m)
    , HasMessagesCfg  mConf t, Monoid mConf
    , HasNetwork model t
    )
  => model
  -> ModuleExplorer t
  -> Event t ()
  -> Event t ModuleRef
  -> Event t ()
  -> m (mConf, Dynamic t [(ModuleRef, ModDef)])
pushPopModule m explr onClear onPush onPop = mdo
    let onFileModRef = fmapMaybe getFileModuleRef onPush
    onFileModule <- waitForFile (explr ^. moduleExplorer_selectedFile) onFileModRef

    (lCfg, onDeployedModule) <- loadModule m $ fmapMaybe getDeployedModuleRef onPush

    stack <- holdUniqDyn <=< foldDyn id [] $ leftmost
      [ (:) . (_1 . moduleRef_source %~ ModuleSource_File) <$> onFileModule
      , (:) . (_1 . moduleRef_source %~ ModuleSource_Deployed) <$> onDeployedModule
      , tailSafe <$ onPop
      , const [] <$ onClear
      , updateStack <$> onRefresh
      ]
    (rCfg, onRefresh) <- refreshHead $
      tagPromptlyDyn stack $ leftmost [ onPop, m ^. network_deployed ]

    pure
      ( lCfg <> rCfg
      , stack
      )
  where
    waitForFile
      :: MDynamic t (FileRef, PactFile)
      -> Event t FileModuleRef
      -> m (Event t (FileModuleRef, ModDef))
    waitForFile fileL onRef = do
      onReset <- delay 0 $ updated fileL
      modReq <- holdDyn Nothing $ leftmost [Just <$> onRef, Nothing <$ onReset]
      let
        retrievedModule = runMaybeT $ do
          cFile <- MaybeT fileL
          cReq <- MaybeT modReq
          guard $ _moduleRef_source cReq == fst cFile

          let n = _moduleRef_name cReq
          moduleL <- MaybeT . pure $ Map.lookup n $ fileModules (snd cFile)
          pure (cReq, moduleL)
      pure $ fmapMaybe id . updated $ retrievedModule

    updateStack :: (ModuleRef, ModuleDef (Term Name)) -> [(ModuleRef, ModuleDef (Term Name))] -> [(ModuleRef, ModuleDef (Term Name))]
    updateStack update = map (doUpdate update)
      where
        doUpdate new@(uK, _) old@(k, _) = if uK == k then new else old

    refreshHead :: Event t [(ModuleRef, ModDef)] -> m (mConf, Event t (ModuleRef, ModDef))
    refreshHead onMods = do
      let getHeadRef = getDeployedModuleRef <=< fmap fst . listToMaybe
      (cfg, onDeployed) <- loadModule m $ fmapMaybe getHeadRef onMods
      pure $ (cfg, (_1 . moduleRef_source %~ ModuleSource_Deployed) <$> onDeployed)


-- | Load a deployed module.
--
--   Loading errors will be reported to `Messages`.
loadModule
  :: forall m t mConf model
  . ( ReflexConstraints t m
    , Monoid mConf, HasMessagesCfg mConf t
    , MonadSample t (Performable m)
    , HasNetwork model t
    )
  => model
  -> Event t DeployedModuleRef
  -> m (mConf, Event t (DeployedModuleRef, ModuleDef (Term Name)))
loadModule networkL onRef = do
  onErrModule <- fetchModule networkL onRef
  let
    onErr = fmapMaybe (^? _2 . _Left) onErrModule
    onModule = fmapMaybe (traverse (^? _Right)) onErrModule
  pure
    ( mempty & messagesCfg_send .~ fmap (pure . ("Module Explorer, loading of module failed: " <>)) onErr
    , onModule
    )
