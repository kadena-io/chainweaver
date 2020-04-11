{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | Implementation of the Frontend.ModuleExplorer interface.
--
-- Copyright   :  (C) 2020 Kadena
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
import           Data.These.Lens           (_This, there)
import           Data.Tuple                (swap)
import           Reflex
import           Reflex.Dom.Core           (HasJSContext, MonadHold, PostBuild)
import           Safe                      (tailSafe)
------------------------------------------------------------------------------
import           Obelisk.Route.Frontend
import           Pact.Types.Lang
------------------------------------------------------------------------------
import           Common.Route
import           Frontend.AppCfg
import           Frontend.Crypto.Class
import           Frontend.Editor
import           Frontend.Foundation
import           Frontend.GistStore
import           Frontend.JsonData
import           Frontend.Messages
import           Frontend.ModuleExplorer   as API
import           Frontend.Network
import           Frontend.Repl
import           Frontend.Log
import           Frontend.Storage
import           Frontend.VersionedStore

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

-- | Write text to localstorage.
storeEditor :: HasStorage m => Text -> m ()
storeEditor ks = setItemStorage localStorage StoreFrontend_ModuleExplorer_SessionFile ks

-- | Load text from localstorage.
loadEditorFromLocalStorage :: (HasStorage m, Functor m) => m (Maybe Text)
loadEditorFromLocalStorage = getItemStorage localStorage StoreFrontend_ModuleExplorer_SessionFile


makeModuleExplorer
  :: forall key t m cfg mConf model
  . ( ReflexConstraints t m, MonadIO m
    , HasModuleExplorerCfg cfg t
    , HasModuleExplorerModelCfg mConf t
    , HasModuleExplorerModel model t
    , MonadSample t (Performable m)
    , HasStorage (Performable m)
    , HasCrypto key (Performable m)
    , Routed t (R FrontendRoute) m
    , HasLogger model t
    , HasTransactionLogger m
    )
  => AppCfg key t m
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

    inContracts <- ffor askRoute $ fmap $ \case
      FrontendRoute_Contracts :/ _ -> True
      _ -> False
    firstSeen <- headE $ ffilter id $ updated inContracts

    onPostBuild <- getPostBuild
    mInitFile <- _appCfg_loadEditor appCfg
    let
      onInitFile =
        if isNothing mInitFile
           then FileRef_Example ExampleRef_HelloWorld <$ firstSeen
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
      (cfg ^. moduleExplorerCfg_clearLoaded)

    (stckCfg, stack) <- pushPopModule m explr
      (leftmost -- Reset module stack.
        [cfg ^. moduleExplorerCfg_goHome -- on go home
        , () <$ updated (m ^. network_selectedNetwork) -- and on network switch.
        ]
      )
      (cfg ^. moduleExplorerCfg_pushModule)
      (cfg ^. moduleExplorerCfg_popModule)

    growth <- mkSelectionGrowth explr

    modules <- makeModuleList m (cfg ^. moduleExplorerCfg_modules)

    pure
      ( mconcat [ lFileCfg, editorInitCfg, stckCfg, gistCfg, selCfg ]
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



-- | Takes care of loading a file/module into the editor.
loadToEditor
  :: forall key m t mConf model
  . ( ReflexConstraints t m, MonadIO m
    , HasModuleExplorerModelCfg  mConf t
    , HasModuleExplorerModel  model t
    , MonadSample t (Performable m)
    , HasCrypto key (Performable m)
    , HasLogger model t
    , HasTransactionLogger m
    )
  => model
  -> Event t FileRef
  -> Event t ModuleRef
  -> Event t ()
  -> m (mConf, MDynamic t LoadedRef)
loadToEditor m onFileRef onModRef onClear = do
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
      , Nothing <$ onClear
      ]

    let
      onCode = fmap _unCode $ leftmost
        [ snd <$> onFileMod
        , snd <$> onFile
          -- Clear editor in case of loading error:
        , maybe "" (view codeOfModule) . snd <$> onMod
        , "" <$ onClear
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

    getFileModuleCode :: Maybe FileModuleRef -> (FileRef, Code) -> Maybe (FileModuleRef, Code)
    getFileModuleCode = \case
      Nothing -> const Nothing
      Just r@(ModuleRef _ n) ->
        fmap ((r,) . view codeOfModule)
        . Map.lookup n
        . codeModulesDiscardingErrors
        . snd


-- | Select a file, note that a file gets also implicitly selected when
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
  -> m (mConf, MDynamic t (FileRef, Code))
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
  :: forall key m t mConf model
  . ( MonadHold t m, PerformEvent t m, MonadJSM (Performable m)
    , HasJSContext (Performable m), TriggerEvent t m, MonadFix m, PostBuild t m
    , MonadSample t (Performable m), MonadIO m
    , HasMessagesCfg  mConf t, Monoid mConf
    , HasNetwork model t
    , HasLogger model t
    , HasCrypto key (Performable m)
    , HasTransactionLogger m
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

    (lCfg, onMayDeployedModule) <- loadModule m $ fmapMaybe getDeployedModuleRef onPush
    let onDeployedModule = fmapMaybe sequence onMayDeployedModule

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
      :: MDynamic t (FileRef, Code)
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
          moduleL <- MaybeT . pure $ Map.lookup n $ codeModulesDiscardingErrors (snd cFile)
          pure (cReq, moduleL)
      pure $ fmapMaybe id . updated $ retrievedModule

    updateStack :: (ModuleRef, ModuleDef (Term Name)) -> [(ModuleRef, ModuleDef (Term Name))] -> [(ModuleRef, ModuleDef (Term Name))]
    updateStack update = map (doUpdate update)
      where
        doUpdate new@(uK, _) old@(k, _) = if uK == k then new else old

    refreshHead :: Event t [(ModuleRef, ModDef)] -> m (mConf, Event t (ModuleRef, ModDef))
    refreshHead onMods = do
      let getHeadRef = getDeployedModuleRef <=< fmap fst . listToMaybe
      (cfg, onMayDeployed) <- loadModule m $ fmapMaybe getHeadRef onMods
      let onDeployed = fmapMaybe sequence onMayDeployed
      pure $ (cfg, (_1 . moduleRef_source %~ ModuleSource_Deployed) <$> onDeployed)


-- | Load a deployed module.
--
--   Loading errors will be reported to `Messages`.
loadModule
  :: forall key m t mConf model
  . ( ReflexConstraints t m, MonadIO m
    , Monoid mConf, HasMessagesCfg mConf t
    , MonadSample t (Performable m)
    , HasNetwork model t
    , HasLogger model t
    , HasCrypto key (Performable m)
    , HasTransactionLogger m
    )
  => model
  -> Event t DeployedModuleRef
  -> m (mConf, Event t (DeployedModuleRef, Maybe (ModuleDef (Term Name))))
     -- ^ Nothing in case of error, the actual error will be logged to `Messages`.
loadModule model onRef = do
  onErrModule <- fetchModule model onRef
  let
    onErr = fmapMaybe (^? _2 . _This) onErrModule
    onModule = fmap (^? there) <$> onErrModule
  pure
    ( mempty & messagesCfg_send .~ fmap (pure . ("Module Explorer, loading of module failed: " <>)) onErr
    , onModule
    )
