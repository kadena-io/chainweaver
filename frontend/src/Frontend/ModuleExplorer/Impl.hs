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
import           Control.Lens
import           Control.Monad             (guard, (<=<))
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.Map                  as Map
import           Data.Text                 (Text)
import           Reflex
import           Reflex.Dom.Core           (HasJSContext, MonadHold, PostBuild)
import           Safe                      (tailSafe)
------------------------------------------------------------------------------
import           Pact.Types.Lang
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Editor
import           Frontend.Foundation
import           Frontend.JsonData
import           Frontend.Messages
import           Frontend.ModuleExplorer   as API
import           Frontend.Repl

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
makeModuleExplorer m cfg = mfix $ \ ~(_, explr) -> do
    selectedFile <- selectFile
      (fmapMaybe getFileModuleRef $ cfg ^. moduleExplorerCfg_pushModule)
      (leftmost [cfg ^. moduleExplorerCfg_selectFile, Nothing <$ cfg ^. moduleExplorerCfg_goHome])

    (lFileCfg, loadedSource) <- loadToEditor
      (cfg ^. moduleExplorerCfg_loadFile)
      (cfg ^. moduleExplorerCfg_loadModule)

    (stckCfg, stack) <- pushPopModule m explr
      (cfg ^. moduleExplorerCfg_goHome)
      (cfg ^. moduleExplorerCfg_pushModule)
      (cfg ^. moduleExplorerCfg_popModule)

    let
      deployEdCfg = deployEditor m $ cfg ^. moduleExplorerCfg_deployEditor
      deployCodeCfg = deployCode m $ cfg ^. moduleExplorerCfg_deployCode

    growth <- mkSelectionGrowth explr

    pure
      ( mconcat [ lFileCfg, stckCfg, deployEdCfg, deployCodeCfg ]
      , ModuleExplorer
        { _moduleExplorer_moduleStack = stack
        , _moduleExplorer_selectedFile = selectedFile
        , _moduleExplorer_loaded = loadedSource
        , _moduleExplorer_selectionGrowth = growth
        }
      )

-- | Check whether we are going deeper with selections or not.
mkSelectionGrowth
  :: (Reflex t, MonadHold t m, MonadFix m, PerformEvent t m, TriggerEvent t m
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

-- | Takes care of loading a file/module into the editor.
loadToEditor
  :: forall m t mConf
  . ( ReflexConstraints t m
    , HasModuleExplorerModelCfg  mConf t
    )
  => Event t FileRef
  -> Event t ModuleRef
  -> m (mConf, MDynamic t ModuleSource)
loadToEditor onFileRef onModRef = do
  let onFileModRef = fmapMaybe getFileModuleRef onModRef

  onFile <- fetchFile $ leftmost
    [ onFileRef
    , _moduleRef_source <$> onFileModRef
    ]
  let onFetchedFileRef = fst <$> onFile

  (modCfg, onMod)  <- loadModule $ fmapMaybe getDeployedModuleRef onModRef
  let onFetchedModRef = _moduleRef_source . fst <$> onMod

  loaded <- holdDyn Nothing $ Just <$> leftmost
    [ ModuleSource_File <$> onFetchedFileRef
    , ModuleSource_Deployed <$> onFetchedModRef
    ]

  fileModRequested <- holdDyn Nothing $ leftmost
    [ Just <$> onFileModRef
    , Nothing <$ onFileRef -- Order important.
    ]
  let
    onCode = fmap _unCode $ leftmost
      [ fmapMaybe id $ attachPromptlyDynWith getFileModuleCode fileModRequested onFile
      , snd <$> onFile
      , view codeOfModule . snd <$> onMod
      ]

    getFileModuleCode :: Maybe FileModuleRef -> (FileRef, PactFile) -> Maybe Code
    getFileModuleCode = \case
      Nothing -> const Nothing
      Just (ModuleRef _ n) ->
        fmap (view codeOfModule)
        . Map.lookup n
        . fileModules
        . snd

  pure ( mconcat [modCfg, mempty & editorCfg_setCode .~ onCode]
       , loaded
       )

-- | Select a `PactFile`, note that a file gets also implicitely selected when
--   a module of a given file gets selected.
selectFile
  :: forall m t
  . ( MonadHold t m, PerformEvent t m, MonadJSM (Performable m)
    , HasJSContext (Performable m), TriggerEvent t m, MonadFix m
    )
  => Event t FileModuleRef
  -> Event t (Maybe FileRef)
  -> m (MDynamic t (FileRef, PactFile))
selectFile onModRef onMayFileRef = mdo

    onFileSelect <- fetchFile . push (filterNewFileRef selected) $ leftmost
      [ _moduleRef_source <$> onModRef
      , fmapMaybe id onMayFileRef
      ]

    selected <- holdDyn Nothing $ leftmost
      [ Just    <$> onFileSelect
      , Nothing <$  ffilter isNothing onMayFileRef
      ]
    pure selected
  where
    filterNewFileRef oldFile newFileRef = do
      cOld <- sample . current $ oldFile
      pure $ if fmap fst cOld /= Just newFileRef
         then Just newFileRef
         else Nothing


-- | Push/pop a module on the `_moduleExplorer_moduleStack`.
--
--   The deployed module on the top of the stack will always be kept up2date on
--   `_backend_deployed` fires.
pushPopModule
  :: forall m t mConf model
  . ( MonadHold t m, PerformEvent t m, MonadJSM (Performable m)
    , HasJSContext (Performable m), TriggerEvent t m, MonadFix m, PostBuild t m
    , HasMessagesCfg  mConf t, Monoid mConf
    , HasBackend model t
    )
  => model
  -> ModuleExplorer t
  -> Event t ()
  -> Event t ModuleRef
  -> Event t ()
  -> m (mConf, Dynamic t [(ModuleRef, Module)])
pushPopModule m explr onClear onPush onPop = mdo
    let onFileModRef = fmapMaybe getFileModuleRef onPush
    onFileModule <- waitForFile (explr ^. moduleExplorer_selectedFile) onFileModRef

    (lCfg, onDeployedModule) <- loadModule $ fmapMaybe getDeployedModuleRef onPush

    stack <- holdUniqDyn <=< foldDyn id [] $ leftmost
      [ (:) . (_1 . moduleRef_source %~ ModuleSource_File) <$> onFileModule
      , (:) . (_1 . moduleRef_source %~ ModuleSource_Deployed) <$> onDeployedModule
      , tailSafe <$ onPop
      , const [] <$ onClear
      , updateStack <$> onRefresh
      ]
    (rCfg, onRefresh) <- refreshHead $
      tagPromptlyDyn stack $ leftmost [ onPop, m ^. backend_deployed ]

    pure
      ( lCfg <> rCfg
      , stack
      )
  where
    waitForFile
      :: MDynamic t (FileRef, PactFile)
      -> Event t FileModuleRef
      -> m (Event t (FileModuleRef, Module))
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

    updateStack :: (ModuleRef, Module) -> [(ModuleRef, Module)] -> [(ModuleRef, Module)]
    updateStack update = map (doUpdate update)
      where
        doUpdate new@(uK, _) old@(k, _) = if uK == k then new else old

    refreshHead :: Event t [(ModuleRef, Module)] -> m (mConf, Event t (ModuleRef, Module))
    refreshHead onMods = do
      let getHeadRef = getDeployedModuleRef <=< fmap fst . listToMaybe
      (cfg, onDeployed) <- loadModule $ fmapMaybe getHeadRef onMods
      pure $ (cfg, (_1 . moduleRef_source %~ ModuleSource_Deployed) <$> onDeployed)


-- | Load a deployed module.
--
--   Loading errors will be reported to `Messages`.
loadModule
  :: forall m t mConf
  . ( ReflexConstraints t m
    , Monoid mConf, HasMessagesCfg mConf t
    )
  => Event t DeployedModuleRef
  -> m (mConf, Event t (DeployedModuleRef, Module))
loadModule onRef = do
  onErrModule <- fetchModule onRef
  let
    onErr    = fmapMaybe (^? _2 . _Left) onErrModule
    onModule = fmapMaybe (traverse (^? _Right)) onErrModule
  pure
    ( mempty & messagesCfg_send .~ fmap ("Loading of module failed: " <>) onErr
    , onModule
    )
