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

-- | ModuleList: Browse deployed modules.
--
--   Limit results by filtering by `ModuleName` and deployed backend.
--
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.ModuleExplorer.ModuleList
  ( -- * Types and Classes
    ModuleListCfg (..)
  , HasModuleListCfg (..)
  , ModuleList (..)
  , HasModuleList (..)
    -- * Creation
  , makeModuleList
  ) where

------------------------------------------------------------------------------
import           Control.Arrow                     ((&&&))
import           Control.Lens
import qualified Data.List                         as L
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Data.Tuple                        (swap)
import           GHC.Generics                      (Generic)
import           Reflex
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Foundation
import           Frontend.ModuleExplorer.Module    as Module
import           Frontend.ModuleExplorer.ModuleRef as Module



-- | Configuration for `ModuleList`
data ModuleListCfg t = ModuleListCfg
  { _moduleListCfg_setNameFilter    :: Event t Text
    -- ^ Limit list of available module by name. All listed modules will have a
    -- name that matches the given substring.
  , _moduleListCfg_setBackendFilter :: Event t (Maybe BackendName)
    -- ^ Limit the list of shown modules to only those coming from the given
    -- `BackendName`.
  , _moduleListCfg_setPage          :: Event t Word
  }
  deriving Generic

makePactLenses ''ModuleListCfg

-- | Hard coded page size.
--
--   If need be, this can be made customizable via `ModuleListCfg`.
pageSize :: Word
pageSize = 10

-- | List all the currently filtered modules.
data ModuleList t = ModuleList
  { _moduleList_modules       :: Dynamic t [ModuleRef]
    -- ^ Modules available through the current filter.
  , _moduleList_nameFilter    :: Dynamic t Text
    -- ^ The currently applied name filter.
  , _moduleList_backendFilter :: MDynamic t BackendName
    -- ^ The currently applied backend filter.
  , _moduleList_page          :: Dynamic t Word
    -- ^ The current page.
  , _moduleList_pageCount     :: Dynamic t Word
    -- ^ Number of available pages of filtered modules.
  {- , _moduleList_pageSize :: Dynamic t Word -}
  {-   -- ^ The current page size used for pagination. -}
  }
  deriving Generic

makePactLenses ''ModuleList

-- | Make a `ModuleList` given a `ModuleListCfg`
makeModuleList
  :: forall t m cfg model
  . ( MonadHold t m, MonadFix m, Reflex t
    , HasModuleListCfg cfg t
    , HasBackend model t
    )
  => model
  -> cfg
  -> m (ModuleList t)
makeModuleList m cfg = mfix $ \mList -> do
    nameFilter <- holdDyn mempty $ cfg ^. moduleListCfg_setNameFilter
    backendFilter <- holdDyn Nothing $ cfg ^. moduleListCfg_setBackendFilter
    page <- holdDyn 1 $ leftmost
      [ filterValid (mList ^. moduleList_pageCount) $
          cfg ^. moduleListCfg_setPage
      , 1 <$ updated (mList ^. moduleList_pageCount)
      ]

    let
      moduleRefs :: Dynamic t (Map BackendName [DeployedModuleRef])
      moduleRefs = getModuleRefs <$> m ^. backend_backends <*> m ^. backend_modules

      -- All modules after backend filter applied:
      backendModules :: Dynamic t [DeployedModuleRef]
      backendModules = do
        bFilter <- backendFilter
        mRefs   <- moduleRefs
        pure $ case bFilter of
          Nothing -> concat $ Map.elems mRefs
          Just b  -> fromMaybe [] $ Map.lookup b mRefs

      -- Modules after backend filter and search filter applied:
      searchModules :: Dynamic t [DeployedModuleRef]
      searchModules = L.sortBy (compareBy (textModuleName . _moduleRef_name)) <$> do
        needle <- T.toCaseFold <$> nameFilter
        bModules <- backendModules
        let getBName = T.toCaseFold . textModuleRefName
        pure $ filter (T.isInfixOf needle . getBName) bModules

      -- Modules of current page matching the given filters.
      modules :: Dynamic t [DeployedModuleRef]
      modules = do
        sm <- searchModules
        p  <- page
        pure $ take (fromIntegral pageSize) . drop (fromIntegral $ pageSize * pred p) $ sm

    pure $ ModuleList
      { _moduleList_modules = (map (moduleRef_source %~ ModuleSource_Deployed)) <$> modules
      , _moduleList_nameFilter = nameFilter
      , _moduleList_backendFilter = backendFilter
      , _moduleList_page = page
      , _moduleList_pageCount = (calcTotal . length <$> searchModules)
      }
  where
    compareBy f a b = f a `compare` f b
    calcTotal l = ceiling $ (fromIntegral l :: Double)  / fromIntegral pageSize

    filterValid pc = push (\newP -> do
      pCount <- sample $ current pc
      pure $ if newP <= pCount && newP >= 1
         then Just newP
         else Nothing
      )



-- | Get the available module map as proper `DeployedModuleRef`.
getModuleRefs
  :: Maybe (Map BackendName BackendRef)
  -> Map BackendName (Maybe [Text])
  -> Map BackendName [DeployedModuleRef]
getModuleRefs bRefs mMods = fromMaybe Map.empty $ do
    let mods = mapMaybe sequence $ Map.toList mMods
    backends <- bRefs
    let
      byRef :: [(BackendRef, [Text])]
      byRef = map swap . mapMaybe (traverse (flip Map.lookup backends) . swap) $ mods

      modRefs :: [DeployedModuleRef]
      modRefs = concatMap (uncurry buildModRefs) byRef
    pure $
      Map.fromListWith mappend
      . map (backendRefName . _moduleRef_source &&& pure)
      $ modRefs
  where
    buildModRefs :: BackendRef -> [Text] -> [DeployedModuleRef]
    buildModRefs r = map (buildModRef r)
    -- TODO: Proper namespace support:
    buildModRef :: BackendRef -> Text -> DeployedModuleRef
    buildModRef r = ModuleRef r . flip ModuleName Nothing

-- Instances:

instance Reflex t => Semigroup (ModuleListCfg t) where
  ModuleListCfg nfA bFA sPA <> ModuleListCfg nfB bFB sPB =
    ModuleListCfg (leftmost [nfA, nfB]) (leftmost [bFA, bFB]) (leftmost [sPA, sPB])

instance Reflex t => Monoid (ModuleListCfg t) where
  mempty = ModuleListCfg never never never
  mappend = (<>)


instance Flattenable (ModuleListCfg t) t where
  flattenWith doSwitch ev =
    ModuleListCfg
      <$> doSwitch never (_moduleListCfg_setNameFilter <$> ev)
      <*> doSwitch never (_moduleListCfg_setBackendFilter <$> ev)
      <*> doSwitch never (_moduleListCfg_setPage <$> ev)
