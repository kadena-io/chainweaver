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

-- |
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.UI.ModuleExplorer where

------------------------------------------------------------------------------
import           Control.Lens
import qualified Data.List                   as L
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Traversable            (for)
import           Reflex
import           Reflex.Dom
import           Reflex.Network
------------------------------------------------------------------------------
import           Obelisk.Generated.Static
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Ide
import           Frontend.UI.Button
import           Frontend.UI.Widgets
------------------------------------------------------------------------------


moduleExplorer
  :: forall t m. MonadWidget t m
  => Ide t
  -> m (IdeCfg t)
moduleExplorer ideL = do
    exampleClick <- accordionItem True mempty "Example Contracts" $ do
      let showExample c = do
            divClass "module-name" $
              text $ _exampleContract_name c
      contractList showExample demos
    let exampleLoaded = fmap Left . leftmost . Map.elems $ exampleClick

    let mkMap = Map.fromList . map (\k@(BackendName n, _) -> (Just k, n)) . Map.toList
        opts = Map.insert Nothing "All backends" . maybe mempty mkMap <$>
                 _backend_backends (_ide_backend ideL)
    let itemsPerPage = 10 :: Int

    searchLoaded <- accordionItem True mempty "Deployed Contracts" $ mdo
      (filteredCs, updatePage) <- divClass "filter-bar flexbox" $ do
        ti <- divClass "search" $
          textInput $ def & attributes .~ constDyn ("placeholder" =: "Search")
        d <- divClass "backend-filter" $ dropdown Nothing opts def
        let
          search = value ti
          backendL = value d
          deployedContracts = Map.mergeWithKey (\_ a b -> Just (a, b)) mempty mempty
              <$> _backend_modules (_ide_backend ideL)
              <*> (fromMaybe mempty <$> _backend_backends (_ide_backend ideL))
          filteredCsRaw = searchFn <$> search <*> backendL <*> deployedContracts
        filteredCsL <- holdUniqDyn filteredCsRaw
        updatePageL <- divClass "pagination" $
          paginationWidget currentPage totalPages
        return (filteredCsL, updatePageL)

      let paginated = paginate itemsPerPage <$> currentPage <*> filteredCs
          showDeployed c = do
            divClass "module-name" $
              text $ _deployedContract_name c
            divClass "backend-name" $
              text $ unBackendName $ _deployedContract_backendName c
      -- TODO Might need to change back to listWithKey
      searchClick <- divClass "control-block-contents" $
        networkHold (return mempty) $
          contractList showDeployed <$> updated paginated

      let numberOfItems = length <$> filteredCs
          calcTotal a = ceiling $ (fromIntegral a :: Double)  / fromIntegral itemsPerPage
          totalPages = calcTotal <$> numberOfItems
      currentPage <- holdDyn 1 $ leftmost
        [ updatePage
        , 1 <$ updated numberOfItems
        ]

      return $ switch . current $ fmap Right . leftmost . Map.elems <$> searchClick

    pure $ mempty
      { _ideCfg_selContract = leftmost [exampleLoaded, searchLoaded]
      }

paginate :: (Ord k, Ord v) => Int -> Int -> [(k, v)] -> Map k v
paginate itemsPerPage p =
  Map.fromList . take itemsPerPage . drop (itemsPerPage * pred p) . L.sort

searchFn
  :: Text
  -> Maybe (BackendName, Text)
  -> Map BackendName (Maybe [Text], BackendUri)
  -> [(Int, DeployedContract)]
searchFn needle mModule = zip [0..] . concat . fmapMaybe (filtering needle) . Map.toList
  . maybe id (\(k', _) -> Map.filterWithKey $ \k _ -> k == k') mModule

filtering
  :: Text
  -> (BackendName, (Maybe [Text], BackendUri))
  -> Maybe [DeployedContract]
filtering needle (backendName, (m, backendUri)) =
    case fmapMaybe f $ fromMaybe [] m of
      [] -> Nothing
      xs -> Just xs
  where
    f contractName =
      if T.isInfixOf (T.toCaseFold needle) (T.toCaseFold contractName)
      then Just (DeployedContract contractName backendName backendUri)
      else Nothing

contractList :: MonadWidget t m => (a -> m ()) -> Map Int a -> m (Map Int (Event t a))
contractList rowFunc contracts = do
    divClass "contracts" $ elClass "ol" "contracts-list" $
      for contracts $ \c -> el "li" $ do
        divClass "counter" blank
        rowFunc c
        divClass "load-button" $ loadButton c

loadButton :: MonadWidget t m => a -> m (Event t a)
loadButton c = do
  (e,_) <- uiButton def $ imgWithAlt (static @"img/view.svg") "View" blank >> text "View"
  return $ c <$ e
