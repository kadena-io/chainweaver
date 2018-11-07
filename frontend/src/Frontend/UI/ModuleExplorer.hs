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
import           Control.Monad.State.Strict
import           Data.Aeson                  as Aeson (Object, encode, fromJSON, Result(..))
import qualified Data.ByteString.Lazy        as BSL
import           Data.Foldable
import qualified Data.HashMap.Strict         as H
import qualified Data.List                   as L
import qualified Data.List.Zipper            as Z
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.Semigroup
import           Data.Sequence               (Seq)
import qualified Data.Sequence               as S
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Traversable            (for)
import           Generics.Deriving.Monoid    (mappenddefault, memptydefault)
import           GHC.Generics                (Generic)
import           Language.Javascript.JSaddle hiding (Object)
import           Reflex
import           Reflex.Dom.ACE.Extended
import qualified Reflex.Dom.Contrib.Widgets.DynTabs as Tabs
import           Reflex.Dom
import           Reflex.Network
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as Events
------------------------------------------------------------------------------
import qualified Pact.Compile                as Pact
import qualified Pact.Parse                  as Pact
import           Pact.Repl
import           Pact.Repl.Types
import           Pact.Types.Lang
import           Obelisk.Generated.Static
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Ide
import           Frontend.UI.Button
import           Frontend.Widgets
------------------------------------------------------------------------------

------------------------------------------------------------------------------

moduleExplorer
  :: forall t m. MonadWidget t m
  => Ide t
  -> m (IdeCfg t)
moduleExplorer ideL = do
    exampleClick <- accordionItem True mempty "Example Contracts" $ do
      divClass "contracts" $ elClass "ol" "contracts-list" $
        for demos $ \c -> el "li" $ do
          text $ _exampleContract_name c
          loadButton c
    let exampleLoaded = fmap Left . leftmost $ Map.elems exampleClick

    let mkMap = Map.fromList . map (\k@(BackendName n, _) -> (Just k, n)) . Map.toList
        opts = Map.insert Nothing "All backends" . maybe mempty mkMap <$>
                 _backend_backends (_ide_backend ideL)
    let itemsPerPage = 5 :: Int

    searchLoaded <- accordionItem True mempty "Deployed Contracts" $ mdo
      (filteredCs, updatePage) <- divClass "contracts-controls" $ do
        ti <- textInput $ def & attributes .~ constDyn ("placeholder" =: "Search")
        d <- dropdown Nothing opts def
        let
          search = value ti
          backend = value d
          deployedContracts = Map.mergeWithKey (\_ a b -> Just (a, b)) mempty mempty
              <$> _backend_modules (_ide_backend ideL)
              <*> (fromMaybe mempty <$> _backend_backends (_ide_backend ideL))
          filteredCsRaw = searchFn <$> search <*> backend <*> deployedContracts
        filteredCs <- holdUniqDyn filteredCsRaw
        updatePage <- paginationWidget currentPage totalPages
        return (filteredCs, updatePage)

      let paginated = paginate itemsPerPage <$> currentPage <*> filteredCs

      -- TODO Might need to change back to listWithKey
      searchClick <- networkHold (return mempty) $ contractList (text . _deployedContract_name) <$> updated paginated

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

contractList :: MonadWidget t m => (a -> m b) -> Map Int a -> m (Map Int (Event t a))
contractList rowFunc contracts = do
    divClass "contracts" $ elClass "ol" "contracts-list" $
      for contracts $ \c -> el "li" $ do
        rowFunc c
        loadButton c

loadButton :: MonadWidget t m => a -> m (Event t a)
loadButton c = do
  (e,_) <- uiButton $ imgWithAlt (static @"img/view.svg") "View" blank >> text "View"
  return $ c <$ e
