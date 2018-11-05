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
import           Reflex.Dom.Core             (keypress)
import qualified Reflex.Dom.Core             as Core
import           Reflex.Dom.SemanticUI       hiding (mainWidget)
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
moduleExplorer ideL = mdo
    exampleClick <- accordionItem True mempty "Example Contracts" $ do
      divClass "contracts" $ elClass "ol" "contracts-list" $
        for demos $ \c -> el "li" $ do
          text $ _exampleContract_name c
          loadButton
    let exampleLoaded = fmap Left . leftmost $ Map.elems exampleClick

    (search, backend) <- accordionItem True mempty "Deployed Contracts" $ do
      searchI <- field def $ input (def & inputConfig_icon .~ Static (Just RightIcon)) $ do
        ie <- inputElement $ def & initialAttributes .~ ("type" =: "text" <> "placeholder" =: "Search modules")
        icon "black search" def
        pure ie

      let mkMap = Map.fromList . map (\k@(BackendName n, _) -> (Just k, text n)) . Map.toList
          dropdownConf = def
            & dropdownConfig_placeholder .~ "Backend"
            & dropdownConfig_fluid .~ pure True
      d <- field def $ input def $ dropdown dropdownConf (Identity Nothing) $ TaggedDynamic $
        Map.insert Nothing (text "All backends") . maybe mempty mkMap <$> ideL ^. ide_backend . backend_backends
      pure (value searchI, value d)

    let
      deployedContracts = Map.mergeWithKey (\_ a b -> Just (a, b)) mempty mempty
          <$> ideL ^. ide_backend . backend_modules
          <*> (fromMaybe mempty <$> ideL ^. ide_backend . backend_backends)
      searchFn needle (Identity mModule)
        = concat . fmapMaybe (filtering needle) . Map.toList
        . maybe id (\(k', _) -> Map.filterWithKey $ \k _ -> k == k') mModule
      filtering needle (backendName, (m, backendUri)) =
        let f contractName =
              if T.isInfixOf (T.toCaseFold needle) (T.toCaseFold contractName)
              then Just (DeployedContract contractName backendName backendUri, ())
              else Nothing
        in case fmapMaybe f $ fromMaybe [] m of
          [] -> Nothing
          xs -> Just xs
      filteredCsRaw = searchFn <$> search <*> backend <*> deployedContracts
      paginate p =
        Map.fromList . take itemsPerPage . drop (itemsPerPage * pred p) . L.sort
    filteredCs <- holdUniqDyn filteredCsRaw
    let
      paginated = paginate <$> currentPage <*> filteredCs

    searchLoaded <- divClass "ui inverted selection list" $ do
      searchClick <- listWithKey paginated $ \c _ -> do
        label (def & labelConfig_horizontal .~ Static True) $ do
          text $ unBackendName $ _deployedContract_backendName c
        text $ _deployedContract_name c
        loadButton
      let searchLoaded1 = switch . current $ fmap Right . leftmost . Map.elems <$> searchClick
      pure (searchLoaded1)

    let itemsPerPage = 5 :: Int
        numberOfItems = length <$> filteredCs
        calcTotal a = ceiling $ (fromIntegral a :: Double)  / fromIntegral itemsPerPage
        totalPages = calcTotal <$> numberOfItems
    rec
      currentPage <- holdDyn 1 $ leftmost
        [ updatePage
        , 1 <$ updated numberOfItems
        ]
      updatePage <- paginationWidget currentPage totalPages

    pure $ mempty
      { _ideCfg_selContract = never --leftmost [searchLoaded, exampleLoaded]
      }
  where
    selectableItem :: k -> Dynamic t Bool -> m a -> m (Event t k, a)
    selectableItem k s m = do
      let mkAttrs a = Map.fromList
            [ ("style", "position:relative")
            , ("class", "item" <> (if a then " active" else ""))
            ]
      (e, a) <- elDynAttr' "a" (mkAttrs <$> s) m
      pure (k <$ domEvent Click e, a)
    loadButton = do
      (e,_) <- uiButton $ imgWithAlt (static @"img/view.svg") "View" blank >> text "View"
      return e
    loadButtonOld s = switchHold never <=< dyn $ ffor s $ \case
      False -> pure never
      True -> let buttonStyle = "position: absolute; right: 0; top: 0; height: 100%; margin: 0"
                in button (def & classes .~ "primary" & style .~ buttonStyle) $ text "Load"
