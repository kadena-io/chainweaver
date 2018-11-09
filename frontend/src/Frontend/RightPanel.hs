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

module Frontend.RightPanel where

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
import           Reflex
import           Reflex.Dom.ACE.Extended
import           Reflex.Dom.Contrib.Utils
import           Reflex.Dom.Core
import           Reflex.Dom.SemanticUI       hiding (mainWidget)
------------------------------------------------------------------------------
import qualified Pact.Compile                as Pact
import qualified Pact.Parse                  as Pact
import           Pact.Repl
import           Pact.Repl.Types
import           Pact.Types.Lang
import           Obelisk.Generated.Static
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Foundation
import           Frontend.Ide
import           Frontend.JsonData
import           Frontend.UI.Button
import           Frontend.UI.Dialogs.DeployConfirmation
import           Frontend.UI.JsonData
import           Frontend.UI.ModuleExplorer
import           Frontend.UI.Repl
import           Frontend.UI.Wallet
import           Frontend.Wallet
import           Frontend.Widgets
------------------------------------------------------------------------------


selectionToText :: EnvSelection -> Text
selectionToText = \case
  EnvSelection_Repl -> "REPL"
  EnvSelection_Env -> "Env"
  EnvSelection_Msgs -> "Messages"
  EnvSelection_Functions -> "Functions"
  EnvSelection_ModuleExplorer -> "Module Explorer"

tabIndicator :: MonadWidget t m => EnvSelection -> Dynamic t Bool -> m (Event t ())
tabIndicator tab isSelected = do
  let f sel = if sel then ("class" =: "active") else mempty
  (e,_) <- elDynAttr' "button" (f <$> isSelected) $ text $ selectionToText tab
  return $ domEvent Click e

mkTab
    :: (MonadWidget t m)
    => Dynamic t EnvSelection
    -> EnvSelection
    -> m (Event t EnvSelection)
mkTab currentTab t = do
    e <- tabIndicator t ((==t) <$> currentTab)
    return (t <$ e)

tabBar
  :: (MonadWidget t m)
  => EnvSelection
  -> [EnvSelection]
  -> Event t EnvSelection
  -> m (Dynamic t EnvSelection)
tabBar initialSelected initialTabs selectionUpdates = do
  elAttr "div" ("class" =: "tab-nav") $ do
    rec let tabFunc = mapM (mkTab currentTab)
        foo <- widgetHoldHelper tabFunc initialTabs never
        let bar = switch $ fmap leftmost $ current foo
        currentTab <- holdDyn initialSelected $ leftmost [bar, selectionUpdates]
    return currentTab

rightTabBar :: forall t m. MonadWidget t m => Ide t -> m (IdeCfg t)
rightTabBar ideL = do
  elAttr "div" ("id" =: "control-nav" <> "class" =: "tabset") $ do
    let curSelection = _ide_envSelection ideL
    let tabs = [ EnvSelection_Env, EnvSelection_Repl, EnvSelection_Msgs, EnvSelection_ModuleExplorer ]
    curSelection <- tabBar EnvSelection_Env tabs (updated $ _ide_envSelection ideL)

    envCfg <- tabPane ("class" =: "tab-content")  curSelection EnvSelection_Env $ envTab ideL
    replCfg <- tabPane ("class" =: "tab-content") curSelection EnvSelection_Repl $
      replWidget ideL
    errorsCfg <- tabPane ("class" =: "tab-content") curSelection EnvSelection_Msgs $
      msgsWidget ideL
    explorerCfg <- tabPane ("class" =: "tab-content")  curSelection EnvSelection_ModuleExplorer $
      moduleExplorer ideL
    return $ mconcat
      [ envCfg
      , replCfg
      , errorsCfg
      , explorerCfg
      ]

envTab :: MonadWidget t m => Ide t -> m (IdeCfg t)
envTab ideL = do
  jsonCfg <- accordionItem True mempty "Data" $ divClass "control-block-contents" $ do
    conf <- uiJsonData (ideL ^. ide_wallet) (ideL ^. ide_jsonData)
    pure $ mempty &  ideCfg_jsonData .~ conf

  let w = _ide_wallet ideL
      walletHeader = do
        text "Wallet ("
        display (Map.size <$> _wallet_keys w)
        text " keys)"
  (_,keysCfg) <- accordionItem' True mempty walletHeader $ do
    divClass "control-block-contents" $ do
      conf <- uiWallet w
      pure $ mempty & ideCfg_wallet .~ conf

  pure $ jsonCfg <> keysCfg

msgsWidget :: MonadWidget t m => Ide t -> m (IdeCfg t)
msgsWidget ideL = do
  divClass "control-block repl-output iframe" $ do
    divClass "control-block repl-output" $ do
      void . dyn $ traverse_ (snippetWidget . OutputSnippet) <$> _ide_msgs ideL
      pure mempty
