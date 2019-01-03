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

module Frontend.UI.RightPanel where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Foldable
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import           GHCJS.DOM.Element
import           Language.Javascript.JSaddle (liftJSM)
import           Reflex
import           Reflex.Dom.Core
------------------------------------------------------------------------------
import           Frontend.Foundation
import           Frontend.Ide
import           Frontend.Messages
import           Frontend.UI.JsonData
import           Frontend.UI.Modal.Impl
import           Frontend.UI.ModuleExplorer
import           Frontend.UI.Repl
import           Frontend.UI.TabBar
import           Frontend.UI.Wallet
import           Frontend.UI.Widgets
import           Frontend.Wallet
------------------------------------------------------------------------------

selectionToText :: EnvSelection -> Text
selectionToText = \case
  EnvSelection_Repl -> "REPL"
  EnvSelection_Env -> "Env"
  EnvSelection_Msgs -> "Messages"
  EnvSelection_ModuleExplorer -> "Module Explorer"

rightTabBar
  :: forall t m. MonadWidget t m
  => CssClass
  -> Ide (ModalImpl m t) t
  -> m (IdeCfg (ModalImpl m t) t)
rightTabBar cls ideL = elKlass "div" (cls <> "pane") $ do
  let curSelection = _ide_envSelection ideL
  (TabBar onTabClick) <- makeTabBar $ TabBarCfg
    { _tabBarCfg_tabs = [minBound .. maxBound]
    , _tabBarCfg_mkLabel = const $ text . selectionToText
    , _tabBarCfg_selectedTab = Just <$> curSelection
    , _tabBarCfg_classes = "pane__header"
    , _tabBarCfg_type = TabBarType_Primary
    }

  divClass "pane__body-wrapper" $ divClass "tab-set pane__body" $ do

    envCfg <- tabPane mempty curSelection EnvSelection_Env $
      envTab ideL

    (e, replCfg) <- tabPane' mempty curSelection EnvSelection_Repl $
      replWidget ideL
    -- Only set focus on tab click not on updated curSelection! We only want to
    -- set the focus if the switch happened manually.
    setFocusOnSelected e "input" EnvSelection_Repl $ onTabClick

    errorsCfg <- tabPane mempty curSelection EnvSelection_Msgs $
      msgsWidget ideL
    explorerCfg <- tabPane mempty curSelection EnvSelection_ModuleExplorer $
      moduleExplorer ideL
    return $ mconcat
      [ envCfg
      , replCfg
      , errorsCfg
      , explorerCfg
      , mempty & ideCfg_selEnv .~ onTabClick
      ]

envTab :: MonadWidget t m => Ide a t -> m (IdeCfg a t)
envTab ideL = do
  jsonCfg <- accordionItem True "segment" "Data" $ do
    conf <- uiJsonData (ideL ^. ide_wallet) (ideL ^. ide_jsonData)
    pure $ mempty &  ideCfg_jsonData .~ conf

  let w = _ide_wallet ideL
      walletHeader = do
        text "Wallet ("
        display (Map.size <$> _wallet_keys w)
        text " keys)"
  (_,keysCfg) <- accordionItem' True "segment" walletHeader $ do
      conf <- uiWallet w
      pure $ mempty & ideCfg_wallet .~ conf

  pure $ jsonCfg <> keysCfg

msgsWidget :: forall t m a. MonadWidget t m => Ide a t -> m (IdeCfg a t)
msgsWidget ideL = do
    -- This is really slow, but we usually only have a handful messages:
    let
      mNewOld :: Dynamic t (Maybe (Text, [Text]))
      mNewOld = uncons <$> ideL ^. messages_messages

      old = maybe [] snd <$> mNewOld

    void . dyn $ traverse_ (snippetWidget . OldOutputSnippet) . reverse <$> old
    void . dyn $ traverse_ (snippetWithScroll . OutputSnippet . fst) <$> mNewOld

    pure mempty
  where
    snippetWithScroll :: DisplayedSnippet -> m ()
    snippetWithScroll snip = do
      e <- _element_raw <$> snippetWidget' snip
      -- TODO: Find a better/more robust way for deciding when we are good to go ...
      onReady <- delay 0.1 =<< getPostBuild
      performEvent_ $ ffor onReady $ \_ -> liftJSM $
        scrollIntoView e True
