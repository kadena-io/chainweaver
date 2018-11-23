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
import           Frontend.Ide
import           Frontend.Messages
import           Frontend.UI.JsonData
import           Frontend.UI.ModuleExplorer
import           Frontend.UI.Repl
import           Frontend.UI.Wallet
import           Frontend.UI.Widgets
import           Frontend.Wallet
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
  => [EnvSelection]
  -> Dynamic t EnvSelection
  -> m (Event t EnvSelection)
tabBar initialTabs currentTab = do
  elAttr "div" ("class" =: "tab-nav") $ do
    selEvs <- traverse (mkTab currentTab) initialTabs
    pure $ leftmost selEvs

rightTabBar :: forall t m. MonadWidget t m => Ide t -> m (IdeCfg t)
rightTabBar ideL = do
  elAttr "div" ("id" =: "control-nav" <> "class" =: "tabset") $ do
    let curSelection = _ide_envSelection ideL
    let tabs = [ EnvSelection_Env, EnvSelection_Repl, EnvSelection_Msgs, EnvSelection_ModuleExplorer ]
    onTabClick <- tabBar tabs curSelection

    envCfg <- tabPane ("class" =: "tab-content") curSelection EnvSelection_Env $
      envTab ideL
    replCfg <- tabPane ("class" =: "tab-content") curSelection EnvSelection_Repl $
      replWidget ideL
    errorsCfg <- tabPane ("class" =: "tab-content") curSelection EnvSelection_Msgs $
      msgsWidget ideL
    explorerCfg <- tabPane ("class" =: "tab-content") curSelection EnvSelection_ModuleExplorer $
      moduleExplorer ideL
    return $ mconcat
      [ envCfg
      , replCfg
      , errorsCfg
      , explorerCfg
      , mempty & ideCfg_selEnv .~ onTabClick
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

msgsWidget :: forall t m. MonadWidget t m => Ide t -> m (IdeCfg t)
msgsWidget ideL = do
  divClass "control-block repl-output iframe" $ do
    -- This is really slow, but we usually only have a handful messages:
    divClass "control-block repl-output" $ do
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
