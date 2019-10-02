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
{-# LANGUAGE PatternGuards          #-}
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
import           Control.Applicative         (liftA2)
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Either                 (rights)
import           Data.Foldable
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import           GHCJS.DOM.Element
import qualified Pact.Types.ChainId as Pact
import qualified Pact.Types.PactValue as Pact
import qualified Pact.Types.Term as Pact
import           Reflex
import           Reflex.Dom.Core
------------------------------------------------------------------------------
import           Frontend.Crypto.Ed25519
import           Frontend.Foundation
import           Frontend.Ide
import           Frontend.Messages
import           Frontend.UI.DeploymentSettings (uiChainSelection)
import           Frontend.UI.JsonData
import           Frontend.JsonData
import           Frontend.UI.Modal.Impl
import           Frontend.UI.ModuleExplorer
import           Frontend.UI.Repl
import           Frontend.UI.TabBar
import           Frontend.UI.Wallet
import           Frontend.UI.Widgets
import           Frontend.Wallet
import           Frontend.Network
import           Frontend.UI.ErrorList
import           Frontend.Editor (HasEditorCfg, HasEditor)
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

  let specificPaneClass = \case
        EnvSelection_Repl -> " pane__body_heightfix"
        EnvSelection_Msgs -> " pane__body_heightfix"
        _ -> ""
      as = ffor curSelection $ \s -> "class" =: ("tab-set pane__body" <> specificPaneClass s)
  divClass "pane__body-wrapper" $ elDynAttr "div" as $ do

    envCfg <- tabPane mempty curSelection EnvSelection_Env $
      envTab ideL

    (e, replCfg) <- tabPane' mempty curSelection EnvSelection_Repl $
      replWidget ideL
    setFocusOnSelected e "input" EnvSelection_Repl $ updated curSelection

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

envTab
  :: ( MonadWidget t m
     , Frontend.Editor.HasEditorCfg mConf t
     , HasModalCfg mConf (Event t () -> m (ModalCfg mConf t, Event t ())) t
     , Monoid mConf, Monoid (ModalCfg mConf t), Flattenable mConf t
     , Flattenable (ModalCfg mConf t) t
     , HasWalletCfg (ModalCfg mConf t) t, HasWalletCfg mConf t
     , HasJsonDataCfg mConf t, HasWallet model t, HasJsonData model t
     , HasNetwork model t, HasEditor model t
     )
  => model -> m mConf
envTab m = do

  errCfg  <- uiErrorList m

  jsonCfg <- accordionItem True "segment" "Data" $
    uiJsonData (m ^. wallet) (m ^. jsonData)

  let w = m ^. wallet
      walletHeader = do
        text "Wallet ("
        display (Map.size <$> _wallet_keys w)
        text " keys)"
  (_,keysCfg) <-
    accordionItem' True "segment" walletHeader $
      uiWallet w

  accountCfg <- accordionItem True "segment" "Accounts" $ uiAccounts m

  pure $ jsonCfg <> keysCfg <> errCfg <> accountCfg

-- | Add an account on a particular chain. The return event is really a
-- _request_ to add an account: the actual lookup must be done elsewhere.
addAccountForm :: (MonadWidget t m, HasNetwork model t) => model -> m (Event t (ChainId, AccountName))
addAccountForm model = divClass "new-by-name group__header" $ divClass "new-by-name_inputs" $ do
  mChainId <- uiChainSelection ((^? to rights . _head) <$> model ^. network_selectedNodes) "select_no_border"
  rec
    nameText <- uiInputElement $ def
      & initialAttributes .~ "class" =: "new-by-name__input" <> "placeholder" =: "Enter an account name"
      & inputElementConfig_setValue .~ ("" <$ done)
    let values = liftA2 combineFields (value nameText) mChainId
    add <- flip uiButtonDyn (text "Add") $ btnCfgPrimary
      & uiButtonCfg_class <>~ "new-by-name__button"
      & uiButtonCfg_disabled .~ fmap isNothing values
    let done = tagMaybe (current values) add
  pure done
  where
    combineFields n (Just c)
      | Right an <- mkAccountName n = Just (c, an)
    combineFields _ _ = Nothing

-- | Display the list of accounts we know about, and allow the user to add /
-- delete them.
uiAccounts
  :: (MonadWidget t m, Monoid mConf, HasWalletCfg mConf t, HasNetwork model t, HasWallet model t)
  => model
  -> m mConf
uiAccounts model = divClass "group" $ do
  add <- addAccountForm model
  let accountGuardReq acc = "(at \"guard\" (coin.account-info " <> tshow (unAccountName acc) <> "))"
      mkReq pm (chainId, acc) = ((chainId, acc),) <$> mkSimpleReadReq (accountGuardReq acc) pm (ChainRef Nothing chainId)
  networkRequest <- performEvent $ attachWith mkReq (current $ model ^. network_meta) add
  response <- performLocalReadCustom (model ^. network) (pure . snd) networkRequest
  let toKeyset (((chainId, acc), _req), [Right (_, Pact.PGuard g)]) = Just (chainId, acc, fromPactGuard g)
      toKeyset _ = Nothing
  elAttr "table" ("class" =: "table" <> "style" =: "table-layout: fixed; width: 100%;") $ do
    refresh <- el "thead" $ do
      elClass "th" "table__heading" $ text "Chain ID"
      elClass "th" "table__heading" $ text "Account Name"
      refresh <- elClass "th" "table__heading" $ do
        text "Balance "
        refreshButton "button_border_none"
      elClass "th" "table__heading" $ text "Associated Keys"
      elClass "th" "table__heading" $ blank
      pure refresh
    deleteChainAccounts <- el "tbody" $ do
      let chainAccounts = model ^. wallet_accountGuards
      dyn_ $ ffor chainAccounts $ \m -> if all Map.null m
        then elClass "tr" "table__row" $ el "td" $ text "No accounts"
        else blank
      listWithKey chainAccounts $ \chain accounts -> do
        deleteAccounts <- listWithKey accounts $ \account accountGuard -> elClass "tr" "table__row" $ do
          el "td" $ text $ Pact._chainId chain
          el "td" $ text $ unAccountName account
          el "td" $ showBalance model refresh chain account
          el "td" $ dyn_ $ ffor accountGuard $ \case
            AccountGuard_Other g -> text $ pactGuardTypeText g
            AccountGuard_KeySet ks -> for_ (Pact._ksKeys ks) $ \key -> do
              divClass "wallet__key wallet__key_type_public" $ text $ keyToText $ fromPactPublicKey key
          del <- elClass "td" "wallet__delete" $ deleteButton $ def
            & uiButtonCfg_title .~ Just "Delete Account/Key Association"
            & uiButtonCfg_class %~ (<> "wallet__add-delete-button")
          pure $ account <$ del
        pure $ switch $ leftmost . Map.elems <$> current deleteAccounts
    let deleteAccount = switch $ leftmost . fmap (\(c, e) -> fmap (c,) e) . Map.toList <$> current deleteChainAccounts
    pure $ mempty
      & walletCfg_deleteAccount .~ deleteAccount
      & walletCfg_addAccount .~ fmapMaybe toKeyset response

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
