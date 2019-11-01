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

module Frontend.ReplGhcjs where

import Control.Lens
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (..))
import Data.String (IsString)
import Data.Text (Text)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.GlobalEventHandlers (keyPress)
import GHCJS.DOM.KeyboardEvent (getCtrlKey, getKey, getKeyCode, getMetaKey)
import GHCJS.DOM.Types (HTMLElement (..), unElement)
import Language.Javascript.JSaddle (liftJSM)
import Obelisk.Generated.Static
import Obelisk.Route (R)
import Obelisk.Route.Frontend
import Pact.Repl
import Pact.Repl.Types
import Pact.Types.Lang
import Reflex
import Reflex.Dom.ACE.Extended hiding (Annotation (..))
import Reflex.Dom.Core
import qualified Data.Map as Map
import qualified Data.Text as T

import Common.OAuth (OAuthProvider (OAuthProvider_GitHub))
import Common.Route
import Frontend.AppCfg
import Frontend.Crypto.Class
import Frontend.Editor
import Frontend.Foundation
import Frontend.GistStore
import Frontend.Ide
import Frontend.Network
import Frontend.OAuth
import Frontend.Repl
import Frontend.Storage
import Frontend.UI.Button
import Frontend.UI.Dialogs.CreateGist (uiCreateGist)
import Frontend.UI.Dialogs.CreatedGist (uiCreatedGist)
import Frontend.UI.Dialogs.DeployConfirmation (uiDeployConfirmation)
import Frontend.UI.Dialogs.LogoutConfirmation (uiLogoutConfirmation)
import Frontend.UI.Dialogs.NetworkEdit (uiNetworkEdit, uiNetworkSelect, uiNetworkStatus, queryNetworkStatus)
import Frontend.UI.Dialogs.Signing (uiSigning)
import Frontend.UI.Modal
import Frontend.UI.Modal.Impl
import Frontend.UI.RightPanel
import Frontend.UI.Wallet

app
  :: forall key t m.
     ( MonadWidget t m
     , Routed t (R FrontendRoute) m, RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m
     , HasConfigs m
     , HasStorage m, HasStorage (Performable m)
     , HasCrypto key (Performable m)
     , FromJSON key, ToJSON key
     )
  => m ()
  -- ^ Extra widget to display at the bottom of the sidebar
  -> AppCfg key t m -> m ()
app sidebarExtra appCfg = void . mfix $ \ cfg -> do
  ideL <- makeIde appCfg cfg

  walletSidebar sidebarExtra
  route <- demux <$> askRoute
  let mkPage :: R FrontendRoute -> Text -> m a -> m a
      mkPage r c = elDynAttr "div" (ffor (demuxed route r) $ \s -> "class" =: (c <> if s then " page__content visible" else " page__content"))
  updates <- divClass "page" $ do
    netCfg <- networkBar ideL
    walletCfg <- mkPage (FrontendRoute_Wallet :/ ()) "wallet" $ do
      uiWallet ideL
    updates <- mkPage (FrontendRoute_Main :/ ()) "contracts" $ do
      controlCfg <- controlBar appCfg ideL
      mainCfg <- elClass "main" "main page__main" $ do
        uiEditorCfg <- codePanel appCfg "main__left-pane" ideL
        envCfg <- rightTabBar "main__right-pane" ideL
        pure $ uiEditorCfg <> envCfg
      pure $ controlCfg <> mainCfg
    pure $ netCfg <> walletCfg <> updates

  modalCfg <- showModal ideL

  let
    onGistCreatedModal = Just . uiCreatedGist <$> ideL ^. gistStore_created
    gistModalCfg = mempty & modalCfg_setModal .~ onGistCreatedModal
    onSigningModal = Just . uiSigning appCfg ideL <$> _appCfg_signingRequest appCfg
    signingModalCfg = mempty & modalCfg_setModal .~ onSigningModal

  pure $ mconcat
    [ updates
    , modalCfg
    , gistModalCfg
    , signingModalCfg
    , mempty & ideCfg_editor . editorCfg_loadCode .~ _appCfg_externalFileOpened appCfg
    ]

walletSidebar
  :: (DomBuilder t m, PostBuild t m, Routed t (R FrontendRoute) m, SetRoute t (R FrontendRoute) m, RouteToUrl (R FrontendRoute) m)
  => m () -> m ()
walletSidebar sidebarExtra = elAttr "div" ("class" =: "sidebar") $ do
  divClass "sidebar__logo" blank -- TODO missing logo image
  route <- demux <$> askRoute
  let sidebarLink r = routeLink r $ do
        let mkAttrs sel = "class" =: ("sidebar__link" <> if sel then " selected" else "")
        elDynAttr "span" (mkAttrs <$> demuxed route r) $ do
          let (normal, highlighted) = routeIcon r
          elAttr "img" ("class" =: "highlighted" <> "src" =: highlighted) blank
          elAttr "img" ("class" =: "normal" <> "src" =: normal) blank
  sidebarLink $ FrontendRoute_Wallet :/ ()
  sidebarLink $ FrontendRoute_Main :/ ()
  elAttr "div" ("style" =: "flex-grow: 1") blank
  sidebarLink $ FrontendRoute_Resources :/ ()
  sidebarLink $ FrontendRoute_Settings :/ ()
  sidebarExtra

-- | Get the routes to the icon assets for each route
routeIcon :: R FrontendRoute -> (Text, Text)
routeIcon = \case
  FrontendRoute_Main :/ () -> (static @"img/menu/contracts.png", static @"img/menu/contracts_highlighted.png")
  FrontendRoute_Wallet :/ () -> (static @"img/menu/wallet.png", static @"img/menu/wallet_highlighted.png")
  FrontendRoute_Resources :/ () -> (static @"img/menu/resources.png", static @"img/menu/resources_highlighted.png")
  FrontendRoute_Settings :/ () -> (static @"img/menu/settings.png", static @"img/menu/settings_highlighted.png")
  _ -> ("", "")

-- | Code editing (left hand side currently)
codePanel :: forall r key t m a. (MonadWidget t m, Routed t r m) => AppCfg key t m -> CssClass -> Ide a key t -> m (IdeCfg a key t)
codePanel appCfg cls m = elKlass "div" (cls <> "pane") $ do
    (e, eCfg) <- wysiwyg $ do
      onNewCode <- tagOnPostBuild $ m ^. editor_code
      let annotations = map toAceAnnotation <$> m ^. editor_annotations
      onUserCode <- codeWidget appCfg annotations "" onNewCode
      pure $ mempty & editorCfg_setCode .~ onUserCode

    onCtrlEnter <- getCtrlEnterEvent e
    loadCfg <- loadCodeIntoRepl m onCtrlEnter
    pure $ mconcat [ eCfg , loadCfg ]
  where
    wysiwyg = elClass' "div" "wysiwyg pane__body"
    -- We can't use domEvent Keypress because it only gets us the
    -- deprecated key code which does not work cross platform in this case:
    getCtrlEnterEvent e = do
      (onCtrlEnter, triggerEv) <- newTriggerEvent
      let htmlElement = HTMLElement . unElement $ _element_raw e
      void $ liftJSM $ htmlElement `on` keyPress $ do
        ev <- ask
        hasCtrl <- liftJSM $ getCtrlKey ev
        hasMeta <- liftJSM $ getMetaKey ev
        key <- liftJSM $ getKey ev
        code <- liftJSM $ getKeyCode ev
        let hasEnter = key == ("Enter" :: Text) || code == 10 || code == 13

        liftIO $ when ((hasCtrl || hasMeta) && hasEnter) $ triggerEv ()
      pure onCtrlEnter

-- | Load current editor code into REPL.
loadCodeIntoRepl
  :: forall key t m model a
  . (MonadWidget t m, HasEditor model t)
   => model
   -> Event t ()
   -> m (IdeCfg a key t)
loadCodeIntoRepl m onReq = do
  let onLoad = tag (current $ m ^. editor_code) onReq
  pure $ mempty
    & replCfg_sendTransaction .~ onLoad

toAceAnnotation :: Annotation -> AceAnnotation
toAceAnnotation anno = AceAnnotation
  { _aceAnnotation_row = _annotation_line anno -1 -- Ace starts at 0.
  , _aceAnnotation_column = _annotation_column anno
  , _aceAnnotation_text = _annotation_msg anno
  , _aceAnnotation_type = T.pack . show $ _annotation_type anno
  }

codeWidget
  :: (MonadWidget t m, Routed t r m)
  => AppCfg key t m
  -> Event t [AceAnnotation]
  -> Text
  -> Event t Text
  -> m (Event t Text)
codeWidget appCfg anno iv sv = do
    let ac = def { _aceConfigMode = Just "ace/mode/pact"
                 , _aceConfigElemAttrs = "class" =: "ace-code ace-widget"
                 , _aceConfigReadOnly = _appCfg_editorReadOnly appCfg
                 }
    route <- askRoute
    -- Without this delay, sometimes the resize doesn't take place.
    resize <- delay 0.1 . (void (updated route) <>) =<< getPostBuild
    ace <- resizableAceWidget resize mempty ac (AceDynConfig Nothing) anno iv sv
    return $ _extendedACE_onUserChange ace

networkBar
  :: MonadWidget t m
  => ModalIde m key t
  -> m (ModalIdeCfg m key t)
networkBar m = divClass "main-header main-header__network-bar" $ do
  -- Fetch and display the status of the currently selected network.
  queryNetworkStatus (m ^. ide_network . network_networks) (m ^. ide_network . network_selectedNetwork)
    >>= uiNetworkStatus (pure " page__network-bar-status")
  -- Present the dropdown box for selecting one of the configured networks.
  divClass "page__network-bar-select" $
    uiNetworkSelect (m ^. ide_network)

controlBar
  :: forall key t m. (MonadWidget t m, HasCrypto key (Performable m))
  => AppCfg key t m
  -> ModalIde m key t
  ->  m (ModalIdeCfg m key t)
controlBar appCfg m = do
    mainHeader $ do
      controlBarLeft
      controlBarRight appCfg m
  where
    -- Main header with adjusted padding on MacOs (scrollbars take up no space there):
    mainHeader child = do
      isMac <- getBrowserProperty "mac"

      let
        baseCls = "main-header page__main-header "
        cls = if isMac
                 then  baseCls <> "page__main-header_platform_mac"
                 else baseCls
      divClass cls child

controlBarLeft :: forall t m. MonadWidget t m => m ()
controlBarLeft =
  divClass "main-header__page-name" $
    text "Contracts" --TODO: extract from route

getPactVersion :: MonadWidget t m => m Text
getPactVersion = do
    is <- liftIO $ initReplState StringEval Nothing
    ver <- liftIO $ evalStateT (evalRepl' "(pact-version)") is >>= pure . \case
      Right (TLiteral (LString ver) _) -> ver
      _ -> error "failed to get pact version"
    return ver

controlBarRight
  :: forall key t m. (MonadWidget t m, HasCrypto key (Performable m))
  => AppCfg key t m -> ModalIde m key t -> m (ModalIdeCfg m key t)
controlBarRight appCfg m = do
    divClass "main-header__controls-nav" $ do
      elClass "div" "main-header__project-loader" $ do

        _ <- openFileBtn

        onLoadClicked <- loadReplBtn

        onDeployClick <- deployBtn

        onCreateGist <- if _appCfg_gistEnabled appCfg then gistBtn else pure never

        onNetClick <- cogButton headerBtnCfg
        onLogoutClick <- if _appCfg_gistEnabled appCfg then maySignoutBtn else pure never

        loadCfg <- loadCodeIntoRepl m onLoadClicked
        let
          reqConfirmation :: Event t (Maybe (ModalImpl m key t))
          reqConfirmation = attachWith (\c _ -> Just $ uiDeployConfirmation c m) (current $ m ^. editor_code) onDeployClick

          gistConfirmation :: Event t (Maybe (ModalImpl m key t))
          gistConfirmation = Just uiCreateGist <$ onCreateGist

          networkEdit :: Event t (Maybe (ModalImpl m key t))
          networkEdit = Just (uiNetworkEdit m) <$ onNetClick

          logoutConfirmation :: Event t (Maybe (ModalImpl m key t))
          logoutConfirmation = Just uiLogoutConfirmation <$ onLogoutClick

          gistCfg =  mempty & modalCfg_setModal .~  gistConfirmation

          deployCfg = mempty & modalCfg_setModal .~ reqConfirmation

          logoutCfg = mempty & modalCfg_setModal .~ logoutConfirmation

          netCfg = mempty & modalCfg_setModal .~ networkEdit

        pure $ deployCfg <> loadCfg <> gistCfg <> netCfg <> logoutCfg
  where
    maySignoutBtn = do
      let gitHubOnline = Map.member OAuthProvider_GitHub <$> m ^. oAuth_accessTokens
      onEvClick <- networkView $ ffor gitHubOnline $ \isOnline ->
        if isOnline then signoutBtn else pure never
      switchHold never onEvClick

    signoutBtn = signoutButton $
      headerBtnCfg & uiButtonCfg_title .~ Just "Sign out from GitHub"

    deployBtn = uiButton (headerBtnCfg & uiButtonCfg_class <>~ "main-header__deploy-button") $
      text $ "Deploy"

    loadReplBtn =
      uiButton ( headerBtnCfg & uiButtonCfg_title .~ Just "Editor Shortcut: Ctrl+Enter") $ do
        text "Load"
        elClass "span" "main-header__minor-text" $
          text " into REPL"

    gistBtn =
      uiButton
          ( headerBtnCfg
              & uiButtonCfg_title .~ Just "Create gist on GitHub"
              {- & uiButtonCfg_class %~ (<> "main-header__text-icon-button") -}
          ) $ do
        {- btnTextIcon (static @"img/github-gist-dark.svg") "Make Gist" blank -}
        elClass "span" "main-header__minor-text" $ text "Make "
        text "Gist"

    openFileBtn = do
      let cfg = headerBtnCfg & uiButtonCfg_title ?~ "Open a local contract"
      uiButtonWithOnClick (_appCfg_openFileDialog appCfg) cfg $ do
        text "Open"
        elClass "span" "main-header__minor-text" $ text " File"


headerBtnCfg
  :: (Default (UiButtonCfgRep f), IsString (ReflexValue f CssClass), Semigroup (ReflexValue f CssClass))
  => UiButtonCfgRep f
headerBtnCfg = btnCfgPrimary & uiButtonCfg_class %~ (<> "main-header__button")
