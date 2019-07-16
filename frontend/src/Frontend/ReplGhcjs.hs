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

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.Reader                   (ask)
import           Control.Monad.State.Strict
import           Data.Default                           (Default (..))
import qualified Data.Map                               as Map
import           Data.String                            (IsString)
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           GHCJS.DOM.EventM                       (on)
import           GHCJS.DOM.GlobalEventHandlers          (keyPress)
import           GHCJS.DOM.KeyboardEvent                (getCtrlKey, getKey,
                                                         getKeyCode, getMetaKey)
import           GHCJS.DOM.Types                        (HTMLElement (..),
                                                         unElement)
import           Language.Javascript.JSaddle            (liftJSM)
import           Reflex
import           Reflex.Dom.ACE.Extended                hiding (Annotation (..))
import           Reflex.Dom.Core
------------------------------------------------------------------------------
import           Obelisk.Generated.Static
import           Obelisk.Route                          (R)
import           Obelisk.Route.Frontend
import           Pact.Repl
import           Pact.Repl.Types
import           Pact.Types.Lang
------------------------------------------------------------------------------
import           Common.OAuth                           (OAuthProvider (OAuthProvider_GitHub))
import           Common.Route
import           Frontend.Editor
import           Frontend.Foundation
import           Frontend.GistStore
import           Frontend.Ide
import           Frontend.OAuth
import           Frontend.Repl
import           Frontend.UI.Button
import           Frontend.UI.Dialogs.CreatedGist        (uiCreatedGist)
import           Frontend.UI.Dialogs.CreateGist         (uiCreateGist)
import           Frontend.UI.Dialogs.DeployConfirmation (uiDeployConfirmation)
import           Frontend.UI.Dialogs.LogoutConfirmation (uiLogoutConfirmation)
import           Frontend.UI.Dialogs.NetworkEdit        (uiNetworkEdit)
import           Frontend.UI.Modal
import           Frontend.UI.Modal.Impl
import           Frontend.UI.RightPanel
------------------------------------------------------------------------------

app
  :: ( MonadWidget t m
     , Routed t (R FrontendRoute) m, RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m
     , HasConfigs m
     )
  => AppCfg t m -> m ()
app appCfg = void . mfix $ \ cfg -> do

  ideL <- makeIde appCfg cfg

  controlCfg <- controlBar appCfg ideL
  mainCfg <- elClass "main" "main page__main" $ do
    uiEditorCfg <- codePanel "main__left-pane" ideL
    envCfg <- rightTabBar "main__right-pane" ideL
    pure $ uiEditorCfg <> envCfg

  modalCfg <- showModal ideL

  let
    onGistCreatedModal = Just . uiCreatedGist <$> ideL ^. gistStore_created
    gistModalCfg = mempty & modalCfg_setModal .~ onGistCreatedModal

  pure $ mconcat
    [ controlCfg
    , mainCfg
    , modalCfg
    , gistModalCfg
    , mempty & ideCfg_editor . editorCfg_loadCode .~ _appCfg_externalFileOpened appCfg
    ]

-- | Code editing (left hand side currently)
codePanel :: forall t m a. MonadWidget t m => CssClass -> Ide a t -> m (IdeCfg a t)
codePanel cls m = elKlass "div" (cls <> "pane") $ do
    (e, eCfg) <- wysiwyg $ do
      onNewCode <- tagOnPostBuild $ m ^. editor_code
      let annotations = map toAceAnnotation <$> m ^. editor_annotations
      onUserCode <- codeWidget annotations "" onNewCode
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
  :: forall t m model a
  . (MonadWidget t m, HasEditor model t)
   => model
   -> Event t ()
   -> m (IdeCfg a t)
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
  :: MonadWidget t m
  => Event t [AceAnnotation]
  -> Text
  -> Event t Text
  -> m (Event t Text)
codeWidget anno iv sv = do
    let ac = def { _aceConfigMode = Just "ace/mode/pact"
                 , _aceConfigElemAttrs = "class" =: "ace-code ace-widget"
                 }
    ace <- resizableAceWidget mempty ac (AceDynConfig Nothing) anno iv sv
    return $ _extendedACE_onUserChange ace


controlBar
  :: forall t m. MonadWidget t m
  => AppCfg t m
  -> ModalIde m t
  ->  m (ModalIdeCfg m t)
controlBar appCfg m = do
    mainHeader $ do
      controlBarLeft
      controlBarCenter
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

controlBarCenter :: forall t m. MonadWidget t m => m ()
controlBarCenter = divClass "main-header__center-box" $
  divClass "main-header__center" $
    kadenaLogo
  where
    kadenaLogo =
      elAttr "a"
        ( "href" =: "https://kadena.io"
          <> "class" =: "main-header__kadena-logo" <> "target" =: "_blank"
        ) $
        elAttr "img"
          ( "src" =: static @"img/Klogo.png"
            <> "alt" =: "Kadena Logo"
            <> "class" =: "main-header__logo-img"
          ) blank


controlBarLeft :: forall t m. MonadWidget t m => m ()
controlBarLeft =
  divClass "main-header__logos-docs" $ do
    {- kadenaLogo -}
    pactLogo
    docs

  where

    pactLogo =
      elClass "div" "main-header__pact-logo" $ do
        elAttr "img"
          ( "src" =: static @"img/pact-logo.svg"
            <> "alt" =: "Kadena Pact Logo"
            <> "class" =: "main-header__pact-logo-img"
          ) blank
        elClass "span" "main-header__pact-version" $ do
          ver <- getPactVersion
          text $ "v" <> ver

    docs = divClass "main-header__docs" $ do
      elAttr "a" ( "href" =: "https://pactlang.org"
                <> "class" =: "main-header__documents" <> "target" =: "_blank"
                 ) $ do
        elAttr "img" ("src" =: static @"img/instruction.svg" <> "alt" =: "Documentation" <> "class" =: "main-header__documents-img" <> "style" =: "width: 28px;") blank
        text "Tutorials"

      elAttr "a" ( "href" =: "http://pact-language.readthedocs.io"
                <> "class" =: "main-header__documents" <> "target" =: "_blank"
                 ) $ do
        elAttr "img" ("src" =: static @"img/document.svg" <> "class" =: "main-header__documents-img") blank
        text "Docs"


getPactVersion :: MonadWidget t m => m Text
getPactVersion = do
    is <- liftIO $ initReplState StringEval Nothing
    Right (TLiteral (LString ver) _) <- liftIO $ evalStateT (evalRepl' "(pact-version)") is
    return ver

controlBarRight :: forall t m. MonadWidget t m => AppCfg t m -> ModalIde m t -> m (ModalIdeCfg m t)
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
          reqConfirmation :: Event t (Maybe (ModalImpl m t))
          reqConfirmation = Just (uiDeployConfirmation m) <$ onDeployClick

          gistConfirmation :: Event t (Maybe (ModalImpl m t))
          gistConfirmation = Just uiCreateGist <$ onCreateGist

          networkEdit :: Event t (Maybe (ModalImpl m t))
          networkEdit = Just (uiNetworkEdit m) <$ onNetClick

          logoutConfirmation :: Event t (Maybe (ModalImpl m t))
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

    deployBtn = uiButton headerBtnCfg $
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
