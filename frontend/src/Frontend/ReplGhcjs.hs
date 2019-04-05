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
import           Data.Dependent.Sum                     (DSum ((:=>)))
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
import           Common.Route
import           Frontend.Editor
import           Frontend.Foundation
import           Frontend.Ide
import           Frontend.ModuleExplorer
import           Frontend.ModuleExplorer.RefPath
import           Frontend.OAuth                         (HasOAuth (..))
import           Frontend.Repl
import           Frontend.UI.Button
import           Frontend.UI.Dialogs.DeployConfirmation (uiDeployConfirmation)
import           Frontend.UI.Modal
import           Frontend.UI.Modal.Impl
import           Frontend.UI.RightPanel
import           Frontend.UI.Widgets
------------------------------------------------------------------------------

app
  :: ( MonadWidget t m
     , Routed t (R FrontendRoute) m, RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m
     )
  => m ()
app = void . mfix $ \ cfg -> do

  ideL <- makeIde cfg

  controlCfg <- controlBar ideL
  mainCfg <- elClass "main" "main page__main" $ do
    uiEditorCfg <- codePanel "main__left-pane" ideL
    envCfg <- rightTabBar "main__right-pane" ideL
    pure $ uiEditorCfg <> envCfg

  modalCfg <- showModal ideL

  routesCfg <- handleRoutes ideL

  pure $ mconcat
    [ controlCfg
    , mainCfg
    , modalCfg
    , routesCfg
    ]


handleRoutes
  :: ( MonadWidget t m, Routed t (R FrontendRoute) m, SetRoute t (R FrontendRoute) m
     , Monoid mConf, HasModuleExplorer model t , HasModuleExplorerCfg mConf t
     , HasOAuth model t
     )
  => model
  -> m mConf
handleRoutes m = do
    route <- askRoute
    let loaded = m ^. moduleExplorer_loaded
    onRoute <- tagOnPostBuild route
    let
      onNewLoaded = fmapMaybe id
        . attachWith getLoaded (current loaded)
        $ onRoute

      onOAuthRouteReset = (FrontendRoute_Main :/ ()) <$ m ^. oAuth_error

      onNewRoute = fmapMaybe id
        . attachWith buildRoute (current route)
        $ updated loaded

    setRoute $ leftmost [onNewRoute, onOAuthRouteReset]

    pure $ mempty
      & moduleExplorerCfg_loadFile .~ fmapMaybe (^? _LoadedRef_File) onNewLoaded
      & moduleExplorerCfg_loadModule .~ fmapMaybe (^? _LoadedRef_Module) onNewLoaded

  where

    getLoaded :: Maybe LoadedRef -> R FrontendRoute -> Maybe LoadedRef
    getLoaded cLoaded routeL = do
      let rp = parseRoute routeL
      loaded <- runParseRef rp
      guard $ Just loaded /= cLoaded
      pure loaded

    buildRoute :: R FrontendRoute -> Maybe LoadedRef -> Maybe (R FrontendRoute)
    buildRoute cRoute ref =
      let
        newRoute = maybe (FrontendRoute_New :=> Identity ()) (renderRoute . renderRef) ref
      in
        if newRoute == cRoute
           then Nothing
           else Just newRoute

    parseRoute :: R FrontendRoute -> RefPath
    parseRoute = \case
       FrontendRoute_Main     :=> Identity () -> RefPath []
       FrontendRoute_Example  :=> Identity xs -> RefPath ("example":xs)
       FrontendRoute_Stored   :=> Identity xs -> RefPath ("stored":xs)
       FrontendRoute_Gist     :=> Identity xs -> RefPath ("gist":xs)
       FrontendRoute_Deployed :=> Identity xs -> RefPath ("deployed":xs)
       FrontendRoute_New      :=> Identity () -> RefPath []
       FrontendRoute_OAuth    :=> Identity _  -> RefPath []

    renderRoute :: RefPath -> R FrontendRoute
    renderRoute (RefPath (n:ns)) = case n of
      "example"  -> FrontendRoute_Example  :=> Identity ns
      "stored"   -> FrontendRoute_Stored   :=> Identity ns
      "gist"     -> FrontendRoute_Gist     :=> Identity ns
      "deployed" -> FrontendRoute_Deployed :=> Identity ns
      _          -> FrontendRoute_Main     :=> Identity ()
    renderRoute _ = FrontendRoute_Main :=> Identity ()


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
  => ModalIde m t
  ->  m (ModalIdeCfg m t)
controlBar m = do
    divClass "main-header page__main-header" $ do
      ideCfgL <- controlBarLeft m
      controlBarRight
      return ideCfgL


controlBarLeft :: forall t m. MonadWidget t m => ModalIde m t -> m (ModalIdeCfg m t)
controlBarLeft m = do
    divClass "main-header__left-nav" $ do
      elClass "h1" "main-header__pact-logo" $ do
        imgWithAlt (static @"img/pact-logo.svg") "Kadena Pact Logo" blank
        ver <- getPactVersion
        elClass "span" "version" $ text $ "v" <> ver
      elClass "div" "main-header__project-loader" $ do

        {- resetCfg <- resetBtn -}

        onLoadClicked <- loadReplBtn

        onDeployClick <- deployBtn

        onCreateGist <- gistBtn

        loadCfg <- loadCodeIntoRepl m onLoadClicked
        let
          reqConfirmation :: Event t (Maybe (ModalImpl m t))
          reqConfirmation = Just (uiDeployConfirmation m) <$ onDeployClick

          gistCfg =  mempty & moduleExplorerCfg_createGist .~  onCreateGist

          deployCfg = mempty & modalCfg_setModal .~ reqConfirmation
        {- pure $ deployCfg <> loadCfg <> resetCfg <> gistCfg -}
        pure $ deployCfg <> loadCfg <> gistCfg
  where
    headerBtnCfg = btnCfgPrimary & uiButtonCfg_class %~ (<> "main-header__button")

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


getPactVersion :: MonadWidget t m => m Text
getPactVersion = do
    is <- liftIO $ initReplState StringEval Nothing
    Right (TLiteral (LString ver) _) <- liftIO $ evalStateT (evalRepl' "(pact-version)") is
    return ver

controlBarRight :: MonadWidget t m => m ()
controlBarRight = do
    divClass "main-header__docs" $ do
      elAttr "a" ( "href" =: "http://pactlang.org"
                <> "class" =: "main-header__documents" <> "target" =: "_blank"
                 ) $ do
        elAttr "img" ("src" =: (static @"img/instruction.svg") <> "alt" =: "Documentation" <> "style" =: "width: 28px;") blank
        text "Tutorials"
      elAttr "a" ( "href" =: "http://pact-language.readthedocs.io"
                <> "class" =: "main-header__documents" <> "target" =: "_blank"
                 ) $ do
        imgWithAlt (static @"img/document.svg") "Documentation" blank
        text "Docs"
      elAttr "a" ( "href" =: "http://kadena.io"
                <> "class" =: "main-header__documents" <> "target" =: "_blank") $
        elAttr "img" ("src" =: (static @"img/Klogo.png") <> "alt" =: "Kadena Logo" <> "style" =: "height: 28px;") blank
