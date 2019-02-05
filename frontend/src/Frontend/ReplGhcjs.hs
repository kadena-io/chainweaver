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
import           Pact.Repl
import           Pact.Repl.Types
import           Pact.Types.Lang
------------------------------------------------------------------------------
import           Frontend.Editor
import           Frontend.Foundation
import           Frontend.Ide
import           Frontend.Messages
import           Frontend.Repl
import           Frontend.UI.Button
import           Frontend.UI.Dialogs.DeployConfirmation (uiDeployConfirmation)
import           Frontend.UI.Modal
import           Frontend.UI.Modal.Impl
import           Frontend.UI.RightPanel
import           Frontend.UI.Widgets
------------------------------------------------------------------------------

app :: MonadWidget t m => m ()
app = void . mfix $ \ cfg -> do
  ideL <- makeIde cfg

  controlCfg <- controlBar ideL
  mainCfg <- elClass "main" "main page__main" $ do
    uiEditorCfg <- codePanel "main__left-pane" ideL
    envCfg <- rightTabBar "main__right-pane" ideL
    pure $ uiEditorCfg <> envCfg

  modalCfg <- showModal ideL

  pure $ mconcat
    [ controlCfg
    , mainCfg
    , modalCfg
    ]

-- | Code editing (left hand side currently)
codePanel :: forall t m a. MonadWidget t m => CssClass -> Ide a t -> m (IdeCfg a t)
codePanel cls m = elKlass "div" (cls <> "pane") $ do
    quickFixCfg <-
      uiQuickFix "pane__header tab-nav tab-nav_type_primary" m
    (e, eCfg) <- wysiwyg $ do
      onNewCode <- tagOnPostBuild $ m ^. editor_code
      let annotations = map toAceAnnotation <$> m ^. editor_annotations
      onUserCode <- codeWidget annotations "" onNewCode
      pure $ mempty & editorCfg_setCode .~ onUserCode

    onCtrlEnter <- getCtrlEnterEvent e
    loadCfg <- loadCodeIntoRepl m onCtrlEnter
    pure $ mconcat [ eCfg , loadCfg, quickFixCfg ]
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

uiQuickFix
  :: forall t m model mConf
  . ( MonadWidget t m
    , HasEditor model t , HasEditorCfg mConf t, Monoid mConf
    )
    => CssClass -> model -> m mConf
uiQuickFix cls m = do
    onFixesEv <- networkView $ ffor (m ^. editor_quickFixes) $ \case
      []    ->
        pure []
      fixes -> elKlass "div" cls $ do
        traverse renderQuickFix fixes

    onFixes <- switchHold never $ leftmost <$> onFixesEv

    pure $ mempty & editorCfg_applyQuickFix .~ onFixes
  where
    renderQuickFix qf = do
      let btnCls = "error_inline button_size_full-width"
      onClick <- uiButton (btnCfgTertiary & uiButtonCfg_class %~ (<> btnCls)
                            & uiButtonCfg_title .~ Just (renderQuickFixTitle qf)
                          ) $
        text $ renderQuickFixName qf
      pure $ qf <$ onClick

    renderQuickFixName = \case
      QuickFix_MissingEnvKeyset ks -> "Fix Keyset Error"
      QuickFix_MissingKeyset ks -> "Fix Keyset Error"

    renderQuickFixTitle = \case
      QuickFix_MissingEnvKeyset ks -> "Adds keyset '" <> ks <> "' to Env."
      QuickFix_MissingKeyset ks -> "Adds (define-keyset '" <> ks <> " ...) and also adds an empty keyset to Env."

-- | Reset REPL and load current editor text into it.
--
--   Reset REPL on load for now until we get an ok to drop this.
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
      divClass "flexbox even" $ do
        ideCfgL <- controlBarLeft m
        controlBarRight
        return ideCfgL

controlBarLeft :: forall t m. MonadWidget t m => ModalIde m t -> m (ModalIdeCfg m t)
controlBarLeft m = do
    divClass "flex main-header__left-nav" $ do
      elClass "h1" "logo-heading" $ do
        imgWithAlt (static @"img/pact-logo.svg") "Kadena Pact Logo" blank
        ver <- getPactVersion
        elClass "span" "version" $ text $ "v" <> ver
      elClass "div" "main-header__project-loader" $ do
        resetCfg <- resetBtn

        onLoadClicked <- loadReplBtn

        onDeployClick <- deployBtn

        loadCfg <- loadCodeIntoRepl m onLoadClicked
        let
          reqConfirmation :: Event t (Maybe (ModalImpl m t))
          reqConfirmation = Just (uiDeployConfirmation m) <$ onDeployClick

          deployCfg = mempty & modalCfg_setModal .~ reqConfirmation
        pure $ deployCfg <> loadCfg <> resetCfg
  where
    headerBtnCfg = btnCfgPrimary & uiButtonCfg_class %~ (<> "main-header__button")

    deployBtn = uiButton headerBtnCfg $
      text $ "Deploy"

    resetBtn = do
      onClick <- uiButton (headerBtnCfg & uiButtonCfg_title .~ Just "Reset REPL and Messages") $
        text $ "Reset"
      pure $ mempty
        & messagesCfg_clear .~ onClick
        & replCfg_reset .~ onClick

    loadReplBtn =
      uiButton ( headerBtnCfg & uiButtonCfg_title .~ Just "Editor Shortcut: Ctrl+Enter") $
        text "Load into REPL"

getPactVersion :: MonadWidget t m => m Text
getPactVersion = do
    is <- liftIO $ initReplState StringEval Nothing
    Right (TLiteral (LString ver) _) <- liftIO $ evalStateT (evalRepl' "(pact-version)") is
    return ver

controlBarRight :: MonadWidget t m => m ()
controlBarRight = do
    divClass "flex right main-header__docs" $ do
      elAttr "a" ( "href" =: "http://pact-language.readthedocs.io"
                <> "class" =: "main-header__documents" <> "target" =: "_blank"
                 ) $ do
        imgWithAlt (static @"img/document.svg") "Documentation" blank
        text "Docs"
      elAttr "a" ( "href" =: "http://kadena.io"
                <> "class" =: "main-header__documents" <> "target" =: "_blank") $
        imgWithAlt (static @"img/gray-kadena-logo.svg") "Kadena Logo" blank
