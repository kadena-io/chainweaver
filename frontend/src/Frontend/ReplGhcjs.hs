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
  onLoad <- tag (current $ m ^. editor_code) <$> delay 0 onReq
  pure $ mempty
    & replCfg_sendTransaction .~ onLoad
    & replCfg_reset .~ onReq

toAceAnnotation :: Annotation -> AceAnnotation
toAceAnnotation anno = AceAnnotation
  { _aceAnnotation_row = _annotation_line anno -1 -- Ace starts at 0.
  , _aceAnnotation_column = _annotation_column anno
  , _aceAnnotation_text = _annotation_msg anno
  , _aceAnnotation_type = T.pack . show $ _annotation_type anno
  }

{- functionsList :: MonadWidget t m => Ide t -> BackendUri -> [PactFunction] -> m () -}
{- functionsList ideL backendUri functions = divClass "ui very relaxed list" $ do -}
{-   for_ functions $ \(PactFunction (ModuleName moduleName) name _ mdocs funType) -> divClass "item" $ do -}
{-     (e, _) <- elClass' "a" "header" $ do -}
{-       text name -}
{-       text ":" -}
{-       text $ tshow $ _ftReturn funType -}
{-       text " " -}
{-       elAttr "span" ("class" =: "description" <> "style" =: "display: inline") $ do -}
{-         text "(" -}
{-         text $ T.unwords $ tshow <$> _ftArgs funType -}
{-         text ")" -}
{-     for_ mdocs $ divClass "description" . text -}
{-     open <- toggle False $ domEvent Click e -}
{-     dyn_ $ ffor open $ \case -}
{-       False -> pure () -}
{-       True -> el "div" $ el "form" $ do -}
{-         inputs <- for (_ftArgs funType) $ \arg -> field def $ do -}
{-           el "label" $ text $ "Argument: " <> tshow arg -}
{-           case _aType arg of -}
{-             TyPrim TyInteger -> fmap value . input def $ inputElement $ def -}
{-               & inputElementConfig_elementConfig . initialAttributes .~ Map.fromList -}
{-                 [ ("type", "number") -}
{-                 , ("step", "1") -}
{-                 , ("placeholder", _aName arg) -}
{-                 ] -}
{-             TyPrim TyDecimal -> do -}
{-               ti <- input def $ inputElement $ def -}
{-                 & inputElementConfig_elementConfig . initialAttributes .~ Map.fromList -}
{-                   [ ("type", "number") -}
{-                   , ("step", "0.0000000001") -- totally arbitrary -}
{-                   , ("placeholder", _aName arg) -}
{-                   ] -}
{-               pure $ (\x -> if T.isInfixOf "." x then x else x <> ".0") <$> value ti -}
{-             TyPrim TyTime -> do -}
{-               i <- input def $ inputElement $ def -}
{-                 & inputElementConfig_elementConfig . initialAttributes .~ Map.fromList -}
{-                   [ ("type", "datetime-local") -}
{-                   , ("step", "1") -- 1 second step -}
{-                   ] -}
{-               pure $ (\x -> "(time \"" <> x <> "Z\")") <$> value i -}
{-             TyPrim TyBool -> do -}
{-               d <- dropdown def (pure False) $ TaggedStatic $ Map.fromList -}
{-                 [(True, text "true"), (False, text "false")] -}
{-               pure $ T.toLower . tshow . runIdentity <$> value d -}
{-             TyPrim TyString -> do -}
{-               ti <- input def $ textInput (def & textInputConfig_placeholder .~ pure (_aName arg)) -}
{-               pure $ tshow <$> value ti -- TODO better escaping -}
{-             TyPrim TyKeySet -> do -}
{-               d <- dropdown (def & dropdownConfig_placeholder .~ "Select a keyset") Nothing $ TaggedDynamic $ ffor (_jsonData_keysets $ _ide_jsonData ideL) $ -}
{-                 Map.mapWithKey (\k _ -> text k) -}
{-               pure $ maybe "" (\x -> "(read-keyset \"" <> x <> "\")") <$> value d -}
{-             _ -> fmap value . input def $ -}
{-               textInput (def & textInputConfig_placeholder .~ pure (_aName arg)) -}
{-         let buttonConfig = def -}
{-               & buttonConfig_type .~ SubmitButton -}
{-               & buttonConfig_emphasis .~ Static (Just Primary) -}
{-         submit <- button buttonConfig $ text "Call function" -}
{-         let args = tag (current $ sequence inputs) submit -}
{-             callFun = ffor args $ \as -> mconcat ["(", moduleName, ".", name, " ", T.unwords as, ")"] -}
{-         -- for debugging: widgetHold blank $ ffor callFun $ label def . text -}
{-         let ed = ideL ^. ide_jsonData . jsonData_data -}
{-         deployedResult <- backendRequest (ideL ^. ide_wallet) $ -}
{-           ffor (attach (current ed) callFun) $ \(cEd, c) -> -}
{-             BackendRequest -}
{-               { _backendRequest_code = c -}
{-               , _backendRequest_data = either mempty id cEd -}
{-               , _backendRequest_backend = backendUri -}
{-               , _backendRequest_signing = Set.empty -}
{-               } -}
{-               -- FIXME Probably bad...need to pop up the deploy confirmation dialog -}
{-         widgetHold_ blank $ ffor deployedResult $ \x -> case x of -}
{-           Left err -> message (def & messageConfig_type .~ Static (Just (MessageType Negative))) $ do -}
{-             text $ prettyPrintBackendError err -}
{-           Right v -> message def $ text $ tshow v -}

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
        onLoadClicked <- loadReplBtn

        onDeployClick <- deployBtn

        loadCfg <- loadCodeIntoRepl m onLoadClicked
        let
          reqConfirmation :: Event t (Maybe (ModalImpl m t))
          reqConfirmation = Just (uiDeployConfirmation m) <$ onDeployClick

          deployCfg = mempty & modalCfg_setModal .~ reqConfirmation
        pure $ deployCfg <> loadCfg
  where
    deployBtn =
      uiButton (btnCfgPrimary & uiButtonCfg_class .~ "main-header__button" <> "button_type_primary")
      $ text $ "Deploy"

    loadReplBtn =
      uiButton
        ( btnCfgPrimary
            & uiButtonCfg_title .~ Just "Editor Shortcut: Ctrl+Enter"
            & uiButtonCfg_class .~ "main-header__button" <> "button_type_primary"
        )
         $ text "Load into REPL"

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
