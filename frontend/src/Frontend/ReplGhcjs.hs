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
import           Control.Monad.State.Strict
import           Data.Text                   (Text)
import           Reflex
import           Reflex.Dom.Core
import           Reflex.Dom.ACE.Extended hiding (Annotation(..))
import qualified Data.Text as T
import           Reflex.Dom.Builder.Class.Events (EventName (Keypress))
------------------------------------------------------------------------------
import           Pact.Repl
import           Pact.Repl.Types
import           Pact.Types.Lang
import           Obelisk.Generated.Static
------------------------------------------------------------------------------
import           Frontend.Foundation
import           Frontend.Ide
import           Frontend.Editor
import           Frontend.Repl
import           Frontend.UI.RightPanel
import           Frontend.UI.Button
import           Frontend.UI.Modal
import           Frontend.UI.Widgets
------------------------------------------------------------------------------

data ClickState = DownAt (Int, Int) | Clicked | Selected
  deriving (Eq,Ord,Show,Read)

app :: MonadWidget t m => m ()
app = void . mfix $ \ cfg -> do
  ideL <- makeIde cfg

  controlCfg <- controlBar ideL
  mainCfg <- elAttr "main" ("id" =: "main" <> "class" =: "flexbox even") $ do
    uiEditorCfg <- codePanel ideL
    envCfg <- elAttr "div" ("class" =: "flex" <> "id" =: "control-ui") $ do
      rightTabBar ideL
    pure $ uiEditorCfg <> envCfg

  modalCfg <- showModal ideL

  pure $ mconcat
    [ controlCfg
    , mainCfg
    , modalCfg
    ]

-- | Code editing (left hand side currently)
codePanel :: forall t m. MonadWidget t m => Ide t -> m (IdeCfg t)
codePanel m = do
  elAttr "div" ("class" =: "flex" <> "id" =: "main-wysiwyg") $ do
    (e, eCfg) <- elClass' "div" "wysiwyg" $ do
      onNewCode <- tagOnPostBuild $ m ^. editor_code
      let annotations = map toAceAnnotation <$> m ^. editor_annotations
      onUserCode <- codeWidget annotations "" onNewCode
      pure $ mempty & editorCfg_setCode .~ onUserCode

    let onCtrlEnter = fmap (const ()) . ffilter (== 10) $ domEvent Keypress e -- 10 is somehow Ctrl+Enter
    loadCfg <- loadCodeIntoRepl m onCtrlEnter
    pure $ mconcat [ eCfg , loadCfg ]

-- | Reset REPL and load current editor text into it.
--
--   Reset REPL on load for now until we get an ok to drop this.
loadCodeIntoRepl :: forall t m model. (MonadWidget t m, HasEditor model t) => model -> Event t () -> m (IdeCfg t)
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
  :: forall t m model. (MonadWidget t m, HasEditor model t)
  => model
  ->  m (IdeCfg t)
controlBar m = do
    elAttr "header" ("id" =: "header") $ do
      divClass "flexbox even" $ do
        ideCfgL <- controlBarLeft m
        controlBarRight
        return ideCfgL

controlBarLeft :: (MonadWidget t m, HasEditor model t) => model -> m (IdeCfg t)
controlBarLeft m = do
    divClass "flex left-nav" $ do
      el "h1" $ do
        imgWithAlt (static @"img/pact-logo.svg") "Kadena Pact Logo" blank
        ver <- getPactVersion
        elClass "span" "version" $ text $ "v" <> ver
      elAttr "div" ("id" =: "header-project-loader") $ do
        onLoadClicked <- uiButtonSimple "Load into REPL"

        onDeployClick <- uiButtonSimple "Deploy"

        loadCfg <- loadCodeIntoRepl m onLoadClicked
        let
          reqConfirmation = Modal_DeployConfirmation <$ onDeployClick
          deployCfg = mempty & ideCfg_reqModal .~ reqConfirmation
        pure $ deployCfg <> loadCfg

getPactVersion :: MonadWidget t m => m Text
getPactVersion = do
    is <- liftIO $ initReplState StringEval Nothing
    Right (TLiteral (LString ver) _) <- liftIO $ evalStateT (evalRepl' "(pact-version)") is
    return ver

controlBarRight :: MonadWidget t m => m ()
controlBarRight = do
    elAttr "div" ("class" =: "flex right" <> "id" =: "header-links") $ do
      elAttr "a" ( "href" =: "http://pact-language.readthedocs.io"
                <> "class" =: "documents" <> "target" =: "_blank"
                 ) $ do
        imgWithAlt (static @"img/document.svg") "Documentation" blank
        text "Docs"
      elAttr "a" ( "href" =: "http://kadena.io"
                <> "class" =: "documents" <> "target" =: "_blank") $
        imgWithAlt (static @"img/gray-kadena-logo.svg") "Kadena Logo" blank
