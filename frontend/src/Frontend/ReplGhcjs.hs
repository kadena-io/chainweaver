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
import           Data.Traversable            (for)
import           Generics.Deriving.Monoid    (mappenddefault, memptydefault)
import           GHC.Generics                (Generic)
import           Language.Javascript.JSaddle hiding (Object)
import           Reflex
import           Reflex.Dom.ACE.Extended
import           Reflex.Dom.Core             (keypress)
import qualified Reflex.Dom.Core             as Core
import           Reflex.Dom.SemanticUI       hiding (mainWidget)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as Events
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
import           Frontend.JsonData
import           Frontend.UI.JsonData
import           Frontend.UI.Wallet
import           Frontend.Wallet
import           Frontend.Widgets
import           Frontend.Ide
import           Frontend.UI.Dialogs.DeployConfirmation

-- | Retrieve the currently selected signing keys.
ide_getSigningKeyPairs :: Reflex t => Ide t -> Dynamic t [KeyPair]
ide_getSigningKeyPairs ideL = do
  let
    signingKeys = ideL ^. ide_wallet . wallet_signingKeys
    keys = Map.assocs <$> ideL ^. ide_wallet . wallet_keys
  cKeys <- keys
  sKeys <- signingKeys
  let isSigning (n,_) = Set.member n sKeys
  pure $ map snd $ filter isSigning cKeys


data ClickState = DownAt (Int, Int) | Clicked | Selected
  deriving (Eq,Ord,Show,Read)

app :: MonadWidget t m => m ()
app = void . mfix $ \ cfg -> do
  ideL <- makeIde cfg

  elClass "div" "app" $ do
    controlCfg <- controlBar ideL
    elClass "div" "ui two column padded grid main" $ mdo
      editorCfg <- elClass "div" "column" $ do
        {- elClass "div" "ui secondary menu pointing" $ do -}
        {-   elClass "a" "active item" $ text "Contract" -}
        elClass "div" "ui light segment editor-pane" $ codePanel ideL

      envCfg <- elClass "div" "column repl-column" $
        elClass "div" "ui env-pane" $ envPanel ideL

      modalCfg <- showModal ideL

      pure $ mconcat
        [ controlCfg
        , editorCfg
        , envCfg
        , modalCfg
        ]

showModal :: forall t m. MonadWidget t m => Ide t -> m (IdeCfg t)
showModal ideL = do
    document <- DOM.currentDocumentUnchecked

    onEsc <- wrapDomEventMaybe document (`EventM.on` Events.keyDown) $ do
      key <- getKeyEvent
      pure $ if keyCodeLookup (fromIntegral key) == Escape then Just () else Nothing

    (backdropEl, _) <- elDynAttr' "div"
      (ffor isVisible $ \isVis ->
        ("style" =: (isVisibleStyle isVis <> ";" <> existingBackdropStyle))
      )
      blank

    ev <- networkView $ showModal <$> _ide_modal ideL
    onFinish <- switchHold never $ snd <$> ev
    mCfg <- flatten $ fst <$> ev

    let
      onClose = leftmost [ onFinish
                         , onEsc
                         , domEvent Click backdropEl
                         ]
      lConf = mempty & ideCfg_reqModal .~ (Modal_NoModal <$ onClose)
    pure $ lConf <> mCfg
  where
    isVisible = getIsVisible <$> _ide_modal ideL
    getIsVisible = \case
      Modal_NoModal -> False
      _             -> True

    showModal = \case
      Modal_NoModal -> pure (mempty, never)
      Modal_DeployConfirmation -> uiDeployConfirmation ideL

    existingBackdropStyle = "position: fixed; top:0;bottom:0;left:0;right:0;z-index:100; background-color: rgba(0,0,0,0.5);"
    isVisibleStyle isVis = "display:" <> (if isVis then "block" else "none")

-- | Code editing (left hand side currently)
codePanel :: forall t m. MonadWidget t m => Ide t -> m (IdeCfg t)
codePanel ideL = mdo
  {- menu (def & menuConfig_secondary .~ pure True) $ do -}
  {-   menuItem def $ text "Code"  -}
    onNewCode <- tagOnPostBuild $ _ide_code ideL
    onUserCode <- codeWidget "" onNewCode

    pure $ mempty & ideCfg_setCode .~ onUserCode

-- | Tabbed panel to the right
--
--   Offering access to:
--
--   - The REPL
--   - Compiler error messages
--   - Key & Data Editor
--   - Module explorer
envPanel :: forall t m. MonadWidget t m => Ide t -> m (IdeCfg t)
envPanel ideL = mdo
  let
    curSelection = _ide_envSelection ideL

  onSelect <- menu
    ( def & menuConfig_pointing .~ pure True
        & menuConfig_secondary .~ pure True
        & classes .~ pure "dark"
    )
    $ tabs curSelection

  explorerCfg <- tabPane
      ("style" =: "overflow-y: auto; overflow-x: hidden; flex-grow: 1")
      curSelection EnvSelection_ModuleExplorer
      $ moduleExplorer ideL

  replCfg <- tabPane
      ("class" =: "ui flex-content light segment")
      curSelection EnvSelection_Repl
      $ replWidget ideL

  envCfg <- tabPane
      ("class" =: "ui fluid accordion env-accordion")
      curSelection EnvSelection_Env $ mdo

    jsonCfg <- accordionItem True "ui json-data-accordion-item" "Data" $ do
      elClass "div" "json-data full-size" $ do
        conf <- uiJsonData (ideL ^. ide_wallet) (ideL ^. ide_jsonData)
        pure $ mempty &  ideCfg_jsonData .~ conf

    elClass "div" "ui hidden divider" blank

    keysCfg <- accordionItem True "keys ui" "Keys" $ do
      conf <- elClass "div" "ui segment" $ uiWallet $ _ide_wallet ideL
      pure $ mempty & ideCfg_wallet .~ conf

    elClass "div" "ui hidden divider" blank

    pure $ mconcat [ jsonCfg
                   , keysCfg
                   , replCfg
                   , explorerCfg
                   , mempty & ideCfg_selEnv .~ onSelect
                   ]

  errorsCfg <- tabPane
      ("class" =: "ui code-font full-size")
      curSelection EnvSelection_Msgs $ do
    void . dyn $ traverse_ (snippetWidget . OutputSnippet) <$> _ide_msgs ideL
    pure mempty

  _functionsCfg <- tabPane ("style" =: "overflow: auto") curSelection EnvSelection_Functions $ do
    header def $ text "Public functions"
    dyn_ $ ffor (_ide_deployed ideL) $ \case
      Nothing -> paragraph $ text "Load a deployed contract with the module explorer to see the list of available functions."
      Just (backendUri, functions) -> functionsList ideL backendUri functions
    divider $ def & dividerConfig_hidden .~ Static True

  pure $ mconcat [ envCfg, errorsCfg ]

  where
    tabs :: Dynamic t EnvSelection -> m (Event t EnvSelection)
    tabs curSelection = do
      let
        selections = [ EnvSelection_Env, EnvSelection_Repl, EnvSelection_Msgs, EnvSelection_ModuleExplorer ]
      leftmost <$> traverse (tab curSelection) selections

    tab :: Dynamic t EnvSelection -> EnvSelection -> m (Event t EnvSelection)
    tab curSelection self = do
      let
        itemClasses = [boolClass "active" . Dyn $ fmap (== self) curSelection ]
        itemCfg = def & classes .~ dynClasses itemClasses
      onClick <- makeClickable $ menuItem' itemCfg $
        text $ selectionToText self
      pure $ self <$ onClick

functionsList :: MonadWidget t m => Ide t -> BackendUri -> [PactFunction] -> m ()
functionsList ideL backendUri functions = divClass "ui very relaxed list" $ do
  for_ functions $ \(PactFunction (ModuleName moduleName) name _ mdocs funType) -> divClass "item" $ do
    (e, _) <- elClass' "a" "header" $ do
      text name
      text ":"
      text $ tshow $ _ftReturn funType
      text " "
      elAttr "span" ("class" =: "description" <> "style" =: "display: inline") $ do
        text "("
        text $ T.unwords $ tshow <$> _ftArgs funType
        text ")"
    for_ mdocs $ divClass "description" . text
    open <- toggle False $ domEvent Click e
    dyn_ $ ffor open $ \case
      False -> pure ()
      True -> segment def $ form def $ do
        inputs <- for (_ftArgs funType) $ \arg -> field def $ do
          el "label" $ text $ "Argument: " <> tshow arg
          case _aType arg of
            TyPrim TyInteger -> fmap value . input def $ inputElement $ def
              & inputElementConfig_elementConfig . initialAttributes .~ Map.fromList
                [ ("type", "number")
                , ("step", "1")
                , ("placeholder", _aName arg)
                ]
            TyPrim TyDecimal -> do
              ti <- input def $ inputElement $ def
                & inputElementConfig_elementConfig . initialAttributes .~ Map.fromList
                  [ ("type", "number")
                  , ("step", "0.0000000001") -- totally arbitrary
                  , ("placeholder", _aName arg)
                  ]
              pure $ (\x -> if T.isInfixOf "." x then x else x <> ".0") <$> value ti
            TyPrim TyTime -> do
              i <- input def $ inputElement $ def
                & inputElementConfig_elementConfig . initialAttributes .~ Map.fromList
                  [ ("type", "datetime-local")
                  , ("step", "1") -- 1 second step
                  ]
              pure $ (\x -> "(time \"" <> x <> "Z\")") <$> value i
            TyPrim TyBool -> do
              d <- dropdown def (pure False) $ TaggedStatic $ Map.fromList
                [(True, text "true"), (False, text "false")]
              pure $ T.toLower . tshow . runIdentity <$> value d
            TyPrim TyString -> do
              ti <- input def $ textInput (def & textInputConfig_placeholder .~ pure (_aName arg))
              pure $ tshow <$> value ti -- TODO better escaping
            TyPrim TyKeySet -> do
              d <- dropdown (def & dropdownConfig_placeholder .~ "Select a keyset") Nothing $ TaggedDynamic $ ffor (_jsonData_keysets $ _ide_jsonData ideL) $
                Map.mapWithKey (\k _ -> text k)
              pure $ maybe "" (\x -> "(read-keyset \"" <> x <> "\")") <$> value d
            _ -> fmap value . input def $
              textInput (def & textInputConfig_placeholder .~ pure (_aName arg))
        let buttonConfig = def
              & buttonConfig_type .~ SubmitButton
              & buttonConfig_emphasis .~ Static (Just Primary)
        submit <- button buttonConfig $ text "Call function"
        let args = tag (current $ sequence inputs) submit
            callFun = ffor args $ \as -> mconcat ["(", moduleName, ".", name, " ", T.unwords as, ")"]
        -- for debugging: widgetHold blank $ ffor callFun $ label def . text
        let ed = ideL ^. ide_jsonData . jsonData_data
        deployedResult <- backendRequest (ideL ^. ide_wallet) $
          ffor (attach (current ed) callFun) $ \(cEd, c) -> BackendRequest
            { _backendRequest_code = c
            , _backendRequest_data = either mempty id cEd
            , _backendRequest_backend = backendUri
            }
        widgetHold_ blank $ ffor deployedResult $ \(_uri, x) -> case x of
          Left err -> message (def & messageConfig_type .~ Static (Just (MessageType Negative))) $ do
            text $ prettyPrintBackendError err
          Right v -> message def $ text $ tshow v

selectionToText :: EnvSelection -> Text
selectionToText = \case
  EnvSelection_Repl -> "REPL"
  EnvSelection_Env -> "Env"
  EnvSelection_Msgs -> "Messages"
  EnvSelection_Functions -> "Functions"
  EnvSelection_ModuleExplorer -> "Module Explorer"

setDown :: (Int, Int) -> t -> Maybe ClickState
setDown clickLoc _ = Just $ DownAt clickLoc

clickClassifier :: (Int, Int) -> Maybe ClickState -> Maybe ClickState
clickClassifier clickLoc (Just (DownAt loc1)) =
  if clickLoc == loc1 then Just Clicked else Just Selected
clickClassifier _ _ = Nothing

scrollToBottom :: (PToJSVal t, MonadIO m, MonadJSM m) => t -> m ()
scrollToBottom e = liftJSM $ do
    let pElem = pToJSVal e
    (pElem <# ("scrollTop" :: String)) (pElem ^. js ("scrollHeight" :: String))

codeWidget
  :: MonadWidget t m
  => Text -> Event t Text
  -> m (Event t Text)
codeWidget iv sv = do
    let ac = def { _aceConfigMode = Just "ace/mode/pact"
                 , _aceConfigElemAttrs = "class" =: "ace-code ace-widget"
                 }
    ace <- resizableAceWidget mempty ac (AceDynConfig $ Just AceTheme_SolarizedDark) never iv sv
    return $ _extendedACE_onUserChange ace


data DisplayedSnippet
  = InputSnippet Text
  | OutputSnippet Text
  deriving (Eq,Ord,Show,Read)

staticReplHeader :: Seq DisplayedSnippet
staticReplHeader = S.fromList
      [ OutputSnippet ";; Welcome to the Pact interactive repl"
      , OutputSnippet ";; Use LOAD button to execute editor text"
      , OutputSnippet ";; then just type at the \"pact>\" prompt to interact!"
      ]

snippetWidget :: MonadWidget t m => DisplayedSnippet -> m ()
snippetWidget (InputSnippet t)  = elAttr "pre" ("class" =: "replOut code-font") $ text t
snippetWidget (OutputSnippet t) = elAttr "pre" ("class" =: "replOut code-font") $ text t

------------------------------------------------------------------------------
moduleExplorer
  :: forall t m. MonadWidget t m
  => Ide t
  -> m (IdeCfg t)
moduleExplorer ideL = mdo
    demuxSel <- fmap demux $ holdDyn (Left "") $ leftmost [searchSelected, exampleSelected]

    header def $ text "Example Contracts"
    exampleClick <- divClass "ui inverted selection list" $ for demos $ \c -> do
      let isSel = demuxed demuxSel $ Left $ _exampleContract_name c
      selectableItem (_exampleContract_name c) isSel $ do
        text $ _exampleContract_name c
        (c <$) <$> loadButton isSel
    let exampleSelected = fmap Left . leftmost . fmap fst $ Map.elems exampleClick
        exampleLoaded = fmap Left . leftmost . fmap snd $ Map.elems exampleClick

    header def $ text "Deployed Contracts"

    (search, backend) <- divClass "ui form" $ divClass "ui two fields" $ do
      searchI <- field def $ input (def & inputConfig_icon .~ Static (Just RightIcon)) $ do
        ie <- inputElement $ def & initialAttributes .~ ("type" =: "text" <> "placeholder" =: "Search modules")
        icon "black search" def
        pure ie

      let mkMap = Map.fromList . map (\k@(BackendName n, _) -> (Just k, text n)) . Map.toList
          dropdownConf = def
            & dropdownConfig_placeholder .~ "Backend"
            & dropdownConfig_fluid .~ pure True
      d <- field def $ input def $ dropdown dropdownConf (Identity Nothing) $ TaggedDynamic $
        Map.insert Nothing (text "All backends") . maybe mempty mkMap <$> ideL ^. ide_backend . backend_backends
      pure (value searchI, value d)

    let
      deployedContracts = Map.mergeWithKey (\_ a b -> Just (a, b)) mempty mempty
          <$> ideL ^. ide_backend . backend_modules
          <*> (fromMaybe mempty <$> ideL ^. ide_backend . backend_backends)
      searchFn needle (Identity mModule)
        = concat . fmapMaybe (filtering needle) . Map.toList
        . maybe id (\(k', _) -> Map.filterWithKey $ \k _ -> k == k') mModule
      filtering needle (backendName, (m, backendUri)) =
        let f contractName =
              if T.isInfixOf (T.toCaseFold needle) (T.toCaseFold contractName)
              then Just (DeployedContract contractName backendName backendUri, ())
              else Nothing
        in case fmapMaybe f $ fromMaybe [] m of
          [] -> Nothing
          xs -> Just xs
      filteredCsRaw = searchFn <$> search <*> backend <*> deployedContracts
      paginate p =
        Map.fromList . take itemsPerPage . drop (itemsPerPage * pred p) . L.sort
    filteredCs <- holdUniqDyn filteredCsRaw
    let
      paginated = paginate <$> currentPage <*> filteredCs

    (searchSelected, searchLoaded) <- divClass "ui inverted selection list" $ do
      searchClick <- listWithKey paginated $ \c _ -> do
        let isSel = demuxed demuxSel $ Right c
        selectableItem c isSel $ do
          label (def & labelConfig_horizontal .~ Static True) $ do
            text $ unBackendName $ _deployedContract_backendName c
          text $ _deployedContract_name c
          (c <$) <$> loadButton isSel
      let searchSelected1 = switch . current $ fmap Right . leftmost . fmap fst . Map.elems <$> searchClick
          searchLoaded1 = switch . current $ fmap Right . leftmost . fmap snd . Map.elems <$> searchClick
      pure (searchSelected1, searchLoaded1)

    let itemsPerPage = 5 :: Int
        numberOfItems = length <$> filteredCs
        calcTotal a = ceiling $ (fromIntegral a :: Double)  / fromIntegral itemsPerPage
        totalPages = calcTotal <$> numberOfItems
    rec
      currentPage <- holdDyn 1 $ leftmost
        [ updatePage
        , 1 <$ updated numberOfItems
        ]
      updatePage <- paginationWidget currentPage totalPages

    pure $ mempty
      { _ideCfg_selContract = leftmost [searchLoaded, exampleLoaded]
      }
  where
    selectableItem :: k -> Dynamic t Bool -> m a -> m (Event t k, a)
    selectableItem k s m = do
      let mkAttrs a = Map.fromList
            [ ("style", "position:relative")
            , ("class", "item" <> (if a then " active" else ""))
            ]
      (e, a) <- elDynAttr' "a" (mkAttrs <$> s) m
      pure (k <$ domEvent Click e, a)
    loadButton s = switchHold never <=< dyn $ ffor s $ \case
      False -> pure never
      True -> let buttonStyle = "position: absolute; right: 0; top: 0; height: 100%; margin: 0"
                in button (def & classes .~ "primary" & style .~ buttonStyle) $ text "Load"

replWidget
    :: MonadWidget t m
    => Ide t
    -> m (IdeCfg t)
replWidget ideL = mdo
  (e, r) <- elClass' "div" "repl-pane code-font" $ mdo
    mapM_ snippetWidget staticReplHeader
    clickType <- foldDyn ($) Nothing $ leftmost
      [ setDown <$> domEvent Mousedown e
      , clickClassifier <$> domEvent Mouseup e
      ]
    let
      replClick = () <$
        ffilter (== Just Clicked) (updated clickType)

      codeData = do
        code <- ideL ^. ide_code
        eJson <- ideL ^. ide_jsonData . jsonData_data
        pure $ either (const Nothing) (Just . (code,)) eJson

      keysContract =
        fmap sequence $ zipDyn (ide_getSigningKeyPairs ideL) codeData

      onKeysContractLoad =
        fmapMaybe id . tag (current keysContract) $ _ide_load ideL

      onNewReplContent = leftmost
        [ onKeysContractLoad
        , ([], ("", H.empty)) <$ _ide_clearRepl ideL
        ]

    widgetHold
      (replInner replClick ([], ("", H.empty)))
      (replInner replClick <$> onNewReplContent
      )
  let
    err = snd <$> r
    onErrs = fmap maybeToList . updated $ err
    newExpr = fst <$> r

  timeToScroll <- delay 0.1 $ switch $ current newExpr
  void $ performEvent (scrollToBottom (_element_raw e) <$ timeToScroll)
  pure $ mempty & ideCfg_setMsgs .~ onErrs

replInner
    :: MonadWidget t m
    => Event t ()
    -> ([KeyPair], (Text, Object))
    -> m (Event t Text, Maybe LogMsg)
replInner replClick (signingKeys, (code, json)) = mdo
    let pactKeys =
          T.unwords
          . map (keyToText . _keyPair_publicKey)
          $ signingKeys
        codeP = mconcat
          [ "(env-data "
          , toJsonString . T.decodeUtf8 . BSL.toStrict $ encode json
          , ")"
          , "(env-keys ["
          , pactKeys
          , "])"
          , code
          ]
    initState <- liftIO $ initReplState StringEval
    stateOutErr0 <- runReplStep0 (initState, mempty) codeP
    let stateAndOut0 = (\(a,b,_) -> (a, b)) stateOutErr0
    stateAndOut <- holdDyn stateAndOut0 evalResult

    _ <- dyn (mapM_ snippetWidget . snd <$> stateAndOut)
    newInput <- replInput replClick
    evalResult <- performEvent $
      attachWith runReplStep (current stateAndOut) newInput
    return (newInput, stateOutErr0 ^. _3)
  where
      surroundWith :: Semigroup s => s -> s -> s
      surroundWith o i = o <> i <> o

      escapeJSON = T.replace "\"" "\\\""

      toJsonString = surroundWith "\"" . escapeJSON

      keyToText = T.decodeUtf8 . BSL.toStrict . encode


replInput :: MonadWidget t m => Event t () -> m (Event t Text)
replInput setFocus = do
    divClass "repl-input-controls code-font" $ mdo
      elClass "div" "prompt" $ text "pact>"
      let sv = leftmost
            [ mempty <$ enterPressed
            , fromMaybe "" . Z.safeCursor <$> tagPromptlyDyn commandHistory key
            ]
      ti <- Core.textInput (def & Core.textInputConfig_setValue .~ sv
                                & Core.textInputConfig_attributes .~ pure ("class" =: "code-font")
                           )
      let key = ffilter isMovement $ domEvent Keydown ti
      let enterPressed = keypress Enter ti
      _ <- performEvent (liftJSM (pToJSVal (Core._textInput_element ti) ^. js0 ("focus" :: String)) <$ setFocus)
      let newCommand = tag (current $ value ti) enterPressed
      commandHistory <- foldDyn ($) Z.empty $ leftmost
        [ addToHistory <$> newCommand
        , moveHistory <$> key
        ]
      return newCommand

addToHistory :: Eq a => a -> Z.Zipper a -> Z.Zipper a
addToHistory a z =
    if Just a == Z.safeCursor (Z.left zEnd) then zEnd else Z.push a zEnd
  where
    zEnd = Z.end z

isMovement :: (Num a, Eq a) => a -> Bool
isMovement 38 = True
isMovement 40 = True
isMovement _  = False

moveHistory :: (Num a1, Eq a1) => a1 -> Z.Zipper a -> Z.Zipper a
moveHistory 38 = Z.left
moveHistory 40 = Z.right
moveHistory _  = id

runReplStep0
    :: MonadIO m
    => (ReplState, Seq DisplayedSnippet)
    -> Text
    -> m (ReplState, Seq DisplayedSnippet, Maybe LogMsg)
runReplStep0 (s1,snippets1) e = do
    (r,s2) <- liftIO $ runStateT (evalRepl' $ T.unpack e) s1
    let snippet = case r of
                    Left _ -> mempty
                    Right _ ->  S.singleton . OutputSnippet . T.pack $ _rOut s2
        err = either (Just . T.pack) (const Nothing) r
    return (s2, snippets1 <> snippet, err)

runReplStep
    :: MonadIO m
    => (ReplState, Seq DisplayedSnippet)
    -> Text
    -> m (ReplState, Seq DisplayedSnippet)
runReplStep (s1,snippets1) e = do
    (eterm,s2) <- liftIO $ runStateT (evalRepl' $ T.unpack e) s1
    return (s2, snippets1 <> S.fromList [InputSnippet ("pact> " <> e), OutputSnippet $ showResult eterm])

showResult :: Show a => Either String a -> Text
showResult (Right v) = T.pack $ show v
showResult (Left e)  = "Error: " <> T.pack e

controlBar :: forall t m. MonadWidget t m => Ide t -> m (IdeCfg t)
controlBar ideL = do
    elClass "div" "ui borderless menu" $ do
      elClass "div" "item" showPactVersion

      onLoad <- elClass "div" "item" $
        button (def & buttonConfig_emphasis .~ Static (Just Primary)) $ text "Load into REPL"

      onDeployClick <- elClass "div" "item" $ do
        let buttonConfig = def
              & buttonConfig_emphasis .~ Static (Just Primary)
        button buttonConfig $ text "Deploy"

      elClass "div" "right menu" rightMenu
      let
        reqConfirmation = Modal_DeployConfirmation <$ onDeployClick
        lcfg = mempty
          & ideCfg_load .~ onLoad
          & ideCfg_reqModal .~ reqConfirmation
      pure $ lcfg
  where
    showPactVersion = do
      elAttr "a" ( "target" =: "_blank" <> "href" =: "https://github.com/kadena-io/pact") $ do
        is <- liftIO $ initReplState StringEval
        Right (TLiteral (LString ver) _) <- liftIO $ evalStateT (evalRepl' "(pact-version)") is
        elAttr "img" ("src" =: static @"img/PactLogo.svg" <> "class" =: "logo-image" <> "width" =: "80" <> "hegiht" =: "20") blank
        text $ "v" <> ver

    rightMenu = do
      elClass "div" "ui item" $
        el "label" $
          elAttr "a" ("target" =: "_blank" <>
                      "style" =: "color:white;text-decoration:none;" <>
                      "href" =: "http://pact-language.readthedocs.io"
                      ) $ do
            elAttr "i" ("class" =: "fa fa-book" <> "aria-hidden" =: "true") blank
            elAttr "span" ("id" =: "hideIfTiny" <> "class" =: "menu-link") $ text "Docs"
      elClass "div" "ui item" $
        elAttr "a" ("target" =: "_blank" <> "href" =: "http://kadena.io") $
          elAttr "img" ("src" =: static @"img/KadenaWhiteLogo.svg" <> "class" =: "logo-image" <> "width" =: "150" <> "hegiht" =: "20") blank

