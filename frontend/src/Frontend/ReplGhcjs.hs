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
import           Data.Aeson                  (Object, encode)
import qualified Data.ByteString.Lazy        as BSL
import           Data.Foldable
import qualified Data.HashMap.Strict         as H
import qualified Data.List.Zipper            as Z
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.Semigroup
import           Data.Sequence               (Seq)
import qualified Data.Sequence               as S
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Traversable            (for)
import           Generics.Deriving.Monoid    (mappenddefault, memptydefault)
import           GHC.Generics                (Generic)
import qualified GHCJS.DOM.EventM            as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.HTMLElement       as DOM hiding (click)
import qualified GHCJS.DOM.Types             as DOM hiding (click)
import           Language.Javascript.JSaddle hiding (Object)
import           Reflex
import           Reflex.Dom.ACE.Extended
import           Reflex.Dom.Core             (keypress, mainWidget)
import qualified Reflex.Dom.Core             as Core
import           Reflex.Dom.SemanticUI       hiding (mainWidget)
------------------------------------------------------------------------------
import           Pact.Repl
import           Pact.Repl.Types
import           Pact.Types.Lang
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Foundation
import           Frontend.JsonData
import           Frontend.UI.JsonData
import           Frontend.UI.Wallet
import           Frontend.Wallet
import           Frontend.Widgets
import           Static

type LogMsg = Text

-- | Configuration for sub-modules.
--
--   State is controlled via this configuration.
data IdeCfg t = IdeCfg
  { _ideCfg_wallet      :: WalletCfg t
  , _ideCfg_jsonData    :: JsonDataCfg t
  , _ideCfg_selContract :: Event t Text
    -- ^ Select a contract to load into the editor.
    -- Note: Currently this event should only be triggered from the dropdown in
    -- the controlbar, otherwise that dropdown will be out of sync. This is due
    -- to the limitation of semantic-reflex dropdown to not being updateable.
  , _ideCfg_load        :: Event t ()
    -- ^ Load code into the repl.
  , _ideCfg_setMsgs   :: Event t [LogMsg]
    -- ^ Set errors that should be shown to the user.
  , _ideCfg_setCode     :: Event t Text
    -- ^ Update the current contract/PACT code.
  }
  deriving Generic

makePactLenses ''IdeCfg

-- | Current IDE state.
data Ide t = Ide
  { _ide_code             :: Dynamic t Text
  -- ^ Currently loaded/edited PACT code.
  , _ide_selectedContract :: Dynamic t Text
  -- ^ The currently selected contract name.
  , _ide_wallet           :: Wallet t
  , _ide_jsonData         :: JsonData t
  , _ide_msgs           :: Dynamic t [LogMsg]
  , _ide_backend          :: Backend t
  }
  deriving Generic

makePactLenses ''Ide

-- | Retrieve the currently selected signing keys.
ide_getSigningKeyPairs :: Reflex t => Ide t -> Dynamic t [DynKeyPair t]
ide_getSigningKeyPairs ideL = do
  let
    keys = Map.elems <$> ideL ^. ide_wallet . wallet_keys
  cKeys <- keys
  let isSigning k = k ^. keyPair_forSigning
  filterM isSigning cKeys


codeExtension :: Text
codeExtension = ".repl"

dataExtension :: Text
dataExtension = ".data.json"

toCodeFile :: Text -> Text
toCodeFile = (<> codeExtension)

toDataFile :: Text -> Text
toDataFile = (<> dataExtension)

codeFromResponse :: XhrResponse -> Text
codeFromResponse =
    fromMaybe "error: could not connect to server" . _xhrResponse_responseText

data ClickState = DownAt (Int, Int) | Clicked | Selected
  deriving (Eq,Ord,Show,Read)

main :: JSM ()
main = mainWidget app

app :: MonadWidget t m => m ()
app = void . mfix $ \ ~(cfg, ideL) -> elClass "div" "app" $ do
    walletL <- makeWallet $ _ideCfg_wallet cfg
    json <- makeJsonData walletL $ _ideCfg_jsonData cfg
    backendL <- makeBackend $ BackendCfg never
    let
      jsonErrorString =
        either (Just . showJsonError) (const Nothing) <$> _jsonData_data json
      jsonErrorsOnLoad =
        fmap maybeToList . tag (current jsonErrorString) $ cfg ^. ideCfg_load

    controlCfg <- controlBar ideL
    contractReceivedCfg <- loadContract $ _ide_selectedContract ideL
    elClass "div" "ui two column padded grid main" $ mdo
      editorCfg <- elClass "div" "column" $ do
        {- elClass "div" "ui secondary menu pointing" $ do -}
        {-   elClass "a" "active item" $ text "Contract" -}
        elClass "div" "ui light segment editor-pane" $ codePanel ideL

      envCfg <- elClass "div" "column repl-column" $
        elClass "div" "ui env-pane" $ envPanel ideL (cfg ^. ideCfg_load)

      code <- holdDyn "" $ cfg ^. ideCfg_setCode
      selContract <- holdDyn initialDemoFile $ cfg ^. ideCfg_selContract
      errors <- holdDyn [] $ cfg ^. ideCfg_setMsgs

      pure
        ( mconcat
          [ controlCfg
          , editorCfg
          , mempty & ideCfg_setMsgs .~ jsonErrorsOnLoad
          , envCfg
          , contractReceivedCfg
          ]
        , Ide { _ide_code = code
              , _ide_selectedContract = selContract
              , _ide_wallet = walletL
              , _ide_jsonData = json
              , _ide_msgs = errors
              , _ide_backend = backendL
              }
        )
  where
    loadContract contractName = do
      onNewContractName <- tagOnPostBuild contractName
      code <- loadContractData toCodeFile onNewContractName
      json <- loadContractData toDataFile onNewContractName
      onCodeJson <- waitForEvents (,) onNewContractName code json
      pure $ mempty
        & ideCfg_setCode .~ fmap fst onCodeJson
        & ideCfg_jsonData . jsonDataCfg_setRawInput .~ fmap snd onCodeJson

    loadContractData getFileName onNewContractName =
      fmap (fmap codeFromResponse)
      . performRequestAsync
      . fmap ((\u -> xhrRequest "GET" u def) . getFileName)
      $ onNewContractName



-- | The available panels in the `envPanel`
data EnvSelection
  = EnvSelection_Repl -- ^ REPL for interacting with loaded contract
  | EnvSelection_Env -- ^ Widgets for editing (meta-)data.
  | EnvSelection_Msgs -- ^ Compiler errors to be shown.
  deriving (Eq, Ord, Show)

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
envPanel :: forall t m. MonadWidget t m => Ide t -> Event t () -> m (IdeCfg t)
envPanel ideL onLoad = mdo
  let onLoaded =
        maybe EnvSelection_Repl (const EnvSelection_Msgs)
          . listToMaybe
          <$> updated (_ide_msgs ideL)

  curSelection <- holdDyn EnvSelection_Env $ leftmost [ onSelect
                                                      , onLoaded
                                                      ]

  onSelect <- menu
    ( def & menuConfig_pointing .~ pure True
        & menuConfig_secondary .~ pure True
        & classes .~ pure "dark"
    )
    $ tabs curSelection

  replCfg <- tabPane
      ("class" =: "ui flex-content light segment")
      curSelection EnvSelection_Repl
      $ replWidget ideL onLoad

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

    pure $ mconcat [ jsonCfg
                   , keysCfg
                   , replCfg
                   ]

  errorsCfg <- tabPane
      ("class" =: "ui code-font full-size")
      curSelection EnvSelection_Msgs $ do
    void . dyn $ traverse_ (snippetWidget . OutputSnippet) <$> _ide_msgs ideL
    pure mempty

  pure $ mconcat [ envCfg, errorsCfg ]

  where
    tabs :: Dynamic t EnvSelection -> m (Event t EnvSelection)
    tabs curSelection = do
      let
        selections = [ EnvSelection_Env, EnvSelection_Repl, EnvSelection_Msgs ]
      leftmost <$> traverse (tab curSelection) selections

    tab :: Dynamic t EnvSelection -> EnvSelection -> m (Event t EnvSelection)
    tab curSelection self = do
      onClick <- makeClickable $ menuItem' (def & classes .~ dynClasses [boolClass "active" . Dyn $ fmap (== self) curSelection ]) $
        text $ selectionToText self
      pure $ self <$ onClick

selectionToText :: EnvSelection -> Text
selectionToText = \case
  EnvSelection_Repl -> "REPL"
  EnvSelection_Env -> "Env"
  EnvSelection_Msgs -> "Messages"

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
replWidget
    :: MonadWidget t m
    => Ide t
    -> Event t ()
    -> m (IdeCfg t)
replWidget ideL onLoad = mdo
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

      onKeysContractLoad = fmapMaybe id . tag (current keysContract) $ onLoad


    widgetHold
      (replInner replClick ([], ("", H.empty)))
      (replInner replClick <$> onKeysContractLoad
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
    -> ([DynKeyPair t], (Text, Object))
    -> m (Event t Text, Maybe LogMsg)
replInner replClick (signingKeys, (code, json)) = mdo
    let pactKeys =
          T.unwords
          . map (keyToText . _keyPair_publicKey)
          $ signingKeys
        codeP = mconcat
          [ "(env-data "
          , toJsonString . T.decodeUtf8 . BSL.toStrict $ encode json
          , ")\n"
          , "(env-keys ["
          , pactKeys
          , "])\n\n"
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

      cfg <- exampleChooser
      onDeploy <- elClass "div" "item" $
        button (def & buttonConfig_emphasis .~ Static (Just Primary)) $ text "Deploy"
      let
        req = do
          c <- ideL ^. ide_code
          ed <- ideL ^. ide_jsonData . jsonData_data
          pure $ either (const Nothing) (Just . BackendRequest c) ed
        onReq = fmapMaybe id $ tag (current req) onDeploy
      onResp <- backendPerformSend (ideL ^. ide_wallet) (ideL ^. ide_backend) onReq

      elClass "div" "right menu" rightMenu
      pure $ cfg &
        ideCfg_setMsgs .~ ((:[]) . prettyPrintBackendErrorResult <$> onResp)
  where
    showPactVersion = do
      elAttr "a" ( "target" =: "_blank" <> "href" =: "https://github.com/kadena-io/pact") $ do
        is <- liftIO $ initReplState StringEval
        Right (TLiteral (LString ver) _) <- liftIO $ evalStateT (evalRepl' "(pact-version)") is
        elAttr "img" ("src" =: static @"img/PactLogo.svg" <> "class" =: "logo-image" <> "width" =: "80" <> "hegiht" =: "20") blank
        text $ "v" <> ver

    exampleChooser :: m (IdeCfg t)
    exampleChooser = do
      d <- listLoadOptions exampleData
      load <- elClass "div" "item" $
        button (def & buttonConfig_emphasis .~ Static (Just Primary)) $ text "Load"
      pure $ mempty
        & ideCfg_selContract .~ updated d
        & ideCfg_load .~ load

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

listLoadOptions
  :: MonadWidget t m
  => [(Text, Text)] -- ^ Example data
  -> m (Dynamic t Text)
listLoadOptions examples = mdo
  (top, selection) <- elAttr' "a" ("class" =: "dropdown item" <> "style" =: "width: 250px") $ mdo
    dynText $ fst <$> selection
    icon "dropdown" $ def & style .~ Static "color: white"
    isOpen <- holdUniqDyn <=< holdDyn False $ leftmost
      [ False <$ updated selection
      , attachWith (\o _ -> not o) (current isOpen) (domEvent Click top)
      ]
    let transition = ffor (updated isOpen) $ \o ->
          Transition SlideDown $ transitionConfig $ if o then In else Out
        transitionConfig dir = TransitionConfig
          { _transitionConfig_duration = 0.2
          , _transitionConfig_cancelling = True
          , _transitionConfig_direction = Just dir
          }
        popupConfig = def
          & classes .~ Static "ui bottom left flowing popup"
          & style .~ Static "top: 100%; width: 200%; right: auto"
          & action ?~ Action
            { _action_event = Just transition
            , _action_initialDirection = Out
            , _action_forceVisible = True
            }
    (popup, (search, update)) <- ui' "div" popupConfig $ divClass "ui two column relaxed divided grid" $ do
      e1 <- divClass "column" $ do
        header def $ text "Examples"
        es <- divClass "ui inverted link list" $ for examples $ \sel@(name, _) -> do
          (e, _) <- elClass' "a" "item" $ text name
          pure $ sel <$ domEvent Click e
        pure $ leftmost es
      (search, e2) <- divClass "column" $ do
        header def $ text "Deployed Contracts"
        search <- input (def & inputConfig_icon .~ Static (Just RightIcon)) $ do
          ie <- inputElement $ def & initialAttributes .~ ("type" =: "text")
          icon "black search" def
          pure ie
        let dexamples = ffor (value search) $ \x -> filter (T.isInfixOf (T.toCaseFold x) . T.toCaseFold . fst) examples
        es <- divClass "ui inverted link list" $ simpleList dexamples $ \d -> do
          (e, _) <- elClass' "a" "item" $ dynText $ fst <$> d
          pure $ attachWith const (current d) (domEvent Click e)
        pure (search, switch . current $ leftmost <$> es)
      pure (search, leftmost [e1, e2])
    -- Prevent clicks on the popup from closing the top menu
    let popupEl = DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw popup
    liftJSM $ DOM.on popupEl DOM.click $ DOM.stopPropagation
    selection <- holdDyn (head examples) update
    pure selection
  pure $ snd <$> selection

exampleData :: [(Text, Text)]
exampleData =
  [ ("Hello World", "examples/helloWorld-1.0")
  , ("Simple Payment", "examples/simplePayments-1.0")
  , ("International Payment", "examples/internationalPayments-1.0")
  , ("Commercial Paper", "examples/commercialPaper-1.0")
  ]

demos :: Map Int (Text, Text)
demos = Map.fromList $ zip [0..] exampleData

-- | What demo do we load on startup:
initialDemo :: Int
initialDemo = 0

-- | File name prefix of `initialDemo`
initialDemoFile :: Text
initialDemoFile = snd . fromJust $ Map.lookup initialDemo demos

-- Instances:

instance Reflex t => Semigroup (IdeCfg t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (IdeCfg t) where
  mempty = memptydefault
  mappend = (<>)
