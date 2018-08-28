{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module ReplGhcjs where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Arrow               ((&&&))
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Aeson                  (Object, decodeStrict)
import qualified Data.List.Zipper            as Z
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Maybe
import           Data.Semigroup
import           Data.Sequence               (Seq)
import qualified Data.Sequence               as S
import           Data.String.QQ
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Generics.Deriving.Monoid    (mappenddefault, memptydefault)
import           GHC.Generics                (Generic)
import           Language.Javascript.JSaddle hiding (Object)
import           Reflex
import           Reflex.Dom.ACE.Extended
import           Reflex.Dom.Core             (keypress, mainWidget, setValue)
import qualified Reflex.Dom.Core             as Core
import           Reflex.Dom.SemanticUI       hiding (mainWidget)
------------------------------------------------------------------------------
import           Pact.Repl
import           Pact.Repl.Types
import           Pact.Types.Lang
------------------------------------------------------------------------------
import           Wallet
import           UI.Wallet
import           Static
import           Widgets

-- | Re-use data constructors more flexibly.
type family ReflexValue (f :: * -> *) x where
    ReflexValue (Dynamic t) x = Dynamic t x

    ReflexValue Identity x = x

    ReflexValue (Behavior t) x = Behavior t x

    ReflexValue (Event t) x = Event t x

data ContractV f =
  Contract
    { _contract_data :: ReflexValue f Text
    , _contract_code :: ReflexValue f Text
    }
    deriving (Generic)

makeLensesWith (lensRules & generateLazyPatterns .~ True) ''ContractV

type Contract = ContractV Identity

deriving instance Show (ContractV Identity)

type DynContract t = ContractV (Dynamic t)

instance Reflex t => Semigroup (DynContract t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (DynContract t) where
  mempty = memptydefault
  mappend = (<>)

instance Semigroup Contract where
  (<>) = mappenddefault

instance Monoid Contract where
  mempty = memptydefault
  mappend = (<>)


data IDE t =
  IDE
    { _ide_contract           :: DynContract t
    -- ^ Currently loaded/edited PACT code (JSON `code` field )and data (JSON `data` field).
    {- , _ide_contracts :: Dynamic t Contracts -}
    {- -- ^ Contracts that can be loaded into the IDE. -}
    , _ide_selectedContract   :: Dynamic t Text
    -- ^ The currently selected contract in the dropdown.
    , _ide_onLoadRequest      :: Event t ()
    -- ^ User pressed the Load button and wants to load the code into the REPL.
    , _ide_onContractReceived :: Event t Contract
    -- ^ Contract was successfully retrieved from server.
    , _ide_wallet             :: Wallet t
    , _ide_walletConfig       :: WalletConfig t
    , _ide_signingKey :: Dynamic t (Maybe Text)
    }
    deriving Generic

makeLensesWith (lensRules & generateLazyPatterns .~ True) ''IDE

-- | Get `_ide_contract` as a single `Dynamic` `Contract`.
ide_getDynamicContract :: Reflex t => IDE t -> Dynamic t Contract
ide_getDynamicContract =
  uncurry (liftA2 Contract) . (_contract_data &&& _contract_code) . _ide_contract

instance Reflex t => Semigroup (IDE t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (IDE t) where
  mempty = memptydefault
  mappend = (<>)

data ContractFile =
  ContractFile
    { _contract_baseName :: Text
    -- ^ Name of the contract needed for locating the files.
    {- , _contract_location :: Location -}
    -- ^ Where is the contract stored?
    }

{- -- | Map of contract names to contract locations. -}
{- -- -}
{- --   This is a list as opposed to a Map in order to preserve order. -}
{- type ContractFiles = [(Text, Contract)] -}



{- data Location -}
{-   = Location_Example -- ^ Predefined example code loaded from the server -}
{-   | Location_Local -- ^ User contract stored in localstorage. -}


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
app = void . mfix $ \ide -> elClass "div" "app" $ do
    wallet <- makeWallet $ _ide_walletConfig ide
    controlIde <- controlBar
    contractReceived <- loadContract $ _ide_selectedContract ide
    elClass "div" "ui two column padded grid main" $ mdo
      editorIde <- elClass "div" "column" $
        elClass "div" "ui segment editor-pane" $ codePanel ide
      envIde <- elClass "div" "column repl-column" $
        elClass "div" "ui segment env-pane" $ envPanel ide
      pure $ mconcat
        [ controlIde
        , editorIde
        , envIde
        , mempty & ide_onContractReceived .~ contractReceived
        , mempty & ide_wallet .~ wallet
        ]
    where
      loadContract contractName = do
        code <- loadContractData toCodeFile contractName
        json <- loadContractData toDataFile contractName
        waitForEvents Contract (updated contractName) json code

      loadContractData getFileName contractName =
        fmap (fmap codeFromResponse)
        . performRequestAsync
        . fmap ((\u -> xhrRequest "GET" u def) . getFileName)
        . updated
        $ contractName



-- | The available panels in the `envPanel`
data EnvSelection
  = EnvSelection_Repl -- ^ REPL for interacting with loaded contract
  | EnvSelection_Env -- ^ Widgets for editing (meta-)data.
  | EnvSelection_Keys -- ^ Keys management pane
  {- | EnvSelection_Compiler -- ^ Any compiler output (errors) -}
  deriving (Eq, Ord, Show)

-- | Code editing (left hand side currently)
codePanel :: forall t m. MonadWidget t m => IDE t -> m (IDE t)
codePanel ide = mdo
  {- menu (def & menuConfig_secondary .~ pure True) $ do -}
  {-   menuItem def $ text "Code"  -}
  (p, codeIDE) <- elClass' "div" "ui segment" $ do
    code <- codeWidget p startingCode $ _contract_code <$> _ide_onContractReceived ide
    pure $ mempty & ide_contract . contract_code .~ code
  pure codeIDE

-- | Tabbed panel to the right
--
--   Offering access to:
--
--   - The REPL
--   - Compiler error messages
--   - Key & Data Editor
envPanel :: forall t m. MonadWidget t m => IDE t -> m (IDE t)
envPanel ide = mdo
  curSelection <- holdDyn EnvSelection_Env onSelect

  onSelect <- menu
    ( def & menuConfig_pointing .~ pure True
        & menuConfig_secondary .~ pure True
    )
    $ tabs curSelection

  tabPane ("class" =: "ui segment") curSelection EnvSelection_Repl $
    replWidget ide

  envIde <- tabPane
      ("class" =: "ui segment styled fluid accordion flex-accordion")
      curSelection EnvSelection_Env $ mdo
    (pJson, jsonIde) <- accordionItem' True "data" "Data" $ do
      json <- dataWidget pJson startingData
        $ _contract_data <$> _ide_onContractReceived ide
      pure $ mempty & ide_contract . contract_data .~ json

    keysIde1 <- accordionItem True "keys" "Sign message with Keys" $ do
      selKey <- uiSelectKey (_ide_wallet ide) hasPrivateKey
      pure $ mempty & ide_signingKey .~ selKey

    pure $ mconcat [ jsonIde
                   , keysIde1
                   ]

  keysIde <- tabPane
      ("class" =: "ui segment")
      curSelection EnvSelection_Keys $ do
    conf <- uiWallet $ _ide_wallet ide
    pure $ mempty & ide_walletConfig .~ conf

  pure $ mconcat [ envIde, keysIde ]

  where
    tabs :: Dynamic t EnvSelection -> m (Event t EnvSelection)
    tabs curSelection = do
      let
        selections = [ EnvSelection_Env, EnvSelection_Repl, EnvSelection_Keys ]
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
  EnvSelection_Keys -> "Keys"

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
  => Element EventResult (DomBuilderSpace m) t -> Text -> Event t Text
  -> m (Dynamic t Text)
codeWidget parent iv sv = do
    let ac = def { _aceConfigMode = Just "ace/mode/pact"
                 , _aceConfigElemAttrs = "class" =: "ace-code ace-widget"
                 }
    ace <- resizableAceWidget ac (AceDynConfig $ Just AceTheme_SolarizedLight) parent iv
    _ <- withAceInstance ace (setValueACE <$> sv)
    return $ aceValue ace

dataWidget
  :: MonadWidget t m
  => Element EventResult (DomBuilderSpace m) t -> Text -> Event t Text
  -> m (Dynamic t Text)
dataWidget parent iv sv = do
    let ac = def { _aceConfigMode = Just "ace/mode/json"
                 , _aceConfigElemAttrs = "class" =: "ace-data ace-widget"
                 }
    ace <- resizableAceWidget ac (AceDynConfig $ Just AceTheme_SolarizedLight) parent iv
    _ <- withAceInstance ace (setValueACE <$> sv)
    return $ aceValue ace


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
snippetWidget (InputSnippet t)  = elAttr "pre" ("class" =: "replOut") $ text t
snippetWidget (OutputSnippet t) = elAttr "pre" ("class" =: "replOut") $ text t

------------------------------------------------------------------------------
replWidget
    :: MonadWidget t m
    => IDE t
    -> m ()
replWidget ide = mdo
  (e, newExpr) <- elClass' "div" "repl-pane" $ mdo
    mapM_ snippetWidget staticReplHeader
    clickType <- foldDyn ($) Nothing $ leftmost
      [ setDown <$> domEvent Mousedown e
      , clickClassifier <$> domEvent Mouseup e
      ]
    let replClick = () <$ ffilter (== Just Clicked) (updated clickType)
    widgetHold
      (replInner replClick startingContract)
      (replInner replClick <$> tag (current $ ide_getDynamicContract ide)
        (_ide_onLoadRequest ide)
      )
  timeToScroll <- delay 0.1 $ switch $ current newExpr
  void $ performEvent (scrollToBottom (_element_raw e) <$ timeToScroll)

replInner
    :: MonadWidget t m
    => Event t ()
    -> Contract
    -> m (Event t Text)
replInner replClick contract = mdo
    let dataIsObject = isJust . toObject $ _contract_data contract
    let code = "(env-data " <> _contract_data contract <> ")\n\n" <> _contract_code contract
    initState <- liftIO $ initReplState StringEval
    stateAndOut0 <-
      if dataIsObject
         then runReplStep0 (initState, mempty) code
         else pure $
         ( initState
         , S.singleton $ OutputSnippet "ERROR: Data must be a valid JSON object!"
         )
    stateAndOut <- holdDyn stateAndOut0 evalResult

    _ <- dyn (mapM_ snippetWidget . snd <$> stateAndOut)
    newInput <- replInput replClick
    evalResult <- performEvent $
      attachWith runReplStep (current stateAndOut) newInput
    return newInput
  where
      toObject :: Text -> Maybe Object
      toObject = decodeStrict . T.encodeUtf8


replInput :: MonadWidget t m => Event t () -> m (Event t Text)
replInput setFocus = do
    divClass "repl-input-controls" $ mdo
      el "span" $ text "pact>"
      let sv = leftmost
            [ mempty <$ enterPressed
            , fromMaybe "" . Z.safeCursor <$> tagPromptlyDyn commandHistory key
            ]
      ti <- Core.textInput (def & setValue .~ sv)
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
    -> m (ReplState, Seq DisplayedSnippet)
runReplStep0 (s1,snippets1) e = do
    (r,s2) <- liftIO $ runStateT (evalRepl' $ T.unpack e) s1
    return (s2, snippets1 <> S.singleton
      (OutputSnippet $ T.pack $ either id (const $ _rOut s2) r))

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

controlBar :: forall t m. MonadWidget t m => m (IDE t)
controlBar = do
    elClass "div" "ui menu" $ do
      elClass "div" "item" showPactVersion

      control <- exampleChooser
      elClass "div" "right menu" rightMenu
      pure control
  where
    showPactVersion = do
      elAttr "a" ( "target" =: "_blank" <> "href" =: "https://github.com/kadena-io/pact") $ do
        is <- liftIO $ initReplState StringEval
        Right (TLiteral (LString ver) _) <- liftIO $ evalStateT (evalRepl' "(pact-version)") is
        text $ "Pact Version " <> ver

    exampleChooser :: m (IDE t)
    exampleChooser = do
      d <- elClass "div" "item" $
        dropdown def (Identity 0) $ TaggedStatic $ text . fst <$> demos
      load <- elClass "div" "item" $
        button (def & buttonConfig_emphasis |?~ Primary) $  text "Load"
      let intToCode n = snd $ fromJust $ M.lookup n demos
      pure $ mempty
        & ide_selectedContract .~  (intToCode . runIdentity <$> _dropdown_value d)
        & ide_onLoadRequest .~ load

    rightMenu = do
      elClass "div" "ui item" $
        el "label" $
          elAttr "a" ("target" =: "_blank" <>
                      "style" =: "color:black;text-decoration:none;" <>
                      "href" =: "http://pact-language.readthedocs.io"
                      ) $ do
            elAttr "i" ("class" =: "fa fa-book" <> "aria-hidden" =: "true") blank
            elAttr "span" ("id" =: "hideIfTiny" <> "class" =: "menu-link") $ text "Docs"
      elClass "div" "ui item" $
        elAttr "a" ("target" =: "_blank" <> "href" =: "http://kadena.io") $
          elAttr "img" ("src" =: static @"img/kadena-logo84x20.png" <> "class" =: "logo-image") blank


exampleData :: [(Text, Text)]
exampleData =
  [ ("Hello World", "examples/helloWorld-1.0")
  , ("Simple Payment", "examples/simplePayments-1.0")
  , ("International Payment", "examples/internationalPayments-1.0")
  , ("Commercial Paper", "examples/commercialPaper-1.0")
  ]

demos :: Map Int (Text, Text)
demos = M.fromList $ zip [0..] exampleData

------------------------------------------------------------------------------
-- | We still have this hard coded initial value because Reflex has to put
-- some value in before the first event fires, so we use this one.  It should
-- match the first element of the exampleData list.
startingCode :: Text
startingCode = [s|
;;
;; "Hello, world!" smart contract/module
;;

;; Simulate message data specifying an administrator keyset.
;; In production use 'mockAdminKey' would be an ED25519 hex-encoded public key.
(env-data { "admin-keyset": ["mockAdminKey"] })

;; Simulate that we've signed this transaction with the keyset.
;; In pact, signatures are pre-validated and represented in the
;; environment as a list of public keys.
(env-keys ["mockAdminKey"])

;; Keysets cannot be created in code, thus we read them in
;; from the load message data.
(define-keyset 'admin-keyset (read-keyset "admin-keyset"))

;; Define the module.
(module helloWorld 'admin-keyset
  "A smart contract to greet the world."
  (defun hello (name)
    "Do the hello-world dance"
    (format "Hello {}!" [name]))
)

;; and say hello!
(hello "world")
|]

startingData :: Text
startingData = [s|
  { "admin-keyset": ["mockAdminKey"] }
|]

startingContract :: Contract
startingContract =
  Contract
    { _contract_code = startingCode
    , _contract_data = startingData
    }

