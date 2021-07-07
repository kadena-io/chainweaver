{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Wallet setup screens
module Frontend.GHCOnly.Setup (WalletExists (..), Password(..), runSetup, splashLogo, setupDiv, setupClass, checkPassword) where

import Control.Lens ((<>~), (??), (^.), _1, _2, _3)
import Control.Error (hush)
import Control.Applicative (liftA2)
import Control.Monad (unless, guard, void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Control.Monad.IO.Class
import Data.Bool (bool)
import Data.Foldable (fold, traverse_)
import Data.Maybe (isNothing, fromMaybe)
import Data.Bifunctor
import Data.ByteArray (ByteArrayAccess)
import Data.ByteString (ByteString)
import Data.String (IsString, fromString)
import Data.Functor ((<&>))
import Data.Text (Text)
import Language.Javascript.JSaddle (MonadJSM, liftJSM)
import Reflex.Dom.Core
-- import qualified Cardano.Crypto.Wallet as Crypto
-- import qualified Crypto.Encoding.BIP39 as Crypto
-- import qualified Crypto.Encoding.BIP39.English as Crypto
-- import qualified Crypto.Random.Entropy
import qualified Data.ByteArray as BA
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.FilePath (takeFileName)

import Frontend.AppCfg (FileFFI(..), FileType(FileType_Import))
-- import Desktop.ImportExport (doImport, Password(..), ImportWalletError(..))
-- import Pact.Server.ApiClient (HasTransactionLogger, askTransactionLogger)
import Frontend.Storage.Class (HasStorage)
import Frontend.UI.Button
import Frontend.UI.Dialogs.ChangePassword (minPasswordLength)
import Frontend.UI.Widgets.Helpers (imgWithAlt)
import Frontend.UI.Widgets
import Obelisk.Generated.Static

import Frontend.Crypto.Ed25519

newtype Password = Password { unPassword :: Text } deriving (Eq)

-- | Used for changing the settings in the passphrase widget.
data PassphraseStage
  = Setup
  | Recover
  deriving (Show, Eq)

-- | Wrapper for the index of the word in the passphrase
newtype WordKey = WordKey { _unWordKey :: Int }
  deriving (Show, Eq, Ord, Enum)

-- | Setup stage
data WalletScreen
  = WalletScreen_Password
  | WalletScreen_PrePassphrase
  | WalletScreen_CreatePassphrase
  | WalletScreen_VerifyPassphrase
  | WalletScreen_RecoverPassphrase
  | WalletScreen_RecoverImport
  | WalletScreen_SplashScreen
  | WalletScreen_Done
  deriving (Show, Eq, Ord)

setupScreenClass :: WalletScreen -> Text
setupScreenClass = setupClass . T.toLower . tshow

wordsToPhraseMap :: [Text] -> Map.Map WordKey Text
wordsToPhraseMap = Map.fromList . zip [WordKey 1 ..]

setupClass :: Text -> Text
setupClass = mappend "setup__"

setupDiv :: DomBuilder t m => Text -> m a -> m a
setupDiv t = divClass (setupClass t)

setupCheckbox
  :: (DomBuilder t m, PostBuild t m)
  => Bool
  -> CheckboxConfig t
  -> m ()
  -> m (Checkbox t)
setupCheckbox initialValue cfg inner = elClass "div" (setupClass "checkbox")
  $ uiCheckbox def initialValue cfg inner

-- mkPhraseMapFromMnemonic
--   :: forall mw.
--      Crypto.ValidMnemonicSentence mw
--   => Crypto.MnemonicSentence mw
--   -> Map.Map WordKey Text
-- mkPhraseMapFromMnemonic = wordsToPhraseMap . T.words . baToText
--   . Crypto.mnemonicSentenceToString @mw Crypto.english

showWordKey :: WordKey -> Text
showWordKey = T.pack . show . _unWordKey

-- | Convenience function for unpacking byte array things into 'Text'
baToText :: ByteArrayAccess b => b -> Text
baToText = T.decodeUtf8 . BA.pack . BA.unpack

textTo :: IsString a => Text -> a
textTo = fromString . T.unpack

tshow :: Show a => a -> Text
tshow = T.pack . show

setupForm :: forall t m a. (DomBuilder t m, PostBuild t m) => Text -> Text -> Dynamic t Bool -> m a -> m (Event t (), a)
setupForm cls lbl disabled = uiForm cfg $ setupDiv cls $ void $ confirmButton (def & uiButtonCfg_disabled .~ disabled) lbl
  where cfg = def & elementConfig_initialAttributes .~ ("class" =: setupClass "form")

restoreForm :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> m a -> m (Event t (), a)
restoreForm = setupForm "recover-restore-button" "Restore"

continueForm :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> m a -> m (Event t (), a)
continueForm = setupForm "continue-button" "Continue"

-- | Wallet logo
kadenaWalletLogo :: DomBuilder t m => m ()
kadenaWalletLogo = divClass "logo" $ do
  elAttr "img" ("src" =: static @"img/kadena_blue_logo.png" <> "class" =: setupClass "kadena-logo") blank
  elClass "div" "chainweaver" $ text "Chainweaver"
  elClass "div" "by-kadena" $ text "by Kadena"

type SetupWF t m = Workflow t m
  (WalletScreen
  , Event t (PrivateKey, Password, Bool)
  , Event t () -- ExitSetup Event
  )

finishSetupWF :: (Reflex t, Applicative m) => WalletScreen -> a -> m ((WalletScreen, Event t x, Event t ()), a)
finishSetupWF ws = pure . (,) (ws, never, never)

-- Make this take a list of the current progress instead so we can maintain
-- the list of how much we have done so far.
--
walletSetupRecoverHeader
  :: DomBuilder t m
  => WalletScreen
  -> m ()
walletSetupRecoverHeader currentScreen = setupDiv "workflow-header" $ do
  unless (currentScreen `elem` [WalletScreen_RecoverPassphrase, WalletScreen_RecoverImport, WalletScreen_SplashScreen]) $ do
    elClass "ol" (setupClass "workflow-icons") $ do
      faEl "1" "Password" WalletScreen_Password
      line WalletScreen_PrePassphrase
      faEl "2" "Recovery" WalletScreen_CreatePassphrase
      line WalletScreen_VerifyPassphrase
      faEl "3" "Verify" WalletScreen_VerifyPassphrase

  where
    addActive sid =
      if isActive sid then
        ["active"]
      else
        []

    line sid =
      elClass "div" (T.unwords $ setupClass "workflow-icon": setupClass "header-line" : addActive sid) blank

    isActive sid = sid `elem` (progress currentScreen)
    progress WalletScreen_Password =
      []
    progress WalletScreen_PrePassphrase =
      [WalletScreen_Password]
    progress WalletScreen_CreatePassphrase =
      [WalletScreen_Password, WalletScreen_PrePassphrase]
    progress WalletScreen_VerifyPassphrase =
      [WalletScreen_Password, WalletScreen_PrePassphrase, WalletScreen_CreatePassphrase]
    progress WalletScreen_Done =
      [WalletScreen_Password, WalletScreen_PrePassphrase, WalletScreen_CreatePassphrase, WalletScreen_VerifyPassphrase]
    progress _ = []

    faEl n lbl sid =
      elClass "li" (setupClass "workflow-icon" <> if isActive sid then " active" else T.empty) $ do
        elClass "div" (setupClass "workflow-icon-circle" <> " " <> setupClass ("workflow-screen-" <> T.toLower lbl)) $
          setupDiv "workflow-icon-symbol" $ do
            if isActive sid
              then elClass "i" ("fa fa-check fa-lg fa-inverse " <> setupClass "workflow-icon-active") blank
              else text n
            setupDiv "workflow-icon-label" $ text lbl

data WalletExists
  = WalletExists_Yes
  | WalletExists_No
  deriving (Show, Eq)

runSetup
  :: forall t m
  . ( DomBuilder t m
    , MonadFix m
    , MonadHold t m
    , PerformEvent t m
    , PostBuild t m
    , MonadJSM (Performable m)
    , TriggerEvent t m
    , HasStorage (Performable m)
    , MonadSample t (Performable m)
    )
  => FileFFI t m
  -> Bool
  -> WalletExists
  -> m (Event t (Either () (PrivateKey, Password, Bool)))
runSetup fileFFI showBackOverride walletExists = setupDiv "fullscreen" $ mdo
  let dCurrentScreen = (^._1) <$> dwf

  eBack <- fmap (domEvent Click . fst) $ elDynClass "div" ((setupClass "back " <>) . hideBack <$> dCurrentScreen) $
    el' "span" $ do
      elClass "i" "fa fa-fw fa-chevron-left" $ blank
      text "Back"

  _ <- dyn_ $ walletSetupRecoverHeader <$> dCurrentScreen

  dwf <- divClass "wrapper" $
    workflow (splashScreen walletExists fileFFI eBack)

  pure $ leftmost
    [ fmap Right $ switchDyn $ (^. _2) <$> dwf
    , attachWithMaybe (\s () -> Left () <$ guard (s == WalletScreen_SplashScreen)) (current dCurrentScreen) eBack
    , fmap Left $ switchDyn $ (^. _3) <$> dwf
    ]
  where
    hideBack ws =
      if not showBackOverride && (ws `elem` [WalletScreen_SplashScreen, WalletScreen_Done]) then
        setupClass "hide"
      else
        setupScreenClass ws

splashLogo :: DomBuilder t m => m ()
splashLogo = do
  elAttr "div"
    (  "style" =: ("background-image: url(" <> (static @"img/Wallet_Graphic_1.png") <> ");")
    <> "class" =: setupClass "splash-bg"
    ) kadenaWalletLogo

splashScreen
  :: (DomBuilder t m, MonadFix m, MonadHold t m, PerformEvent t m
     , PostBuild t m, MonadJSM (Performable m), TriggerEvent t m, HasStorage (Performable m)
     , MonadSample t (Performable m)
     -- , HasTransactionLogger m
     )
  => WalletExists
  -> FileFFI t m
  -> Event t ()
  -> SetupWF t m
splashScreen walletExists fileFFI eBack = selfWF
  where
    selfWF = Workflow $ setupDiv "splash" $ do
      splashLogo

      setupDiv "splash-terms-buttons" $ do
        agreed <- fmap value $ setupCheckbox False def $ el "div" $ do
          text "I have read & agree to the "
          elAttr "a" ?? (text "Terms of Service") $ mconcat
            [ "href" =: "https://kadena.io/chainweaver-tos"
            , "target" =: "_blank"
            , "class" =: setupClass "terms-conditions-link"
            ]

        let hasAgreed = gate (current agreed)
            disabledCfg = uiButtonCfg_disabled .~ fmap not agreed
            restoreCfg = uiButtonCfg_class <>~ "setup__restore-existing-button"

        create <- confirmButton (def & disabledCfg ) "Create a new wallet"

        restoreBipPhrase <- uiButtonDyn (btnCfgSecondary & disabledCfg & restoreCfg)
          $ text "Restore from recovery phrase"

        restoreImport <- uiButtonDyn (btnCfgSecondary & disabledCfg & restoreCfg)
          $ text "Restore from wallet export"

        finishSetupWF WalletScreen_SplashScreen $ leftmost
          [ restoreBipWallet selfWF eBack <$ hasAgreed restoreBipPhrase]
          -- [ createNewWallet selfWF eBack <$ hasAgreed create
          -- , restoreBipWallet selfWF eBack <$ hasAgreed restoreBipPhrase
          -- , restoreFromImport walletExists fileFFI selfWF eBack <$ hasAgreed restoreImport
          -- ]

data BIP39PhraseError
  = BIP39PhraseError_InvalidPhrase
  | BIP39PhraseError_PhraseIncomplete

passphraseLen :: Int
passphraseLen = 12

-- sentenceToSeed :: Crypto.ValidMnemonicSentence mw => Crypto.MnemonicSentence mw -> Crypto.Seed
-- sentenceToSeed s = Crypto.sentenceToSeed s Crypto.english ""

restoreBipWallet
  :: (DomBuilder t m, MonadHold t m, MonadFix m, PerformEvent t m, PostBuild t m, 
      MonadSample t (Performable m), MonadJSM (Performable m), TriggerEvent t m)
  => SetupWF t m -> Event t () -> SetupWF t m
restoreBipWallet backWF eBack = Workflow $ do
  el "h1" $ text "Recover your wallet"

  setupDiv "recovery-text" $ do
    el "div" $ text "Enter your 12 word recovery phrase"
    el "div" $ text "to restore your wallet."

  rec
    phraseMap <- holdDyn (wordsToPhraseMap $ replicate passphraseLen T.empty)
      $ flip Map.union <$> current phraseMap <@> onPhraseMapUpdate

    onPhraseMapUpdate <- passphraseWidget phraseMap (pure Recover) True

  let enoughWords = (== passphraseLen) . length . filter (not . T.null) . Map.elems

  let sentenceOrError = ffor phraseMap $ \pm -> if enoughWords pm then do
        let phrase = Map.elems pm
        unless (length phrase == passphraseLen) $ Left BIP39PhraseError_PhraseIncomplete
        Right phrase
        else Left BIP39PhraseError_PhraseIncomplete

  -- Check phrase for errors
  dyn_ $ ffor sentenceOrError $ \case
    Right _ -> pure ()
    Left e -> setupDiv "phrase-error-message-wrapper" $ setupDiv "phrase-error-message" $ text $ case e of
      BIP39PhraseError_InvalidPhrase
        -> "Invalid phrase"
      BIP39PhraseError_PhraseIncomplete
        -> mempty

  -- GEN SEED: Todo: Make sure wrong words returns Nothing
      -- generateRoot $ T.unwords phrase'
  let 
      ePhraseUpdated = fmapMaybe hush $ updated sentenceOrError

      waitingForPhrase = setupDiv "waiting-passphrase" $ do
        text "Waiting for a valid 12 word passphrase..."
        pure never

      -- withSeedConfirmPassword ::_
      withSeedConfirmPassword sentence = setupDiv "recover-enter-password" $ mdo
        (ePwSubmit, dSetPw) <- restoreForm (fmap isNothing dSetPw) $ do
          holdDyn Nothing =<< setPassword (pure sentence)
        pure $ tagMaybe (current dSetPw) ePwSubmit

  dSetPassword <- widgetHold waitingForPhrase $
    withSeedConfirmPassword <$> ePhraseUpdated
  pure
    ( (WalletScreen_RecoverPassphrase, (\(prv, pw) -> (prv, pw, True)) <$> switchDyn dSetPassword, never)
    , backWF <$ eBack
    )

-- restoreFromImport
--   :: forall t m
--   .  ( DomBuilder t m, MonadFix m, MonadHold t m, PerformEvent t m
--      , PostBuild t m, MonadJSM (Performable m), HasStorage (Performable m)
--      , MonadSample t (Performable m)
--      , HasTransactionLogger m
--      )
--   => WalletExists
--   -> FileFFI t m
--   -> SetupWF t m
--   -> Event t ()
--   -> SetupWF t m
-- restoreFromImport walletExists fileFFI backWF eBack = nagScreen
--   where
--     nagMsgs = case walletExists of
--       WalletExists_Yes ->
--         ("You are about to replace the current wallet's data"
--         ,"Reminder: Importing a wallet file will replace the data within the current wallet."
--         )
--       WalletExists_No ->
--         ("Please select the wallet import file."
--         ,"Reminder: You will need your wallet password to proceed."
--         )

--     nagBack = case walletExists of
--       WalletExists_No -> pure never
--       WalletExists_Yes -> uiButtonDyn
--         -- TODO: Don't reuse this class or at least rename it
--         (btnCfgSecondary & uiButtonCfg_class <>~ "setup__restore-existing-button")
--         (text "Go back and export current wallet")

--     nagScreen = Workflow $ setupDiv "splash" $ do
--       splashLogo
--       let (nagTitle, nagReminder) = nagMsgs
--       elClass "h1" "setup__recover-import-title" $ text nagTitle
--       elClass "p" "setup__recover-import-text" $ text nagReminder
--       eImport <- confirmButton def "Select Import File"
--       eExit <- nagBack
--       pure
--         ( (WalletScreen_RecoverImport, never, eExit)
--         , leftmost
--           [ backWF <$ (eBack <> eExit)
--           , importScreen <$ eImport
--           ]
--         )

--     importScreen = Workflow $ setupDiv "splash" $ mdo
--       splashLogo
--       elClass "h1" "setup__recover-import-title" $ text "Import File Password"
--       elClass "p" "setup__recover-import-text" $ text "Enter the password for the chosen wallet file in order to authorize access to the data."

--       let disabled = isNothing <$> dmValidForm
--       dErr <- holdDyn Nothing (leftmost [Just <$> eImportErr, Nothing <$ updated dmValidForm])
--       (eSubmit, (dFileSelected, pwInput)) <- setupForm "" "Import File" disabled $ mdo
--         ePb <- getPostBuild
--         (selectElt, _) <- elClass' "div" "setup__recover-import-file" $ do
--           imgWithAlt (static @"img/import.svg") "Import" blank
--           divClass "setup__recover-import-file-text" $ dynText $ ffor dFileSelected $
--             maybe "Select a file" (T.pack . takeFileName . fst)

--         performEvent_ $ liftJSM (_fileFFI_openFileDialog fileFFI FileType_Import) <$
--           ((domEvent Click selectElt) <> ePb)

--         dFileSelected <- holdDyn Nothing (Just <$> _fileFFI_externalFileOpened fileFFI)

--         pw <- uiPassword (setupClass "password-wrapper") (setupClass "password") "Enter import wallet password"

--         dyn_ $ ffor dErr $ traverse_ $ \err ->
--           elClass "p" "error_inline" $ text $ case err of
--             ImportWalletError_InvalidCommandLogDestination -> "Destination for transaction log file is invalid"
--             ImportWalletError_CommandLogWriteError -> "Unable to write transaction log file"
--             ImportWalletError_PasswordIncorrect -> "Incorrect Password"
--             ImportWalletError_NoRootKey -> "Backup cannot be restored as it does not contain a BIP Root Key"
--             ImportWalletError_NotJson eMsg -> "Backup cannot be restored as it is not a valid json file. Error: " <> eMsg
--             ImportWalletError_DecodeError section ver eMsg ->
--               "Backup section " <> section <> " cannot be parsed as version " <> tshow ver  <>  " with error: " <> eMsg
--             ImportWalletError_UnknownVersion section ver ->
--               "Backup section " <> section <> " has an unknown version " <> tshow ver <> ". It's likely that this backup is from a newer version of chainweaver."


--         pure (dFileSelected, pw)

--       eExit <- nagBack
--       let dmValidForm = runMaybeT $ (,)
--             <$> MaybeT (nonEmptyPassword <$> (_inputElement_value pwInput))
--             <*> MaybeT (fmap snd <$> dFileSelected)

--       txLogger <- askTransactionLogger
--       eImport <- performEvent $ tagMaybe (fmap (uncurry (doImport @t txLogger)) <$> current dmValidForm) eSubmit

--       let (eImportErr, eImportDone) = fanEither eImport

--       pure
--         ( (WalletScreen_RecoverImport, (\(prv,pw) -> (prv, pw, False)) <$> eImportDone, eExit)
--         , backWF <$ (eBack <> eExit)
--         )

--     nonEmptyPassword "" = Nothing
--     nonEmptyPassword pw = Just (Password pw)

passphraseWordElement
  :: (DomBuilder t m, PostBuild t m, MonadFix m)
  => Dynamic t PassphraseStage
  -> Bool
  -> WordKey
  -> Dynamic t Text
  -> m (Event t Text)
passphraseWordElement currentStage exposeEmpty k wrd = do
  rec
    let eFocused = leftmost
          [ eInputFocused
          , True <$ domEvent Mouseenter elt
          , True <$ domEvent Mousemove elt
          , False <$ domEvent Mouseleave elt
          ]

    (elt, (val, eInputFocused, eInput)) <- elClass' "div" (setupClass "passphrase-widget-elem-wrapper") $ do
      pb <- getPostBuild

      setupDiv "passphrase-widget-key-wrapper" $
        text (showWordKey k)

      let
        eExposed = attachWith (\v f -> T.null v || f) (current val) eFocused

        commonClass cls exposed = "input " <> (setupClass cls <> bool "" (" " <> setupClass cls <> "--exposed") exposed)
        commonAttrs cls exposed =
          "type" =: "text" <>
          "size" =: "8" <>
          "class" =: commonClass cls exposed

      setupDiv "passphrase-widget-word-wrapper" $ do
        void . uiInputElement $ def
          & inputElementConfig_initialValue .~ "********"
          & initialAttributes .~ (commonAttrs "passphrase-widget-word-hider" exposeEmpty <> "disabled" =: "true" <> "tabindex" =: "-1")
          & modifyAttributes <>~ (("class" =:) . Just . commonClass "passphrase-widget-word-hider" <$> eExposed)

        inputElt <- uiInputElement $ def
          & inputElementConfig_setValue .~ (current wrd <@ pb)
          & initialAttributes .~ commonAttrs "passphrase-widget-word" exposeEmpty
          & modifyAttributes <>~ fold
            [ (("readonly" =:) . canEditOnRecover <$> current currentStage <@ pb)
            , (("class" =:) . Just . commonClass "passphrase-widget-word" <$> eExposed)
            ]

        pure (_inputElement_value inputElt, updated $ _inputElement_hasFocus inputElt, _inputElement_input inputElt)
  pure (T.strip <$> eInput)
  where
    canEditOnRecover Recover = Nothing
    canEditOnRecover Setup = Just "true"

passphraseWidget
  :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m)
  => Dynamic t (Map.Map WordKey Text)
  -> Dynamic t PassphraseStage
  -> Bool
  -> m (Event t (Map.Map WordKey Text))
passphraseWidget dWords dStage exposeEmpty = do
  setupDiv "passphrase-widget-wrapper" $
    listViewWithKey dWords (passphraseWordElement dStage exposeEmpty)

-- createNewWallet
--   :: forall t m. (DomBuilder t m, MonadFix m, MonadHold t m, PerformEvent t m, PostBuild t m, MonadJSM (Performable m), TriggerEvent t m)
--   => SetupWF t m -> Event t () -> SetupWF t m
-- createNewWallet backWF eBack = selfWF
--   where
--     selfWF = Workflow $  do
--       ePb <- getPostBuild
--       elAttr "img" ("src" =: static @"img/Wallet_Graphic_2.png" <> "class" =: setupClass "password-bg") blank

--       el "h1" $ text "Set a password"
--       setupDiv "new-wallet-password-text" $ do
--         el "div" $ text "Enter a strong and unique password"
--         el "div" $ text "to protect access to your Chainweaver wallet"

--       (eGenError, eGenSuccess) <- fmap fanEither . performEvent $ genMnemonic <$ ePb

--       let
--         generating = do
--           dynText =<< holdDyn "Generating your mnemonic..." eGenError
--           pure never

--         proceed :: Crypto.MnemonicSentence 12 -> m (Event t (SetupWF t m))
--         proceed mnem = mdo
--           (ePwSubmit, dPassword) <- continueForm (fmap isNothing dPassword) $ do
--             holdDyn Nothing =<< setPassword (pure $ sentenceToSeed mnem)

--           fmap snd $ runWithReplace blank $ ffor (tagMaybe (current dPassword) ePwSubmit) $ \p ->
--             pure $ precreatePassphraseWarning selfWF eBack p mnem

--       dContinue <- widgetHold generating (proceed <$> eGenSuccess)

--       finishSetupWF WalletScreen_Password $ leftmost
--         [ backWF <$ eBack
--         , switchDyn dContinue
--         ]

walletSplashWithIcon :: DomBuilder t m => m () -> m ()
walletSplashWithIcon w = do
  elAttr "div" (
    "style" =: ("background-image: url(" <> (static @"img/Wallet_Graphic_1.png") <> ");")
    <> "class" =: (setupClass "splash-bg " <> setupClass "done-splash-bg")
    ) $ do
      w
      elAttr "img" (
        "src" =: static @"img/Wallet_Icon_Highlighted_Blue.svg" <>
        "class" =: setupClass "wallet-blue-icon"
        ) blank

stackFaIcon :: DomBuilder t m => Text -> m ()
stackFaIcon icon = elClass "span" "fa-stack fa-5x" $ do
  elClass "i" "fa fa-circle fa-stack-2x" blank
  elClass "i" ("fa " <> icon <> " fa-stack-1x fa-inverse") blank

-- precreatePassphraseWarning
--   :: (DomBuilder t m, MonadFix m, MonadHold t m, PerformEvent t m, PostBuild t m, MonadJSM (Performable m))
--   => SetupWF t m
--   -> Event t ()
--   -> (Crypto.XPrv, Password)
--   -> Crypto.MnemonicSentence 12
--   -> SetupWF t m
-- precreatePassphraseWarning backWF eBack (rootKey, password) mnemonicSentence = Workflow $ mdo
--   (eContinue, dUnderstand) <- continueForm (fmap not dUnderstand) $ do

--     setupDiv "warning-splash" $ do
--       walletSplashWithIcon $ do
--         setupDiv "repeat-icon" $ stackFaIcon "fa-repeat"

--     el "h1" $ text "Wallet Recovery Phrase"

--     setupDiv "recovery-phrase-warning" $ do
--       line "In the next step you will record your 12 word recovery phrase."
--       line "Your recovery phrase makes it easy to restore your wallet on a new device."
--       line "Anyone with this phrase can take control of your wallet, keep this phrase private."
--       line "Kadena cannot access your recovery phrase if lost, please store it safely."

--     let chkboxcls = setupClass "checkbox-wrapper"
--     fmap value $ elClass "div" chkboxcls $ setupCheckbox False def $ el "div" $ do
--       line "I understand that if I lose my recovery phrase,"
--       line "I will not be able to restore my wallet."

--   finishSetupWF WalletScreen_PrePassphrase $ leftmost
--     [ backWF <$ eBack
--     , createNewPassphrase backWF eBack (rootKey, password) mnemonicSentence <$ eContinue
--     ]
--   where
--     line = el "div" . text

doneScreen
  :: (DomBuilder t m, PostBuild t m)
  => (PrivateKey, Password)
  -> SetupWF t m
doneScreen (rootKey, passwd) = Workflow $ do
  walletSplashWithIcon blank

  el "h1" $ text "Wallet Created"

  eContinue <- setupDiv "continue-button" $
    confirmButton def "Done"

  pure ( (WalletScreen_Done, (rootKey, passwd, True) <$ eContinue, never)
       , never
       )

-- | UI for generating and displaying a new mnemonic sentence.
-- createNewPassphrase
--   :: (DomBuilder t m, MonadFix m, MonadHold t m, PerformEvent t m, PostBuild t m, MonadJSM (Performable m))
--   => SetupWF t m
--   -> Event t ()
--   -> (Crypto.XPrv, Password)
--   -> Crypto.MnemonicSentence 12
--   -> SetupWF t m
-- createNewPassphrase backWF eBack (rootKey, password) mnemonicSentence = selfWF
--   where
--     selfWF = Workflow $ mdo
--       (eContinue, dIsStored) <- continueForm (fmap not dIsStored) $ do
--         el "h1" $ text "Record Recovery Phrase"
--         el "div" $ do
--           el "div" $ text "Write down or copy these words in the correct order and store them safely."
--           el "div" $ text "The recovery words are hidden for security. Mouseover the numbers to reveal."

--         rec
--           dPassphrase <- passphraseWidget dPassphrase (pure Setup) False
--             >>= holdDyn (mkPhraseMapFromMnemonic mnemonicSentence)

--           let cfg = def
--                 & uiButtonCfg_class .~ "setup__recovery-phrase-copy"
--                 & uiButtonCfg_title .~ pure (Just "Copy")
--           copyButton cfg True $
--             T.unwords . Map.elems <$> current dPassphrase

--         fmap value $ setupDiv "checkbox-wrapper" $ setupCheckbox False def
--           $ text "I have safely stored my recovery phrase."

--       finishSetupWF WalletScreen_CreatePassphrase $ leftmost
--         [ backWF <$ eBack
--         , confirmPhrase selfWF eBack (rootKey, password) mnemonicSentence <$ eContinue
--         ]

-- | UI for mnemonic sentence confirmation: scramble the phrase, make the user
-- choose the words in the right order.
-- confirmPhrase
--   :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m)
--   => SetupWF t m
--   -> Event t ()
--   -> (Crypto.XPrv, Password)
--   -> Crypto.MnemonicSentence 12
--   -- ^ Mnemonic sentence to confirm
--   -> SetupWF t m
-- confirmPhrase backWF eBack (rootKey, password) mnemonicSentence = Workflow $ mdo
--   (continue, done) <- continueForm (fmap not done) $ do
--     el "h1" $ text "Verify Recovery Phrase"
--     el "div" $ do
--       el "div" $ text "Please confirm your recovery phrase by"
--       el "div" $ text "typing the words in the correct order."

--     let actualMap = mkPhraseMapFromMnemonic mnemonicSentence

--     rec
--       onPhraseUpdate <- divClass "setup__verify-passphrase-wrapper" $
--         passphraseWidget dConfirmPhrase (pure Recover) True

--       dConfirmPhrase <- holdDyn (wordsToPhraseMap $ replicate passphraseLen T.empty)
--         $ flip Map.union <$> current dConfirmPhrase <@> onPhraseUpdate

--     pure $ (== actualMap) <$> dConfirmPhrase

--   finishSetupWF WalletScreen_VerifyPassphrase $ leftmost
--     [ doneScreen (rootKey, password) <$ continue
--     , backWF <$ eBack
--     ]

setPassword
  :: (DomBuilder t m, MonadHold t m, MonadFix m, PerformEvent t m, PostBuild t m, 
      MonadSample t (Performable m), MonadJSM (Performable m), TriggerEvent t m)
  => Dynamic t [Text]
  -> m (Event t (Maybe (PrivateKey, Password)))
setPassword dSentence = do
  let uiPassword' = uiPassword (setupClass "password-wrapper") (setupClass "password")

  p1elem <- uiPassword' $ "Enter password (" <> tshow minPasswordLength <> " character min.)"
  p2elem <- uiPassword' "Confirm password"

  p1Dirty <- holdUniqDyn =<< holdDyn False (True <$ _inputElement_input p1elem)
  p2Dirty <- holdUniqDyn =<< holdDyn False (True <$ _inputElement_input p2elem)

  let inputsDirty = current $ liftA2 (||) p1Dirty p2Dirty

  eCheckPassword <- fmap (gate inputsDirty) $ delay 0.2 $ leftmost
    [ _inputElement_input p1elem
    , _inputElement_input p2elem
    ]

  let (err, pass) = fanEither $
        checkPassword <$> current (value p1elem) <*> current (value p2elem) <@ eCheckPassword

  lastError <- holdDyn Nothing $ leftmost
    [ Just <$> err
    , Nothing <$ pass
    ]

  let dMsgClass = lastError <&> \m -> setupClass "password-message " <> case m of
        Nothing -> setupClass "hide-pw-error"
        Just _ -> setupClass "show-pw-error"

  elDynClass "div" dMsgClass $
    dynText $ fromMaybe T.empty <$> lastError

  --Web version doesn't actually encrypt the root
  -- eRoot <- performEvent (updated dSentence) $ (liftJSM . generateRoot . T.unwords)

  resEvent <- performEvent $ ffor pass $ \p -> do
    sentence <- sample $ current dSentence 
    liftIO $ print "HERE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    root <- liftJSM $ generateRoot $ T.unwords sentence
    return $ ffor root $ \r -> (r, Password p)

  pure $ leftmost
    [ Nothing <$ err
    , resEvent
    -- , (\s p -> Just (generateRoot s, Password p)) <$> current dSentence <@> pass
    ]

checkPassword :: Text -> Text -> Either Text Text
checkPassword p1 p2
  | T.length p1 < minPasswordLength =
      Left $ "Passwords must be at least " <> tshow minPasswordLength <> " characters long"
  | p1 /= p2 =
      Left "Passwords must match"
  | otherwise =
      Right p1

-- | Generate a 12 word mnemonic sentence, using cryptonite.
--
-- These values for entropy must be set according to a predefined table:
-- https://github.com/kadena-io/cardano-crypto/blob/master/src/Crypto/Encoding/BIP39.hs#L208-L218
-- genMnemonic :: MonadIO m => m (Either Text (Crypto.MnemonicSentence 12))
-- genMnemonic = liftIO $ bimap tshow Crypto.entropyToWords . Crypto.toEntropy @128
--   -- This size must be a 1/8th the size of the 'toEntropy' size: 128 / 8 = 16
--   <$> Crypto.Random.Entropy.getEntropy @ByteString 16
