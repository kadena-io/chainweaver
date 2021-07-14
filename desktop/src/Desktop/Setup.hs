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
module Desktop.Setup (runSetup, checkPassword) where

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
import qualified Cardano.Crypto.Wallet as Crypto
import qualified Crypto.Encoding.BIP39 as Crypto
import qualified Crypto.Encoding.BIP39.English as Crypto
import qualified Crypto.Random.Entropy
import qualified Data.ByteArray as BA
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.FilePath (takeFileName)

import Frontend.AppCfg (FileFFI(..), FileType(FileType_Import))
import Desktop.ImportExport (doImport, ImportWalletError(..))
import Pact.Server.ApiClient (HasTransactionLogger, askTransactionLogger)
import Frontend.Storage.Class (HasStorage)
import Frontend.UI.Button
import Frontend.UI.Dialogs.ChangePassword (minPasswordLength)
import Frontend.UI.Widgets.Helpers (imgWithAlt)
import Frontend.UI.Widgets
import Frontend.Setup.Widgets
import Frontend.Setup.Common
import Frontend.Crypto.Password
import Obelisk.Generated.Static

mkPhraseMapFromMnemonic
  :: forall mw.
     Crypto.ValidMnemonicSentence mw
  => Crypto.MnemonicSentence mw
  -> Map.Map WordKey Text
mkPhraseMapFromMnemonic = wordsToPhraseMap . T.words . baToText
  . Crypto.mnemonicSentenceToString @mw Crypto.english

showWordKey :: WordKey -> Text
showWordKey = T.pack . show . _unWordKey

-- | Convenience function for unpacking byte array things into 'Text'
baToText :: ByteArrayAccess b => b -> Text
baToText = T.decodeUtf8 . BA.pack . BA.unpack

textTo :: IsString a => Text -> a
textTo = fromString . T.unpack

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
    , HasTransactionLogger m
    )
  => FileFFI t m
  -> Bool
  -> WalletExists
  -> m (Event t (Either () (Crypto.XPrv, Password, Bool)))
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

splashScreen
  :: (DomBuilder t m, MonadFix m, MonadHold t m, PerformEvent t m
     , PostBuild t m, MonadJSM (Performable m), TriggerEvent t m, HasStorage (Performable m)
     , MonadSample t (Performable m)
     , HasTransactionLogger m
     )
  => WalletExists
  -> FileFFI t m
  -> Event t ()
  -> SetupWF Crypto.XPrv t m
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
          [ createNewWallet selfWF eBack <$ hasAgreed create
          , restoreBipWallet selfWF eBack <$ hasAgreed restoreBipPhrase
          , restoreFromImport walletExists fileFFI selfWF eBack <$ hasAgreed restoreImport
          ]

data BIP39PhraseError
  = BIP39PhraseError_Dictionary Crypto.DictionaryError
  | BIP39PhraseError_MnemonicWordsErr Crypto.MnemonicWordsError
  | BIP39PhraseError_InvalidPhrase
  | BIP39PhraseError_PhraseIncomplete

sentenceToSeed :: Crypto.ValidMnemonicSentence mw => Crypto.MnemonicSentence mw -> Crypto.Seed
sentenceToSeed s = Crypto.sentenceToSeed s Crypto.english ""

restoreBipWallet
  :: (DomBuilder t m, MonadFix m, MonadHold t m, PerformEvent t m, PostBuild t m, MonadJSM (Performable m), TriggerEvent t m)
  => SetupWF Crypto.XPrv t m -> Event t () -> SetupWF Crypto.XPrv t m
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
        phrase <- first BIP39PhraseError_MnemonicWordsErr . Crypto.mnemonicPhrase @12 $ textTo <$> Map.elems pm
        unless (Crypto.checkMnemonicPhrase Crypto.english phrase) $ Left BIP39PhraseError_InvalidPhrase
        first BIP39PhraseError_Dictionary $ Crypto.mnemonicPhraseToMnemonicSentence Crypto.english phrase
        else Left BIP39PhraseError_PhraseIncomplete

  dyn_ $ ffor sentenceOrError $ \case
    Right _ -> pure ()
    Left BIP39PhraseError_PhraseIncomplete -> pure ()
    Left e -> setupDiv "phrase-error-message-wrapper" $ setupDiv "phrase-error-message" $ text $ case e of
      BIP39PhraseError_MnemonicWordsErr (Crypto.ErrWrongNumberOfWords actual expected)
        -> "Wrong number of words: expected " <> tshow expected <> ", but got " <> tshow actual
      BIP39PhraseError_InvalidPhrase
        -> "Invalid phrase"
      BIP39PhraseError_Dictionary (Crypto.ErrInvalidDictionaryWord word)
        -> "Invalid word in phrase: " <> baToText word
      BIP39PhraseError_PhraseIncomplete
        -> mempty

  let eSeedUpdated = fmapMaybe (hush . fmap sentenceToSeed) (updated sentenceOrError)

      waitingForPhrase = setupDiv "waiting-passphrase" $ do
        text "Waiting for a valid 12 word passphrase..."
        pure never

      withSeedConfirmPassword seed = setupDiv "recover-enter-password" $ mdo
        (ePwSubmit, dSetPw) <- restoreForm (fmap isNothing dSetPw) $ do
          holdDyn Nothing =<< setPassword (pure seed)

        pure $ tagMaybe (current dSetPw) ePwSubmit

  dSetPassword <- widgetHold waitingForPhrase $
    withSeedConfirmPassword <$> eSeedUpdated

  pure
    ( (WalletScreen_RecoverPassphrase, (\(prv, pw) -> (prv, pw, True)) <$> switchDyn dSetPassword, never)
    , backWF <$ eBack
    )

restoreFromImport
  :: forall t m
  .  ( DomBuilder t m, MonadFix m, MonadHold t m, PerformEvent t m
     , PostBuild t m, MonadJSM (Performable m), HasStorage (Performable m)
     , MonadSample t (Performable m)
     , HasTransactionLogger m
     )
  => WalletExists
  -> FileFFI t m
  -> SetupWF Crypto.XPrv t m
  -> Event t ()
  -> SetupWF Crypto.XPrv t m
restoreFromImport walletExists fileFFI backWF eBack = nagScreen
  where
    nagMsgs = case walletExists of
      WalletExists_Yes ->
        ("You are about to replace the current wallet's data"
        ,"Reminder: Importing a wallet file will replace the data within the current wallet."
        )
      WalletExists_No ->
        ("Please select the wallet import file."
        ,"Reminder: You will need your wallet password to proceed."
        )

    nagBack = case walletExists of
      WalletExists_No -> pure never
      WalletExists_Yes -> uiButtonDyn
        -- TODO: Don't reuse this class or at least rename it
        (btnCfgSecondary & uiButtonCfg_class <>~ "setup__restore-existing-button")
        (text "Go back and export current wallet")

    nagScreen = Workflow $ setupDiv "splash" $ do
      splashLogo
      let (nagTitle, nagReminder) = nagMsgs
      elClass "h1" "setup__recover-import-title" $ text nagTitle
      elClass "p" "setup__recover-import-text" $ text nagReminder
      eImport <- confirmButton def "Select Import File"
      eExit <- nagBack
      pure
        ( (WalletScreen_RecoverImport, never, eExit)
        , leftmost
          [ backWF <$ (eBack <> eExit)
          , importScreen <$ eImport
          ]
        )

    importScreen = Workflow $ setupDiv "splash" $ mdo
      splashLogo
      elClass "h1" "setup__recover-import-title" $ text "Import File Password"
      elClass "p" "setup__recover-import-text" $ text "Enter the password for the chosen wallet file in order to authorize access to the data."

      let disabled = isNothing <$> dmValidForm
      dErr <- holdDyn Nothing (leftmost [Just <$> eImportErr, Nothing <$ updated dmValidForm])
      (eSubmit, (dFileSelected, pwInput)) <- setupForm "" "Import File" disabled $ mdo
        ePb <- getPostBuild
        (selectElt, _) <- elClass' "div" "setup__recover-import-file" $ do
          imgWithAlt (static @"img/import.svg") "Import" blank
          divClass "setup__recover-import-file-text" $ dynText $ ffor dFileSelected $
            maybe "Select a file" (T.pack . takeFileName . fst)

        performEvent_ $ liftJSM (_fileFFI_openFileDialog fileFFI FileType_Import) <$
          ((domEvent Click selectElt) <> ePb)

        dFileSelected <- holdDyn Nothing (Just <$> _fileFFI_externalFileOpened fileFFI)

        pw <- uiPassword (setupClass "password-wrapper") (setupClass "password") "Enter import wallet password"

        dyn_ $ ffor dErr $ traverse_ $ \err ->
          elClass "p" "error_inline" $ text $ case err of
            ImportWalletError_InvalidCommandLogDestination -> "Destination for transaction log file is invalid"
            ImportWalletError_CommandLogWriteError -> "Unable to write transaction log file"
            ImportWalletError_PasswordIncorrect -> "Incorrect Password"
            ImportWalletError_NoRootKey -> "Backup cannot be restored as it does not contain a BIP Root Key"
            ImportWalletError_NotJson eMsg -> "Backup cannot be restored as it is not a valid json file. Error: " <> eMsg
            ImportWalletError_DecodeError section ver eMsg ->
              "Backup section " <> section <> " cannot be parsed as version " <> tshow ver  <>  " with error: " <> eMsg
            ImportWalletError_UnknownVersion section ver ->
              "Backup section " <> section <> " has an unknown version " <> tshow ver <> ". It's likely that this backup is from a newer version of chainweaver."


        pure (dFileSelected, pw)

      eExit <- nagBack
      let dmValidForm = runMaybeT $ (,)
            <$> MaybeT (nonEmptyPassword <$> (_inputElement_value pwInput))
            <*> MaybeT (fmap snd <$> dFileSelected)

      txLogger <- askTransactionLogger
      eImport <- performEvent $ tagMaybe (fmap (uncurry (doImport @t txLogger)) <$> current dmValidForm) eSubmit

      let (eImportErr, eImportDone) = fanEither eImport

      pure
        ( (WalletScreen_RecoverImport, (\(prv,pw) -> (prv, pw, False)) <$> eImportDone, eExit)
        , backWF <$ (eBack <> eExit)
        )

    nonEmptyPassword "" = Nothing
    nonEmptyPassword pw = Just (Password pw)

createNewWallet
  :: forall t m. (DomBuilder t m, MonadFix m, MonadHold t m, PerformEvent t m, PostBuild t m, MonadJSM (Performable m), TriggerEvent t m)
  => SetupWF Crypto.XPrv t m -> Event t () -> SetupWF Crypto.XPrv t m
createNewWallet backWF eBack = selfWF
  where
    selfWF = Workflow $  do
      ePb <- getPostBuild
      elAttr "img" ("src" =: static @"img/Wallet_Graphic_2.png" <> "class" =: setupClass "password-bg") blank

      el "h1" $ text "Set a password"
      setupDiv "new-wallet-password-text" $ do
        el "div" $ text "Enter a strong and unique password"
        el "div" $ text "to protect access to your Chainweaver wallet"

      (eGenError, eGenSuccess) <- fmap fanEither . performEvent $ genMnemonic <$ ePb

      let
        generating = do
          dynText =<< holdDyn "Generating your mnemonic..." eGenError
          pure never

        proceed :: Crypto.MnemonicSentence 12 -> m (Event t (SetupWF Crypto.XPrv t m))
        proceed mnem = mdo
          (ePwSubmit, dPassword) <- continueForm (fmap isNothing dPassword) $ do
            holdDyn Nothing =<< setPassword (pure $ sentenceToSeed mnem)

          fmap snd $ runWithReplace blank $ ffor (tagMaybe (current dPassword) ePwSubmit) $ \p ->
            pure $ precreatePassphraseWarning selfWF eBack p mnem

      dContinue <- widgetHold generating (proceed <$> eGenSuccess)

      finishSetupWF WalletScreen_Password $ leftmost
        [ backWF <$ eBack
        , switchDyn dContinue
        ]

precreatePassphraseWarning
  :: (DomBuilder t m, MonadFix m, MonadHold t m, PerformEvent t m, PostBuild t m, MonadJSM (Performable m))
  => SetupWF Crypto.XPrv t m
  -> Event t ()
  -> (Crypto.XPrv, Password)
  -> Crypto.MnemonicSentence 12
  -> SetupWF Crypto.XPrv t m
precreatePassphraseWarning backWF eBack (rootKey, password) mnemonicSentence = Workflow $ mdo
  (eContinue, dUnderstand) <- continueForm (fmap not dUnderstand) $ do

    setupDiv "warning-splash" $ do
      walletSplashWithIcon $ do
        setupDiv "repeat-icon" $ stackFaIcon "fa-repeat"

    el "h1" $ text "Wallet Recovery Phrase"

    setupDiv "recovery-phrase-warning" $ do
      line "In the next step you will record your 12 word recovery phrase."
      line "Your recovery phrase makes it easy to restore your wallet on a new device."
      line "Anyone with this phrase can take control of your wallet, keep this phrase private."
      line "Kadena cannot access your recovery phrase if lost, please store it safely."

    let chkboxcls = setupClass "checkbox-wrapper"
    fmap value $ elClass "div" chkboxcls $ setupCheckbox False def $ el "div" $ do
      line "I understand that if I lose my recovery phrase,"
      line "I will not be able to restore my wallet."

  finishSetupWF WalletScreen_PrePassphrase $ leftmost
    [ backWF <$ eBack
    , createNewPassphrase backWF eBack (rootKey, password) mnemonicSentence <$ eContinue
    ]
  where
    line = el "div" . text

-- | UI for generating and displaying a new mnemonic sentence.
createNewPassphrase
  :: (DomBuilder t m, MonadFix m, MonadHold t m, PerformEvent t m, PostBuild t m, MonadJSM (Performable m))
  => SetupWF Crypto.XPrv t m
  -> Event t ()
  -> (Crypto.XPrv, Password)
  -> Crypto.MnemonicSentence 12
  -> SetupWF Crypto.XPrv t m
createNewPassphrase backWF eBack (rootKey, password) mnemonicSentence = selfWF
  where
    selfWF = Workflow $ mdo
      (eContinue, dIsStored) <- continueForm (fmap not dIsStored) $ do
        el "h1" $ text "Record Recovery Phrase"
        el "div" $ do
          el "div" $ text "Write down or copy these words in the correct order and store them safely."
          el "div" $ text "The recovery words are hidden for security. Mouseover the numbers to reveal."

        rec
          dPassphrase <- passphraseWidget dPassphrase (pure Setup) False
            >>= holdDyn (mkPhraseMapFromMnemonic mnemonicSentence)

          let cfg = def
                & uiButtonCfg_class .~ "setup__recovery-phrase-copy"
                & uiButtonCfg_title .~ pure (Just "Copy")
          copyButton cfg True $
            T.unwords . Map.elems <$> current dPassphrase

        fmap value $ setupDiv "checkbox-wrapper" $ setupCheckbox False def
          $ text "I have safely stored my recovery phrase."

      finishSetupWF WalletScreen_CreatePassphrase $ leftmost
        [ backWF <$ eBack
        , confirmPhrase selfWF eBack (rootKey, password) mnemonicSentence <$ eContinue
        ]

-- | UI for mnemonic sentence confirmation: scramble the phrase, make the user
-- choose the words in the right order.
confirmPhrase
  :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m)
  => SetupWF Crypto.XPrv t m
  -> Event t ()
  -> (Crypto.XPrv, Password)
  -> Crypto.MnemonicSentence 12
  -- ^ Mnemonic sentence to confirm
  -> SetupWF Crypto.XPrv t m
confirmPhrase backWF eBack (rootKey, password) mnemonicSentence = Workflow $ mdo
  (continue, done) <- continueForm (fmap not done) $ do
    el "h1" $ text "Verify Recovery Phrase"
    el "div" $ do
      el "div" $ text "Please confirm your recovery phrase by"
      el "div" $ text "typing the words in the correct order."

    let actualMap = mkPhraseMapFromMnemonic mnemonicSentence

    rec
      onPhraseUpdate <- divClass "setup__verify-passphrase-wrapper" $
        passphraseWidget dConfirmPhrase (pure Recover) True

      dConfirmPhrase <- holdDyn (wordsToPhraseMap $ replicate passphraseLen T.empty)
        $ flip Map.union <$> current dConfirmPhrase <@> onPhraseUpdate

    pure $ (== actualMap) <$> dConfirmPhrase

  finishSetupWF WalletScreen_VerifyPassphrase $ leftmost
    [ doneScreen (rootKey, password) <$ continue
    , backWF <$ eBack
    ]

setPassword
  :: (DomBuilder t m, MonadHold t m, MonadFix m, PerformEvent t m, PostBuild t m, MonadJSM (Performable m), TriggerEvent t m)
  => Dynamic t Crypto.Seed
  -> m (Event t (Maybe (Crypto.XPrv, Password)))
setPassword dSeed = do
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

  pure $ leftmost
    [ Nothing <$ err
    , (\s p -> Just (Crypto.generate s (T.encodeUtf8 p), Password p)) <$> current dSeed <@> pass
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
genMnemonic :: MonadIO m => m (Either Text (Crypto.MnemonicSentence 12))
genMnemonic = liftIO $ bimap tshow Crypto.entropyToWords . Crypto.toEntropy @128
  -- This size must be a 1/8th the size of the 'toEntropy' size: 128 / 8 = 16
  <$> Crypto.Random.Entropy.getEntropy @ByteString 16
