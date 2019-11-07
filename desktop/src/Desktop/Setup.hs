{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Wallet setup screens
module Desktop.Setup (runSetup, form, kadenaWalletLogo, setupDiv, setupClass) where

import Control.Lens ((<>~))
import Control.Error (hush)
import Control.Applicative (liftA2)
import Control.Monad (unless,void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Data.Bool (bool)
import Data.Maybe (isNothing, fromMaybe)
import Data.Bifunctor
import Data.ByteArray (ByteArrayAccess)
import Data.ByteString (ByteString)
import Data.String (IsString, fromString)
import Data.Functor ((<&>))
import Data.Text (Text)
import Language.Javascript.JSaddle (MonadJSM)
import Reflex.Dom.Core
import qualified Cardano.Crypto.Wallet as Crypto
import qualified Crypto.Encoding.BIP39 as Crypto
import qualified Crypto.Encoding.BIP39.English as Crypto
import qualified Crypto.Random.Entropy
import qualified Data.ByteArray as BA
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Frontend.UI.Button
import Frontend.UI.Widgets
import Obelisk.Generated.Static

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
  | WalletScreen_CreatePassphrase
  | WalletScreen_VerifyPassphrase
  | WalletScreen_RecoverPassphrase
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

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Form wrapper which will automatically handle submit on enter
form :: DomBuilder t m => Text -> m a -> m a
form c = elAttr "form" ("onsubmit" =: "return false;" <> "class" =: c)

-- | Wallet logo
kadenaWalletLogo :: DomBuilder t m => m ()
kadenaWalletLogo = divClass "logo" $ do
  elAttr "img" ("src" =: static @"img/kadena_blue_logo.png" <> "class" =: setupClass "kadena-logo") blank
  elClass "div" "chainweaver" $ text "Chainweaver"
  elClass "div" "by-kadena" $ text "by Kadena"

type SetupWF t m = Workflow t m (WalletScreen, Event t Crypto.XPrv)

finishSetupWF :: (Reflex t, Applicative m) => WalletScreen -> a -> m ((WalletScreen, Event t x), a)
finishSetupWF ws = pure . (,) (ws, never)

-- Make this take a list of the current progress instead so we can maintain
-- the list of how much we have done so far.
--
walletSetupRecoverHeader
  :: DomBuilder t m
  => WalletScreen
  -> m ()
walletSetupRecoverHeader currentScreen = setupDiv "workflow-header" $ do
  unless (currentScreen `elem` [WalletScreen_RecoverPassphrase, WalletScreen_SplashScreen]) $ do
    elClass "ol" (setupClass "workflow-icons") $ do
      faEl "1" "Password" WalletScreen_Password
      line "pw-recovery" WalletScreen_CreatePassphrase
      faEl "2" "Recovery" WalletScreen_CreatePassphrase
      line "recovery-verify" WalletScreen_VerifyPassphrase
      faEl "3" "Verify" WalletScreen_VerifyPassphrase
      line "verify-done" WalletScreen_Done
      faEl "4" "Done" WalletScreen_Done

  where
    addActive sid =
      if isActive sid then
        ["active"]
      else
        []

    line cls sid =
      elClass "div" (T.unwords $ setupClass "workflow-icon": setupClass "header-line" : cls : addActive sid) blank

    isActive sid = sid `elem` (progress currentScreen)
    progress WalletScreen_Password =
      [WalletScreen_Password]
    progress WalletScreen_CreatePassphrase =
      [WalletScreen_Password, WalletScreen_CreatePassphrase]
    progress WalletScreen_VerifyPassphrase =
      [WalletScreen_Password, WalletScreen_CreatePassphrase, WalletScreen_VerifyPassphrase]
    progress WalletScreen_Done =
      [WalletScreen_Password, WalletScreen_CreatePassphrase, WalletScreen_VerifyPassphrase, WalletScreen_Done]
    progress _ = []

    faEl n lbl sid =
      elClass "li" (setupClass "workflow-icon" <> if isActive sid then " active" else T.empty) $ do
        elClass "div" (setupClass "workflow-icon-circle" <> " " <> setupClass ("workflow-screen-" <> T.toLower lbl)) $
          setupDiv "workflow-icon-inner" $
          if isActive sid then
            elClass "i" ("fa fa-check fa-lg fa-inverse " <> setupClass "workflow-icon-active") blank
          else
            text n
        text lbl

runSetup
  :: forall t m. (DomBuilder t m, MonadFix m, MonadHold t m, MonadIO m, PerformEvent t m, PostBuild t m, MonadJSM (Performable m), TriggerEvent t m)
  => m (Event t Crypto.XPrv)
runSetup = setupDiv "fullscreen" $ mdo
  let dCurrentScreen = fst <$> dwf

  eBack <- fmap (domEvent Click . fst) $ elDynClass "div" ((setupClass "back " <>) . hideBack <$> dCurrentScreen) $
    el' "span" $ do
      elClass "i" "fa fa-fw fa-chevron-left" $ blank
      text "Back"

  _ <- dyn_ $ walletSetupRecoverHeader <$> dCurrentScreen

  dwf <- divClass "wrapper" $
    workflow (splashScreen eBack)

  pure $ switchDyn $ snd <$> dwf
  where
    hideBack ws
      | ws `elem` [WalletScreen_SplashScreen, WalletScreen_Done] = setupClass "hide"
      | otherwise = setupScreenClass ws

splashScreen
  :: (DomBuilder t m, MonadFix m, MonadHold t m, MonadIO m, PerformEvent t m, PostBuild t m, MonadJSM (Performable m), TriggerEvent t m)
  => Event t () -> SetupWF t m
splashScreen eBack = Workflow $ setupDiv "splash" $ do
  elAttr "img" ("src" =: static @"img/Wallet_Graphic_1.png" <> "class" =: setupClass "splash-bg") blank
  kadenaWalletLogo

  (agreed, create, recover) <- setupDiv "splash-terms-buttons" $ do
    agreed <- fmap value $ setupCheckbox False def $ setupDiv "terms-conditions-checkbox" $ do
      text "I have read & agree to the "
      elAttr "a" ("href" =: "https://kadena.io/" <> "target" =: "_blank") (text "Terms of Service")

    let dNeedAgree = fmap not agreed

    create <- confirmButton (def & uiButtonCfg_disabled .~ dNeedAgree) "Create a new wallet"
    recover <- uiButtonDyn (btnCfgSecondary & uiButtonCfg_disabled .~ dNeedAgree) $ text "Restore existing wallet"
    pure (agreed, create, recover)

  let hasAgreed = gate (current agreed)

  finishSetupWF WalletScreen_SplashScreen $ leftmost
    [ createNewWallet eBack <$ hasAgreed create
    , recoverWallet eBack <$ hasAgreed recover
    ]

data BIP39PhraseError
  = BIP39PhraseError_Dictionary Crypto.DictionaryError
  | BIP39PhraseError_MnemonicWordsErr Crypto.MnemonicWordsError
  | BIP39PhraseError_InvalidPhrase
  | BIP39PhraseError_PhraseIncomplete

passphraseLen :: Int
passphraseLen = 12

sentenceToSeed :: Crypto.ValidMnemonicSentence mw => Crypto.MnemonicSentence mw -> Crypto.Seed
sentenceToSeed s = Crypto.sentenceToSeed s Crypto.english ""

recoverWallet
  :: (DomBuilder t m, MonadFix m, MonadHold t m, MonadIO m, PerformEvent t m, PostBuild t m, MonadJSM (Performable m), TriggerEvent t m)
  => Event t () -> SetupWF t m
recoverWallet eBack = Workflow $ do
  el "h1" $ text "Recover your wallet"

  setupDiv "recovery-text" $ do
    el "div" $ text "Enter your 12 word recovery phrase"
    el "div" $ text "to restore your wallet."

  rec
    phraseMap <- holdDyn (wordsToPhraseMap $ replicate passphraseLen T.empty)
      $ flip Map.union <$> current phraseMap <@> onPhraseMapUpdate

    onPhraseMapUpdate <- setupDiv "recover-widget-wrapper" $
      passphraseWidget phraseMap (pure Recover)

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

      withSeedConfirmPassword seed = setupDiv "recover-enter-password" $ do
        dSetPw <- holdDyn Nothing =<< setPassword (pure seed)
        continue <- setupDiv "recover-restore-button" $
          confirmButton (def & uiButtonCfg_disabled .~ (fmap isNothing dSetPw)) "Restore"
        pure $ tagMaybe (current dSetPw) continue

  dSetPassword <- widgetHold waitingForPhrase $
    withSeedConfirmPassword <$> eSeedUpdated

  pure
    ( (WalletScreen_RecoverPassphrase, switchDyn dSetPassword)
    , splashScreen eBack <$ eBack
    )

passphraseWordElement
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t PassphraseStage
  -> WordKey
  -> Dynamic t Text
  -> m (Event t Text)
passphraseWordElement currentStage k wrd = setupDiv "passphrase-widget-elem-wrapper" $ do
  pb <- getPostBuild

  setupDiv "passphrase-widget-key-wrapper" $
    text (showWordKey k)

  let
    commonAttrs cls =
      "type" =: "text" <>
      "size" =: "8" <>
      "class" =: setupClass cls

  void . uiInputElement $ def
    & inputElementConfig_initialValue .~ "********"
    & initialAttributes .~ (commonAttrs "passphrase-widget-word-hider" <> "disabled" =: "true" <> "tabindex" =: "-1")

  fmap _inputElement_input $ setupDiv "passphrase-widget-word-wrapper". uiInputElement $ def
    & inputElementConfig_setValue .~ (current wrd <@ pb)
    & initialAttributes .~ commonAttrs "passphrase-widget-word"
    & modifyAttributes <>~ (("readonly" =:) . canEditOnRecover <$> current currentStage <@ pb)
  where
    canEditOnRecover Recover = Nothing
    canEditOnRecover Setup = Just "true"

passphraseWidget
  :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m)
  => Dynamic t (Map.Map WordKey Text)
  -> Dynamic t PassphraseStage
  -> m (Event t (Map.Map WordKey Text))
passphraseWidget dWords dStage = do
  setupDiv "passphrase-widget-wrapper" $
    listViewWithKey dWords (passphraseWordElement dStage)

continueButton
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t Bool
  -> m (Event t ())
continueButton isDisabled =
  setupDiv "continue-button" $
    confirmButton (def & uiButtonCfg_disabled .~ isDisabled) "Continue"

createNewWallet
  :: forall t m. (DomBuilder t m, MonadFix m, MonadHold t m, MonadIO m, PerformEvent t m, PostBuild t m, MonadJSM (Performable m), TriggerEvent t m)
  => Event t () -> SetupWF t m
createNewWallet eBack = Workflow $  do
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

    proceed :: Crypto.MnemonicSentence 12 -> m (Event t (SetupWF t m))
    proceed mnem = do
      dPassword <- setPassword (pure $ sentenceToSeed mnem) >>= holdDyn Nothing
      continue <- continueButton (fmap isNothing dPassword)
      pure $ precreatePassphraseWarning eBack dPassword mnem <$ continue

  dContinue <- widgetHold generating (proceed <$> eGenSuccess)

  finishSetupWF WalletScreen_Password $ leftmost
    [ splashScreen eBack <$ eBack
    , switchDyn dContinue
    ]

walletSplashWithIcon :: DomBuilder t m => m ()
walletSplashWithIcon = do
  elAttr "img" (
    "src" =: static @"img/Wallet_Graphic_1.png" <>
    "class" =: (setupClass "splash-bg " <> setupClass "done-splash-bg")
    ) blank

  elAttr "img" (
    "src" =: static @"img/Wallet_Icon_Highlighted_Blue.png" <>
    "class" =: setupClass "wallet-blue-icon"
    ) blank

stackFaIcon :: DomBuilder t m => Text -> m ()
stackFaIcon icon = elClass "span" "fa-stack fa-lg" $ do
  elClass "i" "fa fa-circle fa-stack-2x" blank
  elClass "i" ("fa " <> icon <> " fa-stack-1x fa-inverse") blank

precreatePassphraseWarning
  :: (DomBuilder t m, MonadFix m, MonadHold t m, MonadIO m, PerformEvent t m, PostBuild t m, MonadJSM (Performable m), TriggerEvent t m)
  => Event t ()
  -> Dynamic t (Maybe Crypto.XPrv)
  -> Crypto.MnemonicSentence 12
  -> SetupWF t m
precreatePassphraseWarning eBack dPassword mnemonicSentence = Workflow $ do
  setupDiv "warning-splash" $ do
    setupDiv "repeat-icon" $ stackFaIcon "fa-repeat"
    walletSplashWithIcon

  el "h1" $ text "Wallet Recovery Phrase"

  setupDiv "recovery-phrase-warning" $ do
    line "In the next step you will record your 12 word recovery phrase."
    line "Your recovery phrase makes it easy to restore your wallet on a new device."
    line "Anyone with this phrase can take control your wallet, keep this phrase private."

  setupDiv "recovery-phrase-highlighted-warning" $
    line "Kadena cannot access your recovery phrase if lost, please store it safely."

  let chkboxcls = setupClass "warning-checkbox " <> setupClass "checkbox-wrapper"
  dUnderstand <- fmap value $ elClass "div" chkboxcls $ setupCheckbox False def
    $ text "I understand that if I lose my recovery phrase, I will not be able to restore my wallet."

  eContinue <- continueButton (fmap not dUnderstand)

  finishSetupWF WalletScreen_Password $ leftmost
    [ createNewWallet eBack <$ eBack
    , createNewPassphrase eBack dPassword mnemonicSentence <$ eContinue
    ]
  where
    line = el "div" . text

doneScreen
  :: (DomBuilder t m, PostBuild t m)
  => Crypto.XPrv
  -> SetupWF t m
doneScreen passwd = Workflow $ do
  walletSplashWithIcon

  el "h1" $ text "Wallet Created"

  eContinue <- setupDiv "continue-button" $
    confirmButton def "Complete"

  pure ( (WalletScreen_Done, passwd <$ eContinue)
       , never
       )

-- | UI for generating and displaying a new mnemonic sentence.
createNewPassphrase
  :: (DomBuilder t m, MonadFix m, MonadHold t m, MonadIO m, PerformEvent t m, PostBuild t m, MonadJSM (Performable m), TriggerEvent t m)
  => Event t ()
  -> Dynamic t (Maybe Crypto.XPrv)
  -> Crypto.MnemonicSentence 12
  -> SetupWF t m
createNewPassphrase eBack dPassword mnemonicSentence = Workflow $ do
  el "h1" $ text "Record Recovery Phrase"
  setupDiv "record-phrase-msg" $ do
    el "div" $ text "Write down or copy these words in the correct order and store them safely."
    el "div" $ text "The recovery words are hidden for security. Mouseover the numbers to reveal."

  rec
    dPassphrase <- passphraseWidget dPassphrase (pure Setup)
      >>= holdDyn (mkPhraseMapFromMnemonic mnemonicSentence)

    eCopyClick <- elClass "div" (setupClass "recovery-phrase-copy") $ do
      uiButton def $ elClass "span" (setupClass "recovery-phrase-copy-word") $ do
        imgWithAlt (static @"img/copy.svg") "Copy" blank
        text "Copy"
        elDynClass "i" ("fa setup__copy-status " <> dCopySuccess) blank

    eCopySuccess <- copyToClipboard $
      T.unwords . Map.elems <$> current dPassphrase <@ eCopyClick

    dCopySuccess <- holdDyn T.empty $
      (setupClass . bool "copy-fail fa-times" "copy-success fa-check") <$> eCopySuccess

  dIsStored <- fmap value $ setupDiv "checkbox-wrapper" $ setupCheckbox False def
    $ text "I have safely stored my recovery phrase."

  eContinue <- continueButton (fmap not dIsStored)

  finishSetupWF WalletScreen_CreatePassphrase $ leftmost
    [ createNewWallet eBack <$ eBack
    , confirmPhrase eBack dPassword mnemonicSentence <$ eContinue
    ]

-- | UI for mnemonic sentence confirmation: scramble the phrase, make the user
-- choose the words in the right order.
confirmPhrase
  :: (DomBuilder t m, MonadFix m, MonadHold t m, MonadIO m, PerformEvent t m, PostBuild t m, MonadJSM (Performable m), TriggerEvent t m)
  => Event t ()
  -> Dynamic t (Maybe Crypto.XPrv)
  -> Crypto.MnemonicSentence 12
  -- ^ Mnemonic sentence to confirm
  -> SetupWF t m
confirmPhrase eBack dPassword mnemonicSentence = Workflow $ do
  el "h1" $ text "Verify Recovery Phrase"
  setupDiv "verify-phrase-msg" $ do
    el "div" $ text "Please confirm your recovery phrase by"
    el "div" $ text "typing the words in the correct order."

  let actualMap = mkPhraseMapFromMnemonic mnemonicSentence

  rec
    onPhraseUpdate <- setupDiv "verify-widget-wrapper" $
      passphraseWidget dConfirmPhrase (pure Recover)

    dConfirmPhrase <- holdDyn (wordsToPhraseMap $ replicate passphraseLen T.empty)
      $ flip Map.union <$> current dConfirmPhrase <@> onPhraseUpdate

  let done = (== actualMap) <$> dConfirmPhrase

  -- TODO: Remove me before release, I'm a dev hack
  skip <- uiButton btnCfgTertiary $ text "Skip"

  continue <- continueButton (fmap not done)

  finishSetupWF WalletScreen_VerifyPassphrase $ leftmost
    [ doneScreen <$> tagMaybe (current dPassword) (leftmost [continue, skip])
    , createNewWallet eBack <$ eBack
    ]

setPassword
  :: (DomBuilder t m, MonadHold t m, MonadFix m, PerformEvent t m, PostBuild t m, MonadJSM (Performable m), TriggerEvent t m)
  => Dynamic t Crypto.Seed
  -> m (Event t (Maybe Crypto.XPrv))
setPassword dSeed = form "" $ do
  let uiPassword ph = elClass "span" (setupClass "password-wrapper") $
        uiInputElement $ def & initialAttributes .~
        ( "type" =: "password" <>
          "placeholder" =: ph <>
          "class" =: setupClass "password"
        )

  p1elem <- uiPassword $ "Enter password (" <> tshow minPasswordLength <> " character min.)"
  p2elem <- uiPassword "Confirm password"

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
    , (\s -> Just . Crypto.generate s) <$> current dSeed <@> (T.encodeUtf8 <$> pass)
    ]

  where
    minPasswordLength = 10
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
