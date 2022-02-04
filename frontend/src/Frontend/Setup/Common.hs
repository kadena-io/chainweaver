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
module Frontend.Setup.Common where

import Control.Lens ((??), (?~), (<>~))
import Control.Error (hush)
import Control.Monad (unless, void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Data.Bool (bool)
import Data.Foldable (fold)
import Data.Maybe (isNothing, isJust)
import Data.Text (Text)
import Language.Javascript.JSaddle (MonadJSM, liftJSM)
import Reflex.Dom.Core
import qualified Data.Map as Map
import qualified Data.Text as T

import Frontend.UI.Button
import Frontend.UI.Dialogs.ChangePassword (minPasswordLength)
import Frontend.UI.Widgets.Helpers (imgWithAlt)
import Frontend.UI.Widgets
import Obelisk.Generated.Static

import Frontend.Setup.Password
import Frontend.Setup.Widgets
import Frontend.Crypto.Class
import Frontend.Crypto.Password

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

showWordKey :: WordKey -> Text
showWordKey = T.pack . show . _unWordKey

tshow :: Show a => a -> Text
tshow = T.pack . show

type SetupWF bipKey t m = Workflow t m
  ( WalletScreen
  , Event t (bipKey, Password, Bool)
  , Event t () -- ExitSetup Event
  )

finishSetupWF :: (Reflex t, Applicative m) => WalletScreen -> a -> m ((WalletScreen, Event t x, Event t ()), a)
finishSetupWF ws = pure . (,) (ws, never, never)

lockScreen
  :: (DomBuilder t m, PostBuild t m, MonadIO m, MonadFix m, MonadHold t m)
  => Event t (Maybe Password) -> m (Event t (), Dynamic t Text, Event t ())
lockScreen isValid = do
  splashLogo
  el "div" $ mdo
    dValid <- holdDyn True $ leftmost
      [ isJust <$> isValid
      , True <$ _inputElement_input pass
      ]

    let unlock = void $ confirmButton (def & uiButtonCfg_type ?~ "submit") "Unlock"
        cfg = def & elementConfig_initialAttributes .~ ("class" =: setupClass "splash-terms-buttons")
    (eSubmit, pass) <- uiForm cfg unlock $ do
      elDynClass "div"
        (("lock-screen__invalid-password" <>) . bool " lock-screen__invalid-password--invalid" "" <$> dValid)
        (text "Invalid Password")
      uiPassword (setupClass "password-wrapper") (setupClass "password") "Password"

    restore <- setupDiv "button-horizontal-group" $ do
      elAttr "a" ( "class" =: "button button_type_secondary setup__help" <>
                   "href" =: "https://www.kadena.io/chainweaver" <>
                   "target" =: "_blank"
                 ) $ do
        elAttr "img" ("src" =: $(static "img/launch_dark.svg") <> "class" =: "button__text-icon") blank
        text "Help"
      uiButton btnCfgSecondary $ text "Restore"
    pure (restore, value pass, eSubmit)

-- Make this take a list of the current progress instead so we can maintain
-- the list of how much we have done so far.
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

splashLogo :: DomBuilder t m => m ()
splashLogo = do
  elAttr "div"
    (  "style" =: ("background-image: url(" <> $(static "img/Wallet_Graphic_1.png") <> ");")
    <> "class" =: setupClass "splash-bg"
    ) kadenaWalletLogo

splashScreenAgreement :: (DomBuilder t m, PostBuild t m) => m (Dynamic t Bool)
splashScreenAgreement = do
  splashLogo
  setupDiv "splash-terms-buttons" $ do
    agreed <- fmap value $ setupCheckbox False def $ el "div" $ do
      text "I have read & agree to the "
      elAttr "a" ?? (text "Terms of Service") $ mconcat
        [ "href" =: "https://kadena.io/chainweaver-tos"
        , "target" =: "_blank"
        , "class" =: setupClass "terms-conditions-link"
        ]
    return agreed

passphraseLen :: Int
passphraseLen = 12

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

walletSplashWithIcon :: DomBuilder t m => m () -> m ()
walletSplashWithIcon w = do
  elAttr "div" (
    "style" =: ("background-image: url(" <> $(static "img/Wallet_Graphic_1.png") <> ");")
    <> "class" =: (setupClass "splash-bg " <> setupClass "done-splash-bg")
    ) $ do
      w
      elAttr "img" (
        "src" =: $(static "img/Wallet_Icon_Highlighted_Blue.svg") <>
        "class" =: setupClass "wallet-blue-icon"
        ) blank

stackFaIcon :: DomBuilder t m => Text -> m ()
stackFaIcon icon = elClass "span" "fa-stack fa-5x" $ do
  elClass "i" "fa fa-circle fa-stack-2x" blank
  elClass "i" ("fa " <> icon <> " fa-stack-1x fa-inverse") blank

doneScreen
  :: (DomBuilder t m, PostBuild t m, BIP39Root key )
  => (key, Password)
  -> SetupWF key t m
doneScreen (rootKey, passwd) = Workflow $ do
  walletSplashWithIcon blank

  el "h1" $ text "Wallet Created"

  eContinue <- setupDiv "continue-button" $
    confirmButton def "Done"

  pure ( (WalletScreen_Done, (rootKey, passwd, True) <$ eContinue, never)
       , never
       )

createNewWallet
  :: forall t m key mnem. (DomBuilder t m, MonadFix m, MonadHold t m, PerformEvent t m, PostBuild t m,
      MonadJSM (Performable m), MonadSample t (Performable m), TriggerEvent t m,
      DerivableKey key mnem
    )
  => SetupWF key t m -> Event t () -> SetupWF key t m
createNewWallet backWF eBack = selfWF
  where
    selfWF = Workflow $  do
      ePb <- getPostBuild
      elAttr "img" ("src" =: $(static "img/Wallet_Graphic_2.png") <> "class" =: setupClass "password-bg") blank

      el "h1" $ text "Set a password"
      setupDiv "new-wallet-password-text" $ do
        el "div" $ text "Enter a strong and unique password"
        el "div" $ text "to protect access to your Chainweaver wallet"

      (eGenError, eGenSuccess) <- fmap fanEither . performEvent $ liftJSM generateMnemonic <$ ePb

      let
        generating = do
          dynText =<< holdDyn "Generating your mnemonic..." eGenError
          pure never

        proceed mnem = mdo
          (ePwSubmit, dPassword) <- continueForm (fmap isNothing dPassword) $ do
            holdDyn Nothing =<< setPassword (pure mnem)

          fmap snd $ runWithReplace blank $ ffor (tagMaybe (current dPassword) ePwSubmit) $ \p ->
            pure $ precreatePassphraseWarning selfWF eBack p mnem

      dContinue <- widgetHold generating (proceed <$> eGenSuccess)

      finishSetupWF WalletScreen_Password $ leftmost
        [ backWF <$ eBack
        , switchDyn dContinue
        ]

precreatePassphraseWarning
  ::
  ( DomBuilder t m, MonadFix m, MonadHold t m, PerformEvent t m,
    PostBuild t m, MonadJSM (Performable m),
    DerivableKey key mnem
  )
  => SetupWF key t m
  -> Event t ()
  -> (key, Password)
  -> mnem
  -> SetupWF key t m
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
  ::
  (DomBuilder t m, MonadFix m, MonadHold t m, PerformEvent t m,
   PostBuild t m, MonadJSM (Performable m), DerivableKey key mnemonic
  )
  => SetupWF key t m
  -> Event t ()
  -> (key, Password)
  -> mnemonic
  -> SetupWF key t m
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
            >>= holdDyn (wordsToPhraseMap $ mnemonicToText mnemonicSentence)

          let cfg = def
                & uiButtonCfg_class .~ "setup__recovery-phrase-copy"
                & uiButtonCfg_title .~ pure (Just "Copy")
          copyButtonLight cfg True $ fmap (T.unwords . Map.elems) $ current dPassphrase

        fmap value $ setupDiv "checkbox-wrapper" $ setupCheckbox False def
          $ text "I have safely stored my recovery phrase."

      finishSetupWF WalletScreen_CreatePassphrase $ leftmost
        [ backWF <$ eBack
        , confirmPhrase selfWF eBack (rootKey, password) mnemonicSentence <$ eContinue
        ]

-- | UI for mnemonic sentence confirmation: scramble the phrase, make the user
-- choose the words in the right order.
confirmPhrase
  :: (
   DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m,
   DerivableKey key mnemonic
  )
  => SetupWF key t m
  -> Event t ()
  -> (key, Password)
  -> mnemonic
  -- ^ Mnemonic sentence to confirm
  -> SetupWF key t m
confirmPhrase backWF eBack (rootKey, password) mnemonicSentence = Workflow $ mdo
  (continue, done) <- continueForm (fmap not done) $ do
    el "h1" $ text "Verify Recovery Phrase"
    el "div" $ do
      el "div" $ text "Please confirm your recovery phrase by"
      el "div" $ text "typing the words in the correct order."

    let actualMap = wordsToPhraseMap $ mnemonicToText mnemonicSentence

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

restoreBipWallet
  :: (DomBuilder t m, MonadHold t m, MonadFix m, PerformEvent t m, PostBuild t m,
      MonadSample t (Performable m), MonadJSM (Performable m), TriggerEvent t m,
      DerivableKey key mnem
     )
  => SetupWF key t m -> Event t () -> SetupWF key t m
restoreBipWallet backWF eBack = Workflow $ do
  el "h1" $ text "Recover your wallet"
  setupDiv "recovery-text" $ do
    el "div" $ text "Enter your 12 word recovery phrase"
    el "div" $ text "to restore your wallet."

  rec
    dPhraseMap <- holdDyn (wordsToPhraseMap $ replicate passphraseLen T.empty)
      $ flip Map.union <$> current dPhraseMap <@> onPhraseMapUpdate
    onPhraseMapUpdate <- passphraseWidget dPhraseMap (pure Recover) True

  sentenceOrError <- performEvent $ ffor (updated dPhraseMap) $ \pMap ->
    let wList = filter (not . T.null) $ Map.elems pMap
    in toMnemonic wList
  widgetHold_ blank $ ffor sentenceOrError $ \case
    Right _ -> blank
    Left e -> setupDiv "phrase-error-message-wrapper" $ setupDiv "phrase-error-message" $
      text $ displayError e
  let
      ePhraseUpdated = fmapMaybe hush sentenceOrError

      waitingForPhrase = setupDiv "waiting-passphrase" $ do
        text "Waiting for a valid 12 word passphrase..."
        pure never

      -- Launch password recovery
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

mkSidebarLogoutLink :: (TriggerEvent t m, PerformEvent t n, PostBuild t n, DomBuilder t n, MonadIO (Performable n)) => m (Event t (), n ())
mkSidebarLogoutLink = do
  (logout, triggerLogout) <- newTriggerEvent
  pure $ (,) logout $ do
    clk <- uiSidebarIcon (pure False) $(static "img/menu/logout.svg") "Logout"
    performEvent_ $ liftIO . triggerLogout <$> clk

