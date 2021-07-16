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
module Frontend.Setup.Browser (runSetup) where

import Control.Lens ((<>~), (??), (^.), _1, _2, _3)
import Control.Error (hush)
import Control.Monad (unless, guard)
import Control.Monad.Fix (MonadFix)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Language.Javascript.JSaddle (MonadJSM, liftJSM)
import qualified Data.Map as Map
import qualified Data.Text as T
import Reflex.Dom.Core

import Frontend.AppCfg (FileFFI(..))
import Frontend.Storage.Class (HasStorage)
import Frontend.UI.Button
import Obelisk.Generated.Static

import Frontend.Setup.Common
import Frontend.Setup.Password
import Frontend.Setup.Widgets
import Frontend.Crypto.Ed25519 (genMnemonic)
import Frontend.Crypto.Class
import Frontend.Crypto.Password

runSetup
  :: forall t m key
  . ( DomBuilder t m
    , MonadFix m
    , MonadHold t m
    , PerformEvent t m
    , PostBuild t m
    , TriggerEvent t m
    , MonadJSM (Performable m)
    , HasStorage (Performable m)
    , MonadSample t (Performable m)
    , BIP39Root key
    , Sentence key ~ [Text]
    )
  => FileFFI t m
  -> Bool
  -> WalletExists
  -> m (Event t (Either () (key, Password, Bool)))
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
     , MonadSample t (Performable m), BIP39Root key -- (Performable m)
     , Sentence key ~ [Text]
     )
  => WalletExists
  -> FileFFI t m
  -> Event t ()
  -> SetupWF key t m
splashScreen _walletExists fileFFI eBack = selfWF
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

        finishSetupWF WalletScreen_SplashScreen $ leftmost
          [ createNewWallet selfWF eBack <$ hasAgreed create
          , restoreBipWallet selfWF eBack <$ hasAgreed restoreBipPhrase
          ]

data BIP39PhraseError
  = BIP39PhraseError_InvalidPhrase
  | BIP39PhraseError_PhraseIncomplete

restoreBipWallet
  :: (DomBuilder t m, MonadHold t m, MonadFix m, PerformEvent t m, PostBuild t m,
      MonadSample t (Performable m), MonadJSM (Performable m), TriggerEvent t m,
      BIP39Root key -- (Performable m)
      , Sentence key ~ [Text]
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

  -- TODO: Generic "validMnemonic" func here
  let sentenceOrError = ffor dPhraseMap $ \pm -> if enoughWords pm then do
        let phrase = Map.elems pm
        unless (length phrase == passphraseLen) $ Left BIP39PhraseError_PhraseIncomplete
        Right phrase
        else Left BIP39PhraseError_PhraseIncomplete

  displaySentenceErrType sentenceOrError

  -- GEN SEED: Todo: Make sure wrong words returns Nothing
  let
      ePhraseUpdated = fmapMaybe hush $ updated sentenceOrError

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
  where
    enoughWords = (== passphraseLen) . length . filter (not . T.null) . Map.elems
    displaySentenceErrType dSentenceOrError =
    -- Check phrase for errors
      dyn_ $ ffor dSentenceOrError $ \case
        Right _ -> pure ()
        Left e -> setupDiv "phrase-error-message-wrapper" $ setupDiv "phrase-error-message" $ text $ case e of
          BIP39PhraseError_InvalidPhrase
            -> "Invalid phrase"
          BIP39PhraseError_PhraseIncomplete
            -> mempty

createNewWallet
  :: forall t m key. (DomBuilder t m, MonadFix m, MonadHold t m, PerformEvent t m, PostBuild t m,
                  MonadJSM (Performable m), MonadSample t (Performable m), TriggerEvent t m
    , BIP39Root key -- (Performable m)
    , Sentence key ~ [Text]
    )
  => SetupWF key t m -> Event t () -> SetupWF key t m
createNewWallet backWF eBack = selfWF
  where
    selfWF = Workflow $  do
      ePb <- getPostBuild
      elAttr "img" ("src" =: static @"img/Wallet_Graphic_2.png" <> "class" =: setupClass "password-bg") blank

      el "h1" $ text "Set a password"
      setupDiv "new-wallet-password-text" $ do
        el "div" $ text "Enter a strong and unique password"
        el "div" $ text "to protect access to your Chainweaver wallet"

      eGen <- performEvent $ liftJSM genMnemonic <$ ePb

      let
        generating = do
          text "Generating your mnemonic"
          pure never

        -- proceed :: [Text] -> m (Event t (SetupWF key t m))
        proceed mnem = mdo
          (ePwSubmit, dPassword) <- continueForm (fmap isNothing dPassword) $ do
            holdDyn Nothing =<< setPassword (pure mnem)

          fmap snd $ runWithReplace blank $ ffor (tagMaybe (current dPassword) ePwSubmit) $ \p ->
            pure $ precreatePassphraseWarning selfWF eBack p mnem

      dContinue <- widgetHold generating (proceed <$> eGen)

      finishSetupWF WalletScreen_Password $ leftmost
        [ backWF <$ eBack
        , switchDyn dContinue
        ]

--TODO: Make generic version
precreatePassphraseWarning
  ::
  ( DomBuilder t m, MonadFix m, MonadHold t m, PerformEvent t m,
    PostBuild t m, MonadJSM (Performable m), BIP39Root key --(Performable m)
    , Sentence key ~ [Text]
  )
  => SetupWF key t m
  -> Event t ()
  -> (key, Password)
  -> Sentence key
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
   PostBuild t m, MonadJSM (Performable m), BIP39Root key -- (Performable m)
   , Sentence key ~ [Text]
  )
  => SetupWF key t m
  -> Event t ()
  -> (key, Password)
  -- TODO: Wrap Sentece type
  -> Sentence key
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
            >>= holdDyn (wordsToPhraseMap mnemonicSentence)

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
  :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m, BIP39Root key -- (Performable m)
  , Sentence key ~ [Text]
  )
  => SetupWF key t m
  -> Event t ()
  -> (key, Password)
  -> Sentence key
  -- ^ Mnemonic sentence to confirm
  -> SetupWF key t m
confirmPhrase backWF eBack (rootKey, password) mnemonicSentence = Workflow $ mdo
  (continue, done) <- continueForm (fmap not done) $ do
    el "h1" $ text "Verify Recovery Phrase"
    el "div" $ do
      el "div" $ text "Please confirm your recovery phrase by"
      el "div" $ text "typing the words in the correct order."

    let actualMap = wordsToPhraseMap mnemonicSentence

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
