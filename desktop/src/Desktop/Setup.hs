{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Wallet setup screens
module Desktop.Setup (runSetup, form, kadenaWalletLogo) where

import Control.Error (hush)
import Control.Applicative (liftA2)
import Control.Lens ((<>~), (?~))
import Control.Monad (unless)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Data.Maybe (isNothing)
import Data.Bifunctor
import Data.ByteArray (ByteArrayAccess)
import Data.ByteString (ByteString)
import Data.String (fromString)
import Data.Functor ((<&>))
import Data.Text (Text)
import Reflex.Dom.Core
import qualified Cardano.Crypto.Wallet as Crypto
import qualified Crypto.Encoding.BIP39 as Crypto
import qualified Crypto.Encoding.BIP39.English as Crypto
import qualified Crypto.Random.Entropy
import qualified Data.ByteArray as BA
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Frontend.UI.Button
import Frontend.UI.Widgets
import Obelisk.Generated.Static

-- | Convenience function for unpacking byte array things into 'Text'
baToText :: ByteArrayAccess b => b -> Text
baToText = T.decodeUtf8 . BA.pack . BA.unpack

-- | Form wrapper which will automatically handle submit on enter
form :: DomBuilder t m => Text -> m a -> m a
form c = elAttr "form" ("onsubmit" =: "return false;" <> "class" =: c)

-- | Wallet logo
kadenaWalletLogo :: DomBuilder t m => m ()
kadenaWalletLogo = divClass "logo" $ do
  elAttr "img" ("src" =: static @"img/Klogo.png") blank
  --el "span" $ text "Wallet"

-- | Display a list of items, returning tagged events
switchList :: (Adjustable t m, MonadHold t m, PostBuild t m, MonadFix m) => Dynamic t [a] -> (Dynamic t a -> m (Event t ())) -> m (Event t a)
switchList l m = fmap (switchDyn . fmap leftmost) . simpleList l $ \a -> tag (current a) <$> m a

type SetupWF t m = Workflow t m (Event t Crypto.XPrv)

finishSetupWF :: (Reflex t, Applicative m) => a -> m (Event t x, a)
finishSetupWF = pure . (,) never

runSetup :: MonadWidget t m => m (Event t Crypto.XPrv)
runSetup = divClass "fullscreen" $ divClass "wrapper" $ do
  kadenaWalletLogo
  fmap (switch . current) $ workflow splashScreen

splashScreen :: MonadWidget t m => SetupWF t m
splashScreen = Workflow $ do
  el "h1" $ text "Welcome to the Kadena Wallet"
  create <- confirmButton def "Setup a new wallet"
  recover <- uiButton btnCfgSecondary $ text "Recover an existing wallet"
  finishSetupWF $ leftmost
    [ createNewWallet Nothing (pure Nothing) <$ create
    , recoverWallet <$ recover
    ]

data BIP39PhraseError
  = BIP39PhraseError_Dictionary Crypto.DictionaryError
  | BIP39PhraseError_MnemonicWordsErr Crypto.MnemonicWordsError
  | BIP39PhraseError_InvalidPhrase

recoverWallet :: MonadWidget t m => SetupWF t m
recoverWallet = Workflow $ do
  el "h1" $ text "Recover your wallet"
  el "p" $ text "Type in your recovery phrase"
  rawPhrase :: Dynamic t Text <- fmap value $ uiTextAreaElement $ def & initialAttributes .~ "class" =: "wallet-recovery-phrase"
  let sentenceOrError = ffor rawPhrase $ \t -> do
        phrase <- first BIP39PhraseError_MnemonicWordsErr . Crypto.mnemonicPhrase @15 . fmap (fromString . T.unpack) $ T.words t
        unless (Crypto.checkMnemonicPhrase Crypto.english phrase) $ Left BIP39PhraseError_InvalidPhrase
        first BIP39PhraseError_Dictionary $ Crypto.mnemonicPhraseToMnemonicSentence Crypto.english phrase
  passphrase <- divClass "checkbox-wrapper" $ do
    usePassphrase <- fmap value $ uiCheckbox def False def $ text "Use a BIP39 passphrase"
    fmap value $ uiInputElement $ def
      & initialAttributes .~ "type" =: "password" <> "placeholder" =: "BIP39 passphrase" <> "class" =: "hidden passphrase"
      & modifyAttributes .~ ffor (updated usePassphrase)
        (\u -> if u then "class" =: Just "input passphrase" else "class" =: Just "hidden passphrase")
  let mkClass = \case Left _ -> "button_type_secondary"; _ -> "button_type_confirm"
  rec
    dyn_ $ ffor lastError $ \case
      Nothing -> pure ()
      Just e -> divClass "message-wrapper" $ divClass "message" $ text $ case e of
        BIP39PhraseError_MnemonicWordsErr (Crypto.ErrWrongNumberOfWords actual expected)
          -> "Wrong number of words: expected " <> T.pack (show expected) <> ", but got " <> T.pack (show actual)
        BIP39PhraseError_InvalidPhrase -> "Invalid phrase"
        BIP39PhraseError_Dictionary (Crypto.ErrInvalidDictionaryWord word)
          -> "Invalid word in phrase: " <> baToText word
    cancel <- cancelButton def "Cancel"
    next <- uiButtonDyn (def & uiButtonCfg_class .~ fmap mkClass sentenceOrError) $ text "Next"
    let (err, sentence) = fanEither $ current sentenceOrError <@ next
    lastError <- holdDyn Nothing $ Just <$> err

  let toSeed :: Crypto.ValidMnemonicSentence mw => Text -> Crypto.MnemonicSentence mw -> Crypto.Seed
      toSeed p s = Crypto.sentenceToSeed s Crypto.english . fromString $ T.unpack p

  dMSeed <- holdDyn Nothing $ pure <$> attachWith toSeed (current passphrase) sentence

  ePassword <- setPassword dMSeed

  pure
    ( ePassword
    , splashScreen <$ cancel
    )

-- | UI for generating and displaying a new mnemonic sentence.
createNewWallet
  :: MonadWidget t m
  => Maybe (Crypto.MnemonicSentence 15)
  -- ^ Initial mnemonic sentence. If missing, a new one will be generated.
  -> Dynamic t (Maybe Crypto.XPrv)
  -> SetupWF t m
createNewWallet mMnemonic dPassword = Workflow $ do
  el "h1" $ text "Wallet recovery phrase"
  el "p" $ text "Write down your recovery phrase on paper"
  initMnemonic <- maybe genMnemonic (pure . Right) mMnemonic
  rec
    mnemonic <- holdDyn initMnemonic =<< performEvent (genMnemonic <$ regen)
    divClass "group" $ dyn_ $ ffor mnemonic $ \case
      Left e -> text e
      Right sentence -> text $ baToText $ Crypto.mnemonicSentenceToString Crypto.english sentence
    stored <- fmap value $ divClass "checkbox-wrapper" $ uiCheckbox def False def $ text "I have safely stored my recovery phrase."
    regen <- uiButton btnCfgTertiary $ do
      elClass "i" "fa fa-lg fa-refresh" blank
      text " Regenerate"

  ePassword <- setPassword (fmap (fmap (\mn -> Crypto.sentenceToSeed mn Crypto.english "") . hush) mnemonic)
    >>= holdDyn Nothing . fmap pure

  let nextGate = liftA2 (||) (fmap isNothing dPassword) (fmap not stored)

  cancel <- cancelButton def "Cancel"
  next <- confirmButton (def & uiButtonCfg_disabled .~ nextGate) "Next"

  finishSetupWF $ leftmost
    [ splashScreen <$ cancel
    , attachWithMaybe (\e () -> confirmPhrase dPassword <$> hush e) (current mnemonic) next
    ]

-- | UI for mnemonic sentence confirmation: scramble the phrase, make the user
-- choose the words in the right order.
confirmPhrase
  :: MonadWidget t m
  => Dynamic t (Maybe Crypto.XPrv)
  -> Crypto.MnemonicSentence 15
  -- ^ Mnemonic sentence to confirm
  -> SetupWF t m
confirmPhrase dPassword mnemonic = Workflow $ do
  let sentence = T.words $ baToText $ Crypto.mnemonicSentenceToString Crypto.english mnemonic
  el "h1" $ text "Confirm your recovery phrase"
  el "p" $ text "Click the words in the correct order"
  rec
    let initialState = ([], S.fromList sentence)
    -- Maintain an (ordered) list of staged words, and an alphabetical list of unstaged words
    (staged, unstaged) <- fmap splitDynPure $ foldDyn ($) initialState $ mconcat
      [ ffor unstage $ \w -> bimap (L.delete w) (S.insert w)
      , ffor stage $ \w -> bimap (++ [w]) (S.delete w)
      , ffor reset $ \() _ -> initialState
      ]
    let stageAttrs = "class" =: "group dark" <> "style" =: "min-height: 9.2rem"
    (unstage, reset) <- elAttr "div" stageAttrs $ do
      -- Display the staging area
      unstage' <- switchList staged $ uiButton btnCfgPrimary . dynText
      -- Button to reset the staging area, only shown when something is staged
      let hiddenClass = ffor staged $ \s -> if null s then "button_hidden" else def
      reset' <- uiButtonDyn (btnCfgPrimary & uiButtonCfg_class <>~ hiddenClass) $
        elClass "i" "fa fa-lg fa-refresh" blank
      pure (unstage', reset')
    -- Unstaged words are displayed in ascending alphabetical order
    let unstageAttrs = ffor unstaged $ \s -> if null s then mempty else "class" =: "group"
    stage <- elDynAttr "div" unstageAttrs $ switchList (S.toAscList <$> unstaged) $
      uiButton btnCfgTertiary . dynText
  let done = (== sentence) <$> staged
  back <- cancelButton def "Back"
  skip <- uiButton btnCfgTertiary $ text "Skip"
  next <- confirmButton (def & uiButtonCfg_disabled .~ fmap not done) "Next"

  pure
    ( fmapMaybe id $ current dPassword <@ gate (current done) next <> skip
    , createNewWallet (Just mnemonic) dPassword <$ back
    )

setPassword
  :: MonadWidget t m
  => Dynamic t (Maybe Crypto.Seed)
  -> m (Event t Crypto.XPrv)
setPassword dSeed = form "" $ do
  el "p" $ text "This password will protect your wallet. You'll need it when signing transactions."
  p1elem <- uiInputElement $ def
    & initialAttributes .~ "type" =: "password" <> "placeholder" =: "Enter password" <> "class" =: "passphrase"
  p2elem <- uiInputElement $ def
    & initialAttributes .~ "type" =: "password" <> "placeholder" =: "Confirm password" <> "class" =: "passphrase"

  let p1 = current $ value p1elem
      p2 = current $ value p2elem

      inputsNotEmpty = not <$> liftA2 (||) (T.null <$> p1) (T.null <$> p2)

  eCheckPassword <- fmap (gate inputsNotEmpty) $ delay 0.4 $ leftmost
    [ _inputElement_input p1elem
    , _inputElement_input p2elem
    ]

  rec
    dyn_ $ ffor lastError $ \case
      Nothing -> pure ()
      Just e -> divClass "message-wrapper" $ divClass "message" $ text e

    let (err, pass) = fanEither $ attachWith (\p _ -> uncurry checkPassword p) (liftA2 (,) p1 p2) eCheckPassword

    lastError <- holdDyn Nothing $ leftmost
      [ Just <$> err
      , Nothing <$ pass
      ]

  pure $ attachWithMaybe (\ms pw -> flip Crypto.generate pw <$> ms) (current dSeed) (T.encodeUtf8 <$> pass)
  where
    minPasswordLength = 10
    checkPassword p1 p2
      | T.length p1 < minPasswordLength = Left $ "Passwords must be at least " <> T.pack (show minPasswordLength) <> " characters long"
      | p1 /= p2 = Left "Passwords must match"
      | otherwise = Right p1

-- | Generate a 15 word mnemonic sentence, using cryptonite.
genMnemonic :: MonadIO m => m (Either Text (Crypto.MnemonicSentence 15))
genMnemonic = liftIO $ bimap (T.pack . show) Crypto.entropyToWords . Crypto.toEntropy @160
  <$> Crypto.Random.Entropy.getEntropy @ByteString 20

