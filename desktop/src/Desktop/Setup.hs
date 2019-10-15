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
import Control.Lens ((<>~), (?~), (%~))
import Control.Monad (unless,void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Data.Proxy (Proxy (..))
import Data.Maybe (isNothing)
import Data.Bifunctor
import Data.ByteArray (ByteArrayAccess)
import Data.ByteString (ByteString)
import Data.String (IsString, fromString)
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

wordsToPhraseMap :: [Text] -> Map.Map WordKey Text
wordsToPhraseMap = Map.fromList . zip [WordKey 1 ..]

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
    [ createNewWallet Nothing <$ create
    , recoverWallet <$ recover
    ]

data BIP39PhraseError
  = BIP39PhraseError_Dictionary Crypto.DictionaryError
  | BIP39PhraseError_MnemonicWordsErr Crypto.MnemonicWordsError
  | BIP39PhraseError_InvalidPhrase

passphraseLen :: Int
passphraseLen = 12

recoverWallet :: MonadWidget t m => SetupWF t m
recoverWallet = Workflow $ do
  el "h1" $ text "Recover your wallet"
  el "p" $ text "Type in your recovery phrase"

  rec 
    phraseMap <- holdDyn (wordsToPhraseMap $ replicate passphraseLen T.empty)
      $ flip Map.union <$> current phraseMap <@> onPhraseMapUpdate

    onPhraseMapUpdate <- passphraseWidget phraseMap (pure Recover)

  let sentenceOrError = ffor phraseMap $ \pm -> do
        phrase <- first BIP39PhraseError_MnemonicWordsErr
          . Crypto.mnemonicPhrase @12
          $ textTo <$> Map.elems pm

        unless (Crypto.checkMnemonicPhrase Crypto.english phrase) $ Left BIP39PhraseError_InvalidPhrase
        first BIP39PhraseError_Dictionary $ Crypto.mnemonicPhraseToMnemonicSentence Crypto.english phrase

  passphrase <- divClass "checkbox-wrapper" $ do
    usePassphrase <- fmap value $ uiCheckbox def False def $ text "Use a BIP39 passphrase"
    fmap value $ uiInputElement $ def
      & initialAttributes .~
        "type" =: "password" <>
        "placeholder" =: "BIP39 passphrase" <>
        "class" =: "hidden passphrase"
      & modifyAttributes .~ ffor (updated usePassphrase)
        (\u -> if u then "class" =: Just "input passphrase" else "class" =: Just "hidden passphrase")
  let mkClass = \case Left _ -> "button_type_secondary"; _ -> "button_type_confirm"
  rec
    dyn_ $ ffor lastError $ \case
      Nothing -> pure ()
      Just e -> divClass "message-wrapper" $ divClass "message" $ text $ case e of
        BIP39PhraseError_MnemonicWordsErr (Crypto.ErrWrongNumberOfWords actual expected)
          -> "Wrong number of words: expected " <> tshow expected <> ", but got " <> tshow actual
        BIP39PhraseError_InvalidPhrase -> "Invalid phrase"
        BIP39PhraseError_Dictionary (Crypto.ErrInvalidDictionaryWord word)
          -> "Invalid word in phrase: " <> baToText word
    cancel <- cancelButton def "Cancel"
    next <- uiButtonDyn (def & uiButtonCfg_class .~ fmap mkClass sentenceOrError) $ text "Next"
    let (err, sentence) = fanEither $ current sentenceOrError <@ next
    lastError <- holdDyn Nothing $ Just <$> err

  let toSeed :: Crypto.ValidMnemonicSentence mw => Text -> Crypto.MnemonicSentence mw -> Crypto.Seed
      toSeed p s = Crypto.sentenceToSeed s Crypto.english $ textTo p

  dMSeed <- holdDyn Nothing $ pure <$> attachWith toSeed (current passphrase) sentence

  ePassword <- setPassword dMSeed

  pure
    ( ePassword
    , splashScreen <$ cancel
    )

passphraseWordElement
  :: MonadWidget t m
  => Dynamic t PassphraseStage
  -> WordKey
  -> Dynamic t Text
  -> m (Event t Text)
passphraseWordElement currentStage k wrd = divClass "passphrase-widget-elem-wrapper" $ do
  pb <- getPostBuild

  divClass "passphrase-widget-key-wrapper" $
    text (showWordKey k)

  rec
    wordElem <- divClass "passphrase-widget-word-wrapper" . uiInputElement $ def
      & inputElementConfig_setValue .~ (current wrd <@ pb)
      & setInitialAttrs .~
        ( "type" =: "password" <>
          "size" =: "10" <>
          "class" =: "passphrase-widget-word"
        )
      & modAttrs .~ mergeWith (<>)
        [ ("type" =: Just "text") <$ domEvent Mouseover wordElem
        , ("type" =: Just "password") <$ domEvent Mouseleave wordElem
        , ("readonly" =:) . enableIfRecovery <$> current currentStage <@ pb
        ]

  pure $ _inputElement_input wordElem
  where
    enableIfRecovery Recover = Nothing
    enableIfRecovery Setup = Just "true"

    setInitialAttrs = inputElementConfig_elementConfig . elementConfig_initialAttributes
    modAttrs = inputElementConfig_elementConfig . elementConfig_modifyAttributes

passphraseWidget
  :: MonadWidget t m
  => Dynamic t (Map.Map WordKey Text)
  -> Dynamic t PassphraseStage
  -> m (Event t (Map.Map WordKey Text))
passphraseWidget dWords dStage = do
  divClass "passphrase-widget-wrapper" $
    listViewWithKey dWords (passphraseWordElement dStage)


-- | UI for generating and displaying a new mnemonic sentence.
createNewWallet
  :: MonadWidget t m
  => Maybe (Crypto.MnemonicSentence 12)
  -- ^ Initial mnemonic sentence. If missing, a new one will be generated.
  -> SetupWF t m
createNewWallet mMnemonic = Workflow $ do
  el "h1" $ text "Wallet recovery phrase"
  el "p" $ text "Write down your recovery phrase on paper"
  initMnemonic <- maybe genMnemonic (pure . Right) mMnemonic
  rec
    mnemonic <- holdDyn initMnemonic =<< performEvent (genMnemonic <$ regen)
    divClass "group" $ dyn $ ffor mnemonic $ \case
      Left e -> text e
      Right sentence -> void $ passphraseWidget
        (pure $ mkPhraseMapFromMnemonic sentence)
        (pure Setup)
        
    stored <- fmap value $ divClass "checkbox-wrapper" $ uiCheckbox def False def $ text "I have safely stored my recovery phrase."
    regen <- uiButton btnCfgTertiary $ do
      elClass "i" "fa fa-lg fa-refresh" blank
      text " Regenerate"

  dPassword <- setPassword (fmap (fmap (\mn -> Crypto.sentenceToSeed mn Crypto.english "") . hush) mnemonic)
    >>= holdDyn Nothing . fmap pure

  let nextDisabled = liftA2 (||) (fmap isNothing dPassword) (fmap not stored)

  cancel <- cancelButton def "Cancel"
  next <- confirmButton (def & uiButtonCfg_disabled .~ nextDisabled) "Next"

  finishSetupWF $ leftmost
    [ splashScreen <$ cancel
    , attachWithMaybe (\e () -> confirmPhrase dPassword <$> hush e) (current mnemonic) next
    ]

-- | UI for mnemonic sentence confirmation: scramble the phrase, make the user
-- choose the words in the right order.
confirmPhrase
  :: MonadWidget t m
  => Dynamic t (Maybe Crypto.XPrv)
  -> Crypto.MnemonicSentence 12
  -- ^ Mnemonic sentence to confirm
  -> SetupWF t m
confirmPhrase dPassword mnemonic = Workflow $ do

  el "h1" $ text "Confirm your recovery phrase"

  el "p" $ text "Enter the words in the correct order"

  let actualMap = mkPhraseMapFromMnemonic mnemonic

  rec
    onPhraseUpdate <- passphraseWidget dConfirmPhrase (pure Recover)
    dConfirmPhrase <- holdDyn (wordsToPhraseMap $ replicate passphraseLen T.empty)
      $ flip Map.union <$> current dConfirmPhrase <@> onPhraseUpdate

  let done = (== actualMap) <$> dConfirmPhrase

  back <- cancelButton def "Back"
  skip <- uiButton btnCfgTertiary $ text "Skip"
  next <- confirmButton (def & uiButtonCfg_disabled .~ fmap not done) "Next"

  pure
    ( tagMaybe (current dPassword) $ gate (current done) next <> skip
    , createNewWallet (Just mnemonic) <$ back
    )

setPassword
  :: MonadWidget t m
  => Dynamic t (Maybe Crypto.Seed)
  -> m (Event t Crypto.XPrv)
setPassword dSeed = form "" $ do
  el "p" $ text "This password will protect your wallet. You'll need it when signing transactions."

  let uiPassword ph = uiInputElement $ def & initialAttributes .~
                      ( "type" =: "password" <>
                        "placeholder" =: ph <>
                        "class" =: "passphrase"
                      )

  p1elem <- uiPassword "Enter password"
  p2elem <- uiPassword "Confirm password" 

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

    let (err, pass) = fanEither $ checkPassword <$> p1 <*> p2 <@ eCheckPassword

    lastError <- holdDyn Nothing $ leftmost
      [ Just <$> err
      , Nothing <$ pass
      ]

  pure $ attachWithMaybe (\ms pw -> flip Crypto.generate pw <$> ms) (current dSeed) (T.encodeUtf8 <$> pass)
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

