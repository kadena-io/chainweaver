{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Desktop where

import Control.Lens ((<>~))
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Language.Javascript.JSaddle (liftJSM)
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import qualified Data.ByteString.Char8 as BSC
import Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Crypto.Error as Ed25519
import qualified Crypto.Random.Entropy

import qualified Crypto.Encoding.BIP39 as Crypto
import qualified Crypto.Encoding.BIP39.English as Crypto
import qualified Cardano.Crypto.Wallet as Crypto
import qualified Cardano.Crypto.Wallet.Encrypted as Crypto

import Data.Bifunctor
import Obelisk.Route
import Obelisk.Configs
import Obelisk.Frontend
import Common.Route
import Reflex.Dom.Core
import Common.Api (getConfigRoute)
import qualified Frontend
import qualified Frontend.ReplGhcjs
import Frontend.ModuleExplorer.Impl (loadEditorFromLocalStorage)
import Frontend.AppCfg
import Frontend.Storage
import Obelisk.Generated.Static

import Frontend.UI.Button
import Frontend.UI.Widgets

data Wallet a where
  Wallet :: Wallet ()
deriving instance Show (Wallet a)

-- | This is for dev.
-- TODO allow `ob run` to use this like `ob run desktop Desktop.desktop`.
-- Right now you have to unpack obelisk and edit the references to
-- Frontend/frontend accordingly.
desktop :: Frontend (R FrontendRoute)
desktop = Frontend
  { _frontend_head = prerender_ blank $ do
      let backendEncoder = either (error "frontend: Failed to check backendRouteEncoder") id $
            checkEncoder backendRouteEncoder
      base <- getConfigRoute
      _ <- Frontend.newHead $ \r -> base <> renderBackendRoute backendEncoder r
      el "style" $ do
        text ".fullscreen { width: 100vw; height: 100vh; display: flex; justify-content: center; align-items: center; color: #fff; }"
        text ".fullscreen { background: rgb(30,40,50); background: radial-gradient(circle, rgba(40,50,60,1) 0%, rgba(27,30,46,1) 100%); }"
        text ".fullscreen .checkbox-wrapper { margin: 2rem auto; }"
        text ".fullscreen .checkbox { font-size: 20px; color: #fff; text-align: left; display: inline-block; padding-left: 40px; }"
        text ".fullscreen .checkbox .checkbox__checkmark { top: 2px; height: 20px; width: 20px; }"
        text ".fullscreen .checkbox input:checked ~ .checkbox__checkmark { background-color: #ed098f; border-color: #ed098f }"
        text ".fullscreen .checkbox .checkbox__checkmark_type_secondary:after { top: 2px; left: 6px; width: 3px; height: 10px; }"
        text ".fullscreen .button { background-color: #ddd; color: #333; }"
        text ".fullscreen .group { color: #222; margin: 2rem 0; }"
        text ".fullscreen .group.dark { background-color: rgba(0,0,0,0.3); }"
        text ".fullscreen .button_type_confirm:not([disabled]) { background-color: #ed098f; }"
        text ".fullscreen .wrapper { max-width: 40rem; text-align: center; }"
        text ".fullscreen .wrapper .logo { width: 20rem; }"
        text ".button_hidden { display: none; }"
        text ".group.group_buttons { text-align: center; }"
        text "button { margin: 0.2rem; }"
        text "button.button_type_confirm { border-color: #ed098f; }"
      pure ()
  , _frontend_body = prerender_ blank $ flip runStorageT browserStorage $ do
    getItemStorage localStorage Wallet

    divClass "fullscreen" $ divClass "wrapper" $ do
      elAttr "img" ("src" =: static @"img/Klogo.png" <> "class" =: "logo") blank
      workflow $ createNewWallet Nothing

    --prerender_ blank $ do
    do
      (fileOpened, triggerOpen) <- Frontend.openFileDialog
      let appCfg = AppCfg
            { _appCfg_gistEnabled = False
            , _appCfg_externalFileOpened = fileOpened
            , _appCfg_openFileDialog = liftJSM triggerOpen
            , _appCfg_loadEditor = loadEditorFromLocalStorage
            , _appCfg_editorReadOnly = False
            , _appCfg_signingRequest = never
            , _appCfg_signingResponse = \_ -> pure ()
            }
      _ <- Frontend.ReplGhcjs.app appCfg
      pure ()
  }

-- | Convenience function for unpacking byte array things into 'Text'
baToText :: ByteArrayAccess b => b -> Text
baToText = T.decodeUtf8 . BA.pack . BA.unpack

type SetupWF t m = Workflow t m (Event t ())

-- | UI for generating and displaying a new mnemonic sentence.
createNewWallet
  :: MonadWidget t m
  => Maybe (Crypto.MnemonicSentence 15)
  -- ^ Initial mnemonic sentence. If missing, a new one will be generated.
  -> SetupWF t m
createNewWallet mMnemonic = Workflow $ do
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

  cancel <- cancelButton def "Cancel"
  next <- confirmButton (def & uiButtonCfg_disabled .~ fmap not stored) "Next"
  pure
    ( cancel
    , attachWithMaybe (\e () -> either (const Nothing) (Just . confirmPhrase) e) (current mnemonic) next
    )

-- | Display a list of items, returning tagged events
switchList :: (Adjustable t m, MonadHold t m, PostBuild t m, MonadFix m) => Dynamic t [a] -> (Dynamic t a -> m (Event t ())) -> m (Event t a)
switchList l m = fmap (switchDyn . fmap leftmost) . simpleList l $ \a -> tag (current a) <$> m a

-- | UI for mnemonic sentence confirmation: scramble the phrase, make the user
-- choose the words in the right order.
confirmPhrase
  :: MonadWidget t m
  => Crypto.MnemonicSentence 15
  -- ^ Mnemonic sentence to confirm
  -> SetupWF t m
confirmPhrase mnemonic = Workflow $ do
  let sentence = T.words $ baToText $ Crypto.mnemonicSentenceToString Crypto.english mnemonic
  el "h1" $ text "Confirm your recovery phrase"
  el "p" $ text "Click the words in the correct order"
  rec
    -- Maintain an (ordered) list of staged words, and an alphabetical list of unstaged words
    (staged, unstaged) <- fmap splitDynPure $ foldDyn ($) ([], S.fromList sentence) $ mconcat
      [ ffor unstage $ \w -> bimap (L.delete w) (S.insert w)
      , ffor stage $ \w -> bimap (++ [w]) (S.delete w)
      , ffor reset $ \() (s, us) -> ([], foldr S.insert us s)
      ]
    let stageAttrs = "class" =: "group dark" <> "style" =: "min-height: 9.2rem"
    (unstage, reset) <- elAttr "div" stageAttrs $ do
      -- Display the staging area
      unstage <- switchList staged $ uiButton btnCfgPrimary . dynText
      -- Button to reset the staging area, only shown when something is staged
      let hiddenClass = ffor staged $ \s -> if null s then "button_hidden" else def
      reset <- uiButtonDyn (btnCfgPrimary & uiButtonCfg_class <>~ hiddenClass) $
        elClass "i" "fa fa-lg fa-refresh" blank
      pure (unstage, reset)
    -- Unstaged words are displayed in ascending alphabetical order
    let unstageAttrs = ffor unstaged $ \s -> if null s then mempty else "class" =: "group"
    stage <- elDynAttr "div" unstageAttrs $ switchList (S.toAscList <$> unstaged) $
      uiButton btnCfgTertiary . dynText
  let done = (== sentence) <$> staged
  back <- cancelButton def "Back"
  skip <- uiButton btnCfgTertiary $ text "Skip"
  next <- confirmButton (def & uiButtonCfg_disabled .~ fmap not done) "Next"
  pure $ (,) never $ leftmost
    [ createNewWallet (Just mnemonic) <$ back
    , setPassword mnemonic <$ (gate (current done) next <> skip)
    ]

setPassword
  :: MonadWidget t m
  => Crypto.MnemonicSentence 15
  -> SetupWF t m
setPassword mnemonic = Workflow $ do
  el "h1" $ text "Set a password"
  el "p" $ text "This password will protect your wallet. You'll need it when signing transactions."
  uiInputElement def
  uiInputElement def
  back <- cancelButton def "Back"
  next <- cancelButton def "Next"
  pure $ (,) never $ leftmost
    [ createNewWallet Nothing <$ back
    ]

    -- These two match iancoleman bip39
    --let seedNoPass = Crypto.sentenceToSeed sentence Crypto.english ""

-- | Generate a 15 word mnemonic sentence, using cryptonite.
genMnemonic :: MonadIO m => m (Either Text (Crypto.MnemonicSentence 15))
genMnemonic = liftIO $ bimap (T.pack . show) Crypto.entropyToWords . Crypto.toEntropy @160
  <$> Crypto.Random.Entropy.getEntropy @ByteString 20

restoreWallet :: m ()
restoreWallet = undefined


unSeed :: Crypto.Seed -> ByteString
unSeed = B16.encode . BA.pack . BA.unpack

toB16 :: BA.ByteArrayAccess b => b -> ByteString
toB16 = B16.encode . BA.pack . BA.unpack

passes = \case
  Ed25519.CryptoPassed p -> p
  e -> error $ "failed: " <> show e

showXPrv xprv = do
  BSC.putStrLn $ "XPrv: " <> splitXPrv xprv
  BSC.putStrLn $ " --> Crypto.xpubGetPublicKey: " <> toB16 (Crypto.xPubGetPublicKey $ Crypto.toXPub xprv)

test :: IO ()
test = do
  let entropy :: Crypto.Entropy 160 = case Crypto.toEntropy (fst $ B16.decode "ea1f6a34f7252a89628b40514af72dbd3eca93a4") of
        Right e -> e -- Mnemonic.genEntropy
        Left e -> error $ show e
  let sentence = Crypto.entropyToWords entropy
  putStrLn $ "MnemonicSentence: " <> show (Crypto.mnemonicSentenceToString Crypto.english sentence)
  -- These two match iancoleman bip39
  let seedNoPass = -- Crypto.sentenceToSeed sentence Crypto.english ""
        fst $ B16.decode
          "fffcf9f6f3f0edeae7e4e1dedbd8d5d2cfccc9c6c3c0bdbab7b4b1aeaba8a5a29f9c999693908d8a8784817e7b7875726f6c696663605d5a5754514e4b484542"
      --seedTestPass = Crypto.sentenceToSeed sentence Crypto.english "test"
  BSC.putStrLn $ "Seed (no passphrase): " <> B16.encode seedNoPass

  let root = Crypto.generate seedNoPass . id @ByteString
      rootPk = Crypto.xPubGetPublicKey . Crypto.toXPub . root

  showXPrv $ root "test"

  let message = "hello, world" :: ByteString
      sig = Crypto.sign ("test" :: ByteString) (root "test") message
      okCrypto = Crypto.verify (Crypto.toXPub (root "test")) message sig
      okEd25519 = Ed25519.verify (rootPk "test") message (passes $ Ed25519.signature sig)
  print sig
  print okCrypto
  print okEd25519

  putStrLn ""
  let derivationScheme = Crypto.DerivationScheme2
      derived k n = Crypto.deriveXPrv derivationScheme ("test" :: ByteString) k (0x80000000 + n)
      printDerived parent n = do
        showXPrv $ derived parent n
        pure $ derived parent n
  putStrLn "0"
  key0 <- printDerived (root "test") 0
  putStrLn "0/0"
  key0_0 <- printDerived key0 0
  putStrLn "0/1"
  key0_1 <- printDerived key0 1
  putStrLn "1"
  key1 <- printDerived (root "test") 1
  putStrLn "2"
  key2 <- printDerived (root "test") 2
  pure ()

splitXPrv x = BSC.unlines $
  [ ""
  , "enc: " <> B16.encode a
  , "enc: " <> B16.encode b
  , "pk : " <> B16.encode c
  , "cc : " <> B16.encode d
  ]
  where bs = BA.pack $ BA.unpack x
        (a, r0) = BA.splitAt 32 bs
        (b, r1) = BA.splitAt 32 r0
        (c, d) = BA.splitAt 32 r1
