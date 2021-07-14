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
  -- (
  --   WalletExists (..), Password(..), runSetup
  -- , splashLogo, setupClass
  -- ) where

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
import Data.ByteString (ByteString)
import Data.String (IsString, fromString)
import Data.Functor ((<&>))
import Data.Text (Text)
import Language.Javascript.JSaddle (MonadJSM, liftJSM)
import Reflex.Dom.Core
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Frontend.AppCfg (FileFFI(..))
import Frontend.Storage.Class (HasStorage)
import Frontend.UI.Button
import Frontend.UI.Dialogs.ChangePassword (minPasswordLength)
import Frontend.UI.Widgets.Helpers (imgWithAlt)
import Frontend.UI.Widgets
import Obelisk.Generated.Static

import Frontend.Setup.Password
import Frontend.Setup.Widgets
import Frontend.Crypto.Ed25519 (genMnemonic)
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

-- | Convenience function for unpacking byte array things into 'Text'
-- baToText :: ByteArrayAccess b => b -> Text
-- baToText = T.decodeUtf8 . BA.pack . BA.unpack

-- textTo :: IsString a => Text -> a
-- textTo = fromString . T.unpack

tshow :: Show a => a -> Text
tshow = T.pack . show

type SetupWF bipKey t m = Workflow t m
  ( WalletScreen
  , Event t (bipKey, Password, Bool)
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

splashLogo :: DomBuilder t m => m ()
splashLogo = do
  elAttr "div"
    (  "style" =: ("background-image: url(" <> (static @"img/Wallet_Graphic_1.png") <> ");")
    <> "class" =: setupClass "splash-bg"
    ) kadenaWalletLogo

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

doneScreen
  :: (DomBuilder t m, PostBuild t m, BIP39Root key )-- (Performable m))
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
