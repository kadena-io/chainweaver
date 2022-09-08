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
module Frontend.Setup.Setup (runSetup) where

import Control.Lens ((<>~), (^.), _1, _2, _3)
import Control.Monad (guard)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Foldable (traverse_)
import Data.Maybe (isNothing)
import Language.Javascript.JSaddle (MonadJSM, liftJSM)
import Reflex.Dom.Core
import qualified Data.Text as T
import System.FilePath (takeFileName)

import Frontend.AppCfg (FileFFI(..), FileType(FileType_Import))
import Pact.Server.ApiClient (HasTransactionLogger, askTransactionLogger)
import Frontend.Storage.Class (HasStorage)
import Frontend.UI.Button
import Frontend.UI.Widgets.Helpers (imgWithAlt)
import Frontend.UI.Widgets
import Frontend.Setup.Widgets
import Frontend.Setup.Common
import Frontend.Setup.ImportExport (doImport, ImportWalletError(..), ImportWidgetApis(..), ImportWidgetConstraints)
import Frontend.Crypto.Class
import Frontend.Crypto.Password
import Obelisk.Generated.Static

runSetup
  :: forall t m n key bipStorage
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
    , BIP39Root key, BIP39Mnemonic (Sentence key)
    , ImportWidgetConstraints bipStorage key n (Performable m)
    )
  => FileFFI t m
  -> Bool
  -> WalletExists
  -> ImportWidgetApis bipStorage key n (Performable m)
  -> m (Event t (Either () (key, Password, Bool)))
runSetup fileFFI showBackOverride walletExists importWidgetApis = setupDiv "fullscreen" $ mdo
  let dCurrentScreen = (^._1) <$> dwf

  eBack <- fmap (domEvent Click . fst) $ elDynClass "div" ((setupClass "back " <>) . hideBack <$> dCurrentScreen) $
    el' "span" $ do
      elClass "i" "fa fa-fw fa-chevron-left" $ blank
      text "Back"

  _ <- dyn_ $ walletSetupRecoverHeader <$> dCurrentScreen

  dwf <- divClass "wrapper" $
    workflow (splashScreenWithImport walletExists fileFFI importWidgetApis eBack)

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

splashScreenWithImport
  :: (DomBuilder t m, MonadFix m, MonadHold t m, PerformEvent t m
     , PostBuild t m, MonadJSM (Performable m), TriggerEvent t m, HasStorage (Performable m)
     , MonadSample t (Performable m)
     , HasTransactionLogger m
     , BIP39Root key, BIP39Mnemonic (Sentence key)
     , ImportWidgetConstraints bipStorage key n (Performable m)
     )
  => WalletExists
  -> FileFFI t m
  -> ImportWidgetApis bipStorage key n (Performable m)
  -> Event t ()
  -> SetupWF key t m
splashScreenWithImport walletExists fileFFI importWidgetApis eBack = selfWF
  where
    selfWF = Workflow $ setupDiv "splash" $ do
      agreed <- splashScreenAgreement
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
        , restoreFromImport walletExists fileFFI importWidgetApis selfWF eBack <$ hasAgreed restoreImport
        ]
