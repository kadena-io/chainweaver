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
import Control.Monad (guard)
import Control.Monad.Fix (MonadFix)
import Language.Javascript.JSaddle (MonadJSM)
import Reflex.Dom.Core

import Frontend.AppCfg (FileFFI(..))
import Frontend.Storage.Class (HasStorage)
import Frontend.UI.Button

import Frontend.Setup.Common
import Frontend.Setup.Widgets
import Frontend.Crypto.Class
import Frontend.Crypto.Password

runSetup
  :: ( DomBuilder t m
    , MonadFix m
    , MonadHold t m
    , PerformEvent t m
    , PostBuild t m
    , TriggerEvent t m
    , MonadJSM (Performable m)
    , HasStorage (Performable m)
    , MonadSample t (Performable m)
    , DerivableKey key mnemonic
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
     , MonadSample t (Performable m), DerivableKey key mnemonic
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
