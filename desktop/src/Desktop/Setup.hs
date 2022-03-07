{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Wallet setup screens
module Desktop.Setup (runSetup) where

import Control.Lens ((<>~), (^.), _1, _2, _3)
import Control.Monad (guard)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Foldable (traverse_)
import Data.Maybe (isNothing)
import Language.Javascript.JSaddle (MonadJSM, liftJSM)
import Reflex.Dom.Core
import qualified Cardano.Crypto.Wallet as Crypto
import qualified Data.Text as T
import System.FilePath (takeFileName)

import Frontend.AppCfg (FileFFI(..), FileType(FileType_Import))
import Desktop.ImportExport (doImport, ImportWalletError(..))
import Pact.Server.ApiClient (HasTransactionLogger, askTransactionLogger)
import Frontend.Storage.Class (HasStorage)
import Frontend.UI.Button
import Frontend.UI.Widgets.Helpers (imgWithAlt)
import Frontend.UI.Widgets
import Frontend.Setup.Widgets
import Frontend.Setup.Common
import Frontend.Crypto.Password
import Obelisk.Generated.Static

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
    workflow (splashScreenWithImport walletExists fileFFI eBack)

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
     )
  => WalletExists
  -> FileFFI t m
  -> Event t ()
  -> SetupWF Crypto.XPrv t m
splashScreenWithImport walletExists fileFFI eBack = selfWF
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
        , restoreFromImport walletExists fileFFI selfWF eBack <$ hasAgreed restoreImport
        ]

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
          imgWithAlt $(static "img/import.svg") "Import" blank
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
