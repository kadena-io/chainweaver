{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright   :  (C) 2020-2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
--
module Frontend.UI.Dialogs.TxLogs
  ( uiTxLogs
  ) where

import Control.Lens (iforM_)
import Control.Monad (void)

import Reflex
import Reflex.Dom.Core

import qualified Data.Text as Text

import Data.Either (isRight)
import Data.Time (formatTime)

import Frontend.Crypto.Class (HasCrypto)

import Frontend.Wallet (PublicKey, PublicKeyPrefix (..), textToKey, genZeroKeyPrefix)
import Frontend.AppCfg (FileFFI (..))
import Frontend.UI.Modal (modalHeader, modalFooter)
import Frontend.Foundation
import Frontend.UI.Widgets

import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Util as Pact
import qualified Pact.Types.Hash as Pact
import qualified Pact.Types.ChainId as Pact

import qualified Pact.Server.ApiClient as Api

type HasUiTxLogModelCfg model mConf key m t =
  ( Monoid mConf
  , Flattenable mConf t
  , Api.HasTransactionLogger m
  , HasCrypto key (Performable m)
  )

uiTxLogs
  :: forall t m model mConf key
  .  ( MonadWidget t m
     , HasUiTxLogModelCfg model mConf key m t
     )
  => FileFFI t m
  -> Event t ()
  -> m (mConf, Event t ())
uiTxLogs fileFFI _onExtClose = do
  txLogger <- Api.askTransactionLogger
  pb <- getPostBuild
  onClose <- modalHeader $ text "Transaction Log"
  onLogLoad <- performEvent $ liftIO (Api._transactionLogger_loadLastNLogs txLogger 10) <$ pb

  divClass "modal__main key-details" $ do
    let
    void $ runWithReplace blank $ ffor onLogLoad $ \case
      Left e -> divClass "tx-log__decode-error" $ text $ Text.pack e
      Right (timeLocale, cmdLogs) -> elAttr "table" tableAttrs $ do
        -- Structure
        el "colgroup" $ do
          mkCol "10%"
          mkCol "40%"
          mkCol "20%"
          mkCol "20%"
          mkCol "10%"
        -- Headings
        el "thead" $ el "tr" $ traverse_ mkHeading $
          [ ""
          , "Creation Time"
          , "Sender"
          , "Chain ID"
          , "Request Key"
          ]

        -- Rows
        el "tbody" $ iforM_ (reverse cmdLogs) $ \i logEntry -> elClass "tr" "table-row" $ do
          td "tx-log-row__ix" $ text $ Text.justifyRight 2 '0' $ Pact.tShow i
          case logEntry of
            Api.LogEntry_Cmd cmdLog ->
              commandLogRow timeLocale cmdLog
            Api.LogEntry_Event eType sender timestamp ->
              walletEventRow timeLocale eType sender timestamp

  _ <- modalFooter $
    exportButton txLogger

  pure (mempty, onClose)
  where
    mkCol w = elAttr "col" ("style" =: ("width: " <> w)) blank
    mkHeading = elClass "th" "table__heading" . text
    td extraClass = elClass "td" ("table__cell " <> extraClass)
    tableAttrs = "class" =: "tx-logs table"

    timestampTD timeLocale t =
      td "tx-log-row__timestamp" $ text $ Text.pack $ formatTime timeLocale "%F %R" t

    senderTD sender =
      td "tx-log-row__sender" $ case (textToKey sender) :: Maybe PublicKey of
        Nothing -> text sender
        Just _ -> text $ Text.take 8 sender

    commandLogRow timeLoc cmdLog = do
      timestampTD timeLoc $ Api._commandLog_timestamp cmdLog
      senderTD $ Api._commandLog_sender cmdLog
      td "tx-log-row__chain" $ text
        $ Pact._chainId $ Api._commandLog_chain cmdLog

      td "tx-log-row__request-key" $ elClass "span" "request-key-text" $ text
        $ Pact.hashToText $ Pact.unRequestKey $ Api._commandLog_requestKey cmdLog

    walletEventRow timeLoc eType sender timestamp = do
      timestampTD timeLoc timestamp
      senderTD sender
      td "" blank
      td "" $ text $ case eType of
        Api.WalletEvent_Import -> "Wallet imported"
        Api.WalletEvent_Export -> "Wallet exported"

    runExport txlog = genZeroKeyPrefix >>=
      liftIO . Api._transactionLogger_exportFile txlog . _unPublicKeyPrefix

    exportButton txlog = mdo
      (onFileErr, onContentsReady) <- fmap fanEither $ performEvent $
        runExport txlog <$ onClick

      onDeliveredFileOk <- _fileFFI_deliverFile fileFFI onContentsReady

      status <- holdDyn "fa-download" $ leftmost
        [ "tx-log-export-fail fa-times" <$ onFileErr
        , "tx-log-export-success fa-check" <$ ffilter isRight onDeliveredFileOk
        ]

      onClick <- uiButtonDyn btnCfgPrimary $ do
        text "Export Full Transaction Log"
        elDynClass "i" ("fa fa-fw tx-log-export-status " <> status) blank

      pure ()
