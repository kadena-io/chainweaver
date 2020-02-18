{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE QuasiQuotes #-}
-- |
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--
module Frontend.UI.Dialogs.TxLogs
  ( uiTxLogs
  ) where

import Control.Lens (iforM_, (^.))
import Control.Monad (void)

import Reflex
import Reflex.Dom.Core

import qualified Data.Text as Text

import Data.Either (isRight)
import Data.Time (formatTime)

import Common.Wallet (PublicKey, textToKey)
import Frontend.AppCfg (FileFFI (..))
import Frontend.UI.Modal (modalHeader, modalFooter)
import Frontend.Foundation
import Frontend.UI.Widgets

import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Util as Pact
import qualified Pact.Types.Hash as Pact
import qualified Pact.Types.ChainId as Pact

import qualified Pact.Server.ApiClient as Api

type HasUiTxLogModelCfg mConf m t =
  ( Monoid mConf
  , Flattenable mConf t
  , Api.HasTransactionLogger m
  )

uiTxLogs
  :: ( MonadWidget t m
     , HasUiTxLogModelCfg mConf m t
     )
  => FileFFI t m
  -> model
  -> Event t ()
  -> m (mConf, Event t ())
uiTxLogs fileFFI _model _onExtClose = do
  txLogger <- Api.askTransactionLogger
  pb <- getPostBuild
  onClose <- modalHeader $ text "Transaction Log"
  onLogLoad <- performEvent $ liftIO (Api._transactionLogger_loadFirstNLogs txLogger 10) <$ pb

  divClass "modal__main key-details" $ do
    let
      mkCol w = elAttr "col" ("style" =: ("width: " <> w)) blank
      mkHeading = elClass "th" "table__heading" . text
      td extraClass = elClass "td" ("table__cell " <> extraClass)
      tableAttrs = "class" =: "tx-logs table"

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
        el "tbody" $ iforM_ cmdLogs $ \i cmdLog -> elClass "tr" "table-row" $ do
          td "tx-log-row__ix" $ text $ Text.justifyRight 2 '0' $ Pact.tShow i

          -- Expected time format : YYYY-MM-DD  HH:MM
          td "tx-log-row__timestamp" $ text $ Text.pack
            $ formatTime timeLocale "%F %R" $ Api._commandLog_timestamp cmdLog

          let sender = Api._commandLog_sender cmdLog
          td "tx-log-row__sender" $ case (textToKey sender) :: Maybe PublicKey of
            Nothing -> text sender
            Just _ -> text $ Text.take 8 sender

          td "tx-log-row__chain" $ text
            $ Pact._chainId $ Api._commandLog_chain cmdLog

          td "tx-log-row__request-key" $ elClass "span" "request-key-text" $ text
            $ Pact.hashToText $ Pact.unRequestKey $ Api._commandLog_requestKey cmdLog

  _ <- modalFooter $
    exportButton txLogger

  pure (mempty, onClose)
  where
    exportButton txlog = mdo
      let cfg' = btnCfgPrimary & uiButtonCfg_title .~ pure (Just "Export Full Transaction Log")
      status <- holdDyn "fa-download" $ ffor (leftmost [False <$ onFileErr, isRight <$> onDeliveredFileOk])
        $ bool "tx-log-export-fail fa-times" "tx-log-export-success fa-check"

      (onFileErr, onContentsReady) <- fmap fanEither $ performEvent $
        liftIO (Api._transactionLogger_exportFile txlog) <$ onClick

      onDeliveredFileOk <- _fileFFI_deliverFile fileFFI onContentsReady

      onClick <- uiButtonDyn cfg' $ do
        dynText $ ffor (cfg' ^. uiButtonCfg_title) fold
        elDynClass "i" ("fa fa-fw tx-log-export-status " <> status) blank

      pure ()
