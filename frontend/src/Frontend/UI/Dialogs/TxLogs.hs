{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuasiQuotes #-}
-- |
-- Copyright   :  (C) 2018 Kadena
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

import Data.Time (formatTime)

import Common.Wallet (PublicKey, textToKey)
import Frontend.UI.Modal (modalHeader, modalFooter)
import Frontend.Foundation
import Frontend.UI.Widgets

import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Util as Pact
import qualified Pact.Types.Hash as Pact
import qualified Pact.Types.ChainId as Pact

import qualified Pact.Server.ApiV1Client as Api

type HasUiTxLogModelCfg mConf m t =
  ( Monoid mConf
  , Flattenable mConf t
  , Api.HasTransactionLogger m
  )

uiTxLogs
  :: ( MonadWidget t m
     , HasUiTxLogModelCfg mConf m t
     )
  => model
  -> Event t ()
  -> m (mConf, Event t ())
uiTxLogs _model _onExtClose = do
  txLogger <- Api.askTransactionLogger
  pb <- getPostBuild
  onClose <- modalHeader $ text "Transaction Log"

  divClass "modal__main key-details" $ do
    onLogLoad <- performEvent $ liftIO (Api._transactionLogger_loadFirstNLogs txLogger 10) <$ pb

    let
      showIndex i = let itxt = Pact.tShow i in
        if Text.length itxt < 2 then
          "0" <> itxt
        else
          itxt

      mkCol w = elAttr "col" ("style" =: ("width: " <> w)) blank
      mkHeading = elClass "th" "table__heading" . text
      td extraClass = elClass "td" ("table__cell " <> extraClass)
      tableAttrs = "class" =: "tx-logs table"

    void $ runWithReplace blank $ ffor onLogLoad $ \case
      Left e -> do
        divClass "tx-log__decode-error" $ text $ "Error: " <> Pact.tShow e
        divClass "tx-log__logs-unavailable-message" $ text "Transaction logs currently unavailable"
      Right (timeLocale, cmdLogs) -> elAttr "table" tableAttrs $ do
        -- Structure
        el "colgroup" $ do
          mkCol "5%"
          mkCol "20%"
          mkCol "15%"
          mkCol "10%"
          mkCol "50%"
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
          td "tx-log-row__ix" $ text $ showIndex i

          -- Expected time format : YYYY-MM-DD  HH:MM
          td "tx-log-row__timestamp" $ text $ Text.pack
            $ formatTime timeLocale "%F %R" $ Api._commandLog_timestamp cmdLog

          let sender = Api._commandLog_sender cmdLog
          td "tx-log-row__sender" $ case (textToKey sender) :: Maybe PublicKey of
            Nothing -> text sender
            Just _ -> text $ Text.take 4 sender <> "..." <> Text.takeEnd 4 sender

          td "tx-log-row__chain" $ text
            $ Pact._chainId $ Api._commandLog_chain cmdLog

          td "tx-log-row__request-key" $ text
            $ Pact.hashToText $ Pact.unRequestKey $ Api._commandLog_requestKey cmdLog

  modalFooter $ do
    onExport <- confirmButton def "Export Full Transaction Log"

    pure (mempty, onClose)
