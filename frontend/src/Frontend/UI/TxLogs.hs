{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuasiQuotes #-}
-- |
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--
module Frontend.UI.TxLogs
  ( uiTxLogs
  ) where

import Control.Monad (replicateM_)

import Reflex
import Reflex.Dom.Core

import Data.ByteString (ByteString)

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Time (UTCTime (..), fromGregorian)

import Frontend.AppCfg (EnabledSettings(..))
import Frontend.Foundation

import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Util as Pact

import qualified Pact.Server.ApiV1Client as Api

type HasUiTxLogModelCfg mConf m t =
  ( Monoid mConf
  , Flattenable mConf t
  , Api.HasTransactionLogger m
  )

dummyLog :: Api.CommandLog
dummyLog = Api.CommandLog
  { Api._commandLog_command = Pact.Command
    { Pact._cmdPayload = "payload"
    , Pact._cmdSigs = [Pact.UserSig "5fd6b3e2836d6dd84114b5948898e6ed63d832c618d01c21e32335e75d4623e5352a63daad37bd1e978ee453255469937ec607e46b97e0364c40a30e0c3dd90a"]
    , Pact._cmdHash = either error id $ Pact.fromText' "n1bWvpxcplbqw3IsaL34bmqvFO1F0iy0FcEcjj8adhA"
    }
  , Api._commandLog_timestamp = UTCTime (fromGregorian 2020 01 24) 47976.199626747
  , Api._commandLog_url = "https://eu1.testnet.chainweb.com/chainweb/0.0/testnet04/chain/0/pact"
  }

dummyData :: [Api.CommandLog]
dummyData = replicate 10 dummyLog

uiTxLogs
  :: ( MonadWidget t m
     , HasUiTxLogModelCfg mConf m t
     )
  => EnabledSettings
  -> model
  -> m mConf
uiTxLogs _enabledSettings _model = divClass "tx-logs__page" $ do
  txLogger <- Api.askTransactionLogger

  let
    loadN :: MonadIO m => Int -> m (Either SQLiteResponse [(UTCTime, Text, Text, Int, ByteString)])
    loadN n = liftIO $ Api._transactionLogger_queryLog txLogger
        [sql| SELECT (cmd_timestamp, request_hash, payload, version, raw_blob) FROM chainweaver_txn_logs LIMIT ? |]
        [n]

  pb <- getPostBuild

  onLogLoad <- performEvent $ loadN 10 <$ pb

  let
    mkCol w = elAttr "col" ("style" =: ("width: " <> w)) blank
    mkHeading = elClass "th" "table__heading" . text
    td extraClass = elClass "td" ("table__cell" <> extraClass)
    td' = td ""
    tableAttrs = mconcat
      [ "style" =: "table__layout: fixed; width: 98%"
      , "class" =: "tx-logs table"
      ]

  _ <- elAttr "table" tableAttrs $ do
    -- Structure
    el "colgroup" $ replicateM_ 3 (mkCol "33%")
    -- Headings
    el "thead" $ el "tr" $ traverse_ mkHeading $
      [ "Timestamp"
      , "Request Key"
      , "Payload"
      ]

    -- Rows
    el "tbody" $ widgetHold (text "wat") $ ffor onLogLoad $ \case
      Left e -> do
        text $ "Error: " <> Pact.tShow e
        text "Transaction logs currently unavailable"
      Right xs -> forM_ xs $ \(ts, rqHash, payload, version, _) -> elClass "tr" "table-row" $ do
        td' $ text $ Pact.tShow ts
        td' $ text $ Pact.tShow rqHash
        td' $ text $ payload

  pure mempty
