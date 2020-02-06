{-# LANGUAGE ConstraintKinds #-}
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

import qualified Data.Text as Text

import Data.Time (UTCTime (..), fromGregorian)

import Frontend.AppCfg (EnabledSettings(..))
import Frontend.Foundation

import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Util as Pact
import qualified Pact.Server.ApiV1Client as Api

type HasUiTxLogModelCfg mConf t =
  ( Monoid mConf
  , Flattenable mConf t
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
     , HasUiTxLogModelCfg mConf t
     )
  => EnabledSettings
  -> model
  -> m mConf
uiTxLogs _enabledSettings _model = divClass "tx-logs__page" $ do
  let
    mkCol w = elAttr "col" ("style" =: ("width: " <> w)) blank
    mkHeading = elClass "th" "table__heading" . text
    td extraClass = elClass "td" ("table__cell" <> extraClass)
    td' = td ""
    tableAttrs = mconcat
      [ "style" =: "table__layout: fixed; width: 98%"
      , "class" =: "tx-logs table"
      ]

  elAttr "table" tableAttrs $ do
    -- Structure
    el "colgroup" $ replicateM_ 3 (mkCol "33%")
    -- Headings
    el "thead" $ el "tr" $ traverse_ mkHeading $
      [ "Timestamp"
      , "Request Key"
      , "Payload"
      ]

    -- Rows
    el "tbody" $ forM_ dummyData $ \cmdlog -> do
      elClass "tr" "table-row" $ do
        td' $ text $ Pact.tShow $ Api._commandLog_timestamp cmdlog
        td' $ text $ Pact.tShow $ Pact._cmdHash $ Api._commandLog_command cmdlog
        td' $ text $ Pact._cmdPayload $ Api._commandLog_command cmdlog

  pure mempty
