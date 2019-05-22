{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-- Workaround for untyped and unparsable Pact API. It should get properly fixed in Pact at some point, see:
-- https://github.com/kadena-io/pact/pull/375
-- Note: As those types are not shared with pact, we don't really gain much*)
-- safety/guarantees by those types - it just makes handling of the API easier. At
-- some point the above mentioned PR or something similar should really get merged.

--  *) not much, but not nothing, as some types are shared.

-- |
-- Module      :  Pact.Types.Command
-- Copyright   :  (C) 2016 Stuart Popejoy, Will Martino
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>, Will Martino <will@kadena.io>
--
-- Specifies types for commands in a consensus/DL setting.
--

module Pact.Typed.Types.Command
  ( Command(..) -- ,cmdPayload,cmdSigs,cmdHash
#if !defined(ghcjs_HOST_OS)
  ,mkCommand,mkCommand',verifyUserSig,verifyCommand
#else
  ,PPKScheme(..)
#endif
  , ProcessedCommand(..) -- ,_ProcSucc,_ProcFail
  , Address(..),aFrom,aTo
  , PrivateMeta(..),pmAddress
  , PublicMeta(..),pmChainId,pmSender,pmGasLimit,pmGasPrice
  , HasPlafMeta(..)
  , Payload(..) -- ,pMeta,pNonce,pPayload
  , ParsedCode(..) -- ,pcCode,pcExps
  , UserSig(..)
  , CommandError(..)
  {- , CommandResult(..),crReqKey,crTxId,crResult,crGas -}
  , CommandValue(..)
  , ExecutionMode(..)
  , CommandExecInterface(..),ceiApplyCmd,ceiApplyPPCmd
  , ApplyCmd, ApplyPPCmd
  , RequestKey(..)
  , cmdToRequestKey, requestKeyToB16Text
  , Signer (..)
  ) where


import Control.Applicative
import Control.Monad.Reader
import Data.Aeson as A
import Data.Maybe
import Data.Text hiding (filter, all)
import Prelude

import Pact.Types.Runtime
import Pact.Types.Orphans ()
import Pact.Types.PactValue

-- CommandResult is not exposed in the servant API, if it were we would need
-- the adjusted version of the PR:
-- https://github.com/kadena-io/pact/pull/375
import Pact.Types.Command hiding (CommandSuccess (..), CommandResult (..))


-- | Actual value of a `CommandResult`.
data CommandValue
  = CommandSuccess PactValue
  | CommandFailure CommandError
  deriving (Eq, Show)

deriving instance Eq CommandError
deriving instance Show CommandError


instance ToJSON CommandValue where
    toJSON = \case
      CommandSuccess a -> object
        [ "status" .= ("success" :: Text)
        , "data"   .= a
        ]
      CommandFailure (CommandError {..}) -> object $
        [ "status" .= ("failure" :: Text)
        , "error" .= _ceMsg
        ]
        ++ maybe [] (pure . ("detail" .=)) _ceDetail

instance FromJSON CommandValue where
  parseJSON = withObject "CommandValue" $ \o -> do
    status <- o .: "status"
    case status of
      "failure" -> do
        msg <- o .: "error"
        detail <- o .:? "detail"
        pure $ CommandFailure $ CommandError msg detail
      "success" ->
        CommandSuccess <$> o .: "data"
      _ -> fail $ "Expected a status of `success` or `failure`, not: " <> status

