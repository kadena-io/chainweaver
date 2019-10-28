{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontend.KadenaAddress
  ( KadenaAddress (unKadenaAddress)
  , AccountName (unAccountName)
  , mkAccountName
  ) where

import "cryptonite" Crypto.Hash (Digest, MD5)

import GHC.Generics (Generic)
import Pact.Types.ChainId

import Kadena.SigningApi (AccountName(..), mkAccountName)

import Common.Network (NetworkName)
import Frontend.Foundation (makePactLenses)

newtype KadenaAddress = KadenaAddress { unKadenaAddress :: Text }
  deriving (Show, Eq)

mkKadenaAddress
  :: NetworkName
  -> ChainId
  -> AccountName
  -> PublicKey
  -> KadenaAddress
mkKadenaAddress network cid acc pubkey =
  _F

