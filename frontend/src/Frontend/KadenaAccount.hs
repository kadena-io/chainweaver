{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontend.KadenaAccount
  ( KadenaAccount (..)
  , HasKadenaAccount (..)
  , AccountName (unAccountName)
  , mkAccountName
  ) where

import "cryptonite" Crypto.Hash (Digest, MD5)

import GHC.Generics (Generic)
import Pact.Types.ChainId

import Kadena.SigningApi (AccountName(..), mkAccountName)

import Frontend.Foundation (makePactLenses)
import Frontend.Crypto.Ed25519

data KadenaAccount key = KadenaAccount
  { _kadenaAccount_chainId :: ChainId
  , _kadenaAccount_name :: AccountName 
  , _kadenaAccount_key :: key
  , _kadenaAccount_checksum :: Digest MD5
  }
  deriving Generic

makePactLenses ''KadenaAccount
