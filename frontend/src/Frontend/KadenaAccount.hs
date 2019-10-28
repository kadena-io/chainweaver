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

import Common.Network (NetworkName)
import Frontend.Foundation (makePactLenses)

data KadenaAccount = KadenaAccount
  { _kadenaAccount_networkId :: NetworkName
    -- ^ Blockchain specific identifier
  , _kadenaAccount_chainId :: ChainId
    -- ^ Platform specific identifier
  , _kadenaAccount_name :: AccountName 
    -- ^ Name provided for this account
  , _kadenaAccount_checksum :: Digest MD5
    -- ^ The checksum of NetworkId and AccountName for verification
  }
  deriving Generic

makePactLenses ''KadenaAccount
