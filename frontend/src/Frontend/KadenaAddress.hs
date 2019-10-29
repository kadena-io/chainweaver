{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontend.KadenaAddress
  ( KadenaAddress (unKadenaAddress)
  , AccountName (unAccountName)
  , mkAccountName
  ) where

import "cryptonite" Crypto.Hash (Digest, MD5)

import Data.Text (Text)
import GHC.Generics (Generic)
import Pact.Types.ChainId

import Kadena.SigningApi (AccountName(..), mkAccountName)

import Common.Network (NetworkName)
import Frontend.Crypto.Ed25519 (PublicKey)
import Frontend.Foundation (makePactLenses)

newtype KadenaAddress = KadenaAddress { unKadenaAddress :: Text }
  deriving (Show, Eq)

{-
Encoder / Decoder should not be derived automatically as package updates could silently
change the format and break things. Keep to simple handrolled parser & printer that
satisfies the round-trip property: (print . parse = id), hedgehog has 'tripping' for this.

Final encoding will be Base64 because it needs to be reliably cut'n'paste between
platforms without risking corruption. (Does this need to be cryptographically secure?)

We probably should tighten the restrictions for some of the components of the address:

Account Name:
Currently only requires that it not be empty. This should be changed to have a max
character limit and limitations on the characters that can be entered. Limit to
alpha-numeric only with '_' and '-' allowed?

ChainId:
At the moment is a numeric identifier. How many chains can we expect to see? It's probably
acceptable to leave this as is with some extra steps to ensure that this can't be a
negative value, and can't contain decimal values.

NetworkName(Id):
This is a hostname, which according to the RFC can be up to 253 characters, including '.'
separators. Do we expect to allow different ports in this configuration? There should be
library support we can lean on to ensure that the network is RFC compliant and that should
give us enough reliability.

Public Key & Checksum:
Both of these are by definition fixed size pieces of information with redefined structures.

Checksum will be MD5 hash of 'AccountName', 'ChainId', 'NetworkName', 'PublicKey'. This
will be the last field as well so we can parse the initial fields, create the checksum,
parse the checksum and compare, failing immediately if we don't have a match.

Some ideas for layout:

Newlines:

us1.tn1.chainweb.com
1
doug
1bfafb513a26979b48bafe3f9c0fba7703065066a84421f71ca41b54692de67d 
71f920fa275127a7b60fa4d4d41432a3

Or some other delimiter:

us1.tn1.chainweb.com%1%doug%1bfafb513a26979b48bafe3f9c0fba7703065066a84421f71ca41b54692de67d%71f920fa275127a7b60fa4d4d41432a3

-}


-- mkKadenaAddress
--   :: NetworkName
--   -> ChainId
--   -> AccountName
--   -> PublicKey
--   -> KadenaAddress
-- mkKadenaAddress network cid acc pubkey =
--   _F

