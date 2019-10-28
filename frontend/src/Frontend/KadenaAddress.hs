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
Encoder / Decoder probably should not be derived automatically, as package updates could
silently change the format and break _lots_ of things. No need for JSON, simple
parser/printer enough?

Final encoding uses base64 because it needs to be reliably cut'n'paste between platforms
without risking corruption. Does this need to be cryptographically secure??

Known Fixed/maximum sizes for the different fields would be neat.

Need restrictions on what is allowed as account name, max size, [a-zA-Z0-9].

Delimited? : Line breaks? Pipes?

Thought:

<network name, up to 253 (hostname RFC max length including '.')>\n
<chain id, six digits, leading zeros 000001, how many chains can we expect?>\n
<acc name, max length?, pad to size? names containing delimiters?>\n
<pkey, known fixed size>\n
<checksum, known fixed size>

Example:

us1.tn1.chainweb.com
000001
doug
1bfafb513a26979b48bafe3f9c0fba7703065066a84421f71ca41b54692de67d 
71f920fa275127a7b60fa4d4d41432a3

or:

us1.tn1.chainweb.com|000001|doug|1bfafb513a26979b48bafe3f9c0fba7703065066a84421f71ca41b54692de67d|71f920fa275127a7b60fa4d4d41432a3
-}


-- mkKadenaAddress
--   :: NetworkName
--   -> ChainId
--   -> AccountName
--   -> PublicKey
--   -> KadenaAddress
-- mkKadenaAddress network cid acc pubkey =
--   _F

