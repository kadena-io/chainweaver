{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Frontend.KadenaAddress
  ( KadenaAddressError (..)
  , KadenaAddress
    ( _kadenaAddress_encoded
    , _kadenaAddress_networkName
    , _kadenaAddress_accountName
    , _kadenaAddress_chainId
    , _kadenaAddress_checksum
    )
  , mkKadenaAddress
  , decodeKadenaAddress
  , bytestringChecksum
  , mkAddressChecksum
  , showChecksum
  , parseKadenaAddressBlob
  , delimiter
  , humanReadableDelimiter
  , textKadenaAddress
  , isValidKadenaAddress
  ) where

import Control.Lens
import Control.Monad.Except (MonadError (..), liftEither)
import Control.Error (note)
import Control.Monad ((<=<))
import Data.Functor (void)
import Data.List (intersperse)
import qualified Data.Char as C
import Data.Word (Word8)
import Data.String (fromString)
import Data.Bifunctor (first)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE

import qualified Data.Attoparsec.ByteString.Char8 as A

import qualified URI.ByteString as URI
import qualified Data.ByteString.Base64 as Base64
import "hashing" Crypto.Hash

import Pact.Types.ChainId
import Kadena.SigningApi (AccountName(..), mkAccountName)

import Common.Network (NetworkName,textNetworkName,mkNetworkName)

{-
Encoder / Decoder should not be derived automatically as package updates could silently
change the format and break things. Keep to simple hand-rolled parser & printer that
satisfies the round-trip property: (print . parse = id), hedgehog has 'tripping' for this.

Final encoding will be Base64 because it needs to be reliably cut'n'paste between
platforms without risking corruption.

The address will contain the following fields:

AccountName: Compliant with the requirements contained in pact. In the case of wallet-only
accounts that do not exist on the blockchain, the Public Key will be used.
ChainId: Non-negative integer.
Network: The network where the chain resides.
Checksum: The checksum will be SHA256 of the 'AccountName/PublicKey', 'ChainId',
‘Network’. An invalid checksum will be a parse failure.

Some examples of the encoding (delimiter yet to be determined):

Using newlines:
doug
1
us1.tn1.chainweb.com
1bfafb513a26979b48bafe3f9c0fba7703065066a84421f71ca41b54692de67d
71f920fa275127a7b60fa4d4d41432a3

Or some other delimiter:
doug%1%us1.tn1.chainweb.com%1bfafb513a26979b48bafe3f9c0fba7703065066a84421f71ca41b54692de67d%71f920fa275127a7b60fa4d4d41432a3

The final form includes a human readable component as well as machine readable components:

<account name>|<chain id>|<base64 encoding of address>

The use of an address does not require the human readable component. Only the Base64
encoded address is validated against the checksum. If the human readable prefix is
included then it will be checked against the values decoded in the address as an extra
preventative measure.
-}

data KadenaAddressError
  = ParseError Text
  | ChecksumMismatch String String
  | Base64Error String
  | InvalidHumanReadablePiece Text
  deriving (Show, Eq)

data KadenaAddress = KadenaAddress
  { _kadenaAddress_encoded :: ByteString
    -- ^ The complete encoded version of this address
  , _kadenaAddress_accountName :: AccountName
    -- ^ The account name associated with this address
  , _kadenaAddress_chainId :: ChainId
    -- ^ The chain where this account resides
  , _kadenaAddress_networkName :: NetworkName
    -- ^ The network where this chain resides
  , _kadenaAddress_checksum :: Checksum
    -- ^ The checksum of the AccountName, ChainId, and NetworkName
  }
  deriving (Show, Eq)

type Checksum = SHA256

textKadenaAddress :: KadenaAddress -> Text
textKadenaAddress = TE.decodeUtf8With TE.lenientDecode . _kadenaAddress_encoded

humanReadableDelimiter :: Word8
humanReadableDelimiter = fromIntegral $ C.ord '|'

delimiter :: ByteString
delimiter = "\n"

bsshow :: Show a => a -> ByteString
bsshow = fromString . show

-- Show the checksum value as a hexadecimal string
showChecksum :: Checksum -> String
showChecksum = show

-- Convert the checksum to a bytestring
bytestringChecksum :: Checksum -> ByteString
bytestringChecksum = BS8.pack . showChecksum

-- Build the checksum for the given information
mkAddressChecksum :: AccountName -> ChainId -> NetworkName -> Checksum
mkAddressChecksum acc cid net = hash $ name <> bsshow cid <> net0
  where
    name = encodeToLatin1 $ unAccountName acc
    net0 = encodeToLatin1 $ textNetworkName net

isValidKadenaAddress :: KadenaAddress -> Bool
isValidKadenaAddress ka =
  let checksum = mkAddressChecksum
        (_kadenaAddress_accountName ka)
        (_kadenaAddress_chainId ka)
        (_kadenaAddress_networkName ka)

      decoded = decodeKadenaAddress (_kadenaAddress_encoded ka)
  in
    _kadenaAddress_checksum ka == checksum && Right ka == decoded

encodeToLatin1 :: Text -> ByteString
encodeToLatin1 = BS8.pack . T.unpack

mkKadenaAddress
  :: NetworkName
  -> ChainId
  -> AccountName
  -> KadenaAddress
mkKadenaAddress network cid acc =
  let
    bcid = encodeToLatin1 $ cid ^. chainId
    name = encodeToLatin1 $ unAccountName acc
    net = encodeToLatin1 $ textNetworkName network
    checksum = mkAddressChecksum acc cid network

    encoded = Base64.encode $ mconcat $ intersperse delimiter
      [ name
      , bcid
      , net
      , bytestringChecksum checksum
      ]
  in
    KadenaAddress (name <> cons humanReadableDelimiter bcid <> cons humanReadableDelimiter encoded)
      acc
      cid
      network
      checksum

decodeKadenaAddress
  :: ByteString
  -> Either KadenaAddressError KadenaAddress
decodeKadenaAddress inp = do
  let hr = attemptHumanReadable
      mname = hr ^? _Right . _1
      mcid = hr ^? _Right . _2
      pkg0 = either id (^. _3) hr

  blob <- liftEither $ first Base64Error $ Base64.decode pkg0
  (name, cid, net, checksum) <- first (ParseError . T.pack) $ A.parseOnly parseKadenaAddressBlob blob

  let checkpiece og nm mbs
        | Just bs <- mbs, bs /= encodeToLatin1 og = throwError (InvalidHumanReadablePiece nm)
        | otherwise = pure ()

  -- If we have the human readable components, do an extra check that everything matches
  _ <- checkpiece (unAccountName name) "AccountName" mname
  _ <- checkpiece (cid ^. chainId) "Chain Id" mcid

  pure (KadenaAddress inp name cid net checksum)
  where
    attemptHumanReadable = case BS.split humanReadableDelimiter inp of
      [name, cid, pkg] -> Right (name, cid, pkg)
      _ -> Left inp

parseKadenaAddressBlob :: A.Parser (AccountName, ChainId, NetworkName, Checksum)
parseKadenaAddressBlob = do
  acc <- parseAccountName
  cid <- parseChainId
  net <- parseNetworkName
  let chk = mkAddressChecksum acc cid net
  void $ parseChecksum chk
  pure (acc, cid, net, chk)
  where
    mkParser :: A.Parser a -> (a -> Either String c) -> A.Parser c
    mkParser p c = (c <$> p) >>= either fail pure

    latin1ToEOL = A.manyTill (A.satisfy C.isLatin1) A.endOfLine

    parseAccountName = mkParser latin1ToEOL (first T.unpack . mkAccountName . T.pack)
    parseNetworkName = mkParser latin1ToEOL (networkNameParser . BS8.pack)
    networkNameParser = first T.unpack . mkNetworkName <=< parseURIToText

    parseChecksum expected = mkParser (A.string $ bytestringChecksum expected) pure

    parseURIToText bs = do
      uri <- first show $ URI.parseRelativeRef URI.strictURIParserOptions bs
      note "Missing Network Name" (getHost uri)

    parseChainId = mkParser
      (A.manyTill A.digit A.endOfLine)
      (note "Invalid ChainId" . pure . ChainId . T.pack)

getHost :: URI.URIRef a -> Maybe Text
getHost = preview (URI.pathL . to (TE.decodeUtf8With TE.lenientDecode))
