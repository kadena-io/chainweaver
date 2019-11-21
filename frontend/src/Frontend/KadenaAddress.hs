{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
module Frontend.KadenaAddress
  ( KadenaAddressError (..)
  , KadenaAddressPrefix (..)
  , AccountCreated (..)
  , KadenaAddress
    ( _kadenaAddress_accountName
    , _kadenaAddress_accountCreated
    , _kadenaAddress_chainId
    , _kadenaAddress_checksum
    )
  , mkKadenaAddress
  , decodeKadenaAddress
  , decodeKadenaAddressText
  , encodeKadenaAddress
  , textKadenaAddress
  , bytestringChecksum
  , mkAddressChecksum
  , showChecksum
  , parseKadenaAddressBlob
  , delimiter
  , humanReadableDelimiter
  , isValidKadenaAddress
  ) where

import Control.Lens
import Control.Monad.Except (MonadError (..), liftEither)
import Control.Monad (guard)
import Control.Error (note,hush)
import Data.Functor (void)
import Data.List (intersperse)
import qualified Data.Char as C
import Data.Word (Word8)
import Data.String (fromString)
import Data.Bifunctor (first)
import Text.Printf (printf)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE

import qualified Data.Attoparsec.ByteString.Char8 as A

import qualified Data.ByteString.Base64 as Base64

import qualified Data.Digest.CRC32 as CRC32

import Pact.Types.ChainId
import Kadena.SigningApi (AccountName(..), mkAccountName)

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

data KadenaAddressPrefix
  = KadenaAddressPrefix_Keep
  | KadenaAddressPrefix_None
  deriving (Show, Eq)

data AccountCreated
  = AccountCreated_Yes
  | AccountCreated_No
  deriving (Show, Eq)

data KadenaAddress = KadenaAddress
  { _kadenaAddress_accountCreated :: AccountCreated
    -- ^ Indicates the type of account, vanity or wallet only
  , _kadenaAddress_accountName :: AccountName
    -- ^ The account name associated with this address
  , _kadenaAddress_chainId :: ChainId
    -- ^ The chain where this account resides
  , _kadenaAddress_checksum :: Checksum
    -- ^ The checksum of the AccountName, ChainId, and NetworkName
  }
  deriving (Show, Eq)

type Checksum = CRC32.CRC32

textKadenaAddress :: KadenaAddress -> Text
textKadenaAddress = TE.decodeUtf8With TE.lenientDecode . encodeKadenaAddress

humanReadableDelimiter :: Word8
humanReadableDelimiter = fromIntegral $ C.ord '|'

delimiter :: ByteString
delimiter = "\n"

bsshow :: Show a => a -> ByteString
bsshow = fromString . show

-- Show the checksum value as a hexadecimal string
showChecksum :: Checksum -> String
showChecksum = printf "%x" . CRC32.crc32

-- Convert the checksum to a bytestring
bytestringChecksum :: Checksum -> ByteString
bytestringChecksum = BS8.pack . showChecksum

-- Build the checksum for the given information
mkAddressChecksum :: AccountCreated -> AccountName -> ChainId -> Checksum
mkAddressChecksum acctype acc cid = CRC32.digest $ acctype0 <> name <> bsshow cid
  where
    name = encodeToLatin1 $ unAccountName acc
    acctype0 = encodeAccountCreated acctype

isValidKadenaAddress :: KadenaAddress -> Bool
isValidKadenaAddress ka =
  let checksum = mkAddressChecksum
        (_kadenaAddress_accountCreated ka)
        (_kadenaAddress_accountName ka)
        (_kadenaAddress_chainId ka)

      decoded = decodeKadenaAddress (encodeKadenaAddress ka)
  in
    _kadenaAddress_checksum ka == checksum && Right ka == decoded

encodeToLatin1 :: Text -> ByteString
encodeToLatin1 = BS8.pack . T.unpack

encodeAccountCreated :: AccountCreated -> ByteString
encodeAccountCreated AccountCreated_No = "n"
encodeAccountCreated AccountCreated_Yes = "y"

encodeKadenaAddress :: KadenaAddress -> ByteString
encodeKadenaAddress ka =
  let
    cid = encodeToLatin1 $ _kadenaAddress_chainId ka ^. chainId
    name = encodeToLatin1 $ unAccountName $ _kadenaAddress_accountName ka
    checksum = bytestringChecksum $ _kadenaAddress_checksum ka

    encoded = Base64.encode $ mconcat $ intersperse delimiter
      [ encodeAccountCreated $ _kadenaAddress_accountCreated ka
      , name
      , cid
      , checksum
      ]
  in
    case _kadenaAddress_accountCreated ka of
      AccountCreated_Yes -> name <> cons humanReadableDelimiter cid <> cons humanReadableDelimiter encoded
      AccountCreated_No -> name <> cons humanReadableDelimiter cid <> cons humanReadableDelimiter checksum

mkKadenaAddress
  :: AccountCreated
  -> ChainId
  -> AccountName
  -> KadenaAddress
mkKadenaAddress acctype cid acc =
  KadenaAddress acctype acc cid (mkAddressChecksum acctype acc cid)

decodeKadenaAddressText :: Text -> Either KadenaAddressError KadenaAddress
decodeKadenaAddressText = decodeKadenaAddress . TE.encodeUtf8

decodeKadenaAddress
  :: ByteString
  -> Either KadenaAddressError KadenaAddress
decodeKadenaAddress inp = do
  let
    hr = attemptHumanReadable
    mname = hr ^? _Right . _1
    mcid = hr ^? _Right . _2
    pkg0 = either id (^. _3) hr

    uncreatedAccount = do
      n <- hush . mkAccountName . TE.decodeLatin1 =<< mname
      c <- ChainId . TE.decodeLatin1 <$> mcid
      let cksum = mkAddressChecksum AccountCreated_No n c
      guard $ bytestringChecksum cksum == pkg0
      pure $ KadenaAddress AccountCreated_No n c cksum

  case uncreatedAccount of
    Just acc -> pure acc
    Nothing -> do
      blob <- liftEither $ first Base64Error $ Base64.decode pkg0
      (acctype, name, cid, checksum) <- first (ParseError . T.pack) $ A.parseOnly parseKadenaAddressBlob blob

      let checkpiece og nm mbs
            | Just bs <- mbs, bs /= encodeToLatin1 og = throwError (InvalidHumanReadablePiece nm)
            | otherwise = pure ()

      -- If we have the human readable components, do an extra check that everything matches
      _ <- checkpiece (unAccountName name) "AccountName" mname
      _ <- checkpiece (cid ^. chainId) "Chain Id" mcid

      pure (KadenaAddress acctype name cid checksum)
  where
    attemptHumanReadable = case BS.split humanReadableDelimiter inp of
      [name, cid, pkg] -> Right (name, cid, pkg)
      _ -> Left inp

parseKadenaAddressBlob :: A.Parser (AccountCreated, AccountName, ChainId, Checksum)
parseKadenaAddressBlob = do
  acctype <- parseAccountCreated
  acc <- parseAccountName
  cid <- parseChainId
  let chk = mkAddressChecksum acctype acc cid
  void $ parseChecksum chk
  pure (acctype, acc, cid, chk)
  where
    mkParser :: A.Parser a -> (a -> Either String c) -> A.Parser c
    mkParser p c = (c <$> p) >>= either fail pure

    latin1ToEOL = A.manyTill (A.satisfy C.isLatin1) A.endOfLine

    parseAccountName = mkParser latin1ToEOL (first T.unpack . mkAccountName . T.pack)
    parseChecksum expected = mkParser (A.string $ bytestringChecksum expected) pure

    parseAccountCreated = mkParser (A.letter_ascii <* A.endOfLine) $ \case
      'y' -> Right AccountCreated_Yes
      'n' -> Right AccountCreated_No
      _ -> Left "Unknown Account Type"

    parseChainId = mkParser
      (A.manyTill A.digit A.endOfLine)
      (note "Invalid ChainId" . pure . ChainId . T.pack)
