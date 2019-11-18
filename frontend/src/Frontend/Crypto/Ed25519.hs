{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE ExtendedDefaultRules   #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoOverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Crypto and keys needed for signing transactions.
module Frontend.Crypto.Ed25519
  ( -- * Types & Classes
    PublicKey
  , PrivateKey
  , Signature
  -- * Creation
  , genKeyPair
  -- * Signing
  , mkSignature
  -- * Parsing
  , parseKeyPair
  , parsePublicKey
  , parsePrivateKey
  -- * Utilities
  , keyToText
  , keyToTextFuture
  , textToKey
  , textToKeyFuture
  , fromPactPublicKey
  , toPactPublicKey
  , unsafePublicKey
  )
  where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Fail          (MonadFail)
import           Control.Newtype.Generics    (Newtype (..))
import           Data.Aeson                  hiding (Object)
import           Data.Aeson.Types            (toJSONKeyText)
import           Control.Monad.Except        (MonadError, throwError)
import qualified Data.Text as T
import qualified Data.ByteString.Base16 as Base16
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import           Data.Text                   (Text)
import qualified Data.Text.Encoding          as T
import           GHC.Generics                (Generic)
import           Language.Javascript.JSaddle (call, eval, fromJSValUnchecked,
                                              js, valNull)

import qualified Pact.Types.Term             as Pact
import           Pact.Types.Util             (encodeBase64UrlUnpadded, decodeBase64UrlUnpadded)

import           Frontend.Foundation

-- | PublicKey with a Pact compatible JSON representation.
newtype PublicKey = PublicKey ByteString
  deriving (Generic, Eq, Ord, Show)

-- | Input must be base16
unsafePublicKey :: ByteString -> PublicKey
unsafePublicKey = PublicKey

fromPactPublicKey :: Pact.PublicKey -> PublicKey
fromPactPublicKey = PublicKey . fst . Base16.decode . Pact._pubKey

toPactPublicKey :: PublicKey -> Pact.PublicKey
toPactPublicKey (PublicKey pk) = Pact.PublicKey $ Base16.encode pk

instance FromJSONKey PublicKey where
  fromJSONKey = PublicKey . fst . Base16.decode . T.encodeUtf8 <$> fromJSONKey
instance ToJSONKey PublicKey where
  toJSONKey = toJSONKeyText keyToText

--
-- | PrivateKey with a Pact compatible JSON representation.
newtype PrivateKey = PrivateKey ByteString
  deriving (Generic)
--
-- | Signature with a Pact compatible JSON representation.
newtype Signature = Signature ByteString
  deriving (Generic)

-- | Generate a `PublicKey`, `PrivateKey` keypair.
genKeyPair :: MonadJSM m => m (PrivateKey, PublicKey)
genKeyPair = liftJSM $ do
  jsPair <- eval "nacl.sign.keyPair()"
  privKey <- fromJSValUnchecked =<< jsPair ^. js "secretKey"
  pubKey <- fromJSValUnchecked =<< jsPair ^. js "publicKey"
  pure ( PrivateKey . BS.pack $ privKey
       , PublicKey . BS.pack $ pubKey
       )


-- | Create a signature based on the given payload and `PrivateKey`.
mkSignature :: MonadJSM m => ByteString -> PrivateKey -> m Signature
mkSignature msg (PrivateKey key) = liftJSM $ do
  jsSign <- eval "(function(m, k) {return window.nacl.sign.detached(Uint8Array.from(m), Uint8Array.from(k));})"
  jsSig <- call jsSign valNull [BS.unpack msg, BS.unpack key]
  Signature . BS.pack <$> fromJSValUnchecked jsSig
  {- pure $ Signature BS.empty -}


-- | Parse a private key with additional checks given the corresponding public key.
-- `parsePublicKey` and `parsePrivateKey`.
parseKeyPair :: MonadError Text m => PublicKey -> Text -> m (PublicKey, Maybe PrivateKey)
parseKeyPair pubKey priv = do
    privKey <- parsePrivateKey pubKey priv
    unless (sanityCheck pubKey privKey) $ do
      throwError $ T.pack "Private key is not compatible with public key"
    pure (pubKey, privKey)
  where
    sanityCheck (PublicKey pubRaw) = \case
      Nothing -> True
      Just (PrivateKey privRaw) -> BS.isSuffixOf pubRaw privRaw


-- | Parse just a public key with some sanity checks applied.
parsePublicKey :: MonadError Text m => Text -> m PublicKey
parsePublicKey = throwDecodingErr . textToKey <=< checkPub . T.strip

-- | Parse a private key, with some basic sanity checking.
parsePrivateKey :: MonadError Text m => PublicKey -> Text -> m (Maybe PrivateKey)
parsePrivateKey pubKey = throwDecodingErr . textToMayKey <=< throwWrongLengthPriv pubKey . T.strip


-- Utilities:

-- | Display key in Base64 format, as expected by some future Pact version (maybe).
--
--   Despite the name, this function is also used for serializing signatures.
keyToTextFuture :: (Newtype key, O key ~ ByteString) => key -> Text
keyToTextFuture = safeDecodeUtf8 . encodeBase64UrlUnpadded . unpack


-- | Display key in Base16 format, as expected by older Pact versions.
--
--   Despite the name, this function is also used for serializing signatures.
keyToText :: (Newtype key, O key ~ ByteString) => key -> Text
keyToText = T.decodeUtf8 . Base16.encode . unpack

-- | Read a key in Base64 format, as exepected by Pact in some future..? .
--
--   Despite the name, this function is also used for reading signatures.
textToKeyFuture
  :: (Newtype key, O key ~ ByteString, Monad m, MonadFail m)
  => Text
  -> m key
textToKeyFuture = fmap pack . decodeBase64M . T.encodeUtf8

-- | Read a key in Base16 format, as expected by older Pact versions.
--
--   Despite the name, this function is also used for reading signatures.
textToKey
  :: (Newtype key, O key ~ ByteString, Monad m, MonadFail m)
  => Text
  -> m key
textToKey = fmap pack . decodeBase16M . T.encodeUtf8


-- Internal parsing helpers:
--

textToMayKey :: (Newtype key, O key ~ ByteString, MonadFail m) => Text -> m (Maybe key)
textToMayKey t =
  if T.null t
     then pure Nothing
     else Just <$> textToKey t

throwDecodingErr
  :: MonadError Text m
  => Maybe v
  -> m v
throwDecodingErr = throwNothing $ T.pack "Invalid base16 encoding"
  where
    throwNothing err = maybe (throwError err) pure

checkPub :: MonadError Text m => Text -> m Text
checkPub t = void (throwEmpty t) >> throwWrongLength 64 t
  where
    throwEmpty k =
      if T.null k
         then throwError $ T.pack "Key must not be empty"
         else pure k

-- | Throw in case of invalid length, but accept zero length.
throwWrongLengthPriv :: MonadError Text m => PublicKey -> Text -> m Text
throwWrongLengthPriv pk t
  | T.null t = pure t
  | T.length t == 64 = do
    when (t == keyToText pk) $ throwError $ T.pack "Private key is the same as public key"
    pure $ t <> keyToText pk -- User entered a private key, append the public key
  | T.length t == 128 = pure t -- User entered a private+public key
  | otherwise = throwError $ T.pack "Key has unexpected length"

-- | Check length of string key representation.
throwWrongLength :: MonadError Text m => Int -> Text -> m Text
throwWrongLength should k =
  if T.length k /= should
     then throwError $ T.pack "Key has unexpected length"
     else pure k


-- Boring instances:

instance ToJSON PublicKey where
  toEncoding = toEncoding . keyToText
  toJSON = toJSON . keyToText

instance ToJSON PrivateKey where
  toEncoding = toEncoding . keyToText
  toJSON = toJSON . keyToText

instance FromJSON PublicKey where
  parseJSON = textToKey <=< parseJSON

instance FromJSON PrivateKey where
  parseJSON = fmap pack . decodeBase16M <=< fmap T.encodeUtf8 . parseJSON

instance ToJSON Signature where
  toEncoding = toEncoding . keyToText
  toJSON = toJSON . keyToText

instance FromJSON Signature where
  parseJSON = fmap pack . decodeBase16M <=< fmap T.encodeUtf8 . parseJSON

decodeBase64M :: (Monad m, MonadFail m) => ByteString -> m ByteString
decodeBase64M i =
  case decodeBase64UrlUnpadded i of
    Left err -> fail err
    Right v -> pure v

-- | Decode a Base16 value in a MonadFail monad and fail if there is input that
-- cannot be parsed.
decodeBase16M :: (Monad m, MonadFail m) => ByteString -> m ByteString
decodeBase16M i =
  let
    (r, rest) = Base16.decode i
  in
    if BS.null rest
       then pure r
       else fail "Input was no valid Base16 encoding."

instance Newtype PublicKey

instance Newtype PrivateKey

instance Newtype Signature
