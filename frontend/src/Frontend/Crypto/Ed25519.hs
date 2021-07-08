{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Crypto and keys needed for signing transactions.
module Frontend.Crypto.Ed25519
  ( -- * Types & Classes
    PublicKey(..)
  , PrivateKey(..)
  , Signature(..)
  , unverifiedUserSuppliedSignature
  , parseSignature
  -- * Creation
  -- , genKeyPair
  , deriveKeyPairFromPrivateKey
  -- * Verifying
  , verifySignature
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
  ----------------------
  , generateRoot
  , generateKeypair
  )
  where

import           Control.Lens hiding ((#))
import           Control.Monad
import           Control.Monad.Fail          (MonadFail)
import           Control.Newtype.Generics    (Newtype (..))
import           Data.Aeson                  hiding (Object)
import           Control.Monad.Except        (MonadError, throwError)
import qualified Data.Text as T
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import           Data.Text                   (Text)
import qualified Data.Text.Encoding          as T
import           GHC.Generics                (Generic)
import           Language.Javascript.JSaddle
import           Pact.Types.Util             (encodeBase64UrlUnpadded, decodeBase64UrlUnpadded)

import Common.Wallet
import Frontend.Foundation

import           GHCJS.Buffer                       (createFromArrayBuffer, freeze, toByteString)
import           Language.Javascript.JSaddle
import           Language.Javascript.JSaddle.Helper (mutableArrayBufferFromJSVal)
import           Language.Javascript.JSaddle.Types  (ghcjsPure)
import           Data.Bits ((.|.))

--
-- | PrivateKey with a Pact compatible JSON representation.
newtype PrivateKey = PrivateKey { unPrivateKey :: ByteString }
  deriving (Generic)
--
-- | Signature with a Pact compatible JSON representation.
newtype Signature = Signature { unSignature :: ByteString }
  deriving (Eq,Ord,Show,Generic)

unverifiedUserSuppliedSignature :: MonadFail m => Text -> m Signature
unverifiedUserSuppliedSignature = fmap Signature . decodeBase16M . T.encodeUtf8

-- | Parse just a public key with some sanity checks applied.
parseSignature :: MonadError Text m => Text -> m Signature
parseSignature = throwDecodingErr . textToKey <=< checkSig . T.strip

checkSig :: MonadError Text m => Text -> m Text
checkSig t =
    if len /= 128
      then throwError $ T.pack "Signature is not the right length"
      else pure t
  where
    len = T.length t

------------------------------------------
arrayBufToByteString :: JSVal -> JSM ByteString
arrayBufToByteString jsBuf = do
  mMutArrBuf <- catch (fmap Just $ mutableArrayBufferFromJSVal jsBuf) $
    \(_e::JSException) -> pure Nothing
  case mMutArrBuf of
    Nothing -> pure BS.empty
    Just mutArrBuf -> do
      mutBuf <- ghcjsPure $ createFromArrayBuffer mutArrBuf
      arrBuf <- freeze mutBuf
      ghcjsPure $ toByteString 0 Nothing arrBuf

generateRoot :: Text -> JSM (Maybe PrivateKey)
generateRoot phrase = do
  let jsPhrase = toJSString phrase
  lib <- jsg "lib"
  mRes <- catch (fmap Just $ lib # "kadenaMnemonicToRootKeypair" $ [ jsPhrase ])
    $ \(e::JSException) -> liftIO $ print e >> pure Nothing
  case mRes of
    Nothing -> pure Nothing
    Just res -> do
      res' <- res ^.js "buffer"
      Just . PrivateKey <$> arrayBufToByteString res'

-- TODO: Test empty bs, test various int values
generateKeypair :: PrivateKey -> Int -> JSM (Maybe (PrivateKey, PublicKey))
generateKeypair (PrivateKey root) idx = do
  rootBuf <- toJSVal $ BS.unpack root
  let idx' = fromIntegral $ 0x80000000 .|. idx
  idxJS <- toJSVal idx'
  lib <- jsg "lib"
  resRaw <- lib # "kadenaGenKeypair" $ [ rootBuf, idxJS ]
  res <- fromJSVal resRaw
  case res of
    Nothing -> pure Nothing
    Just (prvJS, pubJS) -> fmap Just $
      (\a b -> (PrivateKey a, PublicKey b))
        <$> arrayBufToByteString prvJS
        <*> arrayBufToByteString pubJS

sign :: ByteString -> ByteString -> JSM (ByteString)
sign msg prv = do
  msgBuf <- toJSVal $ BS.unpack msg
  prvBuf <- toJSVal $ BS.unpack prv
  lib <- jsg "lib"
  res <- lib # "kadenaSign" $ [ msgBuf, prvBuf]
  arrayBufToByteString res

verify :: ByteString -> ByteString -> ByteString -> JSM Bool
verify msg pub sig = do
  msgB <- toBuf msg
  pubB <- toBuf pub
  sigB <- toBuf sig
  lib <- jsg "lib"
  res <- lib # "kadenaVerify" $ [ msgB, pubB, sigB]
  --TODO: All JSVals are a bool, so make sure this isn't an exception or somethign
  valToBool res

  where toBuf = toJSVal . BS.unpack

genMnemonic :: MonadJSM m => m [Text]
genMnemonic = liftJSM $ do
  rawMnem <- eval "lib.kadenaGenMnemonic()"
  fmap (T.words . fromJSString) $ fromJSValUnchecked rawMnem

------------------------------------------

-- -- | Create a signature based on the given payload and `PrivateKey`.
verifySignature :: MonadJSM m => ByteString -> Signature -> PublicKey -> m Bool
verifySignature msg (Signature sig) (PublicKey key) = liftJSM $ verify msg key sig

-- | Create a signature based on the given payload and `PrivateKey`.
mkSignature :: MonadJSM m => ByteString -> PrivateKey -> m Signature
mkSignature msg (PrivateKey key) = liftJSM $ Signature <$> sign msg key

------------------------------------------
mkKeyPairFromJS :: MakeObject s => s -> JSM (PrivateKey, PublicKey)
mkKeyPairFromJS jsPair = do
  privKey <- fromJSValUnchecked =<< jsPair ^. js "secretKey"
  pubKey <- fromJSValUnchecked =<< jsPair ^. js "publicKey"
  pure ( PrivateKey . BS.pack $ privKey
       , PublicKey . BS.pack $ pubKey
       )

-- -- -- | Generate a `PublicKey`, `PrivateKey` keypair.
-- genKeyPair :: MonadJSM m => m (PrivateKey, PublicKey)
-- genKeyPair = liftJSM $ generateKeypair 



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

-- | Derive a keypair from the private key
deriveKeyPairFromPrivateKey :: MonadJSM m => ByteString -> m (PrivateKey, PublicKey)
deriveKeyPairFromPrivateKey privKeyBS = liftJSM $ do
  jsFrom <- eval "(function(k) {return window.nacl.sign.keyPair.fromSecretKey(Uint8Array.from(k));})"
  call jsFrom valNull [BS.unpack privKeyBS] >>= mkKeyPairFromJS

-- | Parse a private key, with some basic sanity checking.
parsePrivateKey :: MonadError Text m => PublicKey -> Text -> m (Maybe PrivateKey)
parsePrivateKey pubKey = throwDecodingErr . textToMayKey <=< throwWrongLengthPriv pubKey . T.strip

-- Utilities:

-- | Display key in Base64 format, as expected by some future Pact version (maybe).
--
--   Despite the name, this function is also used for serializing signatures.
keyToTextFuture :: (Newtype key, O key ~ ByteString) => key -> Text
keyToTextFuture = safeDecodeUtf8 . encodeBase64UrlUnpadded . unpack


-- | Read a key in Base64 format, as exepected by Pact in some future..? .
--
--   Despite the name, this function is also used for reading signatures.
textToKeyFuture
  :: (Newtype key, O key ~ ByteString, Monad m, MonadFail m)
  => Text
  -> m key
textToKeyFuture = fmap pack . decodeBase64M . T.encodeUtf8


-- Internal parsing helpers:
--

textToMayKey :: (Newtype key, O key ~ ByteString, MonadFail m) => Text -> m (Maybe key)
textToMayKey t =
  if T.null t
     then pure Nothing
     else Just <$> textToKey t

-- | Throw in case of invalid length, but accept zero length.
throwWrongLengthPriv :: MonadError Text m => PublicKey -> Text -> m Text
throwWrongLengthPriv pk t
  | T.null t = pure t
  | T.length t == 64 = do
    when (t == keyToText pk) $ throwError $ T.pack "Private key is the same as public key"
    pure $ t <> keyToText pk -- User entered a private key, append the public key
  | T.length t == 128 = pure t -- User entered a private+public key
  | otherwise = throwError $ T.pack "Key has unexpected length"


-- Boring instances:

instance ToJSON PrivateKey where
  toEncoding = toEncoding . keyToText
  toJSON = toJSON . keyToText

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

instance Newtype PrivateKey

instance Newtype Signature
