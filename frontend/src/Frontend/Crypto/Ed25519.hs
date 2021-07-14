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
  -- * Mnemonic
  , genMnemonic
  -- * Creation
  , generateRoot
  , generateKeypair
  , toPublic
  -- * Verifying
  , verifySignature
  -- * Signing
  , mkSignature
  , mkSignatureLegacyJS
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

import           Control.Lens hiding ((#))
import           Control.Monad
import           Control.Monad.Fail          (MonadFail)
import           Control.Newtype.Generics    (Newtype (..))
import           Data.Aeson                  hiding (Object)
import qualified Data.Text as T
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import           Data.Text                   (Text)
import qualified Data.Text.Encoding          as T
import           GHC.Generics                (Generic)
import           Pact.Types.Util             (encodeBase64UrlUnpadded, decodeBase64UrlUnpadded)

import Common.Wallet
import Frontend.Foundation

import           GHCJS.Buffer                       (createFromArrayBuffer, freeze, toByteString)
import           Control.Exception.Base             (displayException)
import           Language.Javascript.JSaddle
import           Language.Javascript.JSaddle.Helper (mutableArrayBufferFromJSVal)
import           Language.Javascript.JSaddle.Types  (ghcjsPure)
import           Data.Bits ((.|.))
import           Frontend.Crypto.Signature
import           Frontend.Crypto.Password
import Control.Monad.Except
import Control.Monad.Catch

--
-- | PrivateKey with a Pact compatible JSON representation.
newtype PrivateKey = PrivateKey { unPrivateKey :: ByteString }
  deriving (Generic)

-- Boring instances:

instance ToJSON PrivateKey where
  toEncoding = toEncoding . keyToText
  toJSON = toJSON . keyToText

instance FromJSON PrivateKey where
  parseJSON = fmap pack . decodeBase16M <=< fmap T.encodeUtf8 . parseJSON

instance Newtype PrivateKey

-- Interface with JS

exceptionToText :: JSException -> Text
exceptionToText = T.pack . displayException

callJSFunction :: (MonadCatch m, MonadJSM m) => Text -> m (Either Text a) -> m (Either Text a)
callJSFunction funcName exec = catch exec $ handleException funcName
  where
    handleException :: (MonadJSM m) => Text -> JSException -> m (Either Text a)
    handleException fName e = do
      liftJSM $ liftIO $ print "HERE"
      pure $ Left $ mconcat
        [ T.pack "JSException in "
        , fName
        , T.pack ": "
        , exceptionToText e
        ]

-- Unhandled exception here
arrayBufToByteString :: (MonadJSM m) => JSVal -> m ByteString
arrayBufToByteString jsBuf = liftJSM $ do
  mutArrBuf <- mutableArrayBufferFromJSVal jsBuf
  mutBuf <- ghcjsPure $ createFromArrayBuffer mutArrBuf
  arrBuf <- freeze mutBuf
  ghcjsPure $ toByteString 0 Nothing arrBuf

generateRoot :: Password -> Text -> JSM (Either Text PrivateKey)
generateRoot (Password pwd) phrase = callJSFunction (T.pack "generateRoot") $ do
  let args = fmap toJSString [pwd, phrase]
  lib <- liftJSM $ jsg "lib"
  root <- liftJSM $ lib # "kadenaMnemonicToRootKeypair" $ args
  rootBuffer <- liftJSM $ root ^.js "buffer"
  fmap (Right . PrivateKey) $ arrayBufToByteString rootBuffer

generateKeypair :: Password -> PrivateKey -> Int -> JSM (Either Text (PrivateKey, PublicKey))
generateKeypair (Password pwd) (PrivateKey root) idx = callJSFunction (T.pack "generateKeypair") $ do
  let idx' = fromIntegral $ 0x80000000 .|. idx
  pwdJS <- toJSVal $ toJSString pwd
  idxJS <- toJSVal idx'
  rootBuf <- toJSVal $ BS.unpack root
  lib <- jsg "lib"
  resRaw <- lib # "kadenaGenKeypair" $ [ pwdJS, rootBuf, idxJS ]
  res <- fromJSVal resRaw
  maybe (pure $ Left $ T.pack "Error: marshaling keypair from js in generateKeypair failed")
    (fmap Right . jsPairToBS) $ res
  where
    jsPairToBS (prvJS, pubJS) = (\a b -> (PrivateKey a, PublicKey b))
      <$> arrayBufToByteString prvJS
      <*> arrayBufToByteString pubJS

toPublic :: PrivateKey -> JSM (Either Text PublicKey)
toPublic (PrivateKey pkey) = callJSFunction (T.pack "toPublic") $ do
  pkey' <- toBuf pkey
  lib <- jsg "lib"
  res <- lib # "kadenaGetPublic" $ [ pkey' ]
  Right . PublicKey <$> arrayBufToByteString res

sign :: Password -> ByteString -> PrivateKey -> JSM (Either Text ByteString)
sign (Password pwd) msg (PrivateKey prv) = callJSFunction (T.pack "sign") $ do
  pwdJS <- toJSVal $ toJSString pwd
  msgBuf <- toJSVal $ BS.unpack msg
  prvBuf <- toJSVal $ BS.unpack prv
  lib <- jsg "lib"
  res <- lib # "kadenaSign" $ [ pwdJS, msgBuf, prvBuf]
  Right <$> arrayBufToByteString res

toBuf :: ByteString -> JSM JSVal
toBuf = toJSVal . BS.unpack

verify :: ByteString -> ByteString -> ByteString -> JSM Bool
verify msg pub sig = do
  args <- mapM toBuf [msg, pub, sig]
  lib <- jsg "lib"
  res <- lib # "kadenaVerify" $ args
  --TODO: All JSVals are a bool, so make sure this isn't an exception or somethign
  valToBool res

genMnemonic :: MonadJSM m => m [Text]
genMnemonic = liftJSM $ do
  rawMnem <- eval "lib.kadenaGenMnemonic()"
  fmap (T.words . fromJSString) $ fromJSValUnchecked rawMnem

changePassword :: PrivateKey -> Password -> Password -> JSM (Either Text PrivateKey)
changePassword (PrivateKey oldKey) (Password oldPwd) (Password newPwd) =
  callJSFunction (T.pack "changePassword") $ do
    oldKey' <- toBuf oldKey
    [oldPwd', newPwd'] <- mapM (toJSVal . toJSString) [oldPwd, newPwd]
    lib <- jsg "lib"
    res <- fromJSVal =<< (lib # "kadenaChangePassword" $ [oldKey', oldPwd', newPwd' ])
    case res of
      Nothing -> pure $ Left $ T.pack "error: marshalling to arrayBuffer in changePassword failed"
      Just key -> Right . PrivateKey <$> arrayBufToByteString key

-- -- | Create a signature based on the given payload and `PrivateKey`.
verifySignature :: MonadJSM m => ByteString -> Signature -> PublicKey -> m Bool
verifySignature msg (Signature sig) (PublicKey key) = liftJSM $ verify msg key sig

-- | Create a signature based on the given payload and `PrivateKey`.
mkSignature :: MonadJSM m => Password -> ByteString -> PrivateKey -> m (Either Text Signature)
mkSignature pwd msg key = liftJSM $ (fmap . fmap) Signature $ sign pwd msg key

mkSignatureLegacyJS :: MonadJSM m => ByteString -> PrivateKey -> m Signature
mkSignatureLegacyJS msg (PrivateKey key) = liftJSM $ do
 jsSign <- eval "(function(m, k) {return window.nacl.sign.detached(Uint8Array.from(m), Uint8Array.from(k));})"
 jsSig <- call jsSign valNull [BS.unpack msg, BS.unpack key]
 Signature . BS.pack <$> fromJSValUnchecked jsSig

mkKeyPairFromJS :: MakeObject s => s -> JSM (PrivateKey, PublicKey)
mkKeyPairFromJS jsPair = do
  privKey <- fromJSValUnchecked =<< jsPair ^. js "secretKey"
  pubKey <- fromJSValUnchecked =<< jsPair ^. js "publicKey"
  pure ( PrivateKey . BS.pack $ privKey
       , PublicKey . BS.pack $ pubKey
       )

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
  where
    decodeBase64M :: (Monad m, MonadFail m) => ByteString -> m ByteString
    decodeBase64M i =
      case decodeBase64UrlUnpadded i of
        Left err -> fail err
        Right v -> pure v


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


