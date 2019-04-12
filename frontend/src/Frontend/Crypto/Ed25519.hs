{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE ExtendedDefaultRules   #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoOverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
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
  -- * Utilities
  , keyToText
  , textToKey
  )
  where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Fail                 (MonadFail)
import           Control.Newtype.Generics           (Newtype (..))
import           Data.Aeson                         hiding (Object)
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Base16             as Base16
import           Data.Text                          (Text)
import qualified Data.Text.Encoding                 as T
import           GHC.Generics                       (Generic)
import           Language.Javascript.JSaddle

import Frontend.Foundation

-- | PublicKey with a Pact compatible JSON representation.
newtype PublicKey = PublicKey ByteString
  deriving (Generic, Show)
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



-- Utilities:

-- | Display key in Base16 format, as expected by Pact.
--
--   Despite the name, this function is also used for serializing signatures.
keyToText :: (Newtype key, O key ~ ByteString) => key -> Text
keyToText = safeDecodeUtf8 . Base16.encode . unpack

-- | Read a key in Base16 format, as exepcted by Pact.
--
--   Despite the name, this function is also used for reading signatures.
textToKey
  :: (Newtype key, O key ~ ByteString, Monad m, MonadFail m)
  => Text
  -> m key
textToKey = fmap pack . decodeBase16M . T.encodeUtf8

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


