module Frontend.Crypto.Signature where

import           Control.Monad               ((<=<))
import           Control.Monad.Fail          (MonadFail)
import           Control.Monad.Except        (MonadError, throwError)
import           Control.Newtype.Generics    (Newtype (..))
import           Data.Aeson                  hiding (Object)
import           Data.ByteString (ByteString)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           GHC.Generics                (Generic)

import           Common.Wallet

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

instance ToJSON Signature where
  toEncoding = toEncoding . keyToText
  toJSON = toJSON . keyToText

instance FromJSON Signature where
  parseJSON = fmap pack . decodeBase16M <=< fmap T.encodeUtf8 . parseJSON

instance Newtype Signature
