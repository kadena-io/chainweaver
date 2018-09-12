{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies           #-}

module Frontend.Wallet
  (  -- * Types & Classes
    KeyName
  , PublicKey
  , PrivateKey
  , KeyPair (..)
  , HasKeyPair (..)
  , KeyPairs
  , WalletCfg (..)
  , HasWalletCfg (..)
  , Wallet (..)
  , HasWallet (..)
  -- * Creation
  , makeWallet
  -- * Utility functions
  , keyToText
  ) where


import           Control.Lens
import           Control.Monad
import           Control.Monad.Fail       (MonadFail)
import           Control.Monad.Fix
import           Control.Newtype          (Newtype (..))
import           Data.Aeson
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Base16   as Base16
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Semigroup
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text.Encoding       as T
import           Generics.Deriving.Monoid (mappenddefault, memptydefault)
import           GHC.Generics             (Generic)
import           Reflex

import           Frontend.Foundation

-- | Type of a `Key` name.
--
--   All keys are accessible by a name of type `KeyName`
type KeyName = Text

-- | Type of a `PublicKey` in a `KeyPair`.
newtype PublicKey = PublicKey ByteString
  deriving (Generic, Show)
--
-- | Type of a `PrivateKey` in a `KeyPair`.
newtype PrivateKey = PrivateKey ByteString
  deriving (Generic, Show)

-- | A key consists of a public key and an optional private key.
data KeyPair t = KeyPair
  { _keyPair_publicKey  :: PublicKey
  , _keyPair_privateKey :: Maybe PrivateKey
  , _keyPair_forSigning :: Dynamic t Bool
  }

makePactLenses ''KeyPair

-- | `KeyName` to `Key` mapping
type KeyPairs t = Map KeyName (KeyPair t)

data WalletCfg t = WalletCfg
  { _walletCfg_onRequestNewKey :: Event t KeyName
  -- ^ Request generation of a new key, that will be named as specified.
  , _walletCfg_onSetSigning    :: Event t (KeyName, Bool)
  -- ^ Use a given key for signing messages/or not.
  }
  deriving Generic

makePactLenses ''WalletCfg

data Wallet t = Wallet
  { _wallet_keys        :: Dynamic t (KeyPairs t)
  }
  deriving Generic

makePactLenses ''Wallet

makeWallet
  :: forall t m. (MonadHold t m, PerformEvent t m, MonadFix m)
  => WalletCfg t
  -> m (Wallet t)
makeWallet conf = do
    let onNewDeleted p = fmap fst . ffilter (p . snd)
    signingKeys <- foldDyn id Set.empty
      $ leftmost [ Set.delete <$> onNewDeleted not (conf ^. walletCfg_onSetSigning)
                 , Set.insert <$> onNewDeleted id (conf ^. walletCfg_onSetSigning)
                 , Set.insert <$> conf ^. walletCfg_onRequestNewKey
                 ]

    onNewKey <- performEvent $ createKey signingKeys <$>
      -- Filter out keys with empty names
      ffilter (/= "") (conf ^. walletCfg_onRequestNewKey)

    -- Filter out duplicate keys
    keys <- foldDyn (uncurry (Map.insertWith (flip const))) Map.empty onNewKey
    pure $ Wallet
      { _wallet_keys = keys
      }
  where
    -- TODO: Dummy implementation for now
    createKey :: Dynamic t (Set KeyName) -> KeyName -> Performable m (KeyName, KeyPair t)
    createKey signing n = do
      pure (n, KeyPair (makeKeyDummy n) (Just (makeKeyDummy n)) (Set.member n <$> signing))

    makeKeyDummy :: forall key. (Newtype key, O key ~ ByteString) => Text -> key
    makeKeyDummy = pack . T.encodeUtf8


-- Utility functions:

-- | Display key in Base16 format, as expected by Pact.
keyToText :: (Newtype key, O key ~ ByteString) => key -> Text
keyToText = T.decodeUtf8 . Base16.encode . unpack

-- Boring instances:

instance ToJSON PublicKey where
  toEncoding = toEncoding . keyToText
  toJSON = toJSON . keyToText

instance ToJSON PrivateKey where
  toEncoding = toEncoding . keyToText
  toJSON = toJSON . keyToText

instance FromJSON PublicKey where
  parseJSON = fmap pack . decodeBase16M <=< fmap T.encodeUtf8 . parseJSON

instance FromJSON PrivateKey where
  parseJSON = fmap pack . decodeBase16M <=< fmap T.encodeUtf8 . parseJSON

-- | Decode a Base16 value in a MonadFail monad and fail if there is input that
-- cannot be parsed.
decodeBase16M :: (Monad m, MonadFail m) => ByteString -> m ByteString
decodeBase16M i =
  let
    (r, rest) = Base16.decode i
  in
    if BS.null rest
       then fail "Input was no valid Base16 encoding."
       else pure r

instance Newtype PublicKey

instance Newtype PrivateKey


instance Reflex t => Semigroup (WalletCfg t) where
  c1 <> c2 =
    WalletCfg
      { _walletCfg_onRequestNewKey = leftmost [ _walletCfg_onRequestNewKey c1
                                              , _walletCfg_onRequestNewKey c2
                                              ]
      , _walletCfg_onSetSigning = leftmost [ _walletCfg_onSetSigning c1
                                           , _walletCfg_onSetSigning c2
                                           ]
      }

instance Reflex t => Monoid (WalletCfg t) where
  mempty = WalletCfg never never
  mappend = (<>)

instance Reflex t => Semigroup (Wallet t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (Wallet t) where
  mempty = memptydefault
  mappend = (<>)
