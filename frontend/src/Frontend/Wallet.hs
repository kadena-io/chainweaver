{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ConstraintKinds           #-}

module Frontend.Wallet
  (  -- * Types & Classes
    KeyName
  , PublicKey
  , PrivateKey
  , KeyPair (..)
  , KeyPairs
  , WalletCfg (..)
  , HasWalletCfg (..)
  , IsWalletCfg
  , Wallet (..)
  , HasWallet (..)
  -- * Creation
  , emptyWallet
  , makeWallet
  , isPredefinedKey
  , chainwebDefaultSenders
  -- * Flattening key pairs
  ) where


import           Control.Lens
import           Control.Monad.Fix
import           Control.Newtype.Generics           (Newtype (..))
import           Data.ByteString                    (ByteString)
import           Data.Aeson
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Generics.Deriving.Monoid    (mappenddefault, memptydefault)
import           GHC.Generics                (Generic)
import           Reflex

import           Frontend.Crypto.Ed25519
import           Frontend.Foundation
import           Frontend.Storage

-- | Type of a `Key` name.
--
--   All keys are accessible by a name of type `KeyName`
type KeyName = Text

-- | A key consists of a public key and an optional private key.
--
data KeyPair = KeyPair
  { _keyPair_publicKey  :: PublicKey
  , _keyPair_privateKey :: Maybe PrivateKey
  } deriving Generic

makePactLenses ''KeyPair

-- | `KeyName` to `Key` mapping
type KeyPairs = Map KeyName KeyPair

data WalletCfg t = WalletCfg
  { _walletCfg_genKey     :: Event t KeyName
  -- ^ Request generation of a new key, that will be named as specified.
  , _walletCfg_delKey     :: Event t KeyName
  -- ^ Delete a key from your wallet.
  }
  deriving Generic

makePactLenses ''WalletCfg

-- | HasWalletCfg with additional constraints to make it behave like a proper
-- "Config".
type IsWalletCfg cfg t = (HasWalletCfg cfg t, Monoid cfg, Flattenable cfg t)

data Wallet t = Wallet
  { _wallet_keys        :: Dynamic t KeyPairs
    -- ^ Keys added and removed by the user and chainwebDefaultSenders which can't be removed.
  }
  deriving Generic

makePactLenses ''Wallet

-- | Pre-defined senders in chainweb. This is used for testnet, later on we
-- will some kind of a real wallet in order to manage real senders.
chainwebDefaultSenders :: KeyPairs
chainwebDefaultSenders = Map.fromList
  [ ( "sender00"
    , KeyPair
      { _keyPair_publicKey = textToKeyThrow "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"
      , _keyPair_privateKey = Just $ textToKeyThrow "251a920c403ae8c8f65f59142316af3c82b631fba46ddea92ee8c95035bd2898368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"
      }
    )
  , ( "sender01"
    , KeyPair
      { _keyPair_publicKey = textToKeyThrow "6be2f485a7af75fedb4b7f153a903f7e6000ca4aa501179c91a2450b777bd2a7"
      , _keyPair_privateKey = Just $ textToKeyThrow "2beae45b29e850e6b1882ae245b0bab7d0689ebdd0cd777d4314d24d7024b4f76be2f485a7af75fedb4b7f153a903f7e6000ca4aa501179c91a2450b777bd2a7"
      }
    )
  , ( "sender02"
    , KeyPair
      { _keyPair_publicKey = textToKeyThrow "3a9dd532d73dace195dbb64d1dba6572fb783d0fdd324685e32fbda2f89f99a6"
      , _keyPair_privateKey = Just $ textToKeyThrow "9b54e924f7acdb03ad4e471308f9a512dac26a50398b41cab8bfe7a496804dbd3a9dd532d73dace195dbb64d1dba6572fb783d0fdd324685e32fbda2f89f99a6"
      }
    )
  , ( "sender03"
    , KeyPair
      { _keyPair_publicKey = textToKeyThrow "43f2adb1de192000cb3777bacc7f983b6614fd9c1715cd44cd484b6d3a0d34c8"
      , _keyPair_privateKey = Just $ textToKeyThrow "a477c755abcaaac7bc9aadfc616c0cb7a8a9df9e15adc168e39c247c648544ac43f2adb1de192000cb3777bacc7f983b6614fd9c1715cd44cd484b6d3a0d34c8"
      }
    )
  , ( "sender04"
    , KeyPair
      { _keyPair_publicKey = textToKeyThrow "2d70aa4f697c3a3b8dd6d97745ac074edcfd0eb65c37774cde25135483bea71e"
      , _keyPair_privateKey = Just $ textToKeyThrow "1685ef1cb774a517fdfd5c08cf1ae898084d8ad39b04717a4b5b27c2a3190a752d70aa4f697c3a3b8dd6d97745ac074edcfd0eb65c37774cde25135483bea71e"
      }
    )
  , ( "sender05"
    , KeyPair
      { _keyPair_publicKey = textToKeyThrow "f09d8f6394aea425fe6783d88cd81363d8017f16afd3711c575be0f5cd5c9bb9"
      , _keyPair_privateKey = Just $ textToKeyThrow "2505b87dca474955597fed38178df9e66b16b0c8dc140817c0ad75bd14be32d4f09d8f6394aea425fe6783d88cd81363d8017f16afd3711c575be0f5cd5c9bb9"
      }
    )
  , ( "sender06"
    , KeyPair
      { _keyPair_publicKey = textToKeyThrow "5ffc1f7fef7a44738625762f75a4229454951e03f2afc6f81309c0c1bdf9ee6f"
      , _keyPair_privateKey = Just $ textToKeyThrow "c2873d7226845228a6422d15599aa9371a6cd105d29f6157c07d4e9f5b5d7a3c5ffc1f7fef7a44738625762f75a4229454951e03f2afc6f81309c0c1bdf9ee6f"
      }
    )
  , ( "sender07"
    , KeyPair
      { _keyPair_publicKey = textToKeyThrow "4c31dc9ee7f24177f78b6f518012a208326e2af1f37bb0a2405b5056d0cad628"
      , _keyPair_privateKey = Just $ textToKeyThrow "f1c1923e49cb23d15fe45bdc3f65d7fc1d031ce50dd81bb5085bdd2c63364d7f4c31dc9ee7f24177f78b6f518012a208326e2af1f37bb0a2405b5056d0cad628"
      }
    )
  , ( "sender08"
    , KeyPair
      { _keyPair_publicKey = textToKeyThrow "63b2eba4ed70d4612d3e7bc90db2fbf4c76f7b074363e86d73f0bc617f8e8b81"
      , _keyPair_privateKey = Just $ textToKeyThrow "e0018d4e39736c9c22400cdd88ef60f0ca203466bb27b8435a72e0868289690d63b2eba4ed70d4612d3e7bc90db2fbf4c76f7b074363e86d73f0bc617f8e8b81"
      }
    )
  , ( "sender09"
    , KeyPair
      { _keyPair_publicKey = textToKeyThrow "c59d9840b0b66090836546b7eb4a73606257527ec8c2b482300fd229264b07e6"
      , _keyPair_privateKey = Just $ textToKeyThrow "adbe3793a0daf70c7e7a5d59349e0f51d928178de55c6328302ef5b628ed448bc59d9840b0b66090836546b7eb4a73606257527ec8c2b482300fd229264b07e6"
      }
    )
  ]

textToKeyThrow
  :: (Newtype key, O key ~ ByteString)
  => Text
  -> key
textToKeyThrow = fromMaybe (error "Predefined key was invalid!") . textToKey

-- | Check whether a KeyName is in `chainwebDefaultSenders`.
--   The user should not be able to rename or remove those.
isPredefinedKey :: KeyName -> Bool
isPredefinedKey = flip Map.member chainwebDefaultSenders

-- | An empty wallet that will never contain any keys.
emptyWallet :: Reflex t => Wallet t
emptyWallet = Wallet $ pure mempty

-- | Make a functional wallet that can contain actual keys.
makeWallet
  :: forall t m.
    ( MonadHold t m, PerformEvent t m
    , MonadFix m, MonadJSM (Performable m)
    , MonadJSM m
    )
  => WalletCfg t
  -> m (Wallet t)
makeWallet conf = do
    initialKeys <- (<> chainwebDefaultSenders) . fromMaybe Map.empty <$> loadKeys
    let
      onGenKey = T.strip <$> conf ^. walletCfg_genKey
      onDelKey = ffilter (not . isPredefinedKey) $ _walletCfg_delKey conf

    onNewKey <- performEvent $ createKey <$>
      -- Filter out keys with empty names
      ffilter (\x -> x /= "" && not (isPredefinedKey x)) onGenKey

    keys <- foldDyn id initialKeys $
      -- Filter out duplicate keys:
      leftmost [ uncurry (Map.insertWith (flip const)) <$> onNewKey
               , Map.delete <$> onDelKey
               ]

    performEvent_ $ storeKeys <$> updated keys

    pure $ Wallet
      { _wallet_keys = keys
      }
  where
    createKey :: KeyName -> Performable m (KeyName, KeyPair)
    createKey n = do
      (privKey, pubKey) <- genKeyPair
      pure (n, KeyPair pubKey (Just privKey))


-- Storing data:

-- | Storage keys for referencing data to be stored/retrieved.
data StoreWallet a
  = StoreWallet_Keys
  deriving (Generic, Show)


--  GADT did not work with `Generic` deriving last time I checked.
storeWallet_Keys :: StoreWallet KeyPairs
storeWallet_Keys = StoreWallet_Keys

-- | Write key pairs to localstorage.
storeKeys :: MonadJSM m => KeyPairs -> m ()
storeKeys ks = setItemStorage localStorage storeWallet_Keys (ks Map.\\ chainwebDefaultSenders)

-- | Load key pairs from localstorage.
loadKeys :: MonadJSM m => m (Maybe KeyPairs)
loadKeys = getItemStorage localStorage storeWallet_Keys


-- Utility functions:

instance Reflex t => Semigroup (WalletCfg t) where
  c1 <> c2 =
    WalletCfg
      { _walletCfg_genKey = leftmost [ _walletCfg_genKey c1
                                     , _walletCfg_genKey c2
                                     ]
      , _walletCfg_delKey = leftmost [ _walletCfg_delKey c1
                                     , _walletCfg_delKey c2
                                     ]
      }

instance Reflex t => Monoid (WalletCfg t) where
  mempty = WalletCfg never never
  mappend = (<>)

instance Flattenable (WalletCfg t) t where
  flattenWith doSwitch ev =
    WalletCfg
      <$> doSwitch never (_walletCfg_genKey <$> ev)
      <*> doSwitch never (_walletCfg_delKey <$> ev)

instance Reflex t => Semigroup (Wallet t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (Wallet t) where
  mempty = memptydefault
  mappend = (<>)

instance ToJSON KeyPair where
  -- Yeah aeson serialization changes bit me too once. But I guess this is fine for a testnet?
  toEncoding = genericToEncoding defaultOptions

instance FromJSON KeyPair

instance ToJSON (StoreWallet a) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON (StoreWallet a)
