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
  , makeWallet
  -- * Flattening key pairs
  ) where


import           Control.Lens
import           Control.Monad.Fix
import           Data.Aeson
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Generics.Deriving.Monoid    (mappenddefault, memptydefault)
import           GHC.Generics                (Generic)
import           Language.Javascript.JSaddle (MonadJSM)
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
  , _walletCfg_setSigning :: Event t (KeyName, Bool)
  -- ^ Use a given key for signing messages/or not.
  , _walletCfg_delKey     :: Event t KeyName
  -- ^ Delete a key from your wallet.
  , _walletCfg_clearAll   :: Event t ()
  -- ^ Clear the signing state of all keys.
  }
  deriving Generic

makePactLenses ''WalletCfg

-- | HasWalletCfg with additional constraints to make it behave like a proper
-- "Config".
type IsWalletCfg cfg t = (HasWalletCfg cfg t, Monoid cfg, Flattenable cfg t)

data Wallet t = Wallet
  { _wallet_keys        :: Dynamic t KeyPairs
  , _wallet_signingKeys :: Dynamic t (Set KeyName)
  }
  deriving Generic

makePactLenses ''Wallet

makeWallet
  :: forall t m.
    ( MonadHold t m, PerformEvent t m
    , MonadFix m, MonadJSM (Performable m)
    , MonadJSM m
    )
  => WalletCfg t
  -> m (Wallet t)
makeWallet conf = do
    initialKeys <- fromMaybe Map.empty <$> loadKeys
    let
      onNewDeleted p = fmap fst . ffilter (p . snd)
      onGenKey = T.strip <$> conf ^. walletCfg_genKey
    signingKeys <- foldDyn id Set.empty
      $ leftmost [ const Set.empty <$ conf ^. walletCfg_clearAll
                 , Set.delete <$> onNewDeleted not (conf ^. walletCfg_setSigning)
                 , Set.insert <$> onNewDeleted id (conf ^. walletCfg_setSigning)
                 , Set.delete <$> conf ^. walletCfg_delKey
                 ]

    onNewKey <- performEvent $ createKey <$>
      -- Filter out keys with empty names
      ffilter (/= "") onGenKey

    keys <- foldDyn id initialKeys $
      -- Filter out duplicate keys:
      leftmost [ uncurry (Map.insertWith (flip const)) <$> onNewKey
               , Map.delete <$> _walletCfg_delKey conf
               ]

    performEvent_ $ storeKeys <$> updated keys

    pure $ Wallet
      { _wallet_keys = keys
      , _wallet_signingKeys = signingKeys
      }
  where
    -- TODO: Dummy implementation for now
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
storeKeys ks = do
    store <- getLocalStorage
    setItemStorage store storeWallet_Keys ks

-- | Load key pairs from localstorage.
loadKeys :: MonadJSM m => m (Maybe KeyPairs)
loadKeys = do
    store <- getLocalStorage
    getItemStorage store storeWallet_Keys


-- Utility functions:

instance Reflex t => Semigroup (WalletCfg t) where
  c1 <> c2 =
    WalletCfg
      { _walletCfg_genKey = leftmost [ _walletCfg_genKey c1
                                     , _walletCfg_genKey c2
                                     ]
      , _walletCfg_setSigning = leftmost [ _walletCfg_setSigning c1
                                         , _walletCfg_setSigning c2
                                         ]
      , _walletCfg_delKey = leftmost [ _walletCfg_delKey c1
                                     , _walletCfg_delKey c2
                                     ]
      , _walletCfg_clearAll = leftmost [ _walletCfg_clearAll c1
                                       , _walletCfg_clearAll c2
                                       ]
      }

instance Reflex t => Monoid (WalletCfg t) where
  mempty = WalletCfg never never never never
  mappend = (<>)

instance Flattenable (WalletCfg t) t where
  flattenWith doSwitch ev =
    WalletCfg
      <$> doSwitch never (_walletCfg_genKey <$> ev)
      <*> doSwitch never (_walletCfg_setSigning <$> ev)
      <*> doSwitch never (_walletCfg_delKey <$> ev)
      <*> doSwitch never (_walletCfg_clearAll <$> ev)

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
