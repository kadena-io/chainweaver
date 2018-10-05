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

module Frontend.Wallet
  (  -- * Types & Classes
    KeyName
  , PublicKey
  , PrivateKey
  , KeyPairV (..)
  , HasKeyPairV (..)
  , KeyPair
  , DynKeyPair
  , KeyPairs
  , DynKeyPairs
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
import           Control.Monad.Fix
import           Data.Aeson
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Semigroup
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
data KeyPairV f = KeyPair
  { _keyPair_publicKey  :: PublicKey
  , _keyPair_privateKey :: Maybe PrivateKey
  , _keyPair_forSigning :: ReflexValue f Bool
  } deriving Generic

makePactLenses ''KeyPairV
--
-- | Variant of `KeyPairV` with fixed values.
type KeyPair = KeyPairV Identity

-- | Variant of `KeyPairV` with `Dynamic` values.
type DynKeyPair t = KeyPairV (Dynamic t)

-- | `KeyName` to `Key` mapping
type KeyPairs = Map KeyName KeyPair

-- | `KeyName` to `Key` mapping, dynamic variant.
type DynKeyPairs t = Map KeyName (DynKeyPair t)

data WalletCfg t = WalletCfg
  { _walletCfg_genKey     :: Event t KeyName
  -- ^ Request generation of a new key, that will be named as specified.
  , _walletCfg_setSigning :: Event t (KeyName, Bool)
  -- ^ Use a given key for signing messages/or not.
  , _walletCfg_delKey     :: Event t KeyName
  -- ^ Delete a key from your wallet.
  }
  deriving Generic

makePactLenses ''WalletCfg

data Wallet t = Wallet
  { _wallet_keys        :: Dynamic t (DynKeyPairs t)
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
      initialSigning =
        Set.fromList
        . map fst . filter (_keyPair_forSigning . snd)
        . Map.toList
        $ initialKeys

      onNewDeleted p = fmap fst . ffilter (p . snd)

      onGenKey = T.strip <$> conf ^. walletCfg_genKey
    signingKeys <- foldDyn id initialSigning
      $ leftmost [ Set.delete <$> onNewDeleted not (conf ^. walletCfg_setSigning)
                 , Set.insert <$> onNewDeleted id (conf ^. walletCfg_setSigning)
                 , Set.insert <$> onGenKey
                 , Set.delete <$> conf ^. walletCfg_delKey
                 ]

    onNewKey <- performEvent $ createKey signingKeys <$>
      -- Filter out keys with empty names
      ffilter (/= "") onGenKey

    let
      initialDynKeys = toDynKeyPairs signingKeys initialKeys

    keys <- foldDyn id initialDynKeys $
      -- Filter out duplicate keys:
      leftmost [ uncurry (Map.insertWith (flip const)) <$> onNewKey
               , Map.delete <$> _walletCfg_delKey conf
               ]

    performEvent_ $ storeKeys <$> updated (joinKeyPairs keys)

    pure $ Wallet
      { _wallet_keys = keys
      }
  where
    -- TODO: Dummy implementation for now
    createKey :: Dynamic t (Set KeyName) -> KeyName -> Performable m (KeyName, DynKeyPair t)
    createKey signing n = do
      (privKey, pubKey) <- genKeyPair
      pure (n, KeyPair pubKey (Just privKey) (Set.member n <$> signing))


-- Utilities

-- | Join `Dynamic` `DynKeyPairs` to a simple `Dynamic` `KeyPairs`.
joinKeyPairs :: Reflex t => Dynamic t (DynKeyPairs t) -> Dynamic t KeyPairs
joinKeyPairs = joinDynThroughMap . fmap (fmap joinKeyPair)

-- | Join a `Dynamic` `DynKeyPair` to a simple `Dynamic` `KeyPair`.
joinKeyPair :: Reflex t => DynKeyPair t -> Dynamic t KeyPair
joinKeyPair (KeyPair pub priv s) = KeyPair pub priv <$> s


-- | Transform `KeyPairs` to a `DynKeyPairs`.
--
--   The given Set holds keynames that are used for signing.
toDynKeyPairs :: Reflex t => Dynamic t (Set KeyName) -> KeyPairs -> DynKeyPairs t
toDynKeyPairs forSigning = Map.fromList . map toDyn . Map.toList
  where
    toDyn (k, v) = (k, toDynKeyPair (Set.member k <$> forSigning) v)

-- | Transform a `KeyPair` to a `DynKeyPair`.
--
--   `_keyPair_forSigning` of the passed in `KeyPair` gets ignored. Instead the
--   value of the passed in `Dynamic` is used. This is not really pretty, could
--   be fixed by dropping the above signing Set and handling update directly in
--   the `DynKeyPairs` which would be more efficient too.
toDynKeyPair :: Reflex t => Dynamic t Bool -> KeyPair -> DynKeyPair t
toDynKeyPair forSigning (KeyPair pub priv _) = KeyPair pub priv forSigning



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
    {- liftIO $ putStrLn "Stored keys:j" -}
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
      }

instance Reflex t => Monoid (WalletCfg t) where
  mempty = WalletCfg never never never
  mappend = (<>)

instance Flattenable WalletCfg where
  flattenWith doSwitch ev =
    WalletCfg
      <$> doSwitch never (_walletCfg_genKey <$> ev)
      <*> doSwitch never (_walletCfg_setSigning <$> ev)
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

