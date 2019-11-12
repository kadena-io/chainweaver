{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Frontend.Wallet
  (  -- * Types & Classes
    PublicKey
  , PrivateKey
  , KeyPair (..)
  , Accounts
  , WalletCfg (..)
  , HasWalletCfg (..)
  , IsWalletCfg
  , Wallet (..)
  , HasWallet (..)
  , AccountName (..)
  , AccountBalance (..)
  , mkAccountName
  , AccountGuard (..)
  , pactGuardTypeText
  , fromPactGuard
  , accountGuardKeys
  , Account(..)
  , SomeAccount(..)
  , someAccount
  -- * Creation
  , emptyWallet
  , makeWallet
  , loadKeys
  , storeKeys
  , StoreWallet(..)
  -- * Parsing
  , parseWalletKeyPair
  -- * Other helper functions
  , checkAccountNameValidity
  , snocIntMap
  ) where

import Control.Lens
import Control.Monad.Except (runExcept)
import Control.Monad.Fix
import Data.Aeson
import Data.IntMap (IntMap)
import Data.Text (Text)
import GHC.Generics (Generic)
import Kadena.SigningApi (AccountName(..), mkAccountName)
import Pact.Types.ChainId
import Reflex
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import qualified Pact.Types.Term as Pact
import qualified Pact.Types.Type as Pact

import Common.Network (AccountBalance(..))
import Common.Orphans ()
import Frontend.Crypto.Class
import Frontend.Crypto.Ed25519
import Frontend.Foundation
import Frontend.Storage

-- | A key consists of a public key and an optional private key.
--
data KeyPair key = KeyPair
  { _keyPair_publicKey  :: PublicKey
  , _keyPair_privateKey :: Maybe key
  } deriving Generic

makePactLenses ''KeyPair

-- | Account guards. We split this out here because we are only really
-- interested in keyset guards right now. Someday we might end up replacing this
-- with pact's representation for guards directly.
data AccountGuard
  = AccountGuard_KeySet Pact.KeySet
  -- ^ Keyset guards
  | AccountGuard_Other Pact.GuardType
  -- ^ Other types of guard
  deriving (Show, Generic)

fromPactGuard :: Pact.Guard a -> AccountGuard
fromPactGuard = \case
  Pact.GKeySet ks -> AccountGuard_KeySet ks
  g -> AccountGuard_Other $ Pact.guardTypeOf g

pactGuardTypeText :: Pact.GuardType -> Text
pactGuardTypeText = \case
  Pact.GTyKeySet -> "Keyset"
  Pact.GTyKeySetName -> "Keyset Name"
  Pact.GTyPact -> "Pact"
  Pact.GTyUser -> "User"
  Pact.GTyModule -> "Module"

accountGuardKeys :: AccountGuard -> [PublicKey]
accountGuardKeys = \case
  AccountGuard_KeySet ks -> fromPactPublicKey <$> Pact._ksKeys ks
  _ -> []

instance FromJSON AccountGuard
instance ToJSON AccountGuard

data Account key = Account
  { _account_name :: AccountName
  , _account_key :: KeyPair key
  , _account_chainId :: ChainId
  , _account_notes :: Text
  } deriving Generic

-- TODO actually do this properly
instance ToJSON key => ToJSON (Account key) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON key => FromJSON (Account key) where
  parseJSON = genericParseJSON defaultOptions

data WalletCfg key t = WalletCfg
  { _walletCfg_genKey     :: Event t (AccountName, ChainId, Text)
  -- ^ Request generation of a new key, that will be named as specified.
  , _walletCfg_importAccount  :: Event t (Account key)
  , _walletCfg_delKey     :: Event t IntMap.Key
  -- ^ Delete a key from your wallet.
  , _walletCfg_createWalletOnlyAccount :: Event t (ChainId, Text)
  -- ^ Create a wallet only account that uses the public key as the account name
  }
  deriving Generic

makePactLenses ''WalletCfg

-- | HasWalletCfg with additional constraints to make it behave like a proper
-- "Config".
type IsWalletCfg cfg key t = (HasWalletCfg cfg key t, Monoid cfg, Flattenable cfg t)

-- | We keep track of deletions at a given index so that we don't regenerate
-- keys with BIP32.
data SomeAccount key
  = SomeAccount_Deleted
  | SomeAccount_Account (Account key)
  deriving Generic

someAccount :: a -> (Account key -> a) -> SomeAccount key -> a
someAccount a _ SomeAccount_Deleted = a
someAccount _ f (SomeAccount_Account a) = f a

instance ToJSON key => ToJSON (SomeAccount key) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON key => FromJSON (SomeAccount key) where
  parseJSON = genericParseJSON defaultOptions

type Accounts key = IntMap (SomeAccount key)

data Wallet key t = Wallet
  { _wallet_accounts :: Dynamic t (Accounts key)
    -- ^ Accounts added and removed by the user
  , _wallet_walletOnlyAccountCreated :: Event t AccountName
    -- ^ A new wallet only account has been created
  }
  deriving Generic

makePactLenses ''Wallet

-- | An empty wallet that will never contain any keys.
emptyWallet :: Reflex t => Wallet key t
emptyWallet = Wallet mempty never

snocIntMap :: a -> IntMap a -> IntMap a
snocIntMap a m = IntMap.insert (nextKey m) a m

nextKey :: IntMap a -> Int
nextKey = maybe 0 (succ . fst) . IntMap.lookupMax

-- | Make a functional wallet that can contain actual keys.
makeWallet
  :: forall key t m.
    ( MonadHold t m, PerformEvent t m
    , MonadFix m, MonadJSM (Performable m)
    , MonadJSM m
    , HasStorage (Performable m), HasStorage m
    , HasCrypto key (Performable m)
    , FromJSON key, ToJSON key
    )
  => WalletCfg key t
  -> m (Wallet key t)
makeWallet conf = do
  initialKeys <- fromMaybe IntMap.empty <$> loadKeys
  let
    onGenKey = _walletCfg_genKey conf
    onDelKey = _walletCfg_delKey conf
    onCreateWOAcc = _walletCfg_createWalletOnlyAccount conf

  rec
    onNewKey <- performEvent $ attachWith (createKey . nextKey) (current keys) onGenKey
    onWOAccountCreate <- performEvent $ attachWith (createWalletOnlyAccount . nextKey) (current keys) onCreateWOAcc

    keys <- foldDyn id initialKeys $ leftmost
      [ ffor onNewKey $ snocIntMap . SomeAccount_Account
      , ffor (_walletCfg_importAccount conf) $ snocIntMap . SomeAccount_Account
      , ffor onDelKey $ \i -> IntMap.insert i SomeAccount_Deleted
      , ffor onWOAccountCreate $ snocIntMap . SomeAccount_Account
      ]

  performEvent_ $ storeKeys <$> updated keys

  pure $ Wallet
    { _wallet_accounts = keys
    , _wallet_walletOnlyAccountCreated = _account_name <$> onWOAccountCreate
    }
  where
    createWalletOnlyAccount :: Int -> (ChainId, Text) -> Performable m (Account key)
    createWalletOnlyAccount i (c, t) = do
      (privKey, pubKey) <- cryptoGenKey i
      pure $ buildAccount (AccountName $ keyToText pubKey) pubKey privKey c t

    createKey :: Int -> (AccountName, ChainId, Text) -> Performable m (Account key)
    createKey i (n, c, t) = do
      (privKey, pubKey) <- cryptoGenKey i
      pure $ buildAccount n pubKey privKey c t

    buildAccount n pubKey privKey c t = Account
        { _account_name = n
        , _account_key = KeyPair pubKey (Just privKey)
        , _account_chainId = c
        , _account_notes = t
        }


-- Storing data:

-- | Storage keys for referencing data to be stored/retrieved.
data StoreWallet key a where
  StoreWallet_Keys :: StoreWallet key (Accounts key)
deriving instance Show (StoreWallet key a)

-- | Parse a private key with additional checks based on the given public key.
--
--   In case a `Left` value is given instead of a valid public key, the
--   corresponding value will be returned instead.
parseWalletKeyPair :: Either Text PublicKey -> Text -> Either Text (KeyPair PrivateKey)
parseWalletKeyPair errPubKey privKey = do
  pubKey <- errPubKey
  runExcept $ uncurry KeyPair <$> parseKeyPair pubKey privKey

-- | Check account name validity (uniqueness).
--
--   Returns `Left` error msg in case it is not valid.
checkAccountNameValidity
  :: (Reflex t, HasWallet w key t)
  => w
  -> Dynamic t (Text -> Either Text AccountName)
checkAccountNameValidity w = getErr <$> (w ^. wallet_accounts)
  where
    getErr keys k = do
      acc <- mkAccountName k
      if any (\case SomeAccount_Account a -> _account_name a == acc; _ -> False) keys
         then Left $ T.pack "This account name is already in use"
         else Right acc

-- | Write key pairs to localstorage.
storeKeys :: (ToJSON key, HasStorage m, MonadJSM m) => Accounts key -> m ()
storeKeys ks = setItemStorage localStorage StoreWallet_Keys ks

-- | Load key pairs from localstorage.
loadKeys :: (FromJSON key, HasStorage m, MonadJSM m) => m (Maybe (Accounts key))
loadKeys = getItemStorage localStorage StoreWallet_Keys


-- Utility functions:

instance Reflex t => Semigroup (WalletCfg key t) where
  c1 <> c2 =
    WalletCfg
      { _walletCfg_genKey = leftmost [ _walletCfg_genKey c1
                                     , _walletCfg_genKey c2
                                     ]
      , _walletCfg_importAccount = leftmost [ _walletCfg_importAccount c1
                                        , _walletCfg_importAccount c2
                                        ]
      , _walletCfg_delKey = leftmost [ _walletCfg_delKey c1
                                     , _walletCfg_delKey c2
                                     ]
      , _walletCfg_createWalletOnlyAccount = leftmost [ _walletCfg_createWalletOnlyAccount c1
                                                      , _walletCfg_createWalletOnlyAccount c2
                                                      ]
      }

instance Reflex t => Monoid (WalletCfg key t) where
  mempty = WalletCfg never never never never
  mappend = (<>)

instance Flattenable (WalletCfg key t) t where
  flattenWith doSwitch ev =
    WalletCfg
      <$> doSwitch never (_walletCfg_genKey <$> ev)
      <*> doSwitch never (_walletCfg_importAccount <$> ev)
      <*> doSwitch never (_walletCfg_delKey <$> ev)
      <*> doSwitch never (_walletCfg_createWalletOnlyAccount <$> ev)

instance Reflex t => Semigroup (Wallet key t) where
  wa <> wb = Wallet
    { _wallet_accounts = _wallet_accounts wa <> _wallet_accounts wb
    , _wallet_walletOnlyAccountCreated = leftmost
      [ _wallet_walletOnlyAccountCreated wa
      , _wallet_walletOnlyAccountCreated wb
      ]
    }

instance Reflex t => Monoid (Wallet key t) where
  mempty = Wallet mempty never
  mappend = (<>)

instance ToJSON key => ToJSON (KeyPair key) where
  -- Yeah aeson serialization changes bit me too once. But I guess this is fine for a testnet?
  toEncoding = genericToEncoding defaultOptions

instance FromJSON key => FromJSON (KeyPair key)
