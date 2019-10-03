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
  , AccountName (unAccountName)
  , mkAccountName
  , AccountGuard (..)
  , pactGuardTypeText
  , fromPactGuard
  -- * Creation
  , emptyWallet
  , makeWallet
  -- * Parsing
  , parseWalletKeyPair
  -- * Other helper functions
  , checkKeyNameValidityStr
  ) where

import           Control.Lens
import           Control.Monad.Except (runExcept)
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
import           Reflex
import           Pact.Types.ChainId
import qualified Pact.Types.Term             as Pact
import qualified Pact.Types.Type             as Pact

import           Common.Orphans              ()
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

newtype AccountName = AccountName
  { unAccountName :: Text
  } deriving (Eq, Ord, Show, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | Smart constructor for account names. The only restriction in the coin
-- contract (as it stands) appears to be that accounts can't be an empty string
mkAccountName :: Text -> Either Text AccountName
mkAccountName n
  | T.null n = Left "Account name must not be empty"
  | otherwise = Right $ AccountName n

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

instance FromJSON AccountGuard
instance ToJSON AccountGuard

type AccountGuards = Map ChainId (Map AccountName AccountGuard)

-- We might need to track the chainID here too.
type KeyAccounts = Map PublicKey (Set AccountName)

data WalletCfg t = WalletCfg
  { _walletCfg_genKey     :: Event t KeyName
  -- ^ Request generation of a new key, that will be named as specified.
  , _walletCfg_importKey  :: Event t (KeyName, KeyPair)
  , _walletCfg_delKey     :: Event t KeyName
  -- ^ Delete a key from your wallet.
  , _walletCfg_addAccount :: Event t (ChainId, AccountName, AccountGuard)
  , _walletCfg_deleteAccount :: Event t (ChainId, AccountName)
  }
  deriving Generic

makePactLenses ''WalletCfg

-- | HasWalletCfg with additional constraints to make it behave like a proper
-- "Config".
type IsWalletCfg cfg t = (HasWalletCfg cfg t, Monoid cfg, Flattenable cfg t)

data Wallet t = Wallet
  { _wallet_keys        :: Dynamic t KeyPairs
    -- ^ Keys added and removed by the user 
  , _wallet_keyAccounts :: Dynamic t KeyAccounts
    -- ^ Accounts associated with the given public key
  , _wallet_accountGuards :: Dynamic t AccountGuards
    -- ^ Guards associated with the given account (and chain)
  }
  deriving Generic

makePactLenses ''Wallet

-- | An empty wallet that will never contain any keys.
emptyWallet :: Reflex t => Wallet t
emptyWallet = Wallet mempty mempty mempty

-- | Make a functional wallet that can contain actual keys.
makeWallet
  :: forall t m.
    ( MonadHold t m, PerformEvent t m
    , MonadFix m, MonadJSM (Performable m)
    , MonadJSM m
    , HasStorage (Performable m), HasStorage m
    )
  => WalletCfg t
  -> m (Wallet t)
makeWallet conf = do
    initialKeys <- fromMaybe Map.empty <$> loadKeys
    let
      onGenKey = T.strip <$> conf ^. walletCfg_genKey
      onDelKey = _walletCfg_delKey conf

    onNewKey <- performEvent $ createKey <$>
      -- Filter out keys with empty names
      ffilter (not . T.null) onGenKey

    keys <- foldDyn id initialKeys $
      -- Filter out duplicate keys:
      leftmost [ uncurry (Map.insertWith (flip const)) <$> onNewKey
               , uncurry Map.insert <$> (conf ^. walletCfg_importKey)
               , Map.delete <$> onDelKey
               ]

    performEvent_ $ storeKeys <$> updated keys

    initialKeyAccounts <- fromMaybe Map.empty <$> getItemStorage localStorage StoreWallet_KeyAccounts
    keyAccounts <- foldDyn ($) initialKeyAccounts $ leftmost
      [ ffor (_walletCfg_addAccount conf) $ \(_chain, account, guard) -> case guard of
        AccountGuard_Other _gt -> id
        AccountGuard_KeySet ks -> Map.unionWith (<>) (Map.fromList $ fmap (\k -> (fromPactPublicKey k, Set.singleton account)) (Pact._ksKeys ks))
      , ffor (_walletCfg_deleteAccount conf) $ \(_chain, account) -> Map.mapMaybe $ \s ->
        let new = Set.delete account s
         in if Set.null new then Nothing else Just new
      ]
    performEvent_ $ setItemStorage localStorage StoreWallet_KeyAccounts <$> updated keyAccounts

    initialAccountKeys <- fromMaybe Map.empty <$> getItemStorage localStorage StoreWallet_AccountGuards
    accountGuards <- foldDyn ($) initialAccountKeys $ leftmost
      [ ffor (_walletCfg_addAccount conf) $ \(chain, account, guard) -> flip Map.alter chain $ \case
        Nothing -> Just $ Map.singleton account guard
        Just as -> Just $ Map.insert account guard as
      , ffor (_walletCfg_deleteAccount conf) $ \(chain, account) -> flip Map.alter chain $ \case
        Nothing -> Nothing
        Just as ->
          let new = Map.delete account as
          in if Map.null new then Nothing else Just new
      ]
    performEvent_ $ setItemStorage localStorage StoreWallet_AccountGuards <$> updated accountGuards

    pure $ Wallet
      { _wallet_keys = keys
      , _wallet_keyAccounts = keyAccounts
      , _wallet_accountGuards = accountGuards
      }
  where
    createKey :: KeyName -> Performable m (KeyName, KeyPair)
    createKey n = do
      (privKey, pubKey) <- genKeyPair
      pure (n, KeyPair pubKey (Just privKey))


-- Storing data:

-- | Storage keys for referencing data to be stored/retrieved.
data StoreWallet a where
  StoreWallet_Keys :: StoreWallet KeyPairs
  StoreWallet_KeyAccounts :: StoreWallet KeyAccounts
  StoreWallet_AccountGuards :: StoreWallet AccountGuards
deriving instance Show (StoreWallet a)

-- | Parse a private key with additional checks based on the given public key.
--
--   In case a `Left` value is given instead of a valid public key, the
--   corresponding value will be returned instead.
parseWalletKeyPair :: Either Text PublicKey -> Text -> Either Text KeyPair
parseWalletKeyPair errPubKey privKey = do
  pubKey <- errPubKey
  runExcept $ uncurry KeyPair <$> parseKeyPair pubKey privKey

-- | Check key name validity (uniqueness).
--
--   Returns `Just` error msg in case it is not valid.
checkKeyNameValidityStr
  :: (Reflex t, HasWallet w t)
  => w
  -> Dynamic t (KeyName -> Maybe Text)
checkKeyNameValidityStr w = getErr <$> w ^. wallet_keys
  where
    getErr keys k =
      if Map.member k keys
         then Just $ T.pack "This key name is already in use"
         else Nothing

-- | Write key pairs to localstorage.
storeKeys :: (HasStorage m, MonadJSM m) => KeyPairs -> m ()
storeKeys ks = setItemStorage localStorage StoreWallet_Keys ks

-- | Load key pairs from localstorage.
loadKeys :: (HasStorage m, MonadJSM m) => m (Maybe KeyPairs)
loadKeys = getItemStorage localStorage StoreWallet_Keys


-- Utility functions:

instance Reflex t => Semigroup (WalletCfg t) where
  c1 <> c2 =
    WalletCfg
      { _walletCfg_genKey = leftmost [ _walletCfg_genKey c1
                                     , _walletCfg_genKey c2
                                     ]
      , _walletCfg_importKey = leftmost [ _walletCfg_importKey c1
                                        , _walletCfg_importKey c2
                                        ]
      , _walletCfg_delKey = leftmost [ _walletCfg_delKey c1
                                     , _walletCfg_delKey c2
                                     ]
      , _walletCfg_addAccount = leftmost [ _walletCfg_addAccount c1, _walletCfg_addAccount c2 ]
      , _walletCfg_deleteAccount = leftmost [ _walletCfg_deleteAccount c1, _walletCfg_deleteAccount c2 ]
      }

instance Reflex t => Monoid (WalletCfg t) where
  mempty = WalletCfg never never never never never
  mappend = (<>)

instance Flattenable (WalletCfg t) t where
  flattenWith doSwitch ev =
    WalletCfg
      <$> doSwitch never (_walletCfg_genKey <$> ev)
      <*> doSwitch never (_walletCfg_importKey <$> ev)
      <*> doSwitch never (_walletCfg_delKey <$> ev)
      <*> doSwitch never (_walletCfg_addAccount <$> ev)
      <*> doSwitch never (_walletCfg_deleteAccount <$> ev)

instance Reflex t => Semigroup (Wallet t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (Wallet t) where
  mempty = memptydefault
  mappend = (<>)

instance ToJSON KeyPair where
  -- Yeah aeson serialization changes bit me too once. But I guess this is fine for a testnet?
  toEncoding = genericToEncoding defaultOptions

instance FromJSON KeyPair
