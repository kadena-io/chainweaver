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
  , isPredefinedKey
  , chainwebDefaultSenders
  -- * Parsing
  , parseWalletKeyPair
  -- * Other helper functions
  , checkKeyNameValidityStr
  ) where

import           Control.Lens
import           Control.Monad.Except (runExcept)
import           Control.Monad.Fix
import           Control.Newtype.Generics           (Newtype (..))
import           Data.ByteString                    (ByteString)
import           Data.Aeson
import           Data.Default
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
    -- ^ Keys added and removed by the user and chainwebDefaultSenders which can't be removed.
  , _wallet_keyAccounts :: Dynamic t KeyAccounts
    -- ^ Accounts associated with the given public key
  , _wallet_accountGuards :: Dynamic t AccountGuards
    -- ^ Guards associated with the given account (and chain)
  }
  deriving Generic

makePactLenses ''Wallet

-- | Pre-defined senders in chainweb. This is used for testnet, later on we
-- will some kind of a real wallet in order to manage real senders.
--
-- TODO: remove these before release
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
emptyWallet = Wallet (pure mempty) (pure mempty) (pure mempty)

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
    let mkGuard (KeyPair pk _) = AccountGuard_KeySet $ Pact.KeySet
          { Pact._ksKeys = [toPactPublicKey pk]
          , Pact._ksPredFun = Pact.Name "keys-all" def
          }
        -- The chainweb senders aren't really accounts. They don't actually
        -- exist in the accounts table: they are just hardcoded on testnet to
        -- have a large amount of funds.
        testnetAccounts = Map.singleton "0" (Map.map mkGuard (Map.mapKeysMonotonic AccountName chainwebDefaultSenders))
        ini = Map.unionWith (<>) initialAccountKeys testnetAccounts
    accountGuards <- foldDyn ($) ini $ leftmost
      [ ffor (_walletCfg_addAccount conf) $ \(chain, account, guard) -> flip Map.alter chain $ \case
        Nothing -> Just $ Map.singleton account guard
        Just as -> Just $ Map.insert account guard as
      , ffor (_walletCfg_deleteAccount conf) $ \(chain, account) -> flip Map.alter chain $ \case
        Nothing -> Nothing
        Just as -> Just $ Map.delete account as
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
storeKeys ks = setItemStorage localStorage StoreWallet_Keys (ks Map.\\ chainwebDefaultSenders)

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
