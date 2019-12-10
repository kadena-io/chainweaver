{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

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
  , AccountNotes (..)
  , mkAccountNotes
  , mkAccountName
  , AccountGuard (..)
  , pactGuardTypeText
  , fromPactGuard
  , accountGuardKeys
  , Account(..)
  , HasAccount (..)
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
  , accountToKadenaAddress
  , activeAccountOnNetwork
  , accountIsCreated
  , checkAccountNameValidity
  , snocIntMap
  , findNextKey
  , findFirstVanityAccount
  , getSigningPairs
  , findFirstInflightAccount
  ) where

import Control.Applicative ((<|>))
import Control.Lens hiding ((.=))
import Control.Monad (guard)
import Control.Monad.Except (runExcept)
import Control.Monad.Fix
import Data.Aeson
import Data.Function (on)
import Data.IntMap (IntMap)
import Data.Set (Set)
import Data.Text (Text)
import Data.These.Lens (there)
import Data.Traversable (for)
import GHC.Generics (Generic)
import Kadena.SigningApi (AccountName(..), mkAccountName)
import Pact.Types.ChainId
import Pact.Types.ChainMeta (PublicMeta)
import Reflex
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Pact.Types.Exp as Pact
import qualified Pact.Types.PactValue as Pact
import qualified Pact.Types.Term as Pact

import Common.Network (NetworkName)
import Common.Wallet
import Common.Orphans ()

import Frontend.Crypto.Class
import Frontend.Crypto.Ed25519
import Frontend.Foundation
import Frontend.KadenaAddress
import Frontend.Storage
import Frontend.Network

accountIsCreated:: Account key -> AccountCreated
accountIsCreated = maybe AccountCreated_No (const AccountCreated_Yes) . _account_balance

accountToKadenaAddress :: Account key -> KadenaAddress
accountToKadenaAddress a = mkKadenaAddress (accountIsCreated a) (_account_chainId a) (_account_name a)

data WalletCfg key t = WalletCfg
  { _walletCfg_genKey     :: Event t (AccountName, NetworkName, ChainId, AccountNotes)
  -- ^ Request generation of a new key, that will be named as specified.
  , _walletCfg_importAccount  :: Event t (Account key)
  , _walletCfg_delKey     :: Event t IntMap.Key
  -- ^ Delete a key from your wallet.
  , _walletCfg_createWalletOnlyAccount :: Event t (NetworkName, ChainId, AccountNotes)
  -- ^ Create a wallet only account that uses the public key as the account name
  , _walletCfg_refreshBalances :: Event t ()
  -- ^ Refresh balances in the wallet
  , _walletCfg_setCrossChainTransfer :: Event t (IntMap.Key, Maybe UnfinishedCrossChainTransfer)
  -- ^ Start a cross chain transfer on some account. This field allows us to
  -- recover when something goes badly wrong in the middle, since it's
  -- immediately stored with all info we need to retry.
  , _walletCfg_updateAccountNotes :: Event t (IntMap.Key, AccountNotes)
  , _walletCfg_addVanityAccountInflight :: Event t (Account key)
  -- ^ Store the details of a pending vanity acc creation. This allows us to store the
  -- keys in the event something in the application goes wrong.
  , _walletCfg_delInflightAccount :: Event t (Account key)
  -- ^ Remove the inflight vanity account, in the event of an error from the creation
  -- transaction we don't need to keep this information.
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
  | SomeAccount_Inflight (Account key)
  | SomeAccount_Account (Account key)
makePrisms ''SomeAccount

someAccount :: a -> (Account key -> a) -> SomeAccount key -> a
someAccount a _ SomeAccount_Deleted = a
someAccount a _ (SomeAccount_Inflight _) = a
someAccount _ f (SomeAccount_Account a) = f a

someAccountWithInflight :: a -> (Account key -> a) -> SomeAccount key -> a
someAccountWithInflight _ f (SomeAccount_Inflight a) = f a
someAccountWithInflight a f sa = someAccount a f sa

instance ToJSON key => ToJSON (SomeAccount key) where
  toJSON = \case
    SomeAccount_Deleted -> Null
    SomeAccount_Inflight a -> object ["inflight" .= toJSON a]
    SomeAccount_Account a -> toJSON a

instance FromJSON key => FromJSON (SomeAccount key) where
  parseJSON Null = pure SomeAccount_Deleted
  parseJSON x = (SomeAccount_Inflight <$> (withObject "Inflight Acc" (.: "inflight") x))
    <|> SomeAccount_Account <$> parseJSON x

activeAccountOnNetwork :: NetworkName -> SomeAccount key -> Maybe (Account key)
activeAccountOnNetwork net = someAccount
  Nothing
  (\a -> a <$ guard (_account_network a == net))

type Accounts key = IntMap (SomeAccount key)

getSigningPairs :: Set AccountName -> Accounts key -> [KeyPair key]
getSigningPairs signing = fmapMaybe isForSigning . IntMap.elems
  where
    -- isJust filter is necessary so indices are guaranteed stable even after
    -- the following `mapMaybe`:
    isForSigning = \case
      SomeAccount_Account a
        | kp <- _account_key a
        , Just _ <- _keyPair_privateKey kp
        , Set.member (_account_name a) signing
        -> Just kp
      _ -> Nothing

data Wallet key t = Wallet
  { _wallet_accounts :: Dynamic t (Accounts key)
    -- ^ Accounts added and removed by the user
  , _wallet_walletOnlyAccountCreated :: Event t AccountName
    -- ^ A new wallet only account has been created
  }
  deriving Generic

makePactLenses ''Wallet

findFirstInflightAccount :: Accounts key -> Maybe (Account key)
findFirstInflightAccount = fmap (\(SomeAccount_Inflight a) -> a) . find g
  where g = \case SomeAccount_Inflight _ -> True; _ -> False

-- | Find the first vanity account in the wallet
findFirstVanityAccount :: Accounts key -> Maybe (Account key)
findFirstVanityAccount = fmap (\(SomeAccount_Account a) -> a) . find g
  where g SomeAccount_Deleted = False
        g (SomeAccount_Inflight _) = False
        g (SomeAccount_Account a) = unAccountName (_account_name a) /= keyToText (_keyPair_publicKey $ _account_key a)

-- | An empty wallet that will never contain any keys.
emptyWallet :: Reflex t => Wallet key t
emptyWallet = Wallet mempty never

snocIntMap :: a -> IntMap a -> IntMap a
snocIntMap a m = IntMap.insert (nextKey m) a m

nextKey :: IntMap a -> Int
nextKey = maybe 0 (succ . fst) . IntMap.lookupMax

findNextKey :: Reflex t => Wallet key t -> Dynamic t Int
findNextKey = fmap nextKey . _wallet_accounts

-- | Make a functional wallet that can contain actual keys.
makeWallet
  :: forall model key t m.
    ( MonadHold t m, PerformEvent t m
    , MonadFix m, MonadJSM (Performable m)
    , TriggerEvent t m, MonadSample t (Performable m)
    , MonadJSM m
    , HasStorage (Performable m), HasStorage m
    , HasCrypto key (Performable m)
    , FromJSON key, ToJSON key
    , HasNetwork model t
    )
  => model
  -> WalletCfg key t
  -> m (Wallet key t)
makeWallet model conf = do
  initialKeys <- fromMaybe IntMap.empty <$> loadKeys
  let
    onGenKey = _walletCfg_genKey conf
    onDelKey = _walletCfg_delKey conf
    onCreateWOAcc = _walletCfg_createWalletOnlyAccount conf
    refresh = _walletCfg_refreshBalances conf
    setCrossChain = _walletCfg_setCrossChainTransfer conf

  performEvent_ $ liftIO (putStrLn "Refresh wallet balances") <$ refresh

  rec
    onNewKey <- performEvent $ attachWith (createKey . nextKey) (current keys) onGenKey
    onWOAccountCreate <- performEvent $ attachWith (createWalletOnlyAccount . nextKey) (current keys) onCreateWOAcc
    newBalances <- getBalances model $ current keys <@ refresh

    keys <- foldDyn id initialKeys $ leftmost
      [ ffor onNewKey $ snocIntMap . SomeAccount_Account
      , ffor (_walletCfg_importAccount conf) importAccount
      , ffor onDelKey $ \i -> IntMap.insert i SomeAccount_Deleted
      , ffor onWOAccountCreate $ snocIntMap . SomeAccount_Account
      , ffor (_walletCfg_updateAccountNotes conf) updateAccountNotes
      , ffor (_walletCfg_addVanityAccountInflight conf) addInflightAccount
      , ffor (_walletCfg_delInflightAccount conf) delInflightAccount
      , const <$> newBalances
      , let f cc = someAccount SomeAccount_Deleted (\a -> SomeAccount_Account a { _account_unfinishedCrossChainTransfer = cc })
         in ffor setCrossChain $ \(i, cc) -> IntMap.adjust (f cc) i
      ]

  performEvent_ $ storeKeys <$> updated keys

  pure $ Wallet
    { _wallet_accounts = keys
    , _wallet_walletOnlyAccountCreated = _account_name <$> onWOAccountCreate
    }
  where
    updateAccountNotes :: (IntMap.Key, AccountNotes) -> Accounts key -> Accounts key
    updateAccountNotes (k, n) = at k . _Just . _SomeAccount_Account . account_notes .~ n

    matchInflight a b = case b of
      SomeAccount_Inflight b0 -> ((==) `on` (_keyPair_publicKey . _account_key)) a b0
      _ -> False

    importAccount a as = case find (matchInflight a) as of
      Just _ -> confirmInflightAcc a as
      Nothing -> snocIntMap (SomeAccount_Account a) as

    confirmInflightAcc :: Account key -> Accounts key -> Accounts key
    confirmInflightAcc a as = as <&> \v ->
      if a `matchInflight` v then
        SomeAccount_Account a
      else
        v

    addInflightAccount :: Account key -> Accounts key -> Accounts key
    addInflightAccount a as = case find (matchInflight a) as of
      -- Blat the possibly new name, notes, and chainId over the existing inflight account as
      -- the keys have been preserved as part of the account creation process.
      Just _ -> as <&> \sa -> case sa of
        SomeAccount_Inflight _ -> SomeAccount_Inflight a
        _ -> sa
      Nothing -> snocIntMap (SomeAccount_Inflight a) as

    delInflightAccount :: Account key -> Accounts key -> Accounts key
    delInflightAccount a = IntMap.filter (not . matchInflight a)

    createWalletOnlyAccount :: Int -> (NetworkName, ChainId, AccountNotes) -> Performable m (Account key)
    createWalletOnlyAccount i (net, c, t) = do
      (privKey, pubKey) <- cryptoGenKey i
      pure $ buildAccount (AccountName $ keyToText pubKey) pubKey privKey net c t

    createKey :: Int -> (AccountName, NetworkName, ChainId, AccountNotes) -> Performable m (Account key)
    createKey i (n, net, c, t) = do
      (privKey, pubKey) <- cryptoGenKey i
      pure $ buildAccount n pubKey privKey net c t

    buildAccount n pubKey privKey net c t = Account
        { _account_name = n
        , _account_key = KeyPair pubKey (Just privKey)
        , _account_chainId = c
        , _account_network = net
        , _account_notes = t
        , _account_balance = Nothing
        , _account_unfinishedCrossChainTransfer = Nothing
        }

-- | Get the balance of some accounts from the network.
getBalances
  :: forall model key t m.
    ( PerformEvent t m, TriggerEvent t m
    , MonadSample t (Performable m), MonadIO m
    , MonadJSM (Performable m)
    , HasNetwork model t, HasCrypto key (Performable m)
    )
  => model -> Event t (Accounts key) -> m (Event t (Accounts key))
getBalances model accounts = do
  reqs <- performEvent $ attachWith mkReqs (current $ getNetworkNameAndMeta model) accounts
  response <- performLocalReadCustom (model ^. network) toReqList reqs
  pure $ toBalances <$> response
  where
    toDetails nw = case nw  ^? there . _2 of
      Just (Pact.PObject (Pact.ObjectMap om)) -> (,,)
        <$> (om ^? at "account" . _Just . _PLit . Pact._LString . to AccountName)
        <*> (om ^? at "balance" . _Just . _PLit . Pact._LDecimal . to AccountBalance)
        <*> (om ^? at "guard" . _Just . _PGuard . _GKeySet . to Pact._ksKeys . to (fmap fromPactPublicKey))
      _ -> Nothing

    toBalances
      :: (IntMap (SomeAccount key, Maybe NetworkRequest), [NetworkErrorResult])
      -> Accounts key
    toBalances (m, results) =
      IntMap.fromList $ stepwise (IntMap.toList (fmap fst m)) (toDetails <$> results)

    -- I don't like this, I'd rather just block in a forked thread manually
    -- than have to do this dodgy alignment. TODO
    stepwise
      :: [(IntMap.Key, SomeAccount key)]
      -> [Maybe (AccountName, AccountBalance, [PublicKey])]
      -> [(IntMap.Key, SomeAccount key)]
    stepwise ((i, sa):accs) (deet:deets) = case sa of
      SomeAccount_Deleted -> (i, SomeAccount_Deleted) : stepwise accs (deet:deets)

      -- If we have a balance returned for an inflight account then it exists on the chain
      SomeAccount_Inflight a -> case deet of
        Just (upname, upbal, upguard) | _account_name a == upname && (_keyPair_publicKey $ _account_key a) `elem` upguard ->
          (i, SomeAccount_Account a { _account_balance = Just upbal } )
        _ ->
          (i, sa)
        : stepwise accs deets

      SomeAccount_Account a ->
        (i, SomeAccount_Account a { _account_balance = (deet ^? _Just . _2) <|> _account_balance a })
        : stepwise accs deets

    stepwise as _ = as

    toReqList :: Foldable f => f (SomeAccount key, Maybe NetworkRequest) -> [NetworkRequest]
    toReqList = fmapMaybe snd . toList

    accountBalanceReq acc = "(coin.details " <> tshow (unAccountName acc) <> ")"

    mkReqs
      :: (NetworkName, PublicMeta)
      -> Accounts key
      -> Performable m (IntMap (SomeAccount key, Maybe NetworkRequest))
    mkReqs meta someaccs = for someaccs $ \sa ->
      fmap (sa,) . traverse (mkReq meta) $ someAccountWithInflight Nothing Just sa

    mkReq (netName, pm) acc = mkSimpleReadReq
      (accountBalanceReq $ _account_name acc)
      netName
      pm
      (ChainRef Nothing $ _account_chainId acc)

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
  -> Dynamic t (Maybe ChainId -> Text -> Either Text AccountName)
checkAccountNameValidity w = getErr <$> (w ^. wallet_accounts)
  where
    getErr keys mChain k = do
      acc <- mkAccountName k
      let
        onChain a = case mChain of
          -- If we don't have a chain, don't bother checking for duplicates.
          Just chain -> and
            [ _account_name a == acc
            , _account_chainId a == chain
            ]
          Nothing -> False

      if any (someAccount False onChain) keys
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
      , _walletCfg_refreshBalances = leftmost
        [ _walletCfg_refreshBalances c1
        , _walletCfg_refreshBalances c2
        ]
      , _walletCfg_setCrossChainTransfer = leftmost
        [ _walletCfg_setCrossChainTransfer c1
        , _walletCfg_setCrossChainTransfer c2
        ]
      , _walletCfg_updateAccountNotes = leftmost
        [ _walletCfg_updateAccountNotes c1
        , _walletCfg_updateAccountNotes c2
        ]
      , _walletCfg_addVanityAccountInflight = leftmost
        [ _walletCfg_addVanityAccountInflight c1
        , _walletCfg_addVanityAccountInflight c2
        ]
      , _walletCfg_delInflightAccount = leftmost
        [ _walletCfg_delInflightAccount c1
        , _walletCfg_delInflightAccount c2
        ]
      }

instance Reflex t => Monoid (WalletCfg key t) where
  mempty = WalletCfg never never never never never never never never never
  mappend = (<>)

instance Flattenable (WalletCfg key t) t where
  flattenWith doSwitch ev =
    WalletCfg
      <$> doSwitch never (_walletCfg_genKey <$> ev)
      <*> doSwitch never (_walletCfg_importAccount <$> ev)
      <*> doSwitch never (_walletCfg_delKey <$> ev)
      <*> doSwitch never (_walletCfg_createWalletOnlyAccount <$> ev)
      <*> doSwitch never (_walletCfg_refreshBalances <$> ev)
      <*> doSwitch never (_walletCfg_setCrossChainTransfer <$> ev)
      <*> doSwitch never (_walletCfg_updateAccountNotes <$> ev)
      <*> doSwitch never (_walletCfg_addVanityAccountInflight <$> ev)
      <*> doSwitch never (_walletCfg_delInflightAccount <$> ev)

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
