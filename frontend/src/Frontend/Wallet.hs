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
    PrivateKey
  , KeyPair (..)
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
  -- * Creation
  , makeWallet
  , loadKeys
  , storeKeys
  , StoreWallet(..)
  -- * Parsing
  , parseWalletKeyPair
  -- * Other helper functions
  , accountIsCreated
  , accountToKadenaAddress
  , checkAccountNameValidity
  , snocIntMap
  , findNextKey
  , findFirstVanityAccount
  , getSigningPairs
  , findFirstInflightAccount
  , module Common.Wallet
  ) where

import Control.Lens hiding ((.=))
import Control.Monad.Except (runExcept)
import Control.Monad.Fix
import Data.Aeson
import Data.Dependent.Sum (DSum(..))
import Data.Function (on)
import Data.IntMap (IntMap)
import Data.Set (Set)
import Data.Some (Some(Some))
import Data.Text (Text)
import GHC.Generics (Generic)
import Kadena.SigningApi (AccountName(..), mkAccountName)
import Pact.Types.ChainId
import Reflex
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
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

accountIsCreated:: Account -> AccountCreated
accountIsCreated = maybe AccountCreated_No (const AccountCreated_Yes) . _accountInfo_balance . accountInfo

accountToKadenaAddress :: Account -> KadenaAddress
accountToKadenaAddress a = mkKadenaAddress (accountIsCreated a) (accountChain a) (accountToName a)

data WalletCfg key t = WalletCfg
  { _walletCfg_genKey :: Event t ()
  -- ^ Request generation of a new key
  , _walletCfg_delKey :: Event t IntMap.Key
  -- ^ Hide a key in the wallet.
  , _walletCfg_delAccount :: Event t (NetworkName, AccountName, ChainId)
  -- ^ Hide an account in the wallet.
  , _walletCfg_importAccount  :: Event t (NetworkName, AccountName, ChainId, VanityAccount)
  , _walletCfg_createWalletOnlyAccount :: Event t (NetworkName, PublicKey, ChainId)
  -- ^ Create a wallet only account that uses the public key as the account name
  , _walletCfg_refreshBalances :: Event t ()
  -- ^ Refresh balances in the wallet
  , _walletCfg_setCrossChainTransfer :: Event t (IntMap.Key, Maybe UnfinishedCrossChainTransfer)
  -- ^ Start a cross chain transfer on some account. This field allows us to
  -- recover when something goes badly wrong in the middle, since it's
  -- immediately stored with all info we need to retry.
  , _walletCfg_updateAccountNotes :: Event t (NetworkName, AccountName, ChainId, AccountNotes)
  }
  deriving Generic

makePactLenses ''WalletCfg

-- | HasWalletCfg with additional constraints to make it behave like a proper
-- "Config".
type IsWalletCfg cfg key t = (HasWalletCfg cfg key t, Monoid cfg, Flattenable cfg t)

getSigningPairs :: KeyStorage key -> Accounts -> Set (Some AccountRef) -> [KeyPair key]
getSigningPairs allKeys allAccounts signing = filterKeyPairs wantedKeys allKeys
  where
    wantedKeys = Set.fromList $ Map.elems $ Map.restrictKeys accountRefs signing
    accountRefs = flip foldAccounts allAccounts $ \a@(r :=> _) -> Map.singleton (Some r) $ accountKey a

data Wallet key t = Wallet
  { _wallet_keys :: Dynamic t (KeyStorage key)
    -- ^ Accounts added and removed by the user
  , _wallet_accounts :: Dynamic t AccountStorage
    -- ^ Accounts added and removed by the user
  }
  deriving Generic

makePactLenses ''Wallet

findFirstInflightAccount :: Accounts -> Maybe (AccountName, ChainId, VanityAccount)
findFirstInflightAccount as = do
  (n, cm) <- Map.lookupMin $ _accounts_vanity as
  (c, va) <- Map.lookupMin $ Map.filter _vanityAccount_inflight cm
  pure (n, c, va)

-- | Find the first vanity account in the wallet
findFirstVanityAccount :: Accounts -> Maybe (AccountName, ChainId, VanityAccount)
findFirstVanityAccount as = do
  (n, cm) <- Map.lookupMin $ _accounts_vanity as
  (c, va) <- Map.lookupMin cm
  pure (n, c, va)

snocIntMap :: a -> IntMap a -> IntMap a
snocIntMap a m = IntMap.insert (nextKey m) a m

nextKey :: IntMap a -> Int
nextKey = maybe 0 (succ . fst) . IntMap.lookupMax

findNextKey :: Reflex t => Wallet key t -> Dynamic t Int
findNextKey = fmap nextKey . _wallet_keys

-- | Make a functional wallet that can contain actual keys.
makeWallet
  :: forall model key t m.
    ( MonadHold t m, PerformEvent t m
    , MonadFix m, MonadJSM (Performable m)
    , MonadJSM m
    , HasStorage (Performable m), HasStorage m
    , HasCrypto key (Performable m)
    , FromJSON key, ToJSON key
    )
  => model
  -> WalletCfg key t
  -> m (Wallet key t)
makeWallet model conf = do
  initialKeys <- fromMaybe IntMap.empty <$> loadKeys
  initialAccounts <- fromMaybe mempty <$> loadAccounts
  let
    onGenKey = _walletCfg_genKey conf
    onCreateWOAcc = _walletCfg_createWalletOnlyAccount conf
    refresh = _walletCfg_refreshBalances conf
    setCrossChain = _walletCfg_setCrossChainTransfer conf

  performEvent_ $ liftIO (putStrLn "Refresh wallet balances") <$ refresh

  rec
    onNewKey <- performEvent $ attachWith (\a _ -> createKey $ nextKey a) (current keys) onGenKey
    --newBalances <- getBalances model $ current accounts <@ refresh

    keys <- foldDyn id initialKeys $ leftmost
      [ snocIntMap <$> onNewKey
      , ffor (_walletCfg_delKey conf) $ IntMap.adjust $ \k -> k { _key_hidden = True }
     -- , const <$> newBalances
     -- , let f cc = someAccount SomeAccount_Deleted (\a -> SomeAccount_Account a { _account_unfinishedCrossChainTransfer = cc })
     --    in ffor setCrossChain $ \(i, cc) -> IntMap.adjust (f cc) i
      ]

    accounts <- foldDyn id initialAccounts $ leftmost
      [ ffor (_walletCfg_importAccount conf) $ \(net, name, chain, acc) ->
        ((<>) (AccountStorage $ Map.singleton net $ mempty { _accounts_vanity = Map.singleton name $ Map.singleton chain acc }))
      , ffor (_walletCfg_createWalletOnlyAccount conf) $ \(net, pk, chain) ->
        ((<>) (AccountStorage $ Map.singleton net $ mempty
          { _accounts_nonVanity = Map.singleton pk $ Map.singleton chain $ NonVanityAccount blankAccountInfo }))
      , ffor (_walletCfg_updateAccountNotes conf) updateAccountNotes
      ]

  performEvent_ $ storeKeys <$> updated keys
  performEvent_ $ storeAccounts <$> updated accounts

  pure $ Wallet
    { _wallet_keys = keys
    , _wallet_accounts = accounts
    }
  where
    updateAccountNotes :: (NetworkName, AccountName, ChainId, AccountNotes) -> AccountStorage -> AccountStorage
    updateAccountNotes (net, name, chain, notes) = _AccountStorage . at net . _Just . accounts_vanity . at name . _Just . at chain . _Just . vanityAccount_notes .~ notes

    createKey :: Int -> Performable m (Key key)
    createKey i = do
      (privKey, pubKey) <- cryptoGenKey i
      pure $ Key
        { _key_pair = KeyPair pubKey (Just privKey)
        , _key_hidden = False
        , _key_notes = mkAccountNotes ""
        }

-- | Get the balance of some accounts from the network.
getBalances
  :: forall model key t m.
    ( PerformEvent t m, TriggerEvent t m
    , MonadSample t (Performable m), MonadIO m
    , MonadJSM (Performable m)
    , HasNetwork model t, HasCrypto key (Performable m)
    )
  => model -> Event t Accounts -> m (Event t Accounts)
getBalances model accounts = do
  pure never
--  reqs <- performEvent $ attachWith mkReqs (current $ getNetworkNameAndMeta model) accounts
--  response <- performLocalReadCustom (model ^. network) toReqList reqs
--  pure $ toBalances <$> response
--  where
--    toBalance = (^? there . _2 . to (\case (Pact.PLiteral (Pact.LDecimal d)) -> Just $ AccountBalance d; _ -> Nothing) . _Just)
--    toBalances :: (IntMap (SomeAccount key, Maybe NetworkRequest), [NetworkErrorResult]) -> Accounts key
--    toBalances (m, results) = IntMap.fromList $ stepwise (IntMap.toList (fmap fst m)) (toBalance <$> results)
--    -- I don't like this, I'd rather just block in a forked thread manually than have to do this dodgy alignment. TODO
--    stepwise :: [(IntMap.Key, SomeAccount key)] -> [Maybe AccountBalance] -> [(IntMap.Key, SomeAccount key)]
--    stepwise ((i, sa):accs) (bal:bals) = case sa of
--      SomeAccount_Deleted -> (i, SomeAccount_Deleted) : stepwise accs (bal:bals)
--      SomeAccount_Account a ->
--        (i, SomeAccount_Account a { _account_balance = bal <|> _account_balance a })
--        : stepwise accs bals
--    stepwise as _ = as
--    toReqList :: Foldable f => f (SomeAccount key, Maybe NetworkRequest) -> [NetworkRequest]
--    toReqList = fmapMaybe snd . toList
--    accountBalanceReq acc = "(coin.get-balance " <> tshow (unAccountName acc) <> ")"
--    mkReqs :: (NetworkName, PublicMeta) -> Accounts key -> Performable m (IntMap (SomeAccount key, Maybe NetworkRequest))
--    mkReqs meta someaccs = for someaccs $ \sa ->
--      fmap (sa,) . traverse (mkReq meta) $ someAccount Nothing Just sa
--    mkReq (netName, pm) acc = mkSimpleReadReq (accountBalanceReq $ _account_name acc) netName pm (ChainRef Nothing $ _account_chainId acc)

-- Storing data:

-- | Storage keys for referencing data to be stored/retrieved.
data StoreWallet key a where
  StoreWallet_Keys :: StoreWallet key (KeyStorage key)
  StoreWallet_Accounts :: StoreWallet key AccountStorage
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
  :: (Reflex t, HasNetwork m t, HasWallet m key t)
  => m
  -> Dynamic t (Maybe ChainId -> Text -> Either Text AccountName)
checkAccountNameValidity m = getErr <$> (m ^. network_selectedNetwork) <*> (m ^. wallet_accounts)
  where
    getErr net (AccountStorage networks) mChain k = do
      acc <- mkAccountName k
      maybe (Right acc) (\_ -> Left "This account name is already in use") $ do
        accounts <- Map.lookup net networks
        chains <- Map.lookup acc $ _accounts_vanity accounts
        chain <- mChain
        Map.lookup chain chains

-- | Write key pairs to localstorage.
storeKeys :: (ToJSON key, HasStorage m, MonadJSM m) => KeyStorage key -> m ()
storeKeys = setItemStorage localStorage StoreWallet_Keys

-- | Load key pairs from localstorage.
loadKeys :: (FromJSON key, HasStorage m, MonadJSM m) => m (Maybe (KeyStorage key))
loadKeys = getItemStorage localStorage StoreWallet_Keys

-- | Write key pairs to localstorage.
storeAccounts :: (HasStorage m, MonadJSM m) => AccountStorage -> m ()
storeAccounts = setItemStorage localStorage StoreWallet_Accounts

-- | Load accounts from localstorage.
loadAccounts :: (HasStorage m, MonadJSM m) => m (Maybe AccountStorage)
loadAccounts = getItemStorage localStorage StoreWallet_Accounts

-- Utility functions:

instance Reflex t => Semigroup (WalletCfg key t) where
  c1 <> c2 = WalletCfg
    { _walletCfg_genKey = leftmost
      [ _walletCfg_genKey c1
      , _walletCfg_genKey c2
      ]
    , _walletCfg_importAccount = leftmost
      [ _walletCfg_importAccount c1
      , _walletCfg_importAccount c2
      ]
    , _walletCfg_delKey = leftmost
      [ _walletCfg_delKey c1
      , _walletCfg_delKey c2
      ]
    , _walletCfg_delAccount = leftmost
      [ _walletCfg_delAccount c1
      , _walletCfg_delAccount c2
      ]
    , _walletCfg_createWalletOnlyAccount = leftmost
      [ _walletCfg_createWalletOnlyAccount c1
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
    }

instance Reflex t => Monoid (WalletCfg key t) where
  mempty = WalletCfg never never never never never never never never
  mappend = (<>)

instance Flattenable (WalletCfg key t) t where
  flattenWith doSwitch ev =
    WalletCfg
      <$> doSwitch never (_walletCfg_genKey <$> ev)
      <*> doSwitch never (_walletCfg_delKey <$> ev)
      <*> doSwitch never (_walletCfg_delAccount <$> ev)
      <*> doSwitch never (_walletCfg_importAccount <$> ev)
      <*> doSwitch never (_walletCfg_createWalletOnlyAccount <$> ev)
      <*> doSwitch never (_walletCfg_refreshBalances <$> ev)
      <*> doSwitch never (_walletCfg_setCrossChainTransfer <$> ev)
      <*> doSwitch never (_walletCfg_updateAccountNotes <$> ev)

instance Reflex t => Semigroup (Wallet key t) where
  wa <> wb = Wallet
    { _wallet_keys = _wallet_keys wa <> _wallet_keys wb
    , _wallet_accounts = _wallet_accounts wa <> _wallet_accounts wb
    }

instance Reflex t => Monoid (Wallet key t) where
  mempty = Wallet mempty mempty
  mappend = (<>)
