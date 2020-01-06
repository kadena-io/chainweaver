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
  , mkAccountName
  , AccountGuard (..)
  -- * Creation
  , makeWallet
  , loadKeys
  , storeKeys
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
  , module Common.Wallet
  ) where

import Control.Lens hiding ((.=))
import Control.Monad (void)
import Control.Monad.Except (runExcept)
import Control.Monad.Fix
import Data.Aeson
import Data.Dependent.Sum (DSum(..))
import Data.Either (rights)
import Data.IntMap (IntMap)
import Data.Set (Set)
import Data.Some (Some(Some))
import Data.Text (Text)
import GHC.Generics (Generic)
import Kadena.SigningApi (AccountName(..), mkAccountName)
import Pact.Types.ChainId
import Reflex
import qualified Data.Map as Map
import qualified Data.Map.Monoidal as MonoidalMap
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import qualified Pact.Server.ApiV1Client as Api
import qualified Pact.Types.Command as Pact
import qualified Pact.Types.ChainMeta as Pact
import Pact.Types.Pretty

import Common.Network (NetworkName)
import Common.Wallet
import Common.Orphans ()

import Frontend.Crypto.Class
import Frontend.Crypto.Ed25519
import Frontend.Foundation
import Frontend.KadenaAddress
import Frontend.Storage
import Frontend.Network
import Frontend.Store

accountIsCreated :: Account -> AccountCreated
accountIsCreated = maybe AccountCreated_No (const AccountCreated_Yes) . _accountInfo_balance . view accountInfo

accountToKadenaAddress :: Account -> KadenaAddress
accountToKadenaAddress a = mkKadenaAddress (accountIsCreated a) (accountChain a) (accountToName a)

data WalletCfg key t = WalletCfg
  { _walletCfg_genKey :: Event t ()
  -- ^ Request generation of a new key
  , _walletCfg_delKey :: Event t IntMap.Key
  -- ^ Hide a key in the wallet.
  , _walletCfg_delAccount :: Event t (NetworkName, Some AccountRef)
  -- ^ Hide an account in the wallet.
  , _walletCfg_importAccount  :: Event t (NetworkName, AccountName, ChainId, VanityAccount)
  , _walletCfg_createWalletOnlyAccount :: Event t (NetworkName, PublicKey, ChainId)
  -- ^ Create a wallet only account that uses the public key as the account name
  , _walletCfg_refreshBalances :: Event t ()
  -- ^ Refresh balances in the wallet
  , _walletCfg_setCrossChainTransfer :: Event t (NetworkName, Some AccountRef, Maybe UnfinishedCrossChainTransfer)
  -- ^ Start a cross chain transfer on some account. This field allows us to
  -- recover when something goes badly wrong in the middle, since it's
  -- immediately stored with all info we need to retry.
  , _walletCfg_updateAccountNotes :: Event t (NetworkName, AccountName, ChainId, AccountNotes)
  , _walletCfg_updateKeyNotes :: Event t (IntMap.Key, AccountNotes)
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

-- | Find the first vanity account in the wallet which satisfies the predicate
findFirstVanityAccount :: (VanityAccount -> Bool) -> Accounts -> Maybe (AccountName, ChainId, VanityAccount)
findFirstVanityAccount predicate as = do
  (n, cm) <- Map.lookupMin $ _accounts_vanity as
  (c, va) <- Map.lookupMin $ Map.filter predicate cm
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
    , HasStorage (Performable m), HasStorage m
    , HasNetwork model t
    , TriggerEvent t m
    , HasCrypto key (Performable m)
    , FromJSON key, ToJSON key
    )
  => model
  -> WalletCfg key t
  -> m (Wallet key t)
makeWallet model conf = do
  initialKeys <- fromMaybe IntMap.empty <$> loadKeys
  initialAccounts <- fromMaybe mempty <$> loadAccounts

  rec
    onNewKey <- performEvent $ createKey . nextKey <$> current keys <@ _walletCfg_genKey conf
    keys <- foldDyn id initialKeys $ leftmost
      [ snocIntMap <$> onNewKey
      , ffor (_walletCfg_delKey conf) $ IntMap.adjust $ \k -> k { _key_hidden = True }
      , ffor (_walletCfg_updateKeyNotes conf) $ \(i, notes) -> IntMap.adjust (\k -> k { _key_notes = notes }) i
      ]

  rec
    newBalances <- getBalances model $ (,) <$> current keys <*> current accounts <@ _walletCfg_refreshBalances conf
    accounts <- foldDyn id initialAccounts $ leftmost
      [ ffor (_walletCfg_importAccount conf) $ \(net, name, chain, acc) ->
        ((<>) (AccountStorage $ Map.singleton net $ mempty { _accounts_vanity = Map.singleton name $ Map.singleton chain acc }))
      , ffor (_walletCfg_createWalletOnlyAccount conf) $ \(net, pk, chain) ->
        ((<>) (AccountStorage $ Map.singleton net $ mempty
          { _accounts_nonVanity = Map.singleton pk $ Map.singleton chain $ NonVanityAccount blankAccountInfo }))
      , ffor (_walletCfg_updateAccountNotes conf) updateAccountNotes
      , foldr (.) id . fmap updateAccountBalance <$> newBalances
      , ffor (_walletCfg_setCrossChainTransfer conf) updateCrossChain
      , ffor (_walletCfg_delAccount conf) hideAccount
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

    hideAccount :: (NetworkName, Some AccountRef) -> AccountStorage -> AccountStorage
    hideAccount (net, ref) = storageAccountInfo net ref . accountInfo_hidden .~ True

    updateCrossChain :: (NetworkName, Some AccountRef, Maybe UnfinishedCrossChainTransfer) -> AccountStorage -> AccountStorage
    updateCrossChain (net, ref, cct) = storageAccountInfo net ref . accountInfo_unfinishedCrossChainTransfer .~ cct

    updateAccountBalance :: (NetworkName, Some AccountRef, Maybe AccountBalance) -> AccountStorage -> AccountStorage
    updateAccountBalance (net, (Some ref), balance) = case ref of
      (AccountRef_NonVanity pk chain) -> upsertNonVanityBalance net pk chain balance
      (AccountRef_Vanity _ _) -> storageAccountInfo net (Some ref) . accountInfo_balance .~ balance

    createKey :: Int -> Performable m (Key key)
    createKey i = do
      (privKey, pubKey) <- cryptoGenKey i
      pure $ Key
        { _key_pair = KeyPair pubKey (Just privKey)
        , _key_hidden = False
        , _key_notes = mkAccountNotes ""
        }

-- | Get the balance of accounts from the network.
getBalances
  :: forall model key t m.
    ( PerformEvent t m, TriggerEvent t m
    , MonadJSM (Performable m)
    , HasNetwork model t, HasCrypto key (Performable m)
    )
  => model -> Event t (KeyStorage key, AccountStorage) -> m (Event t [(NetworkName, Some AccountRef, Maybe AccountBalance)])
getBalances model accStore = performEventAsync $ flip push accStore $ \(keys, networkAccounts) -> do
  nodes <- fmap rights $ sample $ current $ model ^. network_selectedNodes
  net <- sample $ current $ model ^. network_selectedNetwork
  pure . Just $ mkRequests nodes net keys (Map.lookup net (unAccountStorage networkAccounts))
  where
    allChains = ChainId . tshow <$> ([0..9] :: [Int])
    allChainsLen = length allChains
    onAllChains = MonoidalMap.fromList . zip allChains . replicate allChainsLen

    mkRequests nodes net keys mAccounts cb = do
      -- Transform the vanity accounts structure into a map from chain ID to
      -- set of account names. We grab balances even for keys on all chains, but
      -- stick to only the chains that we know for vanity accounts otherwise we
      -- could easily prevent the user from being able to create accounts that
      -- they may wish to create (not to mention spamming extra vanity accounts
      -- makes the accounts page look ugly).
      --
      -- We spam extra balance requests here for keys rather than just adding the
      -- accounts on generateKey because if we just did it on create then we'd
      -- get the user stuck if they ever added in a new network.
      let
        chainsToAccounts = fold
            [ foldMap
              (\(an, m) ->
                foldMap (\cId ->
                  MonoidalMap.singleton cId (Set.singleton (unAccountName an)))
                (Map.keys m))
              (mAccounts ^.. _Just . accounts_vanity . to Map.toList . traverse)
            , onAllChains $ Set.fromList
              (keys ^.. to toList . traverse . to _key_pair . to _keyPair_publicKey . to keyToText)
            ]

        code = renderCompactText . accountBalanceObject . Set.toList
        pm chain = Pact.PublicMeta
          { Pact._pmChainId = chain
          , Pact._pmSender = "chainweaver"
          , Pact._pmGasLimit = defaultTransactionGasLimit
          , Pact._pmGasPrice = defaultTransactionGasPrice
          , Pact._pmTTL = 3600
          , Pact._pmCreationTime = 0
          }

      -- Build a request for each chain
      requests <- flip MonoidalMap.traverseWithKey chainsToAccounts $ \chain as -> do
        liftIO $ putStrLn $ "getBalances: Building request for get-balance on chain " <> T.unpack (_chainId chain)
        cmd <- buildCmd Nothing net (pm chain) mempty [] (code as) mempty mempty
        liftIO $ print cmd
        let envs = mkClientEnvs nodes chain
        pure $ doReqFailover envs (Api.local Api.apiV1Client cmd) >>= \case
          Left es -> liftIO $ putStrLn $ "getBalances: request failure: " <> show es
          Right cr -> case Pact._crResult cr of
            Pact.PactResult (Right pv) -> case parseAccountBalances pv of
              Left e -> liftIO $ putStrLn $ "getBalances: failed to parse balances:" <> show e
              Right balances -> liftIO $ do
                putStrLn "getBalances: success:"
                print balances
                -- Cheat slightly by checking if the account name is really a public key.
                liftIO $ cb $ ffor (Map.toList balances) $ \(AccountName name, balance) -> case textToKey name of
                  Nothing -> (net, Some $ AccountRef_Vanity (AccountName name) chain, balance)
                  Just pk -> (net, Some $ AccountRef_NonVanity pk chain, balance)
            Pact.PactResult (Left e) -> liftIO $ putStrLn $ "getBalances failed:" <> show e
      -- Perform the requests on a forked thread
      void $ liftJSM $ forkJSM $ void $ sequence requests

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
storeKeys :: (ToJSON key, HasStorage m) => KeyStorage key -> m ()
storeKeys = setItemStorage localStorage StoreFrontend_Wallet_Keys

-- | Load key pairs from localstorage.
loadKeys :: (FromJSON key, HasStorage m, Functor m) => m (Maybe (KeyStorage key))
loadKeys = getItemStorage localStorage StoreFrontend_Wallet_Keys

-- | Write key pairs to localstorage.
storeAccounts :: HasStorage m => AccountStorage -> m ()
storeAccounts = setItemStorage localStorage StoreFrontend_Wallet_Accounts

-- | Load accounts from localstorage.
loadAccounts :: (HasStorage m, Functor m) => m (Maybe AccountStorage)
loadAccounts = getItemStorage localStorage StoreFrontend_Wallet_Accounts

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
    , _walletCfg_updateKeyNotes = leftmost
      [ _walletCfg_updateKeyNotes c1
      , _walletCfg_updateKeyNotes c2
      ]
    }

instance Reflex t => Monoid (WalletCfg key t) where
  mempty = WalletCfg never never never never never never never never never
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
      <*> doSwitch never (_walletCfg_updateKeyNotes <$> ev)

instance Reflex t => Semigroup (Wallet key t) where
  wa <> wb = Wallet
    { _wallet_keys = _wallet_keys wa <> _wallet_keys wb
    , _wallet_accounts = _wallet_accounts wa <> _wallet_accounts wb
    }

instance Reflex t => Monoid (Wallet key t) where
  mempty = Wallet mempty mempty
  mappend = (<>)
