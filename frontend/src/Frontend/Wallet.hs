{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , PublicKeyPrefix (..)
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
  , Account (..)
  , account_status
  , account_storage
  , AccountData (..)
  , _AccountData
  , accountKeys
  , accountHasFunds
  , accountSatisfiesKeysetPredicate
  -- * Creation
  , makeWallet
  , loadKeys
  , storeKeys
  -- * Parsing
  , parseWalletKeyPair
  -- * Other helper functions
  , checkAccountNameAvailability
  , snocIntMap
  , findNextKey
  , getSigningPairs
  , genZeroKeyPrefix
  , addDecimalPoint
  , module Common.Wallet
  ) where

import Control.Error.Util (hush)
import Control.Lens hiding ((.=))
import Control.Monad (guard, void)
import Control.Monad.Except (runExcept)
import Control.Monad.Fix
import Data.Aeson
import Data.Decimal
import Data.Either (rights)
import Data.IntMap (IntMap)
import Data.List (nub)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import Kadena.SigningApi (AccountName(..), mkAccountName)
import Pact.Types.ChainId
import Pact.Types.Pretty
import Reflex
import Reflex.Dom.Core ((=:))

import qualified Data.Text as Text
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Map.Monoidal as MonoidalMap
import qualified Data.Set as Set
import qualified Pact.Server.ApiClient as Api
import qualified Pact.Types.ChainMeta as Pact
import           Pact.Types.Names (ModuleName, parseModuleName)
import qualified Pact.Types.Command as Pact

import Common.Network (NetworkName)
import Common.Wallet
import Common.Orphans ()

import Frontend.AppCfg
import Frontend.Crypto.Class
import Frontend.Crypto.Ed25519
import Frontend.Foundation
import Frontend.Storage
import Frontend.Network
import Frontend.VersionedStore
import Frontend.Log

import System.IO.Unsafe

data WalletCfg key t = WalletCfg
  { _walletCfg_genKey :: Event t ()
  -- ^ Request generation of a new key
  , _walletCfg_delAccount :: Event t (NetworkName, AccountName)
  -- ^ Remove an account from the wallet
  , _walletCfg_importAccount  :: Event t (NetworkName, AccountName)
  -- ^ Add this account to the wallet on the given network
  , _walletCfg_refreshBalances :: Event t ()
  -- ^ Refresh balances in the wallet
  , _walletCfg_setCrossChainTransfer :: Event t (NetworkName, AccountName, ChainId, Maybe UnfinishedCrossChainTransfer)
  -- ^ Start a cross chain transfer on some account. This field allows us to
  -- recover when something goes badly wrong in the middle, since it's
  -- immediately stored with all info we need to retry.
  , _walletCfg_updateAccountNotes :: Event t (NetworkName, AccountName, Maybe ChainId, Maybe AccountNotes)
  , _walletCfg_fungibleModule :: Event t Text
  }
  deriving Generic

makePactLenses ''WalletCfg

-- | HasWalletCfg with additional constraints to make it behave like a proper
-- "Config".
type IsWalletCfg cfg key t = (HasWalletCfg cfg key t, Monoid cfg, Flattenable cfg t)

data Wallet key t = Wallet
  { _wallet_keys :: Dynamic t (KeyStorage key)
    -- ^ Accounts added and removed by the user
  , _wallet_accounts :: Dynamic t AccountData
    -- ^ Accounts added and removed by the user
  , _wallet_fungible :: Dynamic t ModuleName
  }
  deriving Generic

data Account = Account
  { _account_status :: AccountStatus AccountDetails
  , _account_storage :: VanityAccount
  } deriving (Eq, Show)

emptyAccount :: Account
emptyAccount = Account
  { _account_storage = blankVanityAccount
  , _account_status = AccountStatus_Unknown
  }

newtype PublicKeyPrefix = PublicKeyPrefix
  { _unPublicKeyPrefix :: Text
  }

newtype AccountData = AccountData
  { unAccountData :: Map NetworkName (Map AccountName (AccountInfo Account))
  } deriving (Generic, Show)

instance Semigroup AccountData where
  AccountData a1 <> AccountData a2 = AccountData $ Map.unionWith (Map.unionWith (<>)) a1 a2

instance Monoid AccountData where
  mempty = AccountData mempty

makePactPrisms ''AccountData
makePactLenses ''Wallet
makePactLenses ''Account

getSigningPairs
  :: ChainId
  -> KeyStorage key
  -> Map AccountName (AccountInfo Account)
  -> Set AccountName
  -> [KeyPair key]
getSigningPairs chain allKeys allAccounts signing =
  filterKeyPairs (Set.unions wantedKeys) allKeys
  where
    wantedKeys = Map.restrictKeys allAccounts signing ^.. folded
      . accountInfo_chains
      . ix chain
      . account_status
      . _AccountStatus_Exists
      . accountDetails_guard
      . _AccountGuard_KeySetLike
      . ksh_keys

-- TODO replace this at the use sites with proper multisig
accountKeys :: Account -> Set.Set PublicKey
accountKeys a = a ^. account_status . _AccountStatus_Exists . accountDetails_guard . _AccountGuard_KeySetLike . ksh_keys

accountHasFunds :: Account -> Maybe Bool
accountHasFunds a = fmap (> 0) $ a ^? account_status . _AccountStatus_Exists . accountDetails_balance

accountSatisfiesKeysetPredicate :: IntMap (Key key) -> Account -> Bool
accountSatisfiesKeysetPredicate keys a = fromMaybe False
  $ fmap (flip keysetSatisfiesPredicate keys)
  $ a ^? account_status . _AccountStatus_Exists . accountDetails_guard

snocIntMap :: a -> IntMap a -> IntMap a
snocIntMap a m = IntMap.insert (nextKey m) a m

nextKey :: IntMap a -> Int
nextKey = maybe 0 (succ . fst) . IntMap.lookupMax

findNextKey :: Reflex t => Wallet key t -> Dynamic t Int
findNextKey = fmap nextKey . _wallet_keys

genZeroKeyPrefix
  :: ( Functor m
     , HasCrypto key m
     )
  => m PublicKeyPrefix
genZeroKeyPrefix =
  PublicKeyPrefix . Text.take 8 . keyToText . snd <$> cryptoGenKey 0

-- | Make a functional wallet that can contain actual keys.
makeWallet
  :: forall model key t m.
    ( MonadHold t m, PerformEvent t m
    , MonadFix m, MonadJSM (Performable m)
    , HasStorage (Performable m), HasStorage m
    , HasNetwork model t
    , HasLogger model t
    , TriggerEvent t m
    , HasCrypto key (Performable m)
    , MonadJSM (Performable m)
    , FromJSON key, ToJSON key
    , PostBuild t m
    )
  => Maybe (ChangePassword key t m)
  -> model
  -> WalletCfg key t
  -> m (Wallet key t)
makeWallet mChangePassword model conf = do
  pb <- getPostBuild
  initialKeys <- fromMaybe IntMap.empty <$> loadKeys
  initialAccounts <- maybe (AccountData mempty) fromStorage <$> loadAccounts

  rec
    onNewKey <- performEvent $ leftmost
      [ fmapMaybe id $ addStarterKey <$> current keys <@ pb
      , addNewKey <$> current keys <@ _walletCfg_genKey conf
      ]

    keys <- foldDyn id initialKeys $ leftmost
      [ snocIntMap <$> onNewKey
      , fmap const keyChangeOnPwdResetE
      ]
    keyChangeOnPwdResetE <- case fmap _changePassword_updateKeys mChangePassword of
      Nothing -> pure never
      Just (changePwdE, changeKeysAction) -> do
        let
          currentKeyMap = attach (current keys) changePwdE
        performEvent $ ffor currentKeyMap $ \(km, (newRoot, newPwd)) ->
          flip IntMap.traverseWithKey km $ \i _ -> changeKeysAction i newRoot newPwd

  -- Slight hack here, even with prompt tagging we don't pick up the new accounts
  newNetwork <- delay 0.5 $ void $ updated $ model ^. network_selectedNetwork
  -- Another slight hack. We want to load the account details on load, but if we don't delay
  -- the selectedNodes sampled in getAccountStatus is empty. Sadness
  initialLoad <- getPostBuild >>= delay 0.5
  fullRefresh <- throttle 3 $ leftmost [_walletCfg_refreshBalances conf, newNetwork, initialLoad]

  dFungible <- holdDyn "coin" $
    fmapMaybe (hush . parseModuleName) $ _walletCfg_fungibleModule conf
  fetchFungStatus <- delay 0.1 $ updated dFungible

  let newAccountWithNodes = attach
        (fmap rights (current $ model ^. network_selectedNodes))
        (_walletCfg_importAccount conf)

  rec
    newStatuses <- getAccountStatus model dFungible $ leftmost
      [ current accounts <@ fullRefresh
      , current accounts <@ fetchFungStatus
      -- Get details for a new account
      , ffor (_walletCfg_importAccount conf) $ \(net, name) -> AccountData $ net =: name =: mempty
      ]

    -- TODO: Need to enforce the following invariants
    --  1) An "AccountInfo Account" structure must always contain 20 / <max chains> entries in its
    --  map
    --  2) Accounts cannot be deleted except for with "removeAccount" event
    --  3) An Account status update should only be applied if the fungible used to create the
    --  request is still the active fungible
    accounts <- foldDyn id initialAccounts $ leftmost
      [ ffor newAccountWithNodes $ \(nodes, (net, name)) ->
          ((<>) (AccountData $ net =: name =: (newAccountView nodes)))
      -- Add the public key as an account to get people started
      , attachWith addStarterAccount (current $ model ^. network_selectedNetwork) (updated keys)
      , ffor (_walletCfg_updateAccountNotes conf) updateAccountNotes
      , ffor (_walletCfg_setCrossChainTransfer conf) updateCrossChain
      , ffor (_walletCfg_delAccount conf) removeAccount
      , foldr (.) id . fmap updateAccountStatus <$> newStatuses
      -- zero out account balance on new fungible
      , ffor (updated dFungible) $ \_ prevState -> AccountData $
          ffor (unAccountData prevState) $ \networkAccount ->
            ffor networkAccount $ \info -> info
              { _accountInfo_chains = ffor (_accountInfo_chains info)
                $ \val -> val { _account_status = AccountStatus_Unknown }}
      ]

  performEvent_ $ storeKeys <$> updated keys
  store <- holdUniqDyn $ toStorage <$> accounts
  performEvent_ $ storeAccounts <$> updated store


  pure $ Wallet
    { _wallet_keys = keys
    , _wallet_accounts = accounts
    , _wallet_fungible = dFungible
    }
  where
    addStarterKey m = if IntMap.null m then Just (addNewKey m) else Nothing
    --addStarterAccount :: IntMap (Key key) -> AccountData -> AccountData
    addStarterAccount net ks ad =
      case IntMap.toList ks of
        [(i,k)] -> if Map.size (ad ^. _AccountData . ix net) == 0
                     then let accName = (AccountName $ "k:" <> (keyToText $ _keyPair_publicKey $ _key_pair k))
                           in ad <> (AccountData $ net =: accName =: mempty)
                     else ad
        _ -> ad

    -- Creates a new AccountInfo Account with all 20 chains
    newAccountView nodes = AccountInfo Nothing $
      Map.fromList $ zip (nub $ foldMap getChains nodes) $ repeat emptyAccount

    addNewKey :: IntMap a -> Performable m (Key key)
    addNewKey = createKey . nextKey

    fromStorage :: AccountStorage -> AccountData
    fromStorage (AccountStorage m) = AccountData $ (fmap . fmap . fmap) (Account AccountStatus_Unknown) m

    toStorage :: AccountData -> AccountStorage
    toStorage (AccountData m) = AccountStorage $ (fmap . fmap . fmap) _account_storage m

    updateAccountNotes :: (NetworkName, AccountName, Maybe ChainId, Maybe AccountNotes) -> AccountData -> AccountData
    updateAccountNotes (net, name, chain, notes) =
      let optic = case chain of
            Nothing -> accountInfo_notes
            Just c -> accountInfo_chains . ix c . account_storage . vanityAccount_notes
       in _AccountData . ix net . ix name . optic .~ notes

    removeAccount :: (NetworkName, AccountName) -> AccountData -> AccountData
    removeAccount (net, name) = _AccountData . ix net %~ sans name

    updateCrossChain :: (NetworkName, AccountName, ChainId, Maybe UnfinishedCrossChainTransfer) -> AccountData -> AccountData
    updateCrossChain (net, name, chain, cct) =
      _AccountData . ix net . ix name . accountInfo_chains . ix chain . account_storage . vanityAccount_unfinishedCrossChainTransfer .~ cct

    updateAccountStatus :: (NetworkName, AccountName, ChainId, AccountStatus AccountDetails) -> AccountData -> AccountData
    updateAccountStatus (net, name, chain, status) = _AccountData
      . upsert net Map.empty
      . upsert name mempty . accountInfo_chains
      . upsert chain emptyAccount . account_status .~ status

    createKey :: Int -> Performable m (Key key)
    createKey i = do
      (privKey, pubKey) <- cryptoGenKey i
      pure $ Key
        { _key_pair = KeyPair pubKey (Just privKey)
        }

-- | Get the details of accounts from the network.
getAccountStatus
  :: forall model key t m.
    ( PerformEvent t m, TriggerEvent t m
    , MonadJSM (Performable m)
    , HasLogger model t
    , HasNetwork model t, HasCrypto key (Performable m)
    )
  => model -> Dynamic t ModuleName -> Event t AccountData -> m (Event t [(NetworkName, AccountName, ChainId, AccountStatus AccountDetails)])
getAccountStatus model dFungible accStore = performEventAsync $ flip push accStore $ \(AccountData networkAccounts) -> do
  nodes <- fmap rights $ sample $ current $ model ^. network_selectedNodes
  net <- sample $ current $ model ^. network_selectedNetwork
  -- This works because A) we are inside a `push` which means we sample at the
  -- time of firing, and B) because the dFungible event is used as a trigger
  -- for the accStore event. That being said, this code probably should be
  -- refactored and cleaned up so that we arent dependent on dFungible being a
  -- basis event for accStore.
  fungible <- sample $ current $ dFungible
  pure . Just $ mkRequests fungible nodes net (Map.lookup net networkAccounts)
  where
    mkRequests fungible nodes net mAccounts cb = do
      -- Transform the accounts structure into a map from chain ID to
      -- set of account names. We grab balances accounts on all chains,
      -- but only if the user has actually clicked Add Account for that
      -- account name. We no longer automatically add public keys as an
      -- account.
      let
        allAccounts = case mAccounts of
            Nothing -> mempty
            Just as -> Set.fromList $ fmap unAccountName $ Map.keys as

        allChains = foldMap (Set.fromList . getChains) nodes

        chainsToAccounts = MonoidalMap.fromSet (const allAccounts) allChains

        code = renderCompactText . accountDetailsObject fungible . Set.toList
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
        putLog model LevelInfo $ "getAccountStatus: Building request for get-balance on chain " <> _chainId chain
        cmd <- buildCmd Nothing net (pm chain) mempty [] (code as) mempty mempty
        let envs = mkClientEnvs nodes chain
        pure $ doReqFailover envs (Api.local Api.apiV1Client cmd) >>= \case
          Left es -> putLog model LevelInfo $ "getAccountStatus: request failure: " <> tshow es
          Right cr -> case Pact._crResult cr of
            Pact.PactResult (Right pv) -> case parseAccountDetails pv of
              Left e -> putLog model LevelInfo $ "getAccountStatus: failed to parse balances:" <> tshow e
              Right balances -> liftIO $ do
                putLog model LevelInfo $ "getAccountStatus: success:"
                putLog model LevelInfo $ tshow balances
                liftIO $ cb $ ffor (Map.toList balances) $ \(name, balance) -> (net, name, chain, balance)
            Pact.PactResult (Left e) -> putLog model LevelInfo $ "getAccountStatus failed:" <> tshow e
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

checkAccountNameAvailability
  :: NetworkName
  -> AccountData
  -> AccountName
  -> Either Text AccountName
checkAccountNameAvailability net (AccountData networks) acc = do
  maybe (Right acc) (\_ -> Left "This account name is already in use") $ do
    accounts <- Map.lookup net networks
    guard $ Map.member acc accounts
    pure acc

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
    , _walletCfg_delAccount = leftmost
      [ _walletCfg_delAccount c1
      , _walletCfg_delAccount c2
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
    , _walletCfg_fungibleModule = leftmost
      [ _walletCfg_fungibleModule c1
      , _walletCfg_fungibleModule c2
      ]
    }

instance Reflex t => Monoid (WalletCfg key t) where
  mempty = WalletCfg never never never never never never never
  mappend = (<>)

instance Flattenable (WalletCfg key t) t where
  flattenWith doSwitch ev =
    WalletCfg
      <$> doSwitch never (_walletCfg_genKey <$> ev)
      <*> doSwitch never (_walletCfg_delAccount <$> ev)
      <*> doSwitch never (_walletCfg_importAccount <$> ev)
      <*> doSwitch never (_walletCfg_refreshBalances <$> ev)
      <*> doSwitch never (_walletCfg_setCrossChainTransfer <$> ev)
      <*> doSwitch never (_walletCfg_updateAccountNotes <$> ev)
      <*> doSwitch never (_walletCfg_fungibleModule <$> ev)

-- instance Reflex t => Semigroup (Wallet key t) where
--   wa <> wb = Wallet
--     { _wallet_keys = _wallet_keys wa <> _wallet_keys wb
--     , _wallet_accounts = _wallet_accounts wa <> _wallet_accounts wb
--     }
--
-- instance Reflex t => Monoid (Wallet key t) where
--   mempty = Wallet mempty mempty
--   mappend = (<>)

-- Helper for getting around the fact that the coin contract doesn't allow
-- integer amounts.  Can't think of a better place to put this.
addDecimalPoint :: Decimal -> Decimal
addDecimalPoint d@(Decimal ps m) = if ps == 0 then Decimal 1 (m*10) else d
