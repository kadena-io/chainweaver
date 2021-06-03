{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontend.VersionedStore.V2 where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint (Dict(Dict))
import Data.Constraint.Extras
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum (DSum(..))
import qualified Data.Dependent.Map as DMap
import Data.Functor.Identity (Identity(Identity), runIdentity)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)

import Common.Foundation
import Common.Wallet
import Common.Network (NetworkName, NodeRef)
import Common.OAuth (OAuthProvider(..))
import Common.GistStore (GistMeta)

import Frontend.VersionedStore.TH
import qualified Frontend.VersionedStore.V0 as V0
import qualified Frontend.VersionedStore.V1 as V1
import qualified Frontend.VersionedStore.V0.Wallet as V0
import Frontend.VersionedStore.MigrationUtils
import Frontend.Crypto.Class

-- WARNING: Upstream deps. Check this when we bump pact and obelisk!
-- May be worth storing this in upstream independent datatypes.
import Pact.Types.ChainId (ChainId(ChainId))
import Pact.Types.ChainMeta (PublicMeta (..))
import Obelisk.OAuth.Common (AccessToken, OAuthState)

data StoreFrontend key a where
  StoreFrontend_Wallet_Keys :: StoreFrontend key (KeyStorage key)
  StoreFrontend_Wallet_Accounts :: StoreFrontend key AccountStorage

  StoreFrontend_Network_PublicMeta :: StoreFrontend key PublicMeta
  StoreFrontend_Network_Networks :: StoreFrontend key (Map NetworkName [NodeRef])
  StoreFrontend_Network_SelectedNetwork :: StoreFrontend key NetworkName

  StoreFrontend_OAuth_Tokens :: StoreFrontend key (Map OAuthProvider AccessToken)
  StoreFrontend_OAuth_State :: OAuthProvider -> StoreFrontend key OAuthState

  StoreFrontend_Gist_GistRequested :: StoreFrontend key (GistMeta, Text)

  StoreFrontend_ModuleExplorer_SessionFile :: StoreFrontend key Text

deriving instance Show (StoreFrontend key a)

upgradeFromV0 :: (Monad m, HasCrypto key m) => DMap (V0.StoreFrontend key) Identity -> m (DMap (StoreFrontend key) Identity)
upgradeFromV0 v0 = do
  (newKeysList, newAccountStorage) <- foldMapM splitOldKey oldKeysList
  let newKeys = IntMap.fromList newKeysList
  pure $ DMap.fromList . catMaybes $
    [ copyKeyDSum V0.StoreNetwork_PublicMeta StoreFrontend_Network_PublicMeta v0
    , copyKeyDSum V0.StoreNetwork_SelectedNetwork StoreFrontend_Network_SelectedNetwork v0
    -- Technically these are session only and shouldn't be here given the backup restore only works on
    -- local storage, but desktop ignores the session vs local distinction so migrating them probably
    -- does some good and certainly doesn't hurt.
    -- Also, this is currently being very lazy not leaning on the Universe instance of OAuthProvider
    , copyKeyDSum V0.StoreOAuth_Tokens StoreFrontend_OAuth_Tokens v0
    , copyKeyDSum (V0.StoreOAuth_State OAuthProvider_GitHub) (StoreFrontend_OAuth_State OAuthProvider_GitHub) v0

    , copyKeyDSum V0.StoreModuleExplorer_SessionFile StoreFrontend_ModuleExplorer_SessionFile v0

    , Just (StoreFrontend_Wallet_Keys :=> Identity newKeys)
    , Just (StoreFrontend_Wallet_Accounts :=> Identity newAccountStorage)
    , newNetworks
    ]
  where
    oldKeysList = maybe [] (IntMap.toList . runIdentity) (DMap.lookup V0.StoreWallet_Keys v0)

    -- We have to walk through the slightly different encoding of the Network information.
    -- Also if the storage contains _no_ network configuration then we shouldn't break the new version
    -- by storing an empty object.
    newNetworks = (\nets -> StoreFrontend_Network_Networks :=> Identity (V0.unNetworkMap $ runIdentity nets))
      <$> DMap.lookup V0.StoreNetwork_Networks v0

    -- This will regenerate the missing key. Desktop will recover the key with
    -- BIP, but the web version will generate a new key!
    splitOldKey (keyIdx, V0.SomeAccount_Deleted) = do
      (private, public) <- cryptoGenKey keyIdx
      let regenerated = KeyPair
            { _keyPair_publicKey = public
            , _keyPair_privateKey = Just private
            }
      pure ([(keyIdx, Key regenerated)], mempty)

    splitOldKey (keyIdx, V0.SomeAccount_Account a) = pure
      ([(keyIdx, Key (extractKey a))]
      , oldAccountToNewStorage a
      )

    oldAccountToNewStorage :: V0.Account key -> AccountStorage
    oldAccountToNewStorage a =
      let
        accountNameText = V0.unAccountName . V0._account_name $ a
        chainIdText = V0.unChainId . V0._account_chainId $ a
        newChainId = ChainId chainIdText
        accountNotesText = V0.unAccountNotes . V0._account_notes $ a
        newAccountNotes = mkAccountNotes accountNotesText
        newUnfinishedXChain = V0._account_unfinishedCrossChainTransfer a

        accounts = Map.singleton (AccountName accountNameText) $ AccountInfo Nothing
          $ Map.singleton newChainId $ VanityAccount newAccountNotes newUnfinishedXChain

      in AccountStorage $ Map.singleton (V0._account_network a) accounts

    upgradePublicKey = PublicKey . V0.unPublicKey

    extractKey (V0.Account { V0._account_key = kp } ) = KeyPair
      -- This relies on the V0.Wallet.PublicKey FromJSON checking that it is Base16!
      { _keyPair_publicKey = upgradePublicKey $ V0._keyPair_publicKey kp
      , _keyPair_privateKey = V0._keyPair_privateKey kp
      }

upgradeFromV1 :: (Monad m, HasCrypto key m) => DMap (V1.StoreFrontend key) Identity -> m (DMap (StoreFrontend key) Identity)
upgradeFromV1 = undefined

-- The TH doesn't deal with the key type param well because the key in each constructor is actually a
-- different type variable to the one in the data decl.
--
-- src/Frontend.VersionedStore/V0.hs:69:1-29: error:
--    The exact Name ‘key_a2Kfr’ is not in scope
--      Probable cause: you used a unique Template Haskell name (NameU),
--      perhaps via newName, but did not bind it
--      If that's it, then -ddump-splices might be useful

instance ArgDict c (StoreFrontend key) where
  type ConstraintsFor (StoreFrontend key) c
    = ( c (KeyStorage key)
      , c AccountStorage
      , c PublicMeta
      , c (Map NetworkName [NodeRef])
      , c NetworkName
      , c (Map OAuthProvider AccessToken)
      , c OAuthState
      , c (GistMeta, Text)
      , c Text
      )
  argDict = \case
    StoreFrontend_Wallet_Keys {} -> Dict
    StoreFrontend_Wallet_Accounts {} -> Dict
    StoreFrontend_Network_PublicMeta {} -> Dict
    StoreFrontend_Network_Networks {} -> Dict
    StoreFrontend_Network_SelectedNetwork {} -> Dict
    StoreFrontend_OAuth_Tokens {} -> Dict
    StoreFrontend_OAuth_State {} -> Dict
    StoreFrontend_Gist_GistRequested {} -> Dict
    StoreFrontend_ModuleExplorer_SessionFile {} -> Dict

deriveStoreInstances ''StoreFrontend
deriveJSONGADT ''StoreFrontend
