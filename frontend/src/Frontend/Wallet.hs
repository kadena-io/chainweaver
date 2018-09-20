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
  , KeyPair (..)
  , HasKeyPair (..)
  , KeyPairs
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
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Semigroup
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import           Generics.Deriving.Monoid    (mappenddefault, memptydefault)
import           GHC.Generics                (Generic)
import           Language.Javascript.JSaddle (MonadJSM)
import           Reflex

import           Frontend.Crypto.Ed25519
import           Frontend.Foundation

-- | Type of a `Key` name.
--
--   All keys are accessible by a name of type `KeyName`
type KeyName = Text

-- | A key consists of a public key and an optional private key.
data KeyPair t = KeyPair
  { _keyPair_publicKey  :: PublicKey
  , _keyPair_privateKey :: Maybe PrivateKey
  , _keyPair_forSigning :: Dynamic t Bool
  }

makePactLenses ''KeyPair

-- | `KeyName` to `Key` mapping
type KeyPairs t = Map KeyName (KeyPair t)

data WalletCfg t = WalletCfg
  { _walletCfg_onRequestNewKey :: Event t KeyName
  -- ^ Request generation of a new key, that will be named as specified.
  , _walletCfg_onSetSigning    :: Event t (KeyName, Bool)
  -- ^ Use a given key for signing messages/or not.
  }
  deriving Generic

makePactLenses ''WalletCfg

data Wallet t = Wallet
  { _wallet_keys        :: Dynamic t (KeyPairs t)
  }
  deriving Generic

makePactLenses ''Wallet

makeWallet
  :: forall t m.
    ( MonadHold t m, PerformEvent t m
    , MonadFix m, MonadJSM (Performable m)
    )
  => WalletCfg t
  -> m (Wallet t)
makeWallet conf = do
    let onNewDeleted p = fmap fst . ffilter (p . snd)
    signingKeys <- foldDyn id Set.empty
      $ leftmost [ Set.delete <$> onNewDeleted not (conf ^. walletCfg_onSetSigning)
                 , Set.insert <$> onNewDeleted id (conf ^. walletCfg_onSetSigning)
                 , Set.insert <$> conf ^. walletCfg_onRequestNewKey
                 ]

    onNewKey <- performEvent $ createKey signingKeys <$>
      -- Filter out keys with empty names
      ffilter (/= "") (conf ^. walletCfg_onRequestNewKey)

    -- Filter out duplicate keys
    keys <- foldDyn (uncurry (Map.insertWith (flip const))) Map.empty onNewKey
    pure $ Wallet
      { _wallet_keys = keys
      }
  where
    -- TODO: Dummy implementation for now
    createKey :: Dynamic t (Set KeyName) -> KeyName -> Performable m (KeyName, KeyPair t)
    createKey signing n = do
      (privKey, pubKey) <- genKeyPair
      pure (n, KeyPair pubKey (Just privKey) (Set.member n <$> signing))



-- Utility functions:

instance Reflex t => Semigroup (WalletCfg t) where
  c1 <> c2 =
    WalletCfg
      { _walletCfg_onRequestNewKey = leftmost [ _walletCfg_onRequestNewKey c1
                                              , _walletCfg_onRequestNewKey c2
                                              ]
      , _walletCfg_onSetSigning = leftmost [ _walletCfg_onSetSigning c1
                                           , _walletCfg_onSetSigning c2
                                           ]
      }

instance Reflex t => Monoid (WalletCfg t) where
  mempty = WalletCfg never never
  mappend = (<>)

instance Reflex t => Semigroup (Wallet t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (Wallet t) where
  mempty = memptydefault
  mappend = (<>)
