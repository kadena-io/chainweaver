{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
module Frontend.Wallet where


import Control.Lens
import Control.Monad.Fix
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import Data.Text (Text)
import Generics.Deriving.Monoid (mappenddefault, memptydefault)
import GHC.Generics (Generic)
import Reflex
import Data.Set (Set)
import qualified Data.Set as Set

-- | Type of a `Key` name.
--
--   All keys are accessible by a name of type `KeyName`
type KeyName = Text

-- | Type of a `PublicKey` in a `KeyPair`.
newtype PublicKey = PublicKey
  { unPublicKey :: Text -- ^ Get the internal `PublicKey definition`.
  }
--
-- | Type of a `PrivateKey` in a `KeyPair`.
newtype PrivateKey = PrivateKey
  { unPrivateKey :: Text -- ^ Get the internal `PrivateKey definition`.
  }

-- | A key consists of a public key and an optional private key.
data KeyPair t = KeyPair
  { _keyPair_publicKey  :: PublicKey
  , _keyPair_privateKey :: Maybe PrivateKey
  , _keyPair_forSigning :: Dynamic t Bool
  }

makeLensesWith (lensRules & generateLazyPatterns .~ True) ''KeyPair

-- | `KeyName` to `Key` mapping
type KeyPairs t = Map KeyName (KeyPair t)

data WalletCfg t = WalletCfg
  { _walletCfg_onRequestNewKey :: Event t KeyName
  -- ^ Request generation of a new key, that will be named as specified.
  , _walletCfg_onSetSigning    :: Event t (KeyName, Bool)
  -- ^ Use a given key for signing messages/or not.
  }
  deriving Generic

makeLensesWith (lensRules & generateLazyPatterns .~ True) ''WalletCfg

data Wallet t = Wallet
  { _wallet_keys        :: Dynamic t (KeyPairs t)
  {- , _wallet_newKey      :: Event t KeyName -- ^ A new key just got created. -}
  {- , _wallet_delKey      :: Event t KeyName -- ^ A key just got removed. (Removal not yet implemented though.) -}
  }
  deriving Generic

makeLensesWith (lensRules & generateLazyPatterns .~ True) ''Wallet

makeWallet
  :: forall t m
   . (MonadHold t m, PerformEvent t m, MonadFix m)
  => WalletCfg t
  -> m (Wallet t)
makeWallet conf = do
  let onNewDeleted p = fmap fst . ffilter (p . snd)
  signingKeys <- foldDyn id Set.empty $ leftmost
    [ Set.delete <$> onNewDeleted not (conf ^. walletCfg_onSetSigning)
    , Set.insert <$> onNewDeleted id (conf ^. walletCfg_onSetSigning)
    , Set.insert <$> conf ^. walletCfg_onRequestNewKey
    ]

  onNewKey <- performEvent $ createKey signingKeys <$>
    -- Filter out keys with empty names
                                                       ffilter
    (/= "")
    (conf ^. walletCfg_onRequestNewKey)

  -- Filter out duplicate keys
  keys <- foldDyn (uncurry (Map.insertWith (flip const))) Map.empty onNewKey
  pure $ Wallet { _wallet_keys = keys }
 where
    -- TODO: Dummy implementation for now
  createKey
    :: Dynamic t (Set KeyName) -> KeyName -> Performable m (KeyName, KeyPair t)
  createKey signing n = do
    pure
      ( n
      , KeyPair (PublicKey n) (Just (PrivateKey n)) (Set.member n <$> signing)
      )

-- Boring instances:

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
