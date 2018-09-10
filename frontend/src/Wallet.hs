{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts       #-}
module Wallet where


import           Control.Lens
import           Control.Monad.Fix
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Semigroup
import           Data.Text                (Text)
import           Generics.Deriving.Monoid (mappenddefault, memptydefault)
import           GHC.Generics             (Generic)
import           Reflex
import           Data.Set                 (Set)
import qualified Data.Set as Set

-- | A key consists of a public key and an optional private key.
data KeyPair t = KeyPair
  { _keyPair_publicKey  :: Text
  , _keyPair_privateKey :: Maybe Text
  , _keyPair_forSigning :: Dynamic t Bool
  }

makeLensesWith (lensRules & generateLazyPatterns .~ True) ''KeyPair

-- | `Key` name to `Key` mapping
type Keys t = Map Text (KeyPair t)

data WalletCfg t = WalletCfg
  { _walletCfg_onRequestNewKey :: Event t Text
  -- ^ Request generation of a new key, that will be named as specified.
  , _walletCfg_onSetSigning    :: Event t (Text, Bool)
  -- ^ Use a given key for signing messages/or not.
  }
  deriving Generic

makeLensesWith (lensRules & generateLazyPatterns .~ True) ''WalletCfg

data Wallet t = Wallet
  { _wallet_keys        :: Dynamic t (Keys t)
  }
  deriving Generic

makeLensesWith (lensRules & generateLazyPatterns .~ True) ''Wallet

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


makeWallet
  :: forall t m. (MonadHold t m, PerformEvent t m, MonadFix m)
  => WalletCfg t
  -> m (Wallet t)
makeWallet conf = do
    let onNewDeleted p = fmap fst . ffilter (p . snd)
    signingKeys <- foldDyn id Set.empty
      $ leftmost [ Set.delete <$> onNewDeleted not (conf ^. walletCfg_onSetSigning)
                 , Set.insert <$> onNewDeleted id (conf ^. walletCfg_onSetSigning)
                 , Set.insert <$> conf ^. walletCfg_onRequestNewKey
                 ]

    onNewKey <- performEvent $ createKey signingKeys <$> conf ^. walletCfg_onRequestNewKey
    keys <- foldDyn (uncurry Map.insert) Map.empty onNewKey
    pure $ Wallet
      { _wallet_keys = keys
      }
  where
    -- TODO: Dummy implementationf for now
    createKey :: Dynamic t (Set Text) -> Text -> Performable m (Text, KeyPair t)
    createKey signing n = do
      pure (n, KeyPair n (Just n) (Set.member n <$> signing))
