{-# Language TemplateHaskell #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric         #-}
module Wallet where


import Reflex
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text)
import Control.Lens
import Control.Monad.Fix
import           Generics.Deriving.Monoid    (mappenddefault, memptydefault)
import           Data.Semigroup
import           GHC.Generics                (Generic)

-- | A key consists of a public key and an optional private key.
data KeyPair = KeyPair
  { _keyPair_publicKey :: Text
  , _keyPair_privateKey :: Maybe Text
  }

-- | `Key` name to `Key` mapping
type Keys = Map Text KeyPair

data WalletConfig t = WalletConfig
  { _walletConfig_onRequestNewKey :: Event t Text
  -- ^ Request generation of a new key, that will be named as specified.
  }
  deriving Generic

makeLensesWith (lensRules & generateLazyPatterns .~ True) ''WalletConfig

data Wallet t = Wallet
  { _wallet_keys :: Dynamic t Keys
  }
  deriving Generic

makeLensesWith (lensRules & generateLazyPatterns .~ True) ''Wallet

instance Reflex t => Semigroup (WalletConfig t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (WalletConfig t) where
  mempty = memptydefault
  mappend = (<>)

instance Reflex t => Semigroup (Wallet t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (Wallet t) where
  mempty = memptydefault
  mappend = (<>)


makeWallet 
  :: forall t m. (MonadHold t m, PerformEvent t m, MonadFix m) 
  => WalletConfig t 
  -> m (Wallet t)
makeWallet conf = do
    onNewKey <- performEvent $ createKey <$> conf ^. walletConfig_onRequestNewKey
    keys <- foldDyn (uncurry Map.insert) Map.empty onNewKey
    pure $ Wallet
      { _wallet_keys = keys
      }
  where
    -- TODO: Dummy implementationf for now
    createKey :: Text -> Performable m (Text, KeyPair)
    createKey n = pure (n, KeyPair n (Just n))
