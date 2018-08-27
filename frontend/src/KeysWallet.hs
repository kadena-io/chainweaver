{-# Language TemplateHaskell #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric         #-}
module KeysWallet where


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
data Key = Key
  { _key_publicKey :: Text
  , _key_privateKey :: Maybe Text
  }

-- | `Key` name to `Key` mapping
type Keys = Map Text Key

data KeysWalletConfig t = KeysWalletConfig
  { _keysWalletConfig_onRequestNewKey :: Event t Text
  -- ^ Request generation of a new key, that will be named as specified.
  }
  deriving Generic

instance Reflex t => Semigroup (KeysWalletConfig t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (KeysWalletConfig t) where
  mempty = memptydefault
  mappend = (<>)

makeLensesWith (lensRules & generateLazyPatterns .~ True) ''KeysWalletConfig

data KeysWallet t = KeysWallet
  { _keysWallet_keys :: Dynamic t Keys
  }
  deriving Generic

instance Reflex t => Semigroup (KeysWallet t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (KeysWallet t) where
  mempty = memptydefault
  mappend = (<>)

makeLensesWith (lensRules & generateLazyPatterns .~ True) ''KeysWallet

makeKeysWallet 
  :: forall t m. (MonadHold t m, PerformEvent t m, MonadFix m) 
  => KeysWalletConfig t 
  -> m (KeysWallet t)
makeKeysWallet conf = do
    onNewKey <- performEvent $ createKey <$> conf ^. keysWalletConfig_onRequestNewKey
    keys <- foldDyn (uncurry Map.insert) Map.empty onNewKey
    pure $ KeysWallet
      { _keysWallet_keys = keys
      }
  where
    -- TODO: Dummy implementationf for now
    createKey :: Text -> Performable m (Text, Key)
    createKey n = pure (n, Key n Nothing)
