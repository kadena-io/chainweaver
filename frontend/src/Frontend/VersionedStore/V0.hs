{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontend.VersionedStore.V0 where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint (Dict(Dict))
import Data.Constraint.Extras
import Data.Map (Map)
import Data.Text (Text)

import Frontend.VersionedStore.TH
import Frontend.VersionedStore.V0.Wallet

-- WARNING: Be careful about changing stuff in here. Tests will catch snafus here and upstream though
import Common.Network (NetworkName)
import Common.OAuth (OAuthProvider)
import Common.GistStore (GistMeta)

-- WARNING: Upstream deps. Check this when we bump pact and obelisk!
-- May be worth storing this in upstream independent datatypes.
import Pact.Types.ChainMeta (PublicMeta (..))
import Obelisk.OAuth.Common (AccessToken, OAuthState)

data StoreFrontend key a where
  StoreWallet_Keys :: StoreFrontend key (Accounts key)

  StoreNetwork_PublicMeta :: StoreFrontend key PublicMeta

  StoreNetwork_Networks :: StoreFrontend key NetworkMap
  StoreNetwork_SelectedNetwork :: StoreFrontend key NetworkName

  StoreOAuth_Tokens :: StoreFrontend key (Map OAuthProvider AccessToken)
  StoreOAuth_State :: OAuthProvider -> StoreFrontend key OAuthState

  StoreGist_GistRequested :: StoreFrontend key (GistMeta, Text)

  StoreModuleExplorer_SessionFile :: StoreFrontend key Text

deriving instance Show (StoreFrontend key a)

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
    = ( c (Accounts key)
      , c PublicMeta
      , c NetworkMap
      , c NetworkName
      , c (Map OAuthProvider AccessToken)
      , c OAuthState
      , c (GistMeta, Text)
      , c Text
      )
  argDict = \case
    StoreWallet_Keys {} -> Dict
    StoreNetwork_PublicMeta {} -> Dict
    StoreNetwork_Networks {} -> Dict
    StoreNetwork_SelectedNetwork {} -> Dict
    StoreOAuth_Tokens {} -> Dict
    StoreOAuth_State {} -> Dict
    StoreGist_GistRequested {} -> Dict
    StoreModuleExplorer_SessionFile {} -> Dict

deriveStoreInstances ''StoreFrontend
deriveJSONGADT ''StoreFrontend
