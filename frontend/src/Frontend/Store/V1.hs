{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontend.Store.V1 where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint (Dict(Dict))
import Data.Constraint.Extras
import Data.Map (Map)
import Data.Text (Text)

import Common.Wallet (KeyStorage, AccountStorage)
import Common.Network (NetworkName, NodeRef)
import Common.OAuth (OAuthProvider)
import Common.GistStore (GistMeta)
import Frontend.Store.TH

-- WARNING: Upstream deps. Check this when we bump pact and obelisk!
-- May be worth storing this in upstream independent datatypes.
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

-- The TH doesn't deal with the key type param well because the key in each constructor is actually a
-- different type variable to the one in the data decl.
--
-- src/Frontend/Store/V0.hs:69:1-29: error:
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
