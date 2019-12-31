{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.StoreSpec where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decodeFileStrict)
import Data.IORef (readIORef)
import Data.Proxy (Proxy(Proxy))
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Pact.Types.ChainMeta (PublicMeta (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Text.URI (Authority(Authority))
import Text.URI.QQ (host)
import Obelisk.OAuth.Common (AccessToken, OAuthState)
import System.FilePath ((</>))

import Common.Wallet (KeyStorage, AccountStorage)
import Common.Network (NetworkName, uncheckedNetworkName, NodeRef(NodeRef))
import Common.OAuth (OAuthProvider)
import Common.GistStore (GistMeta)

import Frontend.Storage
import Frontend.Storage.InMemoryStorage

import qualified Frontend.Store.V0 as V0
import qualified Frontend.Store.V0.Wallet as V0
import qualified Frontend.Store.V1 as V1
import Frontend.Store (versioner)

type TestPrv = Text

-- Remember! The point of this test is to catch any errors when we bump pact or what not
-- It's important that these tests start from a JSON representation that represents what
-- we will actually be what the users will be reading to detect any changes in the serialisation
-- format!
--
-- Note that this isn't testing the backwards compatibility of the private key format! Something
-- else has to be doing that because the versioner doesn't know about it.
--
-- It's a good idea to steal the json files from an actual live app at the version!

expectedSelectedNetwork :: NetworkName
expectedSelectedNetwork = uncheckedNetworkName "Mainnet"

expectedNetworks :: Map NetworkName [NodeRef]
expectedNetworks = Map.fromList
  [ (uncheckedNetworkName "Mainnet",
    [ mkNodeRef [host|us-e1.chainweb.com|]
    , mkNodeRef [host|us-e2.chainweb.com|]
    , mkNodeRef [host|us-w1.chainweb.com|]
    , mkNodeRef [host|us-w2.chainweb.com|]
    , mkNodeRef [host|jp1.chainweb.com|]
    , mkNodeRef [host|jp2.chainweb.com|]
    , mkNodeRef [host|fr1.chainweb.com|]
    , mkNodeRef [host|fr2.chainweb.com|]
    ])
  , (uncheckedNetworkName "Testnet",
    [ mkNodeRef [host|us1.testnet.chainweb.com|]
    , mkNodeRef [host|us2.testnet.chainweb.com|]
    , mkNodeRef [host|eu1.testnet.chainweb.com|]
    , mkNodeRef [host|eu2.testnet.chainweb.com|]
    , mkNodeRef [host|ap1.testnet.chainweb.com|]
    , mkNodeRef [host|ap2.testnet.chainweb.com|]
    ])
  ]
  where
    mkNodeRef h = NodeRef $ Authority Nothing h Nothing

expectedKeys :: KeyStorage TestPrv
expectedKeys = error "write expected keys"

expectedAccounts :: AccountStorage
expectedAccounts = error "Write expected accounts"

testVersioner :: StorageVersioner (V1.StoreFrontend TestPrv)
testVersioner = versioner

test_v0ToV1Upgrade :: TestTree
test_v0ToV1Upgrade = testCase "V0 to V1 Upgrade" $ do
  (i,localRef,sessionRef) <- inMemoryStorageFromTestData
    (storageVersion_metaPrefix testVersioner)
    (Proxy @(V0.StoreFrontend TestPrv))
    0
    path
  (sn, pm, ns, sf, ks, as) <- flip runStorageT i $ runStorageIO $ do
    storageVersioner_upgrade testVersioner
    sn <- getItemStorage localStorage V1.StoreFrontend_Network_SelectedNetwork
    pm <- getItemStorage localStorage V1.StoreFrontend_Network_PublicMeta
    ns <- getItemStorage localStorage V1.StoreFrontend_Network_Networks
    sf <- getItemStorage localStorage V1.StoreFrontend_ModuleExplorer_SessionFile
    ks <- getItemStorage localStorage (V1.StoreFrontend_Wallet_Keys @TestPrv)
    as <- getItemStorage localStorage V1.StoreFrontend_Wallet_Accounts
    pure (sn, pm, ns, sf, ks, as)

  curV0Seq <- lookupRef localRef "StoreFrontend_Meta_Backups_V0_Latest"
  curV0Seq @?= Just "0"
  curV <- lookupRef localRef "StoreFrontend_Meta_Version"
  curV @?= Just "1"

  sn @?= Just expectedSelectedNetwork
  ns @?= Just expectedNetworks
  pm @?= Nothing
  expectedSfText <- decodeFileStrict (path </> "StoreModuleExplorer_SessionFile")
  sf @?= expectedSfText

  -- Check that we just have the keys from the new schema in the DB plus the backup.
  lkeys <- sort . Map.keys <$> readIORef localRef
  skeys <- sort . Map.keys <$> readIORef sessionRef

  lkeys @?=
    [ "StoreFrontend_Meta_Backups_V0_0"
    , "StoreFrontend_Meta_Backups_V0_Latest"
    , "StoreFrontend_Meta_Version"
    , "StoreFrontend_ModuleExplorer_SessionFile"
    , "StoreFrontend_Network_Networks"
    , "StoreFrontend_Network_SelectedNetwork"
    , "StoreFrontend_Wallet_Accounts"
    , "StoreFrontend_Wallet_Keys"
    ]
  skeys @?= []

  ks @?= Just expectedKeys
  as @?= Just expectedAccounts

  pure ()

  where
    path = "tests" </> "Frontend" </> "StoreSpec.files" </> "V0"

tests :: TestTree
tests = testGroup "StoreSpec"
  [ test_v0ToV1Upgrade
  ]
