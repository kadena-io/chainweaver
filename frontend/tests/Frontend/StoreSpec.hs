{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.StoreSpec where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decodeFileStrict)
import qualified Data.IntMap as IntMap
import Data.IORef (readIORef)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Pact.Types.Util (ParseText, fromText')
import Pact.Types.Command (RequestKey(..))
import Pact.Types.ChainMeta (PublicMeta (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Text.URI (Authority(Authority))
import Text.URI.QQ (host)
import Obelisk.OAuth.Common (AccessToken, OAuthState)
import System.FilePath ((</>))
import TestUtils ((@?~))

import Common.Wallet
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
--
-- TODO: There's no data for the gist and oauth stuff here, but given desktop doesn't do that
-- and it hasn't changed, that should be okay.

expectedSelectedNetwork :: NetworkName
expectedSelectedNetwork = uncheckedNetworkName "devnet"

expectedNetworks :: Map NetworkName [NodeRef]
expectedNetworks = Map.fromList
  [ (uncheckedNetworkName "devnet",
    [ mkNodeRef [host|us1.tn1.chainweb.com|]
    ])
  , (uncheckedNetworkName "testnet",
    [ mkNodeRef [host|ap1.testnet.chainweb.com|]
    , mkNodeRef [host|ap2.testnet.chainweb.com|]
    , mkNodeRef [host|eu1.testnet.chainweb.com|]
    , mkNodeRef [host|eu2.testnet.chainweb.com|]
    , mkNodeRef [host|us1.testnet.chainweb.com|]
    , mkNodeRef [host|us2.testnet.chainweb.com|]
    ])
  ]
  where
    mkNodeRef h = NodeRef $ Authority Nothing h Nothing

fromTextYolo :: ParseText a => Text -> a
fromTextYolo = either error id . fromText'

publicKeyYolo :: Text -> PublicKey
publicKeyYolo = fromJust . textToKey

expectedKeys :: KeyStorage TestPrv
expectedKeys = IntMap.fromList
  [ ( 0
    , Key
      (KeyPair
        (publicKeyYolo "d40aada7036bfe72e7ff0aaa16e1f4446b7dea9f26a6c0d0824cd69c42dc1118")
        (Just "bab338cd6510acf0440f45d753de91e8a2901ca92541f68fe5b167852d65063611561014fe215833c4b56e6f2f23989584a1094c11a09e1262932e89ba2e7245d40aada7036bfe72e7ff0aaa16e1f4446b7dea9f26a6c0d0824cd69c42dc1118118f697b363694dc169bae879d116bf06e99cfd585d4d5194ecb87d321356d8c"))
      False
      (mkAccountNotes ""))
  , ( 1
    , Key
      (KeyPair
        (publicKeyYolo "4e6756f17642c430795e2780d7a1193449ece6e5d9b0ab8d9c01d6a3b8a4d1a1")
        (Just "bab9fdfcc2e61269539a280dc735de1284f156c5b38b34b7f7bc3d6c2d650636c0031b438d1f3307d7fbe52a802ebe43a9e6b2840f7213fdcdaca3c5ea396c554e6756f17642c430795e2780d7a1193449ece6e5d9b0ab8d9c01d6a3b8a4d1a1307df71c95e281131c15d116c8385f08d389a0d8acf76ee9e14d5591956214e2"))
      False
      (mkAccountNotes ""))
  , ( 2
    , Key
      (KeyPair
        (unsafePublicKey $ "0000000000000000000000000000000000000000000000000000000000000000")
        Nothing)
      True
      (mkAccountNotes ""))
  , ( 3
    , Key
      (KeyPair
        (publicKeyYolo "c9bf2214b6c134fe387798c4994f44606b61ed05c98191c0e00ab79e75b54c2e")
        (Just "da9710f00e8702a97753036b16927adfa32562a887d2d72c23e702b623650636ddb34cc37878f0a5634b443a413835089863fa0e9221186c1fe7c6f60baa267ac9bf2214b6c134fe387798c4994f44606b61ed05c98191c0e00ab79e75b54c2e83212f3e6631a68e7f0a1a77688478a40b9e0af5b3baefe5a82fb71db0c499e2"))
      False
      (mkAccountNotes ""))
  , ( 4
    , Key
      (KeyPair
        (publicKeyYolo "b7d6d1e3f20df081b5bbd3159406d9593ac2973e3297cfc13b962513bd537630")
        (Just "027bac5dccb253b1aa4c7300b134648e244b6e128b20e99153512fcd2f650636d832474457579ce0ce4700c80d80261a3e2ad8ad35fdae576b238fc8c0ce5674b7d6d1e3f20df081b5bbd3159406d9593ac2973e3297cfc13b962513bd53763052340252cbc1d7eac3a821309269ba5e4c8e80d1de3b928a782b5c323be8a02f"))
      False
      (mkAccountNotes ""))
  , ( 5
    , Key
      (KeyPair
        (publicKeyYolo "b92a5ffed43abe09d3679a6c1ce084a87916dfc3bbb0e1cb10d2b5c5152117fd")
        (Just "2ac31da7751977744ef3966e6a0bd170bf3203081bae3d1f26135c4a2e65063605bded4e9067cd7017b1c489ca4819988154c07a4b07d5618b59cf5053cc5619b92a5ffed43abe09d3679a6c1ce084a87916dfc3bbb0e1cb10d2b5c5152117fd1b4beac9855c53695f78d7dd65e58d9cac79069daff050a321465b967a6c8403"))
      False
      (mkAccountNotes ""))
  , ( 6
    , Key
      (KeyPair
        (publicKeyYolo "c5535652d34724d3489d82a1b9217617eaa1ba607b8c123ecd4b393ea5ee293f")
        (Just "d275c3272c88692d94082f901114c83682c485e4bdd8f1e749e125432c650636cf5ca7a8677723fbe798824d51d2d1f22078844ecd5126eb2dadf0cf2ece759cc5535652d34724d3489d82a1b9217617eaa1ba607b8c123ecd4b393ea5ee293feae0b2a51164d673b62b2998bf05e805bbad2552853df0dacfcd55fcb62c9382"))
      False
      (mkAccountNotes ""))
  , ( 7
    , Key
      (KeyPair
        (publicKeyYolo "496df4caddbb907e8ff1c76e4979a176ab1b12bd30e7d3136a8e566b50e07b52")
        (Just "221d56b80fedb108b109fff560460cf508168d741d8d68c70a8250ca2e650636fd12ce16dcff50ee70b99e216105032716360f05db2d1d34955ffdf8d4a48f68496df4caddbb907e8ff1c76e4979a176ab1b12bd30e7d3136a8e566b50e07b523e7cf52e3c529f4886cc87b874574d1cbaaf55254b00c5df047ed345c33c9e7a"))
      False
      (mkAccountNotes "I bet you think this account is about you"))
  , ( 8
    , Key
      (KeyPair
        (publicKeyYolo "c7fb88b67dee06b1f610411e48da6b87328fa4aea80c533a0ca96f64d3eb0632")
        (Just "2a55322b611d5ef426166f4d5f2cb6d5351066e193ce3fec137ab793216506368afdee7d23dabf73936a5fa7d309afa9bb22830d587eb006246114dd183b33f4c7fb88b67dee06b1f610411e48da6b87328fa4aea80c533a0ca96f64d3eb06320b73af7e218872031e6323b7baee4fae75a9c5e3d73baf30568aee5bd07946c2"))
      False
      (mkAccountNotes ""))
  ]

expectedAccounts :: AccountStorage
expectedAccounts = AccountStorage . Map.fromList $
  [ ( uncheckedNetworkName "devnet"
    , Accounts
      (Map.fromList
        [ ( AccountName "benkolera"
          , Map.fromList
            [ ("0" , VanityAccount (publicKeyYolo "496df4caddbb907e8ff1c76e4979a176ab1b12bd30e7d3136a8e566b50e07b52") (mkAccountNotes "I bet you think this account is about you") (AccountInfo Nothing Nothing False) False )
            ]
          )
        -- This isn't actually in flight
        , ( AccountName "inflight"
          , Map.fromList
            [ ("3" , VanityAccount (publicKeyYolo "c7fb88b67dee06b1f610411e48da6b87328fa4aea80c533a0ca96f64d3eb0632") (mkAccountNotes "") (AccountInfo Nothing Nothing False) False)
            ]
          )
        ])
      (Map.fromList
        [ ( publicKeyYolo "b7d6d1e3f20df081b5bbd3159406d9593ac2973e3297cfc13b962513bd537630"
          , Map.fromList
            [ ( "0"
              , NonVanityAccount (AccountInfo (Just (AccountBalance 10)) Nothing False)
              )
            ])
        , ( publicKeyYolo "b92a5ffed43abe09d3679a6c1ce084a87916dfc3bbb0e1cb10d2b5c5152117fd"
          , Map.fromList
            [ ( "1"
              , NonVanityAccount (AccountInfo (Just (AccountBalance 7.99605)) (Just (UnfinishedCrossChainTransfer (RequestKey (fromTextYolo "pddhO9LkJdh3r71kahDZktTOIf7Pwu3T8VqjJdDKssA")) "2" (AccountName "c5535652d34724d3489d82a1b9217617eaa1ba607b8c123ecd4b393ea5ee293f") 2)) False)
              )
            ])
        , ( publicKeyYolo "c5535652d34724d3489d82a1b9217617eaa1ba607b8c123ecd4b393ea5ee293f"
          , Map.fromList
            [ ( "2"
              , NonVanityAccount (AccountInfo Nothing Nothing False)
              )
            ])
        ])
    )
  , ( uncheckedNetworkName "testnet"
    , Accounts
      Map.empty
      (Map.fromList
        [ ( publicKeyYolo  "4e6756f17642c430795e2780d7a1193449ece6e5d9b0ab8d9c01d6a3b8a4d1a1"
          , Map.fromList
            [ ( "1"
              , NonVanityAccount (AccountInfo Nothing Nothing False)
              )
            ]
          )
        , ( publicKeyYolo  "c9bf2214b6c134fe387798c4994f44606b61ed05c98191c0e00ab79e75b54c2e"
          , Map.fromList
            [ ( "5"
              , NonVanityAccount (AccountInfo Nothing Nothing False)
              )
            ]
          )
        , ( publicKeyYolo  "d40aada7036bfe72e7ff0aaa16e1f4446b7dea9f26a6c0d0824cd69c42dc1118"
          , Map.fromList
            [ ( "0"
              , NonVanityAccount (AccountInfo (Just (AccountBalance 10)) Nothing False)
              )
            ]
          )
        ])
     )
  ]

testVersioner :: (HasStorage m, Monad m) => StorageVersioner m (V1.StoreFrontend TestPrv)
testVersioner = versioner

test_v0ToV1Upgrade :: TestTree
test_v0ToV1Upgrade = testCase "V0 to V1 Upgrade" $ do
  let v = testVersioner
  ims@(localRef, sessionRef) <- inMemoryStorageFromTestData
    (_storageVersioner_metaPrefix v)
    (Proxy @(V0.StoreFrontend TestPrv))
    0
    path
  (sn, pm, ns, sf, ks, as) <- flip runInMemoryStorage ims $ do
    _storageVersioner_upgrade v
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

  ks @?~ Just expectedKeys
  as @?~ Just expectedAccounts

  pure ()

  where
    path = "tests" </> "Frontend" </> "StoreSpec.files" </> "V0"

tests :: TestTree
tests = testGroup "StoreSpec"
  [ test_v0ToV1Upgrade
  ]
