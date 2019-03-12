{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE ExtendedDefaultRules   #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ConstraintKinds        #-}

-- | Github gist support.
--
--   Share your code via github gists.
--
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.GistStore where

------------------------------------------------------------------------------
import           Control.Arrow                     ((&&&), first)
import           Control.Lens
import qualified Data.List                         as L
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           GHC.Generics                      (Generic)
import           Reflex
import Control.Applicative (liftA2)
import Network.GitHub.API
import Network.GitHub.Types.Gist as G
import Network.GitHub.Types.Gist.Core as G
import Servant.Client.Core (RunClient (..), clientIn, HasClient (..), ServantError (..), BaseUrl (..), Scheme (Https))
import Servant.API
import Data.Proxy (Proxy (..))
import Servant.Client.JSaddle (client, ClientM, runClientM, ClientEnv (..), mkClientEnv)
import Language.Javascript.JSaddle (liftJSM)
import GHCJS.DOM.XMLHttpRequest (setRequestHeader)
------------------------------------------------------------------------------
import Obelisk.OAuth.Common (AccessToken (..))
------------------------------------------------------------------------------
import           Frontend.Storage
import           Frontend.Messages
import           Frontend.Foundation
import           Frontend.OAuth
import           Common.OAuth
import           Frontend.Editor
import           Frontend.ModuleExplorer.Module    as Module
import           Frontend.ModuleExplorer.ModuleRef as Module

type GistRef = GistId


-- | Create and manage gists.
data GistStoreCfg t = GistStoreCfg
  { _gistStoreCfg_create :: Event t Text
    -- ^ Create gist with current editor content.
  , _gistStoreCfg_load :: Event t GistRef
    -- ^ Load a Gist specifified by the given `GistRef`.
  }
  deriving Generic

makePactLenses ''GistStoreCfg


-- | Information about Gists.
data GistStore t = GistStore
  { _gistStore_loaded :: Event t Gist
  , _gistStore_created :: Event t Gist
  }
  deriving Generic

makePactLenses ''GistStore

-- | Model/dependencies of Backend.
type HasGistStoreModel model t = (HasOAuth model t, HasEditor model t)

-- | Model config needed by gistStore.
type HasGistStoreModelCfg mConf t = (Monoid mConf, HasOAuthCfg mConf t, HasMessagesCfg mConf t)

-- Storing data:

-- | Storage keys for referencing data to be stored/retrieved.
data StoreGist a where
  -- | User wanted a new gist, but was not authorized: Create gist when coming back.
  StoreGist_GistRequested :: StoreGist Text

deriving instance Show (StoreGist a)

-- | Make a `ModuleList` given a `ModuleListCfg`
makeGistStore
  :: forall t m cfg model mConf
  . ( MonadHold t m, MonadFix m, Reflex t, MonadJSM m, PostBuild t m
    , PerformEvent t m, MonadJSM (Performable m)
    , HasGistStoreCfg cfg t
    , HasGistStoreModel model t
    , HasGistStoreModelCfg mConf t
    )
  => model
  -> cfg
  -> m (mConf, GistStore t)
makeGistStore m cfg = mdo

  -- TODO: Also clear on all errors we cannot recover from (anything other than
  -- response status 401):
  let
    onClear = onResp
    onUnAuthorizedCreate = gate (isNothing <$> current mGitHubToken) $ cfg ^. gistStoreCfg_create

  mGistWasRequestedInit <- getItemStorage sessionStorage StoreGist_GistRequested

  mGistWasRequested <- holdDyn mGistWasRequestedInit $ leftmost
    [ Just <$> onUnAuthorizedCreate
    , Nothing <$ onClear
    ]
  onStoredReq <- performEvent $
    setItemStorage sessionStorage StoreGist_GistRequested  <$> onUnAuthorizedCreate
  performEvent_ $ removeItemStorage sessionStorage StoreGist_GistRequested <$ onClear

  onMayNewToken <- tagOnPostBuild mGitHubToken
  let
    onDelayedReq :: Event t (Text, AccessToken)
    onDelayedReq = fmapMaybe id . attachWith (liftA2 (,)) (current mGistWasRequested) $ onMayNewToken


  -- TODO: Report errors/re-authorize where it makes sense.
  errResp <- performEvent $
     fmapMaybe id $  leftmost
        [ attachWith (traverse runGitHubClientM) (current mGitHubToken) . fmap simpleCreateGist $ cfg ^. gistStoreCfg_create
        , Just . uncurry (flip runGitHubClientM) . first simpleCreateGist <$> onDelayedReq
        ]

  let
    (onErr, onResp) = fanEither errResp

    -- Authorize when we want a gist but have no token yet:
    -- TODO: Also authorize on auth problems (and schedule gist sharing):
    onAuthorize = (AuthorizationRequest OAuthProvider_GitHub [ "gist" ]) <$ onStoredReq

  pure
    ( mempty
        & messagesCfg_send .~ fmap tshow onErr
        -- Trigger auth if no token yet (when in progress, requests will be ignored):
        & oAuthCfg_authorize .~ onAuthorize
        {- let authorizeCfg =  mempty & oAuthCfg_authorize .~ (AuthorizationRequest OAuthProvider_GitHub [ "gist" ] <$ onAuthorize) -}
    , GistStore
      { _gistStore_created = onResp
      , _gistStore_loaded = never
      }
    )

  where

    mGitHubToken = Map.lookup OAuthProvider_GitHub <$> m ^. oAuth_accessTokens

    simpleCreateGist :: Text -> ClientM Gist
    simpleCreateGist = createGist . mkGistCreate

    mkGistCreate f = GistCreate
      { gistCreateDescription = Just "Pact shared with pact-web."
      , gistCreateFiles = mempty & at "pact-web-share" .~ Just (FileCreate f)
      , gistCreatePublic = Just True
      }



-- API stuff:



-- | Client for github gists.
{- data GistApiClient m = GistApiClient -}
{-   { createGist :: GistCreate -> m Gist -}
{-   , getGist :: GistId -> m Gist -}
{-   } -}

type GetGist = "gists" :> Capture "gistId" GistId :> Get '[JSON] Gist

-- | Gist API
--
--   For simplicity we moved authentication related stuff and general headers
--   to `fixUpXhr` in `ClientEnv` of servant-client-jsaddle.
type GistApi =
  CreateGist :<|> GetGist


gistApi :: Proxy GistApi
gistApi = Proxy

createGist :: GistCreate -> ClientM Gist
getGist :: GistId -> ClientM Gist
(createGist :<|> getGist) = client gistApi


-- | Nothing if there is no Authorization token yet.
runGitHubClientM
  :: MonadJSM m
  => AccessToken
  -> ClientM a
  -> m (Either ServantError a)
runGitHubClientM (AccessToken token) action = liftJSM $
  runClientM action $ ClientEnv
    { baseUrl = BaseUrl Https "api.github.com" 443 ""
    , fixUpXhr = \req -> do
        setRequestHeader req "Authorization" ("token " <> token)
        setRequestHeader req "Accept" "application/vnd.github.v3+json"
    }


-- Instances:

instance Reflex t => Semigroup (GistStoreCfg t) where
  GistStoreCfg nfA bFA <> GistStoreCfg nfB bFB =
    GistStoreCfg (leftmost [nfA, nfB]) (leftmost [bFA, bFB])

instance Reflex t => Monoid (GistStoreCfg t) where
  mempty = GistStoreCfg never never
  mappend = (<>)


instance Flattenable (GistStoreCfg t) t where
  flattenWith doSwitch ev =
    GistStoreCfg
      <$> doSwitch never (_gistStoreCfg_create <$> ev)
      <*> doSwitch never (_gistStoreCfg_load <$> ev)
