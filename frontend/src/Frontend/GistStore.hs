{-# LANGUAGE ConstraintKinds        #-}
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

-- | Github gist support.
--
--   Share your code via github gists.
--
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.GistStore where

------------------------------------------------------------------------------
import           Control.Applicative         (liftA2)
import           Control.Arrow               (first)
import           Control.Lens
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.Map                    as Map
import           Data.Proxy                  (Proxy (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Generics.Deriving.Monoid    (mappenddefault)
import           GHC.Generics                (Generic)
import           GHCJS.DOM.XMLHttpRequest    (setRequestHeader)
import           Network.GitHub.API
import           Network.GitHub.Types.Gist   as G
import           Reflex
import           Servant.API
import           Servant.Client.Core         (BaseUrl (..), Scheme (Https),
                                              ServantError (..))
import           Servant.Client.JSaddle      (ClientEnv (..), ClientM, client,
                                              runClientM)
------------------------------------------------------------------------------
import           Obelisk.OAuth.Common        (AccessToken (..))
------------------------------------------------------------------------------
import           Common.OAuth
import           Frontend.Foundation
import           Frontend.Messages
import           Frontend.OAuth
import           Frontend.Storage

type GistRef = GistId

-- | Meta data about a gist. (Like name and description)
data GistMeta = GistMeta
  { _gistMeta_fileName    :: Text
    -- ^ What filename to use in the gist.
  , _gistMeta_description :: Text
    -- ^ The description of the gist to create.
  }
  deriving (Show, Generic)

instance ToJSON GistMeta
instance FromJSON GistMeta
instance Semigroup GistMeta where
  (<>) = mappenddefault

-- | Create and manage gists.
data GistStoreCfg t = GistStoreCfg
  { _gistStoreCfg_create :: Event t (GistMeta, Text)
    -- ^ Create gist with current editor content.
  , _gistStoreCfg_load   :: Event t GistRef
    -- ^ Load a Gist specifified by the given `GistRef`.
  }
  deriving Generic

makePactLenses ''GistStoreCfg


-- | Information about Gists.
data GistStore t = GistStore
  { _gistStore_loaded  :: Event t Gist
  , _gistStore_created :: Event t GistRef
  }
  deriving Generic

makePactLenses ''GistStore

-- | Model/dependencies of Backend.
type HasGistStoreModel model t = (HasOAuth model t)

-- | Model config needed by gistStore.
type HasGistStoreModelCfg mConf t = (Monoid mConf, HasOAuthCfg mConf t, HasMessagesCfg mConf t)

-- Storing data:

-- | Storage keys for referencing data to be stored/retrieved.
data StoreGist a where
  -- | User wanted a new gist, but was not authorized: Create gist when coming back.
  StoreGist_GistRequested :: StoreGist (GistMeta, Text)

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

  let
    onClear = leftmost [() <$ onResp, () <$ onPermErr]
    onUnAuthorizedCreate = leftmost
      [ gate (isNothing <$> current mGitHubToken) $ cfg ^. gistStoreCfg_create
      , onRetry
      ]

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
    onDelayedReq :: Event t ((GistMeta, Text), AccessToken)
    onDelayedReq = fmapMaybe id . attachWith (liftA2 (,)) (current mGistWasRequested) $ onMayNewToken

  onErrResp <- performEvent $
     fmapMaybe id $ leftmost
        [ attachWith (traverse runGitHubClientMAuthorized) (current mGitHubToken)
            . fmap simpleCreateGist $ cfg ^. gistStoreCfg_create

        , Just . uncurry (flip runGitHubClientMAuthorized)
            . first simpleCreateGist <$> onDelayedReq
        ]

  let
    onResp = fmapMaybe (^? _Right) onErrResp

  -- Shall we retry onErr or fail for good?
  -- We only retry on initial requests, not if we just got a token!
  (onRetry, onPermErr) <- getRetry onErrResp $ cfg ^. gistStoreCfg_create

    -- Authorize when we want a gist but have no token yet or when we got some failure response:
  let onAuthorize = (AuthorizationRequest OAuthProvider_GitHub [ "gist" ]) <$ onStoredReq

  -- Loading of gists:
  onGotErrGist <- performEvent $ runGitHubClientM Nothing . getGist <$> cfg ^. gistStoreCfg_load
  let (onLoadErr, onLoad) = fanEither onGotErrGist

  pure
    ( mempty
        & messagesCfg_send .~ mconcat (fmap (pure . tshow) <$> [onPermErr, onLoadErr])
        -- Trigger auth if no token yet (when in progress, requests will be ignored):
        & oAuthCfg_authorize .~ onAuthorize
        {- let authorizeCfg =  mempty & oAuthCfg_authorize .~ (AuthorizationRequest OAuthProvider_GitHub [ "gist" ] <$ onAuthorize) -}
    , GistStore
      { _gistStore_created = gistId <$> onResp
      , _gistStore_loaded = onLoad
      }
    )

  where
    runGitHubClientMAuthorized = runGitHubClientM . Just

    mGitHubToken = Map.lookup OAuthProvider_GitHub <$> m ^. oAuth_accessTokens

    simpleCreateGist :: (GistMeta, Text) -> ClientM Gist
    simpleCreateGist = createGist . mkGistCreate

    mkGistCreate (GistMeta n d, f) = GistCreate
      { gistCreateDescription = Just d
      , gistCreateFiles = mempty & at (fixName n) .~ Just (FileCreate f)
      , gistCreatePublic = Just True
      }

    fixName n = if T.isSuffixOf ".pact" n then n else n <> ".pact"


-- | Retry logic on error.
--
--   WARNING: Only retry on actual user requests, otherwise this will become
--   loopy. We had loop prevention implemented already, see: commit
--   512912ad67ddc60b79fd231647adfa2fa262c4f2 - but it is just a different
--   tradeoff. It won't loop, on programming errors, but it could prevent the
--   user from initiating an authorization at all.
--
--   On error responses coming from github we will retry once with a newly requested access token.
--
--   INFO: Retry on auth errors is actually a more general concept not tied to
--   gists at all. Also having gist sharing doing authorization implicitely
--   when necessary breaks any `Requester` workflow - as the requester will
--   never receive a response, this would apply to all services that require
--   authorization and take care of it by them selves.
--
--   A better solution would be to not have `GistStore` take care of
--   authorization by itself, but just deliver an error when not authorized or
--   the request fails and then have some general purpose `requstingWithRetry`
--   function users can use. Something with the following signature:
--
-- @
--   requestingWithRetry
--     :: Requester ... m -- ^ Wrap up requester, and retry on error.
--     => (resp -> (respChecked, Bool)) -- Process response and determine if a retry is desired.
--     -> Event t req
--     -> m (Event t respChecked)
-- @
getRetry
  :: (MonadHold t m, MonadFix m, PerformEvent t m)
  => Event t (Either ServantError resp)
  -> Event t req
  -> m (Event t req, Event t ServantError)
getRetry onErrResp onReq = mdo
  -- TODO: As usual, logic like this does not play well with multiple
  -- requests occurring while we are still waiting for a response. To do
  -- this properly, use the monadic context of `performEvent` and transfer any
  -- request related information you need at response time to the response. By
  -- simply returning what you need.
  cReq <- hold Nothing $ leftmost
    [ Just <$> onReq
    , Nothing <$ leftmost [() <$ onPermErr, () <$ onResp, onRetry]
    ]

  let
    (onErr, onResp) = fanEither onErrResp
    (onPermErr, onRetry) = fanEither $ ffor onErr $ \case
      FailureResponse _ -> Right () -- Retry on all failure responses (better
        -- retry too often, than to little. Loops should not be possible, as we
        -- only retry on explict (user)requests.)
      err -> Left err

  pure ( fmapMaybe id $ tag cReq onRetry, onPermErr )



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


-- | Run github client request with or without an access token.
runGitHubClientM
  :: MonadJSM m
  => Maybe AccessToken
  -> ClientM a
  -> m (Either ServantError a)
runGitHubClientM mToken action = liftJSM $
  runClientM action $ ClientEnv
    { baseUrl = BaseUrl Https "api.github.com" 443 ""
    , fixUpXhr = \req -> do
        traverse_ (setAuthorization req) mToken
        setRequestHeader req ("Accept" :: Text) ("application/vnd.github.v3+json" :: Text)
    }
  where
    setAuthorization req (AccessToken token) =
      setRequestHeader req ("Authorization" :: Text) (("token " <> token) :: Text)


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
