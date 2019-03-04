{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}

-- | OAuth token manager.
module Frontend.OAuth
  (
  ) where


import Control.Lens
import           Data.Aeson               (FromJSON, ToJSON)
import           Generics.Deriving.Monoid (mappenddefault, memptydefault)
import Data.Map (Map)
import qualified Data.Map as Map

import           Obelisk.OAuth.Provider
import           Obelisk.OAuth.Frontend
import           Obelisk.OAuth.Common
import           Obelisk.OAuth.AuthorizationRequest
import Obelisk.Route.Frontend

import           Frontend.Foundation
import Common.Route
import Frontend.Storage


-- | All the oauth providers we support right now.
data OAuthProvider = OAuthProvider_Github
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON OAuthProvider
instance ToJSON OAuthProvider

instance IsOAuthProvider OAuthProvider where

  oAuthProviderId OAuthProvider_Github = "github"

  oAuthProviderFromId = \case
    "github" -> Just OAuthProvider_Github
    _ -> Nothing

  oAuthAuthorizeEndpoint OAuthProvider_Github =
    "https://github.com/login/oauth/authorize"

  oAuthAccessTokenEndpoint OAuthProvider_Github =
    "https://github.com/login/oauth/access_token"


newtype OAuthCfg t = OAuthCfg
  { _oAuthCfg_authorize :: Event t (AuthorizationRequest OAuthProvider)
  }
  deriving Generic

makePactLenses ''OAuthCfg


data OAuth t = OAuth
  { _oAuth_accessTokens :: Dynamic t (Map OAuthProvider AccessToken)
    -- ^ Currently available OAuth tokens, will be stored to local storage and
    -- restored from there on application startup.
  , _oAuth_error        :: Event t OAuthError
    -- ^ Authorization failed with some error.
  }

data StoreOAuth r where
  StoreOAuth_Tokens :: StoreOAuth (Map OAuthProvider AccessToken)

makeOAuth :: forall t m. (Routed t (R FrontendRoute ) m) => OAuthCfg t -> m (OAuth t)
makeOAuth cfg = do
  r <- askRoute
  let
    oAuthRoute = ffor r $ \case
      FrontendRoute_OAuth :/ r -> Just r
      _ -> Nothing

  sCfg <- buildOAuthConfig

  oAuthL <- makeOAuthFrontend sCfg $ OAuthFrontendConfig
    { _oAuthFrontendConfig_authorize = cfg ^. oAuthCfg_authorize
    , _oAuthFrontendConfig_route = oAuthRoute
    }

  let
    (onErr, onToken) = fanEither $ _oAuthFrontend_authorized oAuthL

  mInitTokens <- getItemStorage localStorage StoreOAuth_Tokens

  tokens <- foldDyn id (fromMaybe Map.empty mInitTokens) $
    uncurry Map.insert <$> onToken

  performEvent_ $ setItemStorage localStorage StoreOAuth_Tokens <$> updated tokens

  pure $ OAuth
    { _oAuth_accessTokens = tokens
    , _oAuth_error = onErr
    }

buildOAuthConfig :: (MonadIO m, RouteToUrl (R FrontendRoute) m) => m (OAuthConfig OAuthProvider)
buildOAuthConfig = do
  renderRoute <- askRouteToUrl
  clientId <- getMandatoryTextCfg "config/common/oauth/github/client-id"
  pure $ OAuthConfig
    { _oAuthConfig_renderRedirectUri =
        \oAuthRoute -> renderRoute $ FrontendRoute_OAuth :/ Identity oAuthRoute

    , _oAuthConfig_providers =
        \case
          OAuthProvider_Github -> ProviderConfig
            { _providerConfig_responseType = AuthorizationResponseType_Code
            , _providerConfig_clientId = clientId
            }
    }

-- Instances

instance Reflex t => Semigroup (OAuthCfg t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (OAuthCfg t) where
  mempty = memptydefault
  mappend = (<>)

instance Flattenable (OAuthCfg t) t where
  flattenWith doSwitch ev =
    OAuthCfg
      <$> doSwitch never (_oAuthFrontendConfig_authorize <$> ev)
