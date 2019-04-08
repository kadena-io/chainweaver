{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.OAuth where

import           Control.Monad
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson             (FromJSON (..), FromJSONKey (..),
                                         FromJSONKeyFunction (..), ToJSON (..),
                                         ToJSONKey (..), ToJSONKeyFunction (..))
import           Data.Text              (Text)
import           GHC.Generics           (Generic)

import           Obelisk.OAuth.Common
import           Obelisk.Route

import           Common.Api
import           Common.Route


-- | All the oauth providers we support right now.
data OAuthProvider = OAuthProvider_GitHub
  deriving (Show, Read, Eq, Ord, Generic, Bounded, Enum)

instance FromJSON OAuthProvider where
  parseJSON = maybe (fail "Invalid provider id") pure <=< fmap oAuthProviderFromId . parseJSON

instance ToJSON OAuthProvider where
  toJSON = toJSON . oAuthProviderId

-- Explicit instances important, otherwise extending OAuthProvider would break backwards compatibility.
instance FromJSONKey OAuthProvider where
  fromJSONKey = FromJSONKeyValue parseJSON

instance ToJSONKey OAuthProvider where
  toJSONKey = ToJSONKeyValue (toJSON . oAuthProviderId) (toEncoding . oAuthProviderId)


instance IsOAuthProvider OAuthProvider where

  oAuthProviderId OAuthProvider_GitHub = "github"

  oAuthProviderFromId = \case
    "github" -> Just OAuthProvider_GitHub
    _ -> Nothing

  oAuthAuthorizeEndpoint OAuthProvider_GitHub =
    "https://github.com/login/oauth/authorize"

  oAuthAccessTokenEndpoint OAuthProvider_GitHub =
    "https://github.com/login/oauth/access_token"


-- | Where to put OAuth related common configs:
oAuthCfgPath :: Text
oAuthCfgPath = "config/common/oauth/"

-- | Config file containing an OAuth provider id:
oAuthClientIdPath :: IsOAuthProvider prov => prov -> Text
oAuthClientIdPath prov =
  oAuthCfgPath <> unOAuthProviderId (oAuthProviderId prov) <> "/client-id"


-- | Retrieve the client id of a particular client from config.
getOAuthClientId :: (MonadIO m, IsOAuthProvider prov) => prov -> m OAuthClientId
getOAuthClientId prov =
  fmap OAuthClientId $ getMandatoryTextCfg $ oAuthClientIdPath prov


-- | Build an OAuthConfig by reading config values.
--
--   - config/common/route -> Base url
--   - config/common/oauth/github/client-id -> Github client id.
--
buildOAuthConfig :: MonadIO m => (R FrontendRoute -> Text) -> m (OAuthConfig OAuthProvider)
buildOAuthConfig renderRoute = do
  clientId <- getOAuthClientId OAuthProvider_GitHub
  baseUri <- getConfigRoute
  pure $ OAuthConfig
    { _oAuthConfig_renderRedirectUri = Just $
        \oAuthRoute -> (baseUri <>) . renderRoute $ FrontendRoute_OAuth :/ oAuthRoute

    , _oAuthConfig_providers =
        \case
          OAuthProvider_GitHub -> ProviderConfig
            { _providerConfig_responseType = AuthorizationResponseType_Code
            , _providerConfig_clientId = clientId
            }
    }

