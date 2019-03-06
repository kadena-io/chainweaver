{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.OAuth where

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Aeson               (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           GHC.Generics             (Generic)
import           Obelisk.ExecutableConfig (get)

import           Obelisk.OAuth.Common
import           Obelisk.OAuth.Provider
import           Obelisk.Route

import           Common.Api
import           Common.Route


-- | All the oauth providers we support right now.
data OAuthProvider = OAuthProvider_Github
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON OAuthProvider
instance ToJSON OAuthProvider
instance FromJSONKey OAuthProvider
instance ToJSONKey OAuthProvider

instance IsOAuthProvider OAuthProvider where

  oAuthProviderId OAuthProvider_Github = "github"

  oAuthProviderFromId = \case
    "github" -> Just OAuthProvider_Github
    _ -> Nothing

  oAuthAuthorizeEndpoint OAuthProvider_Github =
    "https://github.com/login/oauth/authorize"

  oAuthAccessTokenEndpoint OAuthProvider_Github =
    "https://github.com/login/oauth/access_token"



-- | Build an OAuthConfig by reading config values.
--
--   - config/common/route -> Base url
--   - config/common/oauth/github/client-id -> Github client id.
--
buildOAuthConfig :: MonadIO m => (R FrontendRoute -> Text) -> m (OAuthConfig OAuthProvider)
buildOAuthConfig renderRoute = do
  clientId <- getMandatoryTextCfg "config/common/oauth/github/client-id"
  baseUri <- getMandatoryTextCfg "config/common/route"
  pure $ OAuthConfig
    { _oAuthConfig_renderRedirectUri = Just $
        \oAuthRoute -> (baseUri <>) . renderRoute $ FrontendRoute_OAuth :/ oAuthRoute

    , _oAuthConfig_providers =
        \case
          OAuthProvider_Github -> ProviderConfig
            { _providerConfig_responseType = AuthorizationResponseType_Code
            , _providerConfig_clientId = OAuthClientId clientId
            }
    }

