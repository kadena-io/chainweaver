{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | OAuth token manager.
module Frontend.OAuth
  ( -- * Types and Classes
    OAuthCfg (..)
  , HasOAuthCfg (..)
  , OAuth (..)
  , HasOAuth (..)
  , AuthorizationRequest (..)
    -- * Creation
  , makeOAuth
  ) where


import           Control.Concurrent.MVar            (newEmptyMVar, putMVar,
                                                     takeMVar)
import           Control.Lens
import           Control.Monad                      (void)
import           Control.Monad.Free
import qualified Data.Aeson                         as Aeson
import qualified Data.ByteString.Lazy               as BSL
import           Data.Default                       (def)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Text                          (Text)
import           Reflex.Dom.Class                   (HasJSContext)
import           Reflex.Dom.Xhr                     (XhrRequestConfig (..),
                                                     decodeXhrResponse,
                                                     newXMLHttpRequest,
                                                     xhrRequest)

import           Obelisk.Configs
import           Obelisk.OAuth.AuthorizationRequest
import           Obelisk.OAuth.Common
import           Obelisk.OAuth.Frontend
import           Obelisk.OAuth.Frontend.Command
import           Obelisk.Route.Frontend

import           Common.OAuth
import           Common.Route
import           Frontend.Foundation
import           Frontend.Messages
import           Frontend.Storage
import           Frontend.VersionedStore


data OAuthCfg t = OAuthCfg
  { _oAuthCfg_authorize :: Event t (AuthorizationRequest OAuthProvider)
  , _oAuthCfg_logout :: Event t OAuthProvider
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

makePactLenses ''OAuth

type HasOAuthModelCfg mConf t = ( Monoid mConf, HasMessagesCfg mConf t)

makeOAuth
  :: forall t m cfg mConf
  . ( Reflex t, MonadHold t m, PostBuild t m, PerformEvent t m, MonadSample t (Performable m)
    , MonadJSM m, MonadJSM (Performable m), MonadFix m, TriggerEvent t m
    , Routed t (R FrontendRoute) m, RouteToUrl (R FrontendRoute) m
    , HasOAuthCfg cfg t, HasOAuthModelCfg mConf t, HasConfigs m
    , HasStorage m, HasStorage (Performable m)
    )
  => cfg -> m (mConf, OAuth t)
makeOAuth cfg = mdo -- Required to get access to `tokens` for clearing any old tokens before requesting new ones.
  r <- askRoute
  let
    oAuthRoute = ffor r $ \case
      FrontendRoute_Contracts :/ Just (ContractRoute_OAuth :/ oR) -> Just oR
      _ -> Nothing

  sCfg <- buildOAuthConfigFront

  -- A bit hacky, but we need to make sure to have the old token removed, before requesting a new one:
  -- (Below performEvent on `updated tokens` will happen too late!)
  onAuthorizeStored <- performEvent $ ffor (cfg ^. oAuthCfg_authorize) $ \authReq -> do
    cTokens <- sample $ current tokens
    setItemStorage localStorage StoreFrontend_OAuth_Tokens $ Map.delete (_authorizationRequest_provider authReq) cTokens
    pure authReq

  oAuthL <- runOAuthRequester $ makeOAuthFrontend sCfg $ OAuthFrontendConfig
    { _oAuthFrontendConfig_authorize = onAuthorizeStored
    , _oAuthFrontendConfig_route = oAuthRoute
    }

  let
    (onErr, onToken) = fanEither $ _oAuthFrontend_authorized oAuthL

  mInitTokens <- getItemStorage localStorage StoreFrontend_OAuth_Tokens

  tokens <- foldDyn id (fromMaybe Map.empty mInitTokens) $ leftmost
    [ uncurry Map.insert <$> onToken
      -- Get rid of token that is about to be invalidated. (We really don't want to have an invalid token when coming back.)
    , Map.delete . _authorizationRequest_provider <$> cfg ^. oAuthCfg_authorize
    , Map.delete <$> cfg ^. oAuthCfg_logout
    ]

  performEvent_ $ setItemStorage localStorage StoreFrontend_OAuth_Tokens <$> updated tokens

  pure
    ( mempty & messagesCfg_send .~ fmap (pure . textOAuthError) onErr
    , OAuth
      { _oAuth_accessTokens = tokens
      , _oAuth_error = onErr
      }
    )


runOAuthRequester
  :: ( Monad m, MonadFix m, TriggerEvent t m, PerformEvent t m
     , MonadJSM (Performable m)
     )
  => RequesterT t (Command OAuthProvider) Identity m a
  -> m a
runOAuthRequester requester = mdo

  -- RouteToUrl not usable, as it only handles frontend routes (Which makes sense for Routed and SetRoute).
  let
    Right validFullEncoder = checkEncoder backendRouteEncoder

    renderRoute :: R BackendRoute -> Text
    renderRoute = renderBackendRoute validFullEncoder

  (a, onRequest) <- runRequesterT requester onResponse

  onResponse <- performEventAsync $ ffor onRequest $ \req sendResponse -> void $ liftJSM $ forkJSM $ do
    -- We can runBrowserStorageT here because OAuth functionality is only
    -- available in the browser
    r <- traverseRequesterData (fmap Identity . runBrowserStorageT . runOAuthCmds renderRoute) req
    liftIO $ sendResponse r

  pure a



runOAuthCmds
  :: (HasStorage m, MonadJSM m, HasJSContext m)
  => (R BackendRoute -> Text)
  -> Command OAuthProvider a
  -> m a
runOAuthCmds renderRoute = go
  where
    go = \case
      Free (CommandF_StoreState prov state next) ->
        (setItemStorage sessionStorage (StoreFrontend_OAuth_State prov) state) >> go next
      Free (CommandF_LoadState prov getNext) ->
        (getItemStorage sessionStorage (StoreFrontend_OAuth_State prov)) >>= go . getNext
      Free (CommandF_RemoveState prov next) ->
        (removeItemStorage sessionStorage (StoreFrontend_OAuth_State prov)) >> go next
      Free (CommandF_GetToken prov pars getNext) -> do
        let
          uri = renderRoute $ BackendRoute_OAuthGetToken :/ oAuthProviderId prov

          req = xhrRequest "POST" uri $ def
            { _xhrRequestConfig_sendData =
                safeDecodeUtf8 . BSL.toStrict . Aeson.encode $ pars
            }

        resVar <- liftIO newEmptyMVar
        void $ newXMLHttpRequest req (liftIO . putMVar resVar)
        mRes <- liftIO $ decodeXhrResponse <$> takeMVar resVar
        go . getNext . fromMaybe (Left OAuthError_InvalidResponse) $ mRes
      Pure r ->
        pure r


buildOAuthConfigFront
  :: (HasConfigs m, RouteToUrl (R FrontendRoute) m)
  => m (OAuthConfig OAuthProvider)
buildOAuthConfigFront = buildOAuthConfig =<< askRouteToUrl

-- Instances

instance Reflex t => Semigroup (OAuthCfg t) where
  OAuthCfg x1 x2 <> OAuthCfg y1 y2 = OAuthCfg (leftmost [x1, y1]) (leftmost [x2, y2])

instance Reflex t => Monoid (OAuthCfg t) where
  mempty = OAuthCfg never never
  mappend = (<>)

instance Flattenable (OAuthCfg t) t where
  flattenWith doSwitch ev =
    OAuthCfg
      <$> doSwitch never (_oAuthCfg_authorize <$> ev)
      <*> doSwitch never (_oAuthCfg_logout <$> ev)
