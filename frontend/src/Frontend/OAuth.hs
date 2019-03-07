{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}

-- | OAuth token manager.
module Frontend.OAuth
  ( -- * Types and Classes
    OAuthCfg (..)
  , HasOAuthCfg (..)
  , OAuth (..)
  , HasOAuth (..)
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
import qualified Data.Text.Encoding                 as T
import           Reflex.Dom.Class                   (HasJSContext)
import           Reflex.Dom.Xhr                     (XhrRequestConfig (..),
                                                     decodeXhrResponse,
                                                     newXMLHttpRequest,
                                                     xhrRequest)

import           Obelisk.OAuth.AuthorizationRequest
import           Obelisk.OAuth.Common
import           Obelisk.OAuth.Frontend
import           Obelisk.OAuth.Frontend.Command
import           Obelisk.Route.Frontend

import           Common.OAuth
import           Common.Route
import           Frontend.Foundation
import           Frontend.Storage



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

makePactLenses ''OAuth

data StoreOAuth r where
  StoreOAuth_Tokens :: StoreOAuth (Map OAuthProvider AccessToken)
  StoreOAuth_State :: OAuthProvider -> StoreOAuth OAuthState

deriving instance Show (StoreOAuth a)

makeOAuth
  :: forall t m cfg
  . ( Reflex t, MonadHold t m, PostBuild t m, PerformEvent t m
    , MonadJSM m, MonadJSM (Performable m), MonadFix m, TriggerEvent t m
    , Routed t (R FrontendRoute) m, RouteToUrl (R FrontendRoute) m
    , HasOAuthCfg cfg t
    )
  => cfg -> m (OAuth t)
makeOAuth cfg = do
  r <- askRoute
  let
    oAuthRoute = ffor r $ \case
      FrontendRoute_OAuth :/ oR -> Just oR
      _ -> Nothing

  sCfg <- buildOAuthConfigFront

  oAuthL <- runOAuthRequester $ makeOAuthFrontend sCfg $ OAuthFrontendConfig
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

  onResponse <- performEventAsync $ ffor onRequest $ \req sendResponse -> void $ forkJSM $ do
    r <- traverseRequesterData (fmap Identity . runOAuthCmds renderRoute) req
    liftIO $ sendResponse r

  pure a



runOAuthCmds
  :: (MonadJSM m, HasJSContext m)
  => (R BackendRoute -> Text)
  -> Command OAuthProvider a
  -> m a
runOAuthCmds renderRoute = go
  where
    go = \case
      Free (CommandF_StoreState prov state next) ->
        setItemStorage sessionStorage (StoreOAuth_State prov) state >> go next
      Free (CommandF_LoadState prov getNext) ->
        getItemStorage sessionStorage (StoreOAuth_State prov) >>= go . getNext
      Free (CommandF_RemoveState prov next) ->
        removeItemStorage sessionStorage (StoreOAuth_State prov) >> go next
      Free (CommandF_GetToken prov pars getNext) -> do
        let
          uri = renderRoute $ BackendRoute_OAuthGetToken :/ oAuthProviderId prov

          req = xhrRequest "POST" uri $ def
            { _xhrRequestConfig_sendData =
                T.decodeUtf8 . BSL.toStrict . Aeson.encode $ pars
            }

        resVar <- liftIO newEmptyMVar
        void $ newXMLHttpRequest req (liftIO . putMVar resVar)
        mRes <- liftIO $ decodeXhrResponse <$> takeMVar resVar
        go . getNext . fromMaybe (Left OAuthError_InvalidResponse) $ mRes
      Pure r ->
        pure r


buildOAuthConfigFront
  :: (MonadIO m, RouteToUrl (R FrontendRoute) m)
  => m (OAuthConfig OAuthProvider)
buildOAuthConfigFront = buildOAuthConfig =<< askRouteToUrl

-- Instances

instance Reflex t => Semigroup (OAuthCfg t) where
  OAuthCfg x <> OAuthCfg y = OAuthCfg $ leftmost [x, y]

instance Reflex t => Monoid (OAuthCfg t) where
  mempty = OAuthCfg never
  mappend = (<>)

instance Flattenable (OAuthCfg t) t where
  flattenWith doSwitch ev =
    OAuthCfg
      <$> doSwitch never (_oAuthCfg_authorize <$> ev)
