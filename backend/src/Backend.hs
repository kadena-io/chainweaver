{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}

module Backend where

import           Control.Monad             (when, (<=<))
import           Control.Monad.Except      (ExceptT (..), runExceptT,
                                            throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class (lift)
import qualified Data.Aeson                as Aeson
import qualified Data.ByteString           as BS
import qualified Data.CaseInsensitive      as CI
import           Data.Default
import           Data.Dependent.Sum        (DSum ((:=>)))
import qualified Data.Map                  as M
import           Data.Maybe                (isJust, fromMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.Text.Encoding.Error  as T
import qualified Data.Text.IO              as T
import           Network.HTTP.Client       (Manager)
import           Network.HTTP.Client.TLS   (newTlsManager)
import qualified Obelisk.Backend           as Ob
import           Obelisk.ExecutableConfig.Lookup
import           Obelisk.Route             (pattern (:/), R, checkEncoder,
                                            renderFrontendRoute)
import           Snap                      (Method (POST), Request (..), Snap,
                                            addHeader, getRequest,
                                            modifyResponse, pass,
                                            readRequestBody, setResponseStatus,
                                            writeBS, writeLBS)
import qualified Snap
import qualified Snap.Util.CORS            as CORS
import           System.Exit               (exitFailure)
import           System.IO                 (stderr)
import qualified Text.Sass                 as Sass
import           TH.RelativePaths          (withCabalPackageWorkDir)
import "template-haskell" Language.Haskell.TH.Lib   (stringE)

import           Obelisk.OAuth.Backend     (getAccessToken)
import           Obelisk.OAuth.Common

import qualified Backend.Devel             as Devel
import           Common.Api
import           Common.OAuth              (OAuthProvider (..),
                                            buildOAuthConfig', oAuthClientIdPath)
import           Common.Route
import           Common.Network

data BackendCfg = BackendCfg
  { _backendCfg_oAuth                :: OAuthConfig OAuthProvider
  , _backendCfg_getOAuthClientSecret :: Maybe (OAuthProvider -> OAuthClientSecret)
  , _backendCfg_manager              :: Manager
  }

-- | Where to put OAuth related backend configs:
oAuthBackendCfgPath :: Text
oAuthBackendCfgPath = "backend/oauth/"

oAuthClientSecretPath :: IsOAuthProvider prov => prov -> Text
oAuthClientSecretPath prov =
  oAuthBackendCfgPath <> unOAuthProviderId (oAuthProviderId prov) <> "/client-secret"

getConfig :: MonadIO m => Text -> m (Maybe Text)
getConfig k = fmap (T.strip . T.decodeUtf8With T.lenientDecode) . M.lookup k <$> liftIO getConfigs

-- | Retrieve the client id of a particular client from config.
getOAuthClientSecret :: (IsOAuthProvider prov, MonadIO m) => prov -> m (Maybe OAuthClientSecret)
getOAuthClientSecret = (fmap . fmap) OAuthClientSecret . getConfig . oAuthClientSecretPath

-- | Retrieve the client id of a particular client from config.
--getOAuthClientId
--  :: Monad m => IsOAuthProvider prov
--  => prov -> m OAuthClientId
--getOAuthClientId prov =
--  fmap OAuthClientId $ getConfig $ oAuthClientIdPath prov




buildCfg :: MonadIO m => m BackendCfg
buildCfg = do
  let
    Right validFullEncoder = checkEncoder backendRouteEncoder

    renderRoute :: R FrontendRoute -> Text
    renderRoute = renderFrontendRoute validFullEncoder

  clientSecret <- getOAuthClientSecret OAuthProvider_GitHub
  baseRoute <- fromMaybe (error "failed to get route") <$> getConfig "common/route"
  clientId <- OAuthClientId . fromMaybe (error "failed to get github client id") <$> getConfig (oAuthClientIdPath OAuthProvider_GitHub)
  let oCfg = buildOAuthConfig' baseRoute clientId renderRoute
  liftIO $ BackendCfg oCfg (fmap (\x OAuthProvider_GitHub -> x) clientSecret) <$> newTlsManager


-- | Check whether needed configs are present.
--
--   TODO: We should also check whether they are valid as good as we can.
checkDeployment :: MonadIO m => m ()
checkDeployment = do
    let
      filesToCheck =
        map oAuthClientIdPath [ minBound .. maxBound :: OAuthProvider]
        <>
        map oAuthClientSecretPath [ minBound .. maxBound :: OAuthProvider ]
        <>
          [ "common/route"
          , "frontend/tracking-id"
          , networksPath
          , verificationServerPath
          ]
    allConfigs <- fmap (T.decodeUtf8With T.lenientDecode) <$> liftIO getConfigs
    let
      presentState = (`M.member` allConfigs) <$> filesToCheck
      filesChecked = zip filesToCheck presentState
      missingFiles = fmap fst . filter ((== False) . snd) $ filesChecked

    when (not . null $ missingFiles) $ liftIO $ do
      putErrLn "\n\n========================= PACT-WEB ERROR =========================\n\n"
      putErrLn "The deployment failed due to missing config files.\nPlease consult the project's README.md for details.\n"
      putErrLn "Missing files:\n"
      putErrLn $ T.unlines . map ("  " <>) $ missingFiles
      putErrLn "==================================================================\n\n"
      exitFailure

    checkNetworksConfig allConfigs

  where
    checkNetworksConfig allConfigs = case M.lookup networksPath allConfigs of
      Nothing -> reportInvalidNetworksCfg "Networks configuration could not be found."
      Just n -> case parseNetworks n of
        Left e -> reportInvalidNetworksCfg e
        Right _ -> pure ()

    reportInvalidNetworksCfg errMsg = liftIO $ do
      -- TODO: If we add more checks, abstract this:
      putErrLn "\n\n========================= PACT-WEB ERROR =========================\n\n"
      putErrLn "The deployment failed due to networks config being invalid:\n"
      putErrLn errMsg
      putErrLn "==================================================================\n\n"
      exitFailure

    putErrLn = T.hPutStrLn stderr

backend :: Ob.Backend BackendRoute FrontendRoute
backend = Ob.Backend
    { Ob._backend_run = \serve -> do
        cfg <- buildCfg
        networks <- getConfig networksPath
        let hasServerList = isJust networks
            serveIt = serve $ serveBackendRoute networks cfg

        if hasServerList
           -- Production mode:
           then serveIt
           --  Devel mode:
           else Devel.withPactInstances serveIt

    , Ob._backend_routeEncoder = backendRouteEncoder
    }

serveBackendRoute :: Maybe Text -> BackendCfg -> R BackendRoute -> Snap ()
serveBackendRoute networks cfg = \case
  BackendRoute_Networks :/ () -> CORS.applyCORS CORS.defaultOptions $ case networks of
    Nothing -> pass
    Just n -> do
      req <- Snap.getRequest
      when (Snap.rqMethod req /= Snap.GET) $ do
        Snap.modifyResponse $ Snap.setResponseStatus 405 "Invalid Method"
        Snap.writeBS "Invalid Method"
        Snap.finishWith =<< Snap.getResponse
      Snap.modifyResponse
        $ Snap.setContentType "text/css"
        . Snap.addHeader (CI.mk "cache-control") "public"
      Snap.writeText n
  BackendRoute_Robots :=> _
    -> writeBS "User-agent: *\nDisallow: \n"
  BackendRoute_OAuthGetToken :/ providerId
    -> requestToken cfg providerId
  BackendRoute_Css :/ ()
    -> requestCss
  _ -> pure ()

requestCss :: Snap ()
requestCss = do
  req <- Snap.getRequest
  when (Snap.rqMethod req /= Snap.GET) $ do
    Snap.modifyResponse $ Snap.setResponseStatus 405 "Invalid Method"
    Snap.writeBS "Invalid Method"
    Snap.finishWith =<< Snap.getResponse
  Snap.modifyResponse
    $ Snap.setContentType "text/css"
    . Snap.addHeader (CI.mk "cache-control") "public"
  Snap.writeBS renderCss

-- | Render our SASS files to CSS.
renderCss :: BS.ByteString
renderCss = T.encodeUtf8 $ T.pack
  $(either (fail <=< liftIO . Sass.errorMessage) (stringE . Sass.resultString)
    <=< withCabalPackageWorkDir $ liftIO $ Sass.compileFile "sass/index.scss" def)

requestToken :: BackendCfg -> OAuthProviderId -> Snap ()
requestToken (BackendCfg oAuthCfg (Just getSecret) manager) provId = do
  req <- getRequest

  errResp <- runExceptT $ do

    let method = rqMethod req
    when (method /= POST) $ throwError $ OAuthError_InvalidMethod

    mPars <- lift $ Aeson.decode' <$> readRequestBody 10000
    pars <- maybe (throwError OAuthError_InvalidRequest) pure mPars

    prov <- oAuthProviderFromIdErr provId

    ExceptT . liftIO $ getAccessToken oAuthCfg getSecret prov pars manager

  case errResp of
    Left err -> oAuthErrorToResponse err
    Right _ -> do
      modifyResponse $ addHeader (CI.mk "cache-control") "no-cache, no-store, must-revalidate"
      writeLBS $ Aeson.encode errResp
requestToken _ _ = oAuthErrorToResponse OAuthError_InvalidMethod

oAuthErrorToResponse :: OAuthError -> Snap ()
oAuthErrorToResponse err = do
  let
    -- TODO: Move status translation to obelisk-oauth.
    status = case err of
      OAuthError_MissingCodeState       -> 400
      OAuthError_InvalidState           -> 400
      OAuthError_NoSessionState         -> 400
      OAuthError_InvalidProviderId      -> 400
      OAuthError_InvalidResponse        -> 502
      OAuthError_InvalidRequest         -> 400
      OAuthError_GetAccessTokenFailed _ -> 502
      OAuthError_InvalidMethod          -> 405

  modifyResponse $ setResponseStatus status (T.encodeUtf8 $ textOAuthError err)
  writeLBS $ Aeson.encode $ (Left err :: Either OAuthError AccessToken)

