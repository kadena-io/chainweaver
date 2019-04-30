{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend where

import           Control.Monad             (when)
import           Control.Monad.Except      (ExceptT (..), runExceptT,
                                            throwError)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Class (lift)
import qualified Data.Aeson                as Aeson
import qualified Data.CaseInsensitive      as CI
import           Data.Dependent.Sum        (DSum ((:=>)))
import           Data.List                 (foldl')
import qualified Data.List                 as L
import           Data.Maybe                (isJust)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.Text.IO              as T
import           Network.HTTP.Client       (Manager)
import           Network.HTTP.Client.TLS   (newTlsManager)
import qualified Obelisk.Backend           as Ob
import           Obelisk.ExecutableConfig  (get)
import           Obelisk.Route             (pattern (:/), R, checkEncoder,
                                            renderFrontendRoute)
import           Snap                      (Method (POST), Request (..), Snap,
                                            addHeader, getRequest,
                                            modifyResponse, pass,
                                            readRequestBody, setResponseStatus,
                                            writeBS, writeLBS)
import           Snap.Util.FileServe       (serveFile)
import           System.Directory          (canonicalizePath, doesFileExist)
import           System.Exit               (exitFailure)
import           System.FilePath           ((</>))
import           System.IO                 (stderr)

import           Obelisk.OAuth.Backend     (getAccessToken)
import           Obelisk.OAuth.Common      (AccessToken, IsOAuthProvider (..),
                                            OAuthClientSecret (..),
                                            OAuthConfig (..), OAuthError (..),
                                            OAuthProviderId (..),
                                            oAuthProviderFromIdErr,
                                            textOAuthError)

import qualified Backend.Devel             as Devel
import           Common.Api
import           Common.OAuth              (OAuthProvider (..),
                                            buildOAuthConfig, oAuthClientIdPath)
import           Common.Route
import           Common.Network


data BackendCfg = BackendCfg
  { _backendCfg_oAuth                :: OAuthConfig OAuthProvider
  , _backendCfg_getOAuthClientSecret :: OAuthProvider -> OAuthClientSecret
  , _backendCfg_manager              :: Manager
  }

-- | Where to put OAuth related backend configs:
oAuthBackendCfgPath :: Text
oAuthBackendCfgPath = "config/backend/oauth/"

oAuthClientSecretPath :: IsOAuthProvider prov => prov -> Text
oAuthClientSecretPath prov =
  oAuthBackendCfgPath <> unOAuthProviderId (oAuthProviderId prov) <> "/client-secret"

-- | Retrieve the client id of a particular client from config.
getOAuthClientSecret :: IsOAuthProvider prov => prov -> IO OAuthClientSecret
getOAuthClientSecret = fmap OAuthClientSecret . getMandatoryTextCfg . oAuthClientSecretPath


buildCfg :: IO BackendCfg
buildCfg = do
  let
    Right validFullEncoder = checkEncoder backendRouteEncoder

    renderRoute :: R FrontendRoute -> Text
    renderRoute = renderFrontendRoute validFullEncoder

  clientSecret <- getOAuthClientSecret OAuthProvider_GitHub
  oCfg <- buildOAuthConfig renderRoute
  BackendCfg oCfg (\case OAuthProvider_GitHub -> clientSecret) <$> newTlsManager


-- | Check whether needed configs are present.
--
--   TODO: We should also check whether they are valid as good as we can.
checkDeployment :: IO ()
checkDeployment = do
  let
    filesToCheck =
      map oAuthClientIdPath [ minBound .. maxBound :: OAuthProvider]
      <>
      map oAuthClientSecretPath [ minBound .. maxBound :: OAuthProvider ]
      <>
        [ "config/common/route"
        , "config/frontend/tracking-id"
        , networksPath
        ]
  presentState <- traverse (fmap isJust . get) filesToCheck
  let
    filesChecked = zip filesToCheck presentState
    missingFiles = fmap fst . filter ((== False) . snd) $ filesChecked

  when (not . null $ missingFiles) $ do
    let putErrLn = T.hPutStrLn stderr
    putErrLn "\n\n========================= PACT-WEB ERROR =========================\n\n"
    putErrLn "The deployment failed due to missing config files.\nPlease consult the project's README.md for details.\n"
    putErrLn "Missing files:\n"
    putErrLn $ T.unlines . map ("  " <>) $ missingFiles
    putErrLn "==================================================================\n\n"
    exitFailure


backend :: Ob.Backend BackendRoute FrontendRoute
backend = Ob.Backend
    { Ob._backend_run = \serve -> do
        cfg <- buildCfg
        hasServerList <- isJust <$> get networksPath
        let serveIt = serve $ serveBackendRoute "/var/lib/pact-web/dyn-configs" cfg

        if hasServerList
           -- Production mode:
           then serveIt
           --  Devel mode:
           else Devel.withPactInstances serveIt

    , Ob._backend_routeEncoder = backendRouteEncoder
    }

-- | Serve our dynconfigs file.
serveBackendRoute :: FilePath -> BackendCfg -> R BackendRoute -> Snap ()
serveBackendRoute dynConfigs cfg = \case
  BackendRoute_DynConfigs :/ ps
    -> do
      let
        strSegs = map T.unpack ps
        p = foldl' (</>) dynConfigs strSegs
      pNorm <- liftIO $ canonicalizePath p
      baseNorm <- liftIO $ canonicalizePath dynConfigs
      -- Sanity check: Make sure we are serving a file in the target directory.
      exists <- liftIO $ doesFileExist pNorm
      if L.isPrefixOf baseNorm pNorm && exists
         then serveFile pNorm
         else pass
  BackendRoute_Robots :=> _
    -> writeBS "User-agent: *\nDisallow: \n"
  BackendRoute_OAuthGetToken :/ providerId
    -> requestToken cfg providerId
  _ -> pure ()

requestToken :: BackendCfg -> OAuthProviderId -> Snap ()
requestToken (BackendCfg oAuthCfg getSecret manager) provId = do
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

