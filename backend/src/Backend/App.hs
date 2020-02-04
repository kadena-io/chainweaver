{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Backend.App where

import Control.Monad.Logger (LogStr,LogLevel)
import Control.Concurrent
import Control.Exception (bracket, try)
import Control.Monad.IO.Class
import Data.String (IsString(..))
import Language.Javascript.JSaddle.Types (JSM)
import Obelisk.Backend
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Reflex.Dom
import System.FilePath ((</>))
import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Network.Socket as Socket
import qualified Snap.Http.Server as Snap
import qualified System.Directory as Directory
import qualified System.Environment as Env
import Pact.Server.ApiV1Client (runTransactionLoggerT, logTransactionFile)

import Backend (serveBackendRoute)
import Common.Route
import Frontend
import Frontend.AppCfg
import Frontend.ModuleExplorer.Impl (loadEditorFromLocalStorage)
import Desktop.Frontend
import Desktop.SigningApi
import Desktop.Util

data AppFFI = AppFFI
  { _appFFI_activateWindow :: IO ()
  , _appFFI_resizeWindow :: (Int, Int) -> IO ()
  , _appFFI_moveToBackground :: IO ()
  , _appFFI_moveToForeground :: IO ()
  , _appFFI_global_openFileDialog :: IO ()
  , _appFFI_global_getStorageDirectory :: IO String
  , _appFFI_global_logFunction :: LogLevel -> LogStr -> IO ()
  }

getUserLibraryPath :: MonadIO m => AppFFI -> m FilePath
getUserLibraryPath ffi = liftIO $ do
  lib <- _appFFI_global_getStorageDirectory ffi
  Directory.createDirectoryIfMissing True lib
  pure lib

-- | Get a random free port. This isn't quite safe: it is possible for the port
-- to be grabbed by something else between the use of this function and the
-- ultimate use by the backend.
getFreePort :: IO Socket.PortNumber
getFreePort = Socket.withSocketsDo $ do
  addr:_ <- Socket.getAddrInfo (Just Socket.defaultHints) (Just "127.0.0.1") (Just "0")
  bracket (open addr) Socket.close Socket.socketPort
  where
    open addr = do
      sock <- Socket.socket (Socket.addrFamily addr) (Socket.addrSocketType addr) (Socket.addrProtocol addr)
      Socket.bind sock (Socket.addrAddress addr)
      pure sock

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    cfg' <- pure undefined -- TODO FIXME used by OAuth
    serve $ serveBackendRoute Nothing cfg'
  , _backend_routeEncoder = backendRouteEncoder
  }

main'
  :: AppFFI
  -> IO (Maybe BS.ByteString)
  -> (BS.ByteString -> BS.ByteString -> (String -> IO ()) -> (FilePath -> IO Bool) -> JSM () -> IO ())
  -> IO ()
main' ffi mainBundleResourcePath runHTML = do
  fileOpenedMVar :: MVar T.Text <- liftIO newEmptyMVar
  let handleOpen f = try (T.readFile f) >>= \case
        Left (e :: IOError) -> do
          putStrLn $ "Failed reading file " <> f <> ": " <> show e
          pure False
        Right c -> do
          putStrLn $ "Opened file successfully: " <> f
          putMVar fileOpenedMVar c
          pure True
  -- Set the path to z3. I tried using the plist key LSEnvironment, but it
  -- doesn't work with relative paths.
  exePath <- L.dropWhileEnd (/= '/') <$> Env.getExecutablePath
  putStrLn $ "Executable path: " <> exePath
  Env.setEnv "SBV_Z3" $ exePath <> "z3"
  libPath <- getUserLibraryPath ffi
  putStrLn $ "Library path: " <> libPath
  port <- getFreePort
  -- Get the app resources path
  resources <- maybe
    (error "No resources found")
    (T.unpack . T.decodeUtf8)
    <$> mainBundleResourcePath
  let staticAssets = StaticAssets
        { _staticAssets_processed = resources </> "static.assets"
        , _staticAssets_unprocessed = resources </> "static"
        }
      backendEncoder = either (error "frontend: Failed to check backendRouteEncoder") id $
        checkEncoder backendRouteEncoder
      route :: (IsString s, Semigroup s) => s
      route = "http://localhost:" <> fromString (show port) <> "/"
      -- We don't need to serve anything useful here under Frontend
      b = runBackendWith
        (runSnapWithConfig $ Snap.setPort (fromIntegral port) Snap.defaultConfig)
        staticAssets
        backend
        (Frontend blank blank)
  -- Run the backend in a forked thread, and run jsaddle-wkwebview on the main thread
  putStrLn $ "Starting backend on port: " <> show port
  Async.withAsync b $ \_ -> do
    liftIO $ putStrLn "Starting jsaddle"
    (signingRequestMVar, signingResponseMVar) <- signingServer
      (_appFFI_moveToForeground ffi)
      (_appFFI_moveToBackground ffi)
    runHTML "index.html" route putStrLn handleOpen $ do
      let frontendMode = FrontendMode
            { _frontendMode_hydrate = False
            , _frontendMode_adjustRoute = True
            }
          configs = M.fromList -- TODO don't embed all of these into binary
            [ ("common/route", route)
            , ("common/networks", networks)
            , ("common/oauth/github/client-id", "") -- TODO remove
            ]
          networks = T.encodeUtf8 $ T.unlines
            [ "Mainnet: us-e1.chainweb.com us-e2.chainweb.com us-w1.chainweb.com us-w2.chainweb.com jp1.chainweb.com jp2.chainweb.com fr1.chainweb.com fr2.chainweb.com"
            , "Testnet: us1.testnet.chainweb.com us2.testnet.chainweb.com eu1.testnet.chainweb.com eu2.testnet.chainweb.com ap1.testnet.chainweb.com ap2.testnet.chainweb.com"
            ]
      liftIO $ putStrLn "Starting frontend"
      bowserMVar :: MVar () <- liftIO newEmptyMVar
      -- Run real obelisk frontend
      runFrontendWithConfigsAndCurrentRoute frontendMode configs backendEncoder $ Frontend
        -- TODO we shouldn't have to use prerender since we aren't hydrating
        { _frontend_head = prerender_ blank $ do
          bowserLoad <- newHead $ \r -> T.pack $ T.unpack route </> T.unpack (renderBackendRoute backendEncoder r)
          performEvent_ $ liftIO . putMVar bowserMVar <$> bowserLoad
        , _frontend_body = prerender_ blank $ do
          bowserLoad <- mvarTriggerEvent bowserMVar
          fileOpened <- mvarTriggerEvent fileOpenedMVar
          signingRequest <- mvarTriggerEvent signingRequestMVar
          let appCfg = AppCfg
                { _appCfg_gistEnabled = False
                , _appCfg_externalFileOpened = fileOpened
                , _appCfg_openFileDialog = liftIO $ _appFFI_global_openFileDialog ffi
                , _appCfg_loadEditor = loadEditorFromLocalStorage

                -- DB 2019-08-07 Changing this back to False because it's just too convenient this way.
                , _appCfg_editorReadOnly = False
                , _appCfg_signingRequest = signingRequest
                , _appCfg_signingResponse = signingResponseHandler signingResponseMVar
                , _appCfg_enabledSettings = EnabledSettings
                  {
                  }
                , _appCfg_logMessage = _appFFI_global_logFunction ffi
                }
          _ <- mapRoutedT (flip runTransactionLoggerT (logTransactionFile $ libPath </> "transaction_log") . runFileStorageT libPath) $ runWithReplace loaderMarkup $
            (liftIO (_appFFI_activateWindow ffi) >> liftIO (_appFFI_resizeWindow ffi defaultWindowSize) >> bipWallet appCfg) <$ bowserLoad
          pure ()
        }
