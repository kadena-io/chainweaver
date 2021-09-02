{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Backend.App where

import Control.Concurrent
import Control.Exception (bracket, try, catch)
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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Network.Socket as Socket
import qualified Network.HTTP.Client as HTTPClient
import qualified Snap.Http.Server as Snap
import qualified System.Directory as Directory
import qualified System.Environment as Env
import Pact.Server.ApiClient (runTransactionLoggerT, logTransactionFile, commandLogFilename)

import Backend (serveBackendRoute)
import Common.Logger (LogStr,LogLevel)
import Common.Route
import Frontend
import Frontend.AppCfg
import Frontend.ModuleExplorer.Impl (loadEditorFromLocalStorage)
import Desktop.Frontend
import Desktop.WalletApi

data AppFFI = AppFFI
  { _appFFI_activateWindow :: IO ()
  , _appFFI_resizeWindow :: (Int, Int) -> IO ()
  , _appFFI_moveToBackground :: IO ()
  , _appFFI_moveToForeground :: IO ()
  , _appFFI_global_openFileDialog :: FileType -> IO ()
  , _appFFI_global_saveFileDialog :: FilePath -> (FilePath -> IO ()) -> IO ()
  , _appFFI_global_getStorageDirectory :: IO String
  , _appFFI_global_logFunction :: LogLevel -> LogStr -> IO ()
  }

getUserLibraryPath :: MonadIO m => AppFFI -> m FilePath
getUserLibraryPath ffi = liftIO $ do
  lib <- _appFFI_global_getStorageDirectory ffi
  Directory.createDirectoryIfMissing True lib
  pure lib

deliverFile
  :: (TriggerEvent t m, MonadIO (Performable m), PerformEvent t m)
  => AppFFI
  -> Event t (FilePath, Text) -- inputEvent
  -> m (Event t (Either Text FilePath))
deliverFile appFFI eFile = do
  performEventAsync $ ffor eFile $ \(fName,fContent) cb ->
    let doWriteFile chosenPath =
          liftIO $ catch
            ((T.writeFile chosenPath fContent) *> cb (Right chosenPath))
            $ \(e :: IOError) -> do
              cb (Left . T.pack $ "Error '" <> show e <> "' exporting file: " <> show chosenPath)
    in liftIO (_appFFI_global_saveFileDialog appFFI fName doWriteFile)


-- | Get a random free port. This isn't quite safe: it is possible for the port
-- to be grabbed by something else between the use of this function and the
-- ultimate use by the backend.
getFreePort :: IO Socket.PortNumber
getFreePort = Socket.withSocketsDo $ do
  addr:_ <- Socket.getAddrInfo (Just Socket.defaultHints) (Just "localhost") (Just "0")
  bracket (open addr) Socket.close Socket.socketPort
  where
    open addr = do
      sock <- Socket.socket (Socket.addrFamily addr) (Socket.addrSocketType addr) (Socket.addrProtocol addr)
      Socket.bind sock (Socket.addrAddress addr)
      pure sock

waitForBackend :: Socket.PortNumber -> IO ()
waitForBackend port = do
  putStrLn $ "Checking that backend is reachable at " <> urlStr
  manager <- HTTPClient.newManager HTTPClient.defaultManagerSettings
  go manager 5
  where
    urlStr = "http://localhost:" <> show port
    go :: HTTPClient.Manager -> Int -> IO ()
    go _ 0 = error $ "Giving up trying to connect to the backend at " <> urlStr <> "."
    go manager n = do
      threadDelay (1 * 1000 * 1000)
      respEither <- try @HTTPClient.HttpException $ do
        request <- HTTPClient.parseRequest urlStr
        HTTPClient.httpLbs request manager
      case respEither of
        Left e -> do
          putStrLn $ "Could not connect to " <> urlStr <> " with err : " <> show e
          go manager (n - 1)
        Right _ -> pure ()

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
  fileOpenedMVar :: MVar (FilePath, T.Text) <- liftIO newEmptyMVar
  let handleOpen f = try (T.readFile f) >>= \case
        Left (e :: IOError) -> do
          putStrLn $ "Failed reading file " <> f <> ": " <> show e
          pure False
        Right c -> do
          putStrLn $ "Opened file successfully: " <> f
          putMVar fileOpenedMVar (f, c)
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
      bcfg = BackendConfig (runSnapWithConfig $ Snap.setPort (fromIntegral port) Snap.defaultConfig)
                           staticAssets defaultGhcjsWidgets
      b = runBackendWith
        bcfg
        backend
        (Frontend blank blank)

  -- Run the backend in a forked thread, and run jsaddle-wkwebview on the main thread
  putStrLn $ "Starting backend on port: " <> show port
  Async.withAsync b $ \_ -> do
    (signingHandler, quickSignHandler) <- walletServer
      (_appFFI_moveToForeground ffi)
      (_appFFI_moveToBackground ffi)
    waitForBackend port
    liftIO $ putStrLn "Starting jsaddle"
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
            [ "Mainnet: api.chainweb.com"
            , "Testnet: api.testnet.chainweb.com"
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
            bowserLoad <- takeMVarTriggerEvent bowserMVar
            fileOpened <- takeMVarTriggerEvent fileOpenedMVar

            let fileFFI = FileFFI
                  { _fileFFI_openFileDialog = liftIO . _appFFI_global_openFileDialog ffi
                  , _fileFFI_externalFileOpened = fileOpened
                  , _fileFFI_deliverFile = deliverFile ffi
                  }
            let appCfg enabledSettings = AppCfg
                  { _appCfg_gistEnabled = False
                  , _appCfg_loadEditor = loadEditorFromLocalStorage
                  -- DB 2019-08-07 Changing this back to False because it's just too convenient this way.
                  , _appCfg_editorReadOnly = False
                  , _appCfg_signingHandler = mkFRPHandler signingHandler
                  , _appCfg_quickSignHandler = mkFRPHandler quickSignHandler
                  , _appCfg_enabledSettings = enabledSettings
                  , _appCfg_logMessage = _appFFI_global_logFunction ffi
                  }
            _ <- mapRoutedT ( flip runTransactionLoggerT (logTransactionFile $ libPath </> commandLogFilename) .
                              runFileStorageT libPath
                            )
                 $ runWithReplace loaderMarkup
                 $ ( liftIO (_appFFI_activateWindow ffi)
                     >> liftIO (_appFFI_resizeWindow ffi defaultWindowSize)
                     >> bipWallet fileFFI (_mvarHandler_readRequest signingHandler) (_mvarHandler_readRequest quickSignHandler) appCfg
                   )
                 <$ bowserLoad
            pure ()
        }
