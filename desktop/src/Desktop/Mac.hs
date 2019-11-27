{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Desktop.Mac where

import Control.Concurrent
import Control.Exception (bracket_, bracket, try)
import Control.Monad (forever)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import Data.Foldable (for_)
import Data.String (IsString(..))
import Foreign.C.String (CString, peekCString)
import Foreign.StablePtr (StablePtr)
import GHC.IO.Handle
import Language.Javascript.JSaddle.Types (JSM)
import Obelisk.Backend
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Reflex.Dom
import System.FilePath ((</>))
import System.IO
import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Network.Socket as Socket
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Wai
import qualified Servant.Server as Servant
import qualified Snap.Http.Server as Snap
import qualified System.Directory as Directory
import qualified System.Environment as Env
import qualified System.Process as Process

import Backend (serveBackendRoute)
import Common.Route
import Frontend
import Frontend.AppCfg
import Frontend.Storage
import Desktop.Frontend
import Desktop.SigningApi
import Desktop.Util

data MacFFI = MacFFI
  { _macFFI_setupAppMenu :: StablePtr (CString -> IO ()) -> IO ()
  , _macFFI_activateWindow :: IO ()
  , _macFFI_hideWindow :: IO ()
  , _macFFI_resizeWindow :: (Int, Int) -> IO ()
  , _macFFI_moveToBackground :: IO ()
  , _macFFI_moveToForeground :: IO ()
  , _macFFI_global_openFileDialog :: IO ()
  , _macFFI_global_getHomeDirectory :: IO CString
}

getUserLibraryPath :: MonadIO m => MacFFI -> m FilePath
getUserLibraryPath ffi = liftIO $ do
  home <- peekCString =<< _macFFI_global_getHomeDirectory ffi
  -- TODO use the bundle identifier directly, don't duplicate it
  let lib = home </> "Library" </> "Application Support" </> "io.kadena.chainweaver"
  Directory.createDirectoryIfMissing True lib
  pure lib

-- | Redirect the given handles to Console.app
redirectPipes :: [Handle] -> IO a -> IO a
redirectPipes ps m = bracket setup hClose $ \r -> Async.withAsync (go r) $ \_ -> m
  where
    setup = do
      (r, w) <- Process.createPipe
      for_ ps $ \p -> hDuplicateTo w p <> hSetBuffering p LineBuffering
      hClose w
      pure r
      -- TODO figure out how to get the logs to come from the Pact process instead of syslog
    go r = forever $ hGetLine r >>= \l -> do
      Process.callProcess "syslog" ["-s", "-k", "Level", "Notice", "Message", "Pact: " <> l]

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
  :: MacFFI
  -> IO (Maybe BS.ByteString)
  -> (BS.ByteString -> BS.ByteString -> (String -> IO ()) -> (FilePath -> IO Bool) -> JSM () -> IO ())
  -> IO ()
main' ffi mainBundleResourcePath runHTML = redirectPipes [stdout, stderr] $ do
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
  fileOpenedMVar :: MVar T.Text <- liftIO newEmptyMVar
  -- Run the backend in a forked thread, and run jsaddle-wkwebview on the main thread
  putStrLn $ "Starting backend on port: " <> show port
  Async.withAsync b $ \_ -> do
    liftIO $ putStrLn "Starting jsaddle"
    let handleOpen f = try (T.readFile f) >>= \case
          Left (e :: IOError) -> do
            putStrLn $ "Failed reading file " <> f <> ": " <> show e
            pure False
          Right c -> do
            putStrLn $ "Opened file successfully: " <> f
            putMVar fileOpenedMVar c
            pure True
    (signingRequestMVar, signingResponseMVar) <- signingServer
      (_macFFI_moveToForeground ffi)
      (_macFFI_moveToBackground ffi)
    runHTML "index.html" route putStrLn handleOpen $ do
      mInitFile <- liftIO $ tryTakeMVar fileOpenedMVar
      let frontendMode = FrontendMode
            { _frontendMode_hydrate = False
            , _frontendMode_adjustRoute = True
            }
          configs = M.fromList -- TODO don't embed all of these into binary
            [ ("common/route", route)
            , ("common/networks", "remote-source:https://pact.kadena.io/networks")
            , ("common/oauth/github/client-id", "") -- TODO remove
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
          let store = fileStorage libPath
              appCfg = AppCfg
                { _appCfg_gistEnabled = False
                , _appCfg_externalFileOpened = fileOpened
                , _appCfg_openFileDialog = liftIO $ _macFFI_global_openFileDialog ffi
                , _appCfg_loadEditor = pure mInitFile

                -- DB 2019-08-07 Changing this back to False because it's just too convenient this way.
                , _appCfg_editorReadOnly = False
                , _appCfg_signingRequest = signingRequest
                , _appCfg_signingResponse = signingResponseHandler signingResponseMVar
                , _appCfg_enabledSettings = EnabledSettings
                  {
                  }
                }
          _ <- mapRoutedT (flip runStorageT store) $ runWithReplace loaderMarkup $
            (liftIO (_macFFI_activateWindow ffi) >> liftIO (_macFFI_resizeWindow ffi minWindowSize) >> bipWallet appCfg) <$ bowserLoad
          pure ()
        }

