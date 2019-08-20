{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

import Control.Concurrent
import Control.Exception (bracket_, bracket, try, catch)
import Control.Monad (void, forever, (<=<), when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import Data.Foldable (for_)
import Data.Maybe (isNothing)
import Data.Proxy (Proxy(..))
import Data.String (IsString(..))
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt(..))
import Foreign.StablePtr (StablePtr, newStablePtr)
import GHC.IO.Handle
import Language.Javascript.JSaddle.WKWebView
import Obelisk.Backend
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Reflex.Dom
import Servant.API
import System.FilePath ((</>))
import System.IO
import qualified Control.Concurrent.Async as Async
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.IO as T
import qualified Network.Socket as Socket
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Wai
import qualified Servant.Server as Servant
import qualified System.Directory as Directory
import qualified System.Environment as Env
import qualified System.FilePath as FilePath
import qualified System.Process as Process

import Backend (serveBackendRoute)
import Common.Route
import Frontend
import Frontend.AppCfg
import Frontend.ReplGhcjs (app)
import Frontend.Storage

foreign import ccall setupAppMenu :: StablePtr (CString -> IO ()) -> IO ()
foreign import ccall activateWindow :: IO ()
foreign import ccall hideWindow :: IO ()
foreign import ccall global_openFileDialog :: IO ()
foreign import ccall global_requestUserAttention :: IO CInt
foreign import ccall global_cancelUserAttentionRequest :: CInt -> IO ()
foreign import ccall global_getHomeDirectory :: IO CString

getUserLibraryPath :: MonadIO m => m FilePath
getUserLibraryPath = liftIO $ do
  home <- peekCString =<< global_getHomeDirectory
  let lib = home </> "Library" </> "Application Support" </> "io.kadena.pact"
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

type SigningApi = "v1" :> V1SigningApi
type V1SigningApi = "sign" :> ReqBody '[JSON] SigningRequest :> Post '[JSON] SigningResponse

main :: IO ()
main = redirectPipes [stdout, stderr] $ do
  -- Set the path to z3. I tried using the plist key LSEnvironment, but it
  -- doesn't work with relative paths.
  exePath <- L.dropWhileEnd (/= '/') <$> Env.getExecutablePath
  putStrLn $ "Executable path: " <> exePath
  Env.setEnv "SBV_Z3" $ exePath <> "z3"
  libPath <- getUserLibraryPath
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
      b = runBackend' (Just $ fromIntegral port) staticAssets backend $ Frontend blank blank
  fileOpenedMVar :: MVar T.Text <- liftIO newEmptyMVar
  -- Run the backend in a forked thread, and run jsaddle-wkwebview on the main thread
  putStrLn "Starting backend"
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
    signingLock <- liftIO newEmptyMVar -- Only allow one signing request to be served at once
    signingRequestMVar <- liftIO newEmptyMVar
    signingResponseMVar <- liftIO newEmptyMVar
    let runSign obj = do
          resp <- liftIO $ bracket_ (putMVar signingLock ()) (takeMVar signingLock) $ do
            putMVar signingRequestMVar obj -- handoff to app
            bracket
              global_requestUserAttention
              global_cancelUserAttentionRequest
              (\_ -> takeMVar signingResponseMVar)
          case resp of
            Left e -> throwError $ Servant.err409
              { Servant.errBody = LBS.fromStrict $ T.encodeUtf8 e }
            Right v -> pure v
        s = Warp.setPort 9467 Warp.defaultSettings
        laxCors _ = Just $ Wai.simpleCorsResourcePolicy
          { Wai.corsRequestHeaders = Wai.simpleHeaders }
        apiServer
          = Warp.runSettings s $ Wai.cors laxCors
          $ Servant.serve (Proxy @SigningApi) runSign
    _ <- Async.async $ apiServer
    runHTMLWithBaseURL "index.html" route (cfg putStrLn handleOpen) $ do
      mInitFile <- liftIO $ tryTakeMVar fileOpenedMVar
      let frontendMode = FrontendMode
            { _frontendMode_hydrate = False
            , _frontendMode_adjustRoute = True
            }
          configs = M.fromList -- TODO don't embed all of these into binary
            [ ("common/route", route)
            --, ("common/networks", "remote-source:https://pact.kadena.io/networks")
            , ("common/networks", "remote-source:https://pact-web.qa.obsidian.systems/networks")
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
                , _appCfg_openFileDialog = liftIO global_openFileDialog
                , _appCfg_loadEditor = pure mInitFile

                -- DB 2019-08-07 Changing this back to False because it's just too convenient this way.
                , _appCfg_editorReadOnly = False
                , _appCfg_signingRequest = signingRequest
                , _appCfg_signingResponse = liftIO . putMVar signingResponseMVar
                }
          _ <- flip runStorageT store $ runWithReplace loaderMarkup $
            (liftIO activateWindow >> app appCfg) <$ bowserLoad
          pure ()
        }

fileStorage :: FilePath -> Storage
fileStorage libPath = Storage
  { _storage_get = \_ k -> liftIO $ do
    try (BS.readFile $ path k) >>= \case
      Left (e :: IOError) -> do
        putStrLn $ "Error reading storage: " <> show e <> " : " <> path k
        pure Nothing
      Right v -> do
        let result = Aeson.decodeStrict v
        when (isNothing result) $ do
          T.putStrLn $ "Error reading storape: can't decode contents: " <>
            T.decodeUtf8With T.lenientDecode v
        pure result
  , _storage_set = \_ k a -> liftIO $
    catch (LBS.writeFile (path k) (Aeson.encode a)) $ \(e :: IOError) -> do
      putStrLn $ "Error writing storage: " <> show e <> " : " <> path k
  , _storage_remove = \_ k -> liftIO $
    catch (Directory.removeFile (path k)) $ \(e :: IOError) -> do
      putStrLn $ "Error removing storage: " <> show e <> " : " <> path k
  }
    where path :: Show a => a -> FilePath
          path k = libPath </> FilePath.makeValid (show k)

-- | Push writes to the given 'MVar' into an 'Event'.
mvarTriggerEvent
  :: (PerformEvent t m, TriggerEvent t m, MonadIO m, MonadIO (Performable m), PostBuild t m)
  => MVar a -> m (Event t a)
mvarTriggerEvent mvar = do
  (e, trigger) <- newTriggerEvent
  _ <- liftIO $ forkIO $ forever $ trigger =<< takeMVar mvar
  pure e

cfg :: (String -> IO ()) -> (FilePath -> IO Bool) -> AppDelegateConfig
cfg onUniversalLink handleOpen = AppDelegateConfig
  { _appDelegateConfig_willFinishLaunchingWithOptions = do
    putStrLn "will finish launching"
    setupAppMenu <=< newStablePtr $ void . handleOpen <=< peekCString
  , _appDelegateConfig_didFinishLaunchingWithOptions = do
    putStrLn "did finish launching"
    hideWindow
  , _appDelegateConfig_applicationDidBecomeActive = putStrLn "did become active"
  , _appDelegateConfig_applicationWillResignActive = putStrLn "will resign active"
  , _appDelegateConfig_applicationDidEnterBackground = putStrLn "did enter background"
  , _appDelegateConfig_applicationWillEnterForeground = putStrLn "will enter foreground"
  , _appDelegateConfig_applicationWillTerminate = putStrLn "will terminate"
  , _appDelegateConfig_applicationSignificantTimeChange = putStrLn "time change"
  , _appDelegateConfig_applicationUniversalLink = \cs -> do
      s <- peekCString cs
      putStrLn $ "universal link: " <> s
      onUniversalLink s
  , _appDelegateConfig_appDelegateNotificationConfig = def
  , _appDelegateConfig_developerExtrasEnabled = True
  , _appDelegateConfig_applicationOpenFile = \cs -> do
      filePath <- peekCString cs
      putStrLn $ "application open file: " <> filePath
      handleOpen filePath
  }
