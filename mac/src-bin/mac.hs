{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Exception (bracket)
import Control.Monad (forever, (>=>))
import Data.Foldable (for_)
import Data.String (IsString(..))
import Foreign.C.String (peekCString)
import GHC.IO.Handle
import Language.Javascript.JSaddle.WKWebView
import Obelisk.Backend
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Reflex.Dom
import System.FilePath ((</>))
import System.IO
import qualified Control.Concurrent.Async as Async
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Socket as Socket
import qualified System.Process as Process

import Backend (serveBackendRoute)
import qualified Backend.Devel as Devel
import Common.Route
import Frontend
import Frontend.ReplGhcjs (app)

import Foreign.C.String (CString, withCString)
import Foreign.StablePtr (StablePtr, newStablePtr)

foreign import ccall setupAppMenu :: IO ()



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

main :: IO ()
main = redirectPipes [stdout, stderr] $ do
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
  -- Run the backend in a forked thread, and run jsaddle-wkwebview on the main thread
  putStrLn "Starting backend"
  Async.withAsync b $ \_ -> do
    liftIO $ putStrLn "Starting jsaddle"
    runHTMLWithBaseURL "index.html" route (cfg putStrLn) $ do
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
      bowserChan :: Chan () <- liftIO newChan
      -- Run real obelisk frontend
      runFrontendWithConfigsAndCurrentRoute frontendMode configs backendEncoder $ Frontend
        -- TODO we shouldn't have to use prerender since we aren't hydrating
        { _frontend_head = prerender_ blank $ do
          bowserLoad <- newHead $ \r -> T.pack $ T.unpack route </> T.unpack (renderBackendRoute backendEncoder r)
          performEvent_ $ liftIO . writeChan bowserChan <$> bowserLoad
        , _frontend_body = prerender_ blank $ do
          (bowserLoad, triggerBowserLoad) <- newTriggerEvent
          _ <- liftIO $ forkIO $ forever $ triggerBowserLoad =<< readChan bowserChan
          _ <- runWithReplace loaderMarkup $ app False <$ bowserLoad
          pure ()
        }

cfg :: (String -> IO ()) -> AppDelegateConfig
cfg onUniversalLink = AppDelegateConfig
  { _appDelegateConfig_willFinishLaunchingWithOptions = do
    putStrLn "will finish launching"
    setupAppMenu
  , _appDelegateConfig_didFinishLaunchingWithOptions = do
    putStrLn "did finish launching"
--    gogo <- newStablePtr $ putStrLn "click handled"
--    withCString "Test Item" $ \cs -> hs_addMenuItem gogo cs
--    putStrLn "did finish launching done"
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
  , _appDelegateConfig_developerExtrasEnabled = False
  }
