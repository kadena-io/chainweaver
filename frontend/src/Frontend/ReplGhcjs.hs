{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.ReplGhcjs where

import Control.Lens
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (..))
import Data.Some (Some(..))
import Data.String (IsString)
import Data.Text (Text)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.GlobalEventHandlers (keyPress)
import GHCJS.DOM.KeyboardEvent (getCtrlKey, getKey, getKeyCode, getMetaKey)
import GHCJS.DOM.Types (HTMLElement (..), unElement)
import Kadena.SigningApi (SigningRequest, SigningResponse)
import Language.Javascript.JSaddle (liftJSM)
import Obelisk.Generated.Static
import Obelisk.Route (R)
import Obelisk.Route.Frontend
import Pact.Repl
import Pact.Repl.Types
import Pact.Server.ApiClient (HasTransactionLogger)
import Pact.Types.Lang
import Reflex
import Reflex.Dom.ACE.Extended hiding (Annotation (..))
import Reflex.Dom.Core
import qualified Data.Map as Map
import qualified Data.Text as T

import Common.OAuth (OAuthProvider (OAuthProvider_GitHub))
import Common.Route
import Frontend.AppCfg
import Frontend.Crypto.Class
import Frontend.Editor
import Frontend.Foundation
import Frontend.GistStore
import Frontend.Ide
import Frontend.OAuth
import Frontend.Network
import Frontend.Repl
import Frontend.Storage
import qualified Frontend.VersionedStore as Store
import Frontend.UI.Button
import Frontend.UI.Dialogs.AddVanityAccount (uiAddAccountButton)
import Frontend.UI.Dialogs.CreateGist (uiCreateGist)
import Frontend.UI.Dialogs.CreatedGist (uiCreatedGist)
import Frontend.UI.Dialogs.DeployConfirmation (uiDeployConfirmation)
import Frontend.UI.Dialogs.LogoutConfirmation (uiLogoutConfirmation)
import Frontend.UI.Dialogs.NetworkEdit (uiNetworkSelectTopBar)
import Frontend.UI.Dialogs.Signing (uiSigning)
import Frontend.UI.IconGrid (IconGridCellConfig(..), iconGridLaunchLink)
import Frontend.UI.Modal
import Frontend.UI.Modal.Impl
import Frontend.UI.RightPanel
import Frontend.UI.Settings
import Frontend.UI.Transfer
import Frontend.UI.Wallet
import Frontend.UI.Widgets
import Frontend.Wallet hiding (walletCfg)
import Reflex.Dynamic(traceDynWith)

app
  :: forall js key t m.
     ( MonadWidget t m
     , RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m
     , HasConfigs m
     , HasStorage m, HasStorage (Performable m)
     , HasCrypto key (Performable m)
     , HasCrypto key m
     , FromJSON key, ToJSON key
     , HasTransactionLogger m
     , Prerender js t m
     )
  => RoutedT t (R FrontendRoute) m ()
  -- ^ Extra widget to display at the bottom of the sidebar
  -> FileFFI t (RoutedT t (R FrontendRoute) m)
  -> AppCfg key t (RoutedT t (R FrontendRoute) m)
  -> RoutedT t (R FrontendRoute) m ()
app sidebarExtra fileFFI appCfg = Store.versionedFrontend (Store.versionedStorage @key) $ void . mfix $ \ cfg -> do
  makeIde fileFFI appCfg cfg
  walletSidebar sidebarExtra
  route <- askRoute
  let route' = traceDynWith (const "a new route has been set") route
  display route'
  divClass "page" $ do
    subRoute_ $ lift . flip runRoutedT route' . \case
      FrontendRoute_Accounts -> do
        liftIO $ putStrLn "ACCOUNTS"
        text "ACCOUNTS"
        pure mempty
      FrontendRoute_Keys -> do
        liftIO $ putStrLn "KEYS"
        pure mempty
      FrontendRoute_Contracts -> do
        text "Contracts"
        liftIO $ putStrLn "Contracts"
        pure mempty
      FrontendRoute_Resources -> pure mempty 
      FrontendRoute_Settings -> pure mempty
  pure $ mconcat [ ]


walletSidebar
  :: ( DomBuilder t m
     , PostBuild t m
     , Routed t (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , Prerender js t m
     , MonadJSM m
     )
  => m ()
  -> m ()
walletSidebar _sidebarExtra = elAttr "div" ("class" =: "sidebar") $ do
  liftIO $ putStrLn "Asking Route: Resetting sidebar"
  elAttr "div" ("class" =: "sidebar__content") $ do
    routeLink (FrontendRoute_Accounts :/ ()) $ text "Accounts"
    routeLink (FrontendRoute_Contracts :/ ()) $ text "Contracts"

-- | Get the routes to the icon assets for each route
routeIcon :: R FrontendRoute -> Text
routeIcon = \case
  FrontendRoute_Contracts :/ _ -> static @"img/menu/contracts.svg"
  FrontendRoute_Accounts :/ _ -> static @"img/menu/wallet.svg"
  FrontendRoute_Keys :/ _ -> static @"img/menu/keys.svg"
  FrontendRoute_Resources :/ _ -> static @"img/menu/resources.svg"
  FrontendRoute_Settings :/ _ -> static @"img/menu/settings.svg"

