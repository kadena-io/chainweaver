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

module Frontend.App where

import Control.Lens
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (..))
import Data.Some (Some(..))
import Data.Text (Text)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.GlobalEventHandlers (keyPress)
import GHCJS.DOM.KeyboardEvent (getCtrlKey, getKey, getKeyCode, getMetaKey)
import GHCJS.DOM.Types (HTMLElement (..), unElement)
import Kadena.SigningApi
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
import Frontend.UI.Dialogs.SigBuilder
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
  ideL <- makeIde fileFFI appCfg cfg
  FRPHandler signingReq signingResp <- _appCfg_signingHandler appCfg
  FRPHandler contReq contResp <- _appCfg_contHandler appCfg

  sigPopup <- walletSidebar sidebarExtra
  updates <- divClass "page" $ do
    let mkPageContent c = divClass (c <> " page__content visible")
        underNetworkBar lbl sub = do
          netCfg <- networkBar ideL sigPopup
          subBarCfg <- controlBar lbl sub
          pure $ netCfg <> subBarCfg
    -- This route overriding is awkward, but it gets around having to alter the
    -- types of ideL and appCfg, and we don't actually need the true subroute
    -- yet.
    route <- askRoute
    routedCfg <- subRoute $ lift . flip runRoutedT route . \case
      FrontendRoute_Accounts -> mkPageContent "accounts" $ mdo
        query <- askRoute
        let
          startOpen :: Dynamic t (Maybe AccountName)
          startOpen =
            ffor query $ \case
              (FrontendRoute_Accounts :/ q) -> fmap AccountName $ join $ Map.lookup "open" q
              _ -> Nothing
        netCfg <- networkBar ideL sigPopup
        (transferVisible, barCfg) <- controlBar "Accounts You Are Watching" $ do
          refreshCfg <- uiWalletRefreshButton
          watchCfg <- uiWatchRequestButton ideL
          addCfg <- uiAddAccountButton ideL
          xferVisible <- uiTransferButton
          pure $ (xferVisible, watchCfg <> addCfg <> refreshCfg)
        divClass "wallet-scroll-wrapper" $ do
          transferCfg <- uiGenericTransfer ideL $ TransferCfg transferVisible never never
          accountsCfg <- uiAccountsTable ideL startOpen
          pure $ netCfg <> barCfg <> accountsCfg <> transferCfg
      FrontendRoute_Keys -> mkPageContent "keys" $ do
        walletBarCfg <- underNetworkBar "Keys" uiGenerateKeyButton
        walletCfg <- uiWallet ideL
        pure $ walletBarCfg <> walletCfg
      FrontendRoute_Contracts -> mkPageContent "contracts" $ do
        controlCfg <- underNetworkBar "Contracts" (controlBarRight fileFFI appCfg ideL)
        mainCfg <- elClass "main" "main page__main" $ do
          rec
            let collapseAttrs = ffor open $ \o -> Map.fromList
                  [ ("class", "main__pane-collapse-button" <> if o then "" else " collapsed")
                  , ("src", static @"img/double_left_arrow.svg")
                  , ("title", if o then "Collapse" else "Open")
                  ]
            (e, _) <- elDynAttr' "img" collapseAttrs blank
            open <- toggle True $ domEvent Click e
          uiEditorCfg <- codePanel appCfg (ffor open $ \o -> "main__left-pane" <> if o then "" else " pane-fullwidth") ideL
          envCfg <- rightTabBar (ffor open $ \o -> "main__right-pane" <> if o then "" else " pane-collapsed") ideL
          pure $ uiEditorCfg <> envCfg
        pure $ controlCfg <> mainCfg
      FrontendRoute_Resources -> mkPageContent "resources" $ do
        controlCfg <- underNetworkBar "Resources" (mempty <$ blank)
        elClass "main" "main page__main" $ do
          resourcesWidget
        pure controlCfg
      FrontendRoute_Settings -> do
        controlCfg <- underNetworkBar "Settings" (mempty <$ blank)
        mainCfg <- elClass "main" "main page__main" $ do
          uiSettings (_appCfg_enabledSettings appCfg) ideL fileFFI
        pure $ controlCfg <> mainCfg

    accountDatalist ideL
    keyDatalist ideL
    flatten =<< tagOnPostBuild routedCfg

  modalCfg <- showModal ideL

  req <- delay 0 signingReq
  let 
    proof = "eyJjaGFpbiI6MSwib2JqZWN0IjoiQUFBQUVBQUFBQUFBQUFBQkFLNGdycG9XZHM5ZURuUFRkT29JbVctaTJIZl9sQlM5MjJ5SFl1ZzZ6N1FwQVBvT3I1VHN3QWNRWng4U0RvUlUwYmVKTDhxTjgzYXVGYkVhTk5ZQUtHTmZBZWhPUk94V0FyQmJ1emV2dktNR1BlMHVGVV9QTzJ6MzdULS1jS2F0NnV3ekFZeEdVa3JNS0c3bDBBaVFjajVodkdEaVJoelJXVVVacXRucFFxRXVqdDJSQUlNeTdyU29ualh5WmxJY0RST29ieFo4UEV1SDFNV2pHRnRoZE54Z2dFZkdBVWpCODBmaGpYQURGbkpBNUtXSDVGZ1UtejdQbWs3QWU0elhRN1B1aGt2TUFPSUxwcEw1UkVmYjIyYk56emN1UG1KSVRUSjRsSnltS2gxME5jdVZVQlhEQUYzQlBKaV92Z1Jhd2FFbXRuVGVrWVR6T2NwblFnX2owSzNzUjhBRlBuYnFBZkRwSHBkdkxpYTZBeWtVRnhUcjlJT0VrWmlZX0YzNUJVWC1KenhZVTc0WEFELUI5dXRhdGRBc2JrS2hHQ3dvSnZHT1lZRWxSVDItald5eXBUMkl5SmRUQWM3LUhiaFpudWswd3VJRXl1TjZLaDVwVDJfbmdLNmxSRnRMWmp5Yy1sZU5BTWZuQngxVzU1TVFVTWdUcnUtT2dFUVUtWjM3STJ0Mmg2NkhRb3l5V2NGakFJYUxqWHZsTm5kZG0zWFEtbnpSdVNlTFpMTUsydUdrT1drNGpoUkp2WlA4QWVsNVFNaWpfQWgxalptM0VydEZadUZESF9FR3JLNDhVN3BJUGwtYnFkbFlBTDM5UmZCbTl4U2hhcEVpWVRrbkZDNHFYeDkxQnY3eWhKYmtiaGlyZm1VV0FFVS00cUFVR2lOQW16S3VvbGZoZ3NJa09yNzI4RHJneW1MenN1RUlpdGI0Iiwic3ViamVjdCI6eyJpbnB1dCI6IkFCUjdJbWRoY3lJNk16azBMQ0p5WlhOMWJIUWlPbnNpYzNSaGRIVnpJam9pYzNWalkyVnpjeUlzSW1SaGRHRWlPbnNpWVcxdmRXNTBJam94TENKeVpXTmxhWFpsY2lJNkltczZZamhqWVdaa09UQmtOMlEzWkRnMVpUbGxNRE5tWXpnellUUmpaVGd3T1RRMll6STVNekJsTWpFd09UWTRZbVZqWWpBeVlUYzBOakZpTmpCbVpHRTBaQ0lzSW5KbFkyVnBkbVZ5TFdkMVlYSmtJanA3SW5CeVpXUWlPaUpyWlhsekxXRnNiQ0lzSW10bGVYTWlPbHNpWWpoallXWmtPVEJrTjJRM1pEZzFaVGxsTURObVl6Z3pZVFJqWlRnd09UUTJZekk1TXpCbE1qRXdPVFk0WW1WallqQXlZVGMwTmpGaU5qQm1aR0UwWkNKZGZYMTlMQ0p5WlhGTFpYa2lPaUp6TlhCTlRDMU5hbXBSWlRRemIwZFdYekl4Vmxka1VrSnNhaTFOWHpaSmRWQmpjemwxZVZkdlpITm5JaXdpYkc5bmN5STZJbFJOUWpSVlEzQkdNRXRUTVRacFozQkNaM1ZvU0ZkVlFXNDNXVE5EVjJabFNFWlRPVGhtVnpkalJsVWlMQ0psZG1WdWRITWlPbHQ3SW5CaGNtRnRjeUk2V3lKck9tSTRZMkZtWkRrd1pEZGtOMlE0TldVNVpUQXpabU00TTJFMFkyVTRNRGswTm1NeU9UTXdaVEl4TURrMk9HSmxZMkl3TW1FM05EWXhZall3Wm1SaE5HUWlMQ0prWWpjM05qYzVNMkpsTUdaalpqaGxOelpqTnpWaVpHSXpOV0V6Tm1VMk4yWXlPVGd4TVRGa1l6WXhORFZqTmpZMk9UTmlNREV6TXpFNU1tVXlOakUySWl3ekxqazBaUzAwWFN3aWJtRnRaU0k2SWxSU1FVNVRSa1ZTSWl3aWJXOWtkV3hsSWpwN0ltNWhiV1Z6Y0dGalpTSTZiblZzYkN3aWJtRnRaU0k2SW1OdmFXNGlmU3dpYlc5a2RXeGxTR0Z6YUNJNklqRnZjMTl6VEVGVldYWkNlbk53YmpWcWFtRjNkRkp3U2xkcFNERlhVR1pvZVU1eVlXVldkbE5KZDFVaWZTeDdJbkJoY21GdGN5STZXeUpyT21JNFkyRm1aRGt3WkRka04yUTROV1U1WlRBelptTTRNMkUwWTJVNE1EazBObU15T1RNd1pUSXhNRGsyT0dKbFkySXdNbUUzTkRZeFlqWXdabVJoTkdRaUxDSWlMREZkTENKdVlXMWxJam9pVkZKQlRsTkdSVklpTENKdGIyUjFiR1VpT25zaWJtRnRaWE53WVdObElqcHVkV3hzTENKdVlXMWxJam9pWTI5cGJpSjlMQ0p0YjJSMWJHVklZWE5vSWpvaU1XOXpYM05NUVZWWmRrSjZjM0J1TldwcVlYZDBVbkJLVjJsSU1WZFFabWg1VG5KaFpWWjJVMGwzVlNKOUxIc2ljR0Z5WVcxeklqcGJJakVpTENKamIybHVMblJ5WVc1elptVnlMV055YjNOelkyaGhhVzRpTEZzaWF6cGlPR05oWm1RNU1HUTNaRGRrT0RWbE9XVXdNMlpqT0ROaE5HTmxPREE1TkRaak1qa3pNR1V5TVRBNU5qaGlaV05pTURKaE56UTJNV0kyTUdaa1lUUmtJaXdpYXpwaU9HTmhabVE1TUdRM1pEZGtPRFZsT1dVd00yWmpPRE5oTkdObE9EQTVORFpqTWprek1HVXlNVEE1TmpoaVpXTmlNREpoTnpRMk1XSTJNR1prWVRSa0lpeDdJbkJ5WldRaU9pSnJaWGx6TFdGc2JDSXNJbXRsZVhNaU9sc2lZamhqWVdaa09UQmtOMlEzWkRnMVpUbGxNRE5tWXpnellUUmpaVGd3T1RRMll6STVNekJsTWpFd09UWTRZbVZqWWpBeVlUYzBOakZpTmpCbVpHRTBaQ0pkZlN3aU1TSXNNVjFkTENKdVlXMWxJam9pV0Y5WlNVVk1SQ0lzSW0xdlpIVnNaU0k2ZXlKdVlXMWxjM0JoWTJVaU9tNTFiR3dzSW01aGJXVWlPaUp3WVdOMEluMHNJbTF2WkhWc1pVaGhjMmdpT2lJeGIzTmZjMHhCVlZsMlFucHpjRzQxYW1waGQzUlNjRXBYYVVneFYxQm1hSGxPY21GbFZuWlRTWGRWSW4xZExDSnRaWFJoUkdGMFlTSTZiblZzYkN3aVkyOXVkR2x1ZFdGMGFXOXVJanA3SW1WNFpXTjFkR1ZrSWpwdWRXeHNMQ0p3WVdOMFNXUWlPaUp6TlhCTlRDMU5hbXBSWlRRemIwZFdYekl4Vmxka1VrSnNhaTFOWHpaSmRWQmpjemwxZVZkdlpITm5JaXdpYzNSbGNFaGhjMUp2Ykd4aVlXTnJJanBtWVd4elpTd2ljM1JsY0NJNk1Dd2llV2xsYkdRaU9uc2laR0YwWVNJNmV5SmhiVzkxYm5RaU9qRXNJbkpsWTJWcGRtVnlJam9pYXpwaU9HTmhabVE1TUdRM1pEZGtPRFZsT1dVd00yWmpPRE5oTkdObE9EQTVORFpqTWprek1HVXlNVEE1TmpoaVpXTmlNREpoTnpRMk1XSTJNR1prWVRSa0lpd2ljbVZqWldsMlpYSXRaM1ZoY21RaU9uc2ljSEpsWkNJNkltdGxlWE10WVd4c0lpd2lhMlY1Y3lJNld5SmlPR05oWm1RNU1HUTNaRGRrT0RWbE9XVXdNMlpqT0ROaE5HTmxPREE1TkRaak1qa3pNR1V5TVRBNU5qaGlaV05pTURKaE56UTJNV0kyTUdaa1lUUmtJbDE5ZlN3aWMyOTFjbU5sSWpvaU1DSXNJbkJ5YjNabGJtRnVZMlVpT25zaWRHRnlaMlYwUTJoaGFXNUpaQ0k2SWpFaUxDSnRiMlIxYkdWSVlYTm9Jam9pTVc5elgzTk1RVlZaZGtKNmMzQnVOV3BxWVhkMFVuQktWMmxJTVZkUVptaDVUbkpoWlZaMlUwbDNWU0o5ZlN3aVkyOXVkR2x1ZFdGMGFXOXVJanA3SW1GeVozTWlPbHNpYXpwaU9HTmhabVE1TUdRM1pEZGtPRFZsT1dVd00yWmpPRE5oTkdObE9EQTVORFpqTWprek1HVXlNVEE1TmpoaVpXTmlNREpoTnpRMk1XSTJNR1prWVRSa0lpd2lhenBpT0dOaFptUTVNR1EzWkRka09EVmxPV1V3TTJaak9ETmhOR05sT0RBNU5EWmpNamt6TUdVeU1UQTVOamhpWldOaU1ESmhOelEyTVdJMk1HWmtZVFJrSWl4N0luQnlaV1FpT2lKclpYbHpMV0ZzYkNJc0ltdGxlWE1pT2xzaVlqaGpZV1prT1RCa04yUTNaRGcxWlRsbE1ETm1Zemd6WVRSalpUZ3dPVFEyWXpJNU16QmxNakV3T1RZNFltVmpZakF5WVRjME5qRmlOakJtWkdFMFpDSmRmU3dpTVNJc01WMHNJbVJsWmlJNkltTnZhVzR1ZEhKaGJuTm1aWEl0WTNKdmMzTmphR0ZwYmlKOUxDSnpkR1Z3UTI5MWJuUWlPako5TENKMGVFbGtJam94T0RjM05qWTRmUSJ9LCJhbGdvcml0aG0iOiJTSEE1MTJ0XzI1NiJ9"
    cont = ContRequest
      { _contRequest_pactId = "s5pML-MjjQe43oGV_21VWdRBlj-M_6IuPcs9uyWodsg"
      , _contRequest_rollback = False
      , _contRequest_step = 1
      , _contRequest_proof = Just proof
      , _contRequest_data = Nothing
      , _contRequest_caps = []
      , _contRequest_nonce = Just "hey"
      , _contRequest_chainId = Nothing
      , _contRequest_gasLimit = Nothing
      , _contRequest_ttl = Nothing
      , _contRequest_sender = Just $ AccountName "doug"
      , _contRequest_extraSigners = Nothing
      }
  contReq <- elAttr "div" ("style" =: "position: absolute; border: 1px solid black; left: 200px; top: 20px;") $
    uiButton (headerBtnCfgPrimary & uiButtonCfg_class <>~ " main-header__account-button") $
      text "ContSign"
  let
    onGistCreatedModal = Just . uiCreatedGist <$> ideL ^. gistStore_created
    gistModalCfg = mempty & modalCfg_setModal .~ onGistCreatedModal
    onSigningModal = Just . uiSigning ideL signingResp <$> req
    signingModalCfg = mempty & modalCfg_setModal .~ onSigningModal

    onContSignModal = Just . uiSigning ideL signingResp <$> (cont <$ contReq)
    contModalCfg = mempty & modalCfg_setModal .~ onContSignModal

  pure $ mconcat
    [ updates
    , modalCfg
    , gistModalCfg
    , signingModalCfg
    , contModalCfg
    , mempty & ideCfg_editor . editorCfg_loadCode .~ (snd <$> _fileFFI_externalFileOpened fileFFI)
    ]

-- Commented out and not removed since we intend to revisit this functionality soon
-- handleEndpoints
--   :: (HasWallet model key t, MonadIO m, PerformEvent t m)
--   => model -> AppCfg key t m -> m (FRPHandler SigningRequest SigningResponse t m)
-- handleEndpoints m cfg = do
--   let keys = ffor (m ^. wallet_keys) $ Right . toList . fmap (_keyPair_publicKey . _key_pair)
--       accounts = ffor (m ^. wallet_accounts) $ Right . fmap Map.keys . unAccountData

--   FRPHandler keysReqs keysResps <- _appCfg_keysEndpointHandler cfg
--   FRPHandler accountsReqs accountsResps <- _appCfg_accountsEndpointHandler cfg

--   void $ keysResps $ current keys <@ keysReqs
--   void $ accountsResps $ current accounts <@ accountsReqs
--   _appCfg_signingHandler cfg

walletSidebar
  :: ( DomBuilder t m
     , PostBuild t m
     , Routed t (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , Prerender js t m
     )
  => m ()
  -> m (Event t ())
walletSidebar sidebarExtra = elAttr "div" ("class" =: "sidebar") $ do
  divClass "sidebar__logo" $ elAttr "img" ("src" =: static @"img/logo.png") blank

  elAttr "div" ("class" =: "sidebar__content") $ do
    route <- demux . fmap (\(r :/ _) -> Some r) <$> askRoute

    let sidebarLink r@(r' :/ _) label = routeLink r $ do
          let selected = demuxed route (Some r')
          void $ uiSidebarIcon selected (routeIcon r) label
    sidebarLink (FrontendRoute_Accounts :/ mempty) "Accounts"
    sidebarLink (FrontendRoute_Keys :/ ()) "Keys"
    signEvt <- uiSidebarIcon (constDyn False) (static @"img/menu/signature.svg") "SigBuilder"
    sidebarLink (FrontendRoute_Contracts :/ Nothing) "Contracts"
    elAttr "div" ("style" =: "flex-grow: 1") blank
    sidebarLink (FrontendRoute_Resources :/ ()) "Resources"
    sidebarLink (FrontendRoute_Settings :/ ()) "Settings"
    sidebarExtra
    pure signEvt

-- | Get the routes to the icon assets for each route
routeIcon :: R FrontendRoute -> Text
routeIcon = \case
  FrontendRoute_Contracts :/ _ -> static @"img/menu/contracts.svg"
  FrontendRoute_Accounts :/ _ -> static @"img/menu/wallet.svg"
  FrontendRoute_Keys :/ _ -> static @"img/menu/keys.svg"
  FrontendRoute_Resources :/ _ -> static @"img/menu/resources.svg"
  FrontendRoute_Settings :/ _ -> static @"img/menu/settings.svg"

-- | Code editing (left hand side currently)
codePanel :: forall r key t m a. (MonadWidget t m, Routed t r m) => AppCfg key t m -> Dynamic t CssClass -> Ide a key t -> m (IdeCfg a key t)
codePanel appCfg cls m = elDynKlass "div" (cls <> "pane") $ do
    (e, eCfg) <- wysiwyg $ do
      onNewCode <- tagOnPostBuild $ m ^. editor_code
      let annotations = mapMaybe mkCodeAceAnnotation <$> m ^. editor_annotations
      onUserCode <- codeWidget appCfg (updated annotations) "" onNewCode
      pure $ mempty & editorCfg_setCode .~ onUserCode

    setFocusOn e ".ace_text-input" =<< getPostBuild

    onCtrlEnter <- getCtrlEnterEvent e
    loadCfg <- loadCodeIntoRepl m onCtrlEnter
    pure $ mconcat [ eCfg , loadCfg ]
  where
    wysiwyg = elClass' "div" "wysiwyg pane__body"
    -- We can't use domEvent Keypress because it only gets us the
    -- deprecated key code which does not work cross platform in this case:
    getCtrlEnterEvent e = do
      (onCtrlEnter, triggerEv) <- newTriggerEvent
      let htmlElement = HTMLElement . unElement $ _element_raw e
      void $ liftJSM $ htmlElement `on` keyPress $ do
        ev <- ask
        hasCtrl <- liftJSM $ getCtrlKey ev
        hasMeta <- liftJSM $ getMetaKey ev
        key <- liftJSM $ getKey ev
        code <- liftJSM $ getKeyCode ev
        let hasEnter = key == ("Enter" :: Text) || code == 10 || code == 13

        liftIO $ when ((hasCtrl || hasMeta) && hasEnter) $ triggerEv ()
      pure onCtrlEnter

-- | Load current editor code into REPL.
loadCodeIntoRepl
  :: forall key t m model a
  . (MonadWidget t m, HasEditor model t)
   => model
   -> Event t ()
   -> m (IdeCfg a key t)
loadCodeIntoRepl m onReq = do
  let onLoad = tag (current $ m ^. editor_code) onReq
  pure $ mempty
    & replCfg_sendTransaction .~ onLoad

mkCodeAceAnnotation :: Annotation -> Maybe AceAnnotation
mkCodeAceAnnotation anno = ffor rowcol $ \(r,c) -> AceAnnotation
  { _aceAnnotation_row = r
  , _aceAnnotation_column = c
  , _aceAnnotation_text = _annotation_msg anno
  , _aceAnnotation_type = T.pack . show $ _annotation_type anno
  }
  where
    rowcol = case (_annotation_source anno, _annotation_pos anno) of
      (AnnotationSource_Json, _) -> Nothing
      (AnnotationSource_Pact, Just (r, c)) -> Just (r - 1, c) -- Ace starts at 0.
      (AnnotationSource_Pact, Nothing) -> Just (0,0)

codeWidget
  :: (MonadWidget t m, Routed t r m)
  => AppCfg key t m
  -> Event t [AceAnnotation]
  -> Text
  -> Event t Text
  -> m (Event t Text)
codeWidget appCfg anno iv sv = do
    let ac = def { _aceConfigMode = Just "ace/mode/pact"
                 , _aceConfigElemAttrs = "class" =: "ace-code ace-widget"
                 , _aceConfigReadOnly = _appCfg_editorReadOnly appCfg
                 }
    route <- askRoute
    -- Without this delay, sometimes the resize doesn't take place.
    resize <- delay 0.1 . (void (updated route) <>) =<< getPostBuild
    ace <- resizableAceWidget resize mempty ac (AceDynConfig Nothing) anno iv sv
    return $ _extendedACE_onUserChange ace

networkBar
  ::
  ( MonadWidget t m
  , HasCrypto key m
  , HasTransactionLogger m
  )
  => ModalIde m key t
  -> Event t ()
  -> m (ModalIdeCfg m key t)
networkBar m sign = do
  signingPopupCfg <- sigBuilderCfg m sign
  networkCfg <- divClass "main-header main-header__network-bar" $ do
  -- Present the dropdown box for selecting one of the configured networks.
    divClass "page__network-bar-select" $ do
      selectEv <- uiNetworkSelectTopBar "select_type_special" (m ^. network_selectedNetwork) (m ^. network_networks)
      pure $ mempty & networkCfg_selectNetwork .~ selectEv
  pure $ networkCfg <> signingPopupCfg


controlBar
  :: MonadWidget t m
  => Text
  -> m a
  -> m a
controlBar pageTitle controls = do
    mainHeader $ do
      divClass "main-header__page-name" $ text pageTitle
      controls
  where
    -- Main header with adjusted padding on MacOs (scrollbars take up no space there):
    mainHeader child = do
      isMac <- getBrowserProperty "mac"

      let
        baseCls = "main-header page__main-header "
        cls = if isMac
                 then baseCls <> "page__main-header_platform_mac"
                 else baseCls
      divClass cls child

getPactVersion :: MonadWidget t m => m Text
getPactVersion = do
    is <- liftIO $ initReplState StringEval Nothing
    ver <- liftIO $ evalStateT (evalRepl' "(pact-version)") is >>= pure . \case
      Right (TLiteral (LString ver) _) -> ver
      _ -> error "failed to get pact version"
    return ver

controlBarRight  :: forall key t m. (MonadWidget t m, HasCrypto key (Performable m), HasTransactionLogger m)
  => FileFFI t m -> AppCfg key t m -> ModalIde m key t -> m (ModalIdeCfg m key t)
controlBarRight fileFFI appCfg m = do
    divClass "main-header__controls-nav" $ do
      elClass "div" "main-header__project-loader" $ do

        _ <- openFileBtn

        (onCreateGist, onLogoutClick) <- if _appCfg_gistEnabled appCfg
                                         then (,) <$> gistBtn <*> maySignoutBtn
                                         else pure (never, never)

        onLoadClicked <- loadReplBtn


        onDeployClick <- deployBtn

        loadCfg <- loadCodeIntoRepl m onLoadClicked
        let
          reqConfirmation :: Event t (Maybe (ModalImpl m key t))
          reqConfirmation = attachWith (\c _ -> Just $ uiDeployConfirmation c m) (current $ m ^. editor_code) onDeployClick

          gistConfirmation :: Event t (Maybe (ModalImpl m key t))
          gistConfirmation = Just uiCreateGist <$ onCreateGist

          logoutConfirmation :: Event t (Maybe (ModalImpl m key t))
          logoutConfirmation = Just uiLogoutConfirmation <$ onLogoutClick

          gistCfg =  mempty & modalCfg_setModal .~  gistConfirmation

          deployCfg = mempty & modalCfg_setModal .~ reqConfirmation

          logoutCfg = mempty & modalCfg_setModal .~ logoutConfirmation


        pure $ deployCfg <> loadCfg <> gistCfg <> logoutCfg
  where
    maySignoutBtn = do
      let gitHubOnline = Map.member OAuthProvider_GitHub <$> m ^. oAuth_accessTokens
      onEvClick <- networkView $ ffor gitHubOnline $ \isOnline ->
        if isOnline then signoutBtn else pure never
      switchHold never onEvClick

    signoutBtn = signoutButton $
      headerBtnCfg & uiButtonCfg_title .~ Just "Sign out from GitHub"

    deployBtn = uiButton (headerBtnCfg & uiButtonCfg_class <>~ "main-header__primary-button") $
      text $ "Deploy"

    loadReplBtn =
      uiButton ( headerBtnCfg & uiButtonCfg_title .~ Just "Editor Shortcut: Ctrl+Enter") $ do
        text "Load into REPL"

    gistBtn =
      uiButton
          ( headerBtnCfg
              & uiButtonCfg_title .~ Just "Create gist on GitHub"
              {- & uiButtonCfg_class %~ (<> "main-header__text-icon-button") -}
          ) $ do
        {- btnTextIcon (static @"img/github-gist-dark.svg") "Make Gist" blank -}
        text "Make Gist"

    openFileBtn = do
      let cfg = headerBtnCfg & uiButtonCfg_title ?~ "Open a local contract"
      uiButtonWithOnClick (_fileFFI_openFileDialog fileFFI FileType_Pact) cfg $ do
        text "Open File"

resourcesWidget
  :: (DomBuilder t m)
  => m ()
resourcesWidget = elClass "div" "icon-grid" $ do
  resourceCell "Support" (static @"img/resources/support.svg") "https://www.kadena.io/chainweaver"
    "Explore Help Resources to learn about Chainweaver, solve problems and get in touch"
  resourceCell "Documentation" (static @"img/resources/documentation.svg") "https://github.com/kadena-io/chainweaver"
    "Complete technical resources for Chainweaver"
  resourceCell "Tutorials" (static @"img/resources/tutorials.svg") "https://pactlang.org/"
    "Read or watch tutorials for writing smart contracts using the Pact language"
  where
    resourceCell title iconUrl href desc = iconGridLaunchLink href $ IconGridCellConfig
      { _iconGridCellConfig_title = title
      , _iconGridCellConfig_iconUrl = iconUrl
      , _iconGridCellConfig_desc = Just desc
      }
