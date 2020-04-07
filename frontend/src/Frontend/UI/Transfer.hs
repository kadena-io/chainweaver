{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.UI.Transfer where

import Control.Error hiding (bool)
import Control.Lens
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict
import Data.Aeson (FromJSON, ToJSON)
import Data.Decimal
import Data.Default (Default (..))
import Data.Some (Some(..))
import Data.String (IsString)
import Data.Text (Text)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.GlobalEventHandlers (keyPress)
import GHCJS.DOM.KeyboardEvent (getCtrlKey, getKey, getKeyCode, getMetaKey)
import GHCJS.DOM.Types (HTMLElement (..), unElement)
import Kadena.SigningApi (AccountName(..))
import Language.Javascript.JSaddle (liftJSM)
import Obelisk.Generated.Static
import Obelisk.Route (R)
import Obelisk.Route.Frontend
import Pact.Repl
import Pact.Repl.Types
import Pact.Server.ApiClient (HasTransactionLogger)
import Pact.Types.Lang
import qualified Pact.Types.Term as Pact
import Reflex
import Reflex.Dom.ACE.Extended hiding (Annotation (..))
import Reflex.Dom.Contrib.Vanishing
import Reflex.Dom.Core
import qualified Data.Map as Map
import qualified Data.Text as T

import Common.Foundation
import Common.OAuth (OAuthProvider (OAuthProvider_GitHub))
import Common.Route
import Frontend.AppCfg
import Frontend.Crypto.Class
import Frontend.Editor
import Frontend.Foundation
import Frontend.GistStore
import Frontend.Ide
import Frontend.JsonData
import Frontend.Log
import Frontend.OAuth
import Frontend.Network
import Frontend.Repl
import Frontend.Storage
import qualified Frontend.VersionedStore as Store
import Frontend.UI.Button
import Frontend.UI.Dialogs.AddVanityAccount (uiAddAccountButton)
import Frontend.UI.Dialogs.AddVanityAccount.DefineKeyset
import Frontend.UI.Dialogs.CreateGist (uiCreateGist)
import Frontend.UI.Dialogs.CreatedGist (uiCreatedGist)
import Frontend.UI.Dialogs.DeployConfirmation (uiDeployConfirmation)
import Frontend.UI.Dialogs.LogoutConfirmation (uiLogoutConfirmation)
import Frontend.UI.Dialogs.Send
import Frontend.UI.Dialogs.Signing (uiSigning)
import Frontend.UI.Dialogs.WatchRequest
import Frontend.UI.IconGrid (IconGridCellConfig(..), iconGridLaunchLink)
import Frontend.UI.KeysetWidget
import Frontend.UI.Modal
import Frontend.UI.Modal.Impl
import Frontend.UI.RightPanel
import Frontend.UI.Settings
import Frontend.UI.TabBar
import Frontend.UI.Wallet
import Frontend.UI.Widgets
import Frontend.Wallet

data TransferCfg t = TransferCfg
  { _transferCfg_isVisible :: Dynamic t Bool
  , _transferCfg_setFrom :: Event t ChainAccount
  , _transferCfg_setTo :: Event t ReceivingAccount
  }

instance Reflex t => Default (TransferCfg t) where
  def = TransferCfg (constDyn False) never never

data ChainAccount = ChainAccount
  { _ca_chain :: ChainId
  , _ca_account :: Text -- TODO Might use a newtype wrapper and correct account validation
  } deriving (Eq,Ord,Show)

data ReceivingAccount = ReceivingAccount
  { _ra_chainAccount :: ChainAccount
  , _ra_keyset :: UserKeyset
  } deriving (Eq,Ord,Show)

uiChainAccount
  :: (MonadWidget t m, HasNetwork model t)
  => model
  -> m (Dynamic t (Maybe ChainAccount))
uiChainAccount model = do
  cd <- userChainIdSelect $ getChainsFromHomogenousNetwork model
  (_,a) <- uiAccountNameInput True Nothing never noValidation
  return $ runMaybeT $ ChainAccount <$> MaybeT (value cd) <*> MaybeT (unAccountName <$$> a)

data TransferInfo = TransferInfo
  { _ti_fromAccount :: ChainAccount
  , _ti_amount :: Decimal -- Maybe use ParsedDecimal
  , _ti_toAccount :: ChainAccount
  , _ti_toKeyset :: Maybe UserKeyset
  } deriving (Eq,Ord,Show)

-- -> RoutedT t (R FrontendRoute) m ()
uiGenericTransfer
  :: ( MonadWidget t m
     , RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m
     , HasConfigs m
     , HasStorage m, HasStorage (Performable m)
     , HasTransactionLogger m
     , HasLogger model t
     , HasCrypto key (Performable m)
     , HasCrypto key m
     , HasNetwork model t
     , HasNetworkCfg (ModalCfg mConf t) t
     , HasWallet model key t
     , HasJsonData model t
     , Monoid mConf
     , Monoid (ModalCfg mConf t)
     , Flattenable (ModalCfg mConf t) t
     , HasModalCfg mConf (Modal mConf m t) t
     , HasWalletCfg (ModalCfg mConf t) key t
     )
  => model
  -> TransferCfg t
  -> m mConf
uiGenericTransfer model cfg = do
  --let visibility = displayNoneWhen . not <$> _transferCfg_isVisible cfg
  --vanishingAttr "main" ("class" =: "main transfer__pane") visibility $ do
  let attrs = do
        visible <- _transferCfg_isVisible cfg
        pure $ if visible
          then ("class" =: "main transfer transfer__expanded")
          else ("class" =: "main transfer")
  elDynAttr "main" attrs $ do
    transferInfo <- divClass "transfer-fields" $ do
      (fromAcct,amount) <- divClass "transfer__left-pane" $ do
        el "h4" $ text "From"
        fca <- uiChainAccount model
        amt <- mkLabeledInput True "Amount" uiDecimalInputElement def
        return (fca,amt)
      (toAcct,ks) <- divClass "transfer__right-pane" $ do
        el "h4" $ text "To"
        tca <- uiChainAccount model
        k <- keysetWidget Nothing
--        performEvent_ (liftIO . print <$> updated k)
        return (tca,k)
      return $ runMaybeT $ TransferInfo <$>
        MaybeT fromAcct <*>
        MaybeT (hush <$> amount) <*>
        MaybeT toAcct <*>
        lift ks
    signTransfer <- divClass "transfer-fields submit" $ do
      confirmButton (def { _uiButtonCfg_disabled = (isNothing <$> transferInfo) }) "Sign & Transfer"
    let netInfo = flip push signTransfer $ \() -> sampleNetInfo model
    let mkModal (Just ti) ni = Just $ lookupAndTransfer model ti ni
        mkModal Nothing _ = Nothing
    pure $ mempty & modalCfg_setModal .~ (attachWith mkModal (current transferInfo) netInfo)

lookupAndTransfer
  :: ( SendConstraints model mConf key t m
     , Flattenable mConf t
     , HasTransactionLogger m
     )
  => model
  -> TransferInfo
  -> SharedNetInfo NodeInfo
  -> Event t ()
  -> m (mConf, Event t ())
lookupAndTransfer model ti netInfo onCloseExternal = do
    let nodes = _sharedNetInfo_nodes netInfo
        fromAccount = _ca_account $ _ti_fromAccount ti
        fromChain = _ca_chain $ _ti_fromAccount ti
    eks <- lookupKeySet (model ^. logger) (_sharedNetInfo_network netInfo)
                 nodes fromChain (AccountName fromAccount)
--    let eWrapper (Left m) = text m
--        eWrapper (Right ks) = void $ uiTransferModal model ti ks onCloseExternal
--    networkHold (text "Querying sender keyset...") (eWrapper <$> eks)
    return (mempty, never)

uiTransferButton
  :: ( DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => m (Dynamic t Bool)
uiTransferButton = mdo
  let buttonText = bool "Show Transfer" "Hide Transfer" <$> isVisible
  click <- uiButton (def & uiButtonCfg_class <>~ " main-header__account-button") $ do
    dynText buttonText
  isVisible <- toggle False click
  return isVisible

-- | A modal for handling sending coin
uiTransferModal
  :: ( SendConstraints model mConf key t m
     , Flattenable mConf t
     , HasTransactionLogger m
     )
  => model
  -> TransferInfo
  -> Pact.KeySet
  -> Event t ()
  -> m (mConf, Event t ())
uiTransferModal model ti ks _onCloseExternal = do

  (conf, closes) <- fmap splitDynPure $ workflow $ signAndTransfer model ti

  mConf <- flatten =<< tagOnPostBuild conf
  let close = switch $ current closes
  pure (mConf, close)

signAndTransfer
  :: ( SendConstraints model mConf key t m
     , HasTransactionLogger m
     )
  => model
  -> TransferInfo
  -> Workflow t m (mConf, Event t ())
signAndTransfer model ti = Workflow $ do
    close <- modalHeader $ text "Sign Transfer"
    rec
      (currentTab, _done) <- transferTabs $ leftmost [prevTab, fmapMaybe id nextTab]
      conf <- mainSection currentTab
      (cancel, prevTab, nextTab) <- footerSection currentTab
    pure ( (conf, close <> cancel)
         , leftmost
           [ never
           ]
         )
  where
    mainSection currentTab = do
      text "Main Section"
      return mempty
    footerSection currentTab = modalFooter $ do
      let (lbl, fanTag) = splitDynPure $ ffor currentTab $ \case
            TransferTab_Signatures -> ("Cancel", Left ())
            TransferTab_Metadata -> ("Back", Right TransferTab_Signatures)

      ev <- cancelButton def lbl
      let (cancel, back) = fanEither $ current fanTag <@ ev
          (name, disabled) = splitDynPure $ ffor currentTab $ \case
            TransferTab_Signatures -> ("Next", constDyn False) -- TODO Properly enable/disable Next button
            TransferTab_Metadata -> ("Preview", constDyn False)
          cfg = def
            & uiButtonCfg_class <>~ "button_type_confirm"
            & uiButtonCfg_disabled .~ join disabled
      next <- uiButtonDyn cfg $ dynText name
      let nextTab = ffor (current currentTab <@ next) $ \case
            TransferTab_Signatures -> Just TransferTab_Metadata
            TransferTab_Metadata -> Nothing
      pure (cancel, back, nextTab)

data TransferTab
  = TransferTab_Signatures
  | TransferTab_Metadata
  deriving (Eq, Ord, Show, Enum, Bounded)

displayTransferTab :: DomBuilder t m => TransferTab -> m ()
displayTransferTab = text . \case
  TransferTab_Signatures -> "Signatures"
  TransferTab_Metadata -> "Metadata"

transferTabs
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Event t TransferTab
  -> m (Dynamic t TransferTab, Event t ())
transferTabs tabEv = do
  let f t0 g = case g t0 of
        Nothing -> (Just t0, Just ())
        Just t -> (Just t, Nothing)
  rec
    (curSelection, done) <- mapAccumMaybeDyn f TransferTab_Signatures $ leftmost
      [ const . Just <$> onTabClick
      , const . Just <$> tabEv
      ]
    (TabBar onTabClick) <- makeTabBar $ TabBarCfg
      { _tabBarCfg_tabs = [TransferTab_Signatures, TransferTab_Metadata]
      , _tabBarCfg_mkLabel = \_ -> displayTransferTab
      , _tabBarCfg_selectedTab = Just <$> curSelection
      , _tabBarCfg_classes = mempty
      , _tabBarCfg_type = TabBarType_Secondary
      }
  pure (curSelection, done)
