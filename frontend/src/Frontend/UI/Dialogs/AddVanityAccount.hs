{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.UI.Dialogs.AddVanityAccount
  ( --uiAddVanityAccountSettings
    uiAddAccountButton
  , uiCreateAccountButton
  , uiCreateAccountDialog
  ) where

import           Control.Lens                           ((^.),(<>~), (<&>))
import           Control.Error                          (hush)
import           Control.Monad.Trans.Class              (lift)
import           Control.Monad.Trans.Maybe              (MaybeT (..), runMaybeT)
import           Data.Functor.Identity                  (Identity(..))
import           Data.Maybe                             (isNothing, fromMaybe)
import           Data.Either                            (isLeft)
import           Data.Set                               (Set)
import           Data.Text                              (Text)
import           Data.Aeson                             (Object, Value (Array, String))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict                    as HM
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V

import Kadena.SigningApi
import Pact.Types.PactValue
import Pact.Types.Exp
import Data.Some (Some(Some))
import Data.These (These (..))

import           Reflex
import           Reflex.Dom.Contrib.CssClass            (renderClass)
import           Reflex.Dom.Core

import           Reflex.Network.Extended

import Frontend.KadenaAddress
import           Frontend.UI.DeploymentSettings
import           Frontend.UI.Dialogs.DeployConfirmation (Status (..), TransactionSubmitFeedback (..), CanSubmitTransaction, submitTransactionWithFeedback)
import           Frontend.UI.Modal.Impl
import           Frontend.UI.Widgets
import           Frontend.UI.Widgets.Helpers (dialogSectionHeading)
import           Frontend.UI.Button
import           Frontend.UI.Widgets.AccountName (uiAccountNameInput)
import           Frontend.UI.Widgets.Helpers (dialogSectionHeading)

import           Frontend.Crypto.Class
import           Frontend.Crypto.Ed25519                (keyToText)
import           Frontend.JsonData
import           Frontend.Network
import           Frontend.Wallet
import           Frontend.Log
import Reflex.Extended
import Frontend.UI.JsonData (uiKeysetKeys, predDropdown)
import qualified Pact.Types.Term as Pact

-- Allow the user to create a 'vanity' account, which is an account with a custom name
-- that lives on the chain. Requires GAS to create.

type HasUISigningModelCfg mConf key t =
  ( Monoid mConf
  , Flattenable mConf t
  , HasWalletCfg mConf key t
  , HasJsonDataCfg mConf t
  , HasNetworkCfg mConf t
  )

tempkeyset :: Text
tempkeyset = "temp-vanity-keyset"

mkPactCode :: AccountName -> Text
mkPactCode acc = "(coin.create-account \"" <> unAccountName acc <> "\" (read-keyset \"" <> tempkeyset <> "\"))"

uiAddAccountButton
  :: forall t m key mConf
     . ( MonadWidget t m, Monoid mConf
       , HasCrypto key (Performable m)
       , HasModalCfg mConf (ModalImpl m key t) t
       )
  => ModalIde m key t
  -> m mConf
uiAddAccountButton m = do
  eOpenAddAccount <- uiButton (def & uiButtonCfg_class <>~ " main-header__add-account-button")  (text "+ Add Account")
  pure $ mempty & modalCfg_setModal .~ (Just (uiAddAccountDialog m) <$ eOpenAddAccount)

uiAddAccountDialog
  :: ( MonadWidget t m, Monoid mConf, Flattenable mConf t
     , HasWalletCfg mConf key t, HasJsonDataCfg mConf t, HasNetworkCfg mConf t
     , HasCrypto key (Performable m)
     )
  => ModalIde m key t
  -> Event t ()
  -> m (mConf, Event t ())
uiAddAccountDialog model _onCloseExternal = mdo
  onClose <- modalHeader $ text "Add New Account"
  name <- modalMain $ do
    divClass "group" $ do
      uiAccountNameInput model Nothing
  add <- modalFooter $ do
    confirmButton def "Add Account"
  let net = model ^. network_selectedNetwork
      val = runMaybeT $ do
        net <- lift $ current $ model ^. network_selectedNetwork
        n <- MaybeT $ current name
        pure (net, n)
      conf = mempty & walletCfg_importAccount .~ tagMaybe val add
  return (conf, onClose)

uiCreateAccountButton :: DomBuilder t m => UiButtonCfg -> m (Event t ())
uiCreateAccountButton cfg =
  uiButton (cfg & uiButtonCfg_class <>~ "button_type_secondary" <> "button_type_secondary") $ do
    elClass "span" "button__text" $ text "+ Create Account"

uiCreateAccountDialog
  :: ( Monoid mConf, Flattenable mConf t
     , MonadWidget t m
     , HasJsonData model t, HasLogger model t, HasNetwork model t, HasWallet model key t
     , HasCrypto key (Performable m)
     , HasJsonDataCfg mConf t, HasNetworkCfg mConf t, HasWalletCfg mConf key t
     )
  => model -> AccountName -> ChainId -> Event t () -> m (mConf, Event t ())
uiCreateAccountDialog model name chain onCloseExternal = do
  rec
    onClose <- modalHeader $ dynText title
    (title, (conf, closes)) <- fmap (fmap splitDynPure . splitDynPure) $ workflow $ createAccountSplash model name chain
  mConf <- flatten =<< tagOnPostBuild conf
  let close = switch $ current closes
  pure (mConf, onClose <> close)

createAccountSplash
  :: ( Monoid mConf, Flattenable mConf t
     , MonadWidget t m
     , HasJsonData model t, HasLogger model t, HasNetwork model t, HasWallet model key t
     , HasCrypto key (Performable m)
     , HasJsonDataCfg mConf t, HasNetworkCfg mConf t, HasWalletCfg mConf key t
     )
  => model -> AccountName -> ChainId -> Workflow t m (Text, (mConf, Event t ()))
createAccountSplash model name chain = Workflow $ do
  keyset <- modalMain $ do
    dialogSectionHeading mempty "Notice"
    -- Placeholder text
    divClass "group" $ text "In order to receive funds to an Account, the unique Account must be recorded on the blockchain. First configure the Account by defining which keys are required to sign transactions. Then create the Account by recording it on the blockchain."
    dialogSectionHeading mempty "Destination"
    divClass "group" $ transactionDisplayNetwork model
    dialogSectionHeading mempty "Define Account Keyset"
    divClass "group" $ defineKeyset model
  (cancel, next) <- modalFooter $ do
    cancel <- cancelButton def "Cancel"
    let cfg = def & uiButtonCfg_disabled .~ fmap isNothing keyset
    notGasPayer <- confirmButton cfg "I am not the Gas Payer"
    gasPayer <- confirmButton cfg "I am the Gas Payer"
    let next = leftmost
          [ tagMaybe (fmap (createAccountNotGasPayer model) <$> current keyset) notGasPayer
          , tagMaybe (fmap (createAccountConfig model name chain) <$> current keyset) gasPayer
          ]
    pure (cancel, next)
  return (("Create Account", (mempty, cancel)), next)

createAccountNotGasPayer
  :: (Monoid mConf, MonadWidget t m, HasNetwork model t, HasWallet model key t)
  => model -> AddressKeyset -> Workflow t m (Text, (mConf, Event t ()))
createAccountNotGasPayer model keyset = Workflow $ do
  modalMain $ do
    text "Kadena address goes here"
    divClass "group" $ text $ T.pack $ show keyset
  done <- modalFooter $ confirmButton def "Done"
  pure (("Create Account", (mempty, done)), never)

-- TODO make this look like the new design
defineKeyset :: (MonadWidget t m, HasWallet model key t) => model -> m (Dynamic t (Maybe AddressKeyset))
defineKeyset model = do
  let allKeys = fmap (_keyPair_publicKey . _key_pair) . IntMap.elems <$> model ^. wallet_keys
  rec
    selectedKeys <- foldDyn ($) Set.empty $ ffor chooseKey $ \(key, selected) -> case selected of
      False -> Set.delete key
      True -> Set.insert key
    chooseKey <- uiKeysetKeys selectedKeys allKeys
  -- TODO external keys
  -- TODO validate this
  pred <- holdDyn "" =<< predDropdown never
  pure $ mkAddressKeyset <$> selectedKeys <*> pred


createAccountConfig
  :: forall key t m mConf model
  . ( MonadWidget t m
    , HasUISigningModelCfg mConf key t
    , HasCrypto key (Performable m)
    , HasJsonData model t, HasLogger model t, HasNetwork model t, HasWallet model key t
    )
  => model
  -> AccountName
  -> ChainId
  -> AddressKeyset
  -> Workflow t m (Text, (mConf, Event t ()))
createAccountConfig ideL name chainId keyset = Workflow $ do

  let includePreviewTab = False
      customConfigTab = Nothing

  rec
    (curSelection, eNewAccount, _) <- buildDeployTabs customConfigTab includePreviewTab controls

    (conf, result, selChain, gasPayer) <- elClass "div" "modal__main transaction_details" $ do
      (cfg, cChainId, mSender, ttl, gasLimit, _) <- tabPane mempty curSelection DeploymentSettingsView_Cfg $
        -- Is passing around 'Maybe x' everywhere really a good way of doing this ?
        uiCfg
          Nothing
          ideL
          (predefinedChainIdDisplayed chainId ideL)
          Nothing
          (Just defaultTransactionGasLimit)
          [] -- (Identity uiAccSection)
          Nothing
          $ uiSenderDropdown def

      mGasPayer <- tabPane mempty curSelection DeploymentSettingsView_Keys $ do
        let onSenderUpdate = gate (current $ isNothing <$> gasPayer) $ updated mSender

        dialogSectionHeading mempty "Gas Payer"
        divClass "group" $ elClass "div" "segment segment_type_tertiary labeled-input" $ do
          divClass "label labeled-input__label" $ text "Account Name"
          let ddCfg = def & dropdownConfig_attributes .~ pure ("class" =: "labeled-input__input")
          uiSenderDropdown ddCfg ideL cChainId onSenderUpdate

      let payload = addressKeysetObject keyset
          code = mkPactCode name
          deployConfig = DeploymentSettingsConfig
            { _deploymentSettingsConfig_chainId = userChainIdSelect
            , _deploymentSettingsConfig_userTab = Nothing :: Maybe (Text, m ())
            , _deploymentSettingsConfig_code = pure code
            , _deploymentSettingsConfig_sender = uiSenderDropdown def
            , _deploymentSettingsConfig_data = Just payload
            , _deploymentSettingsConfig_nonce = Nothing
            , _deploymentSettingsConfig_ttl = Nothing
            , _deploymentSettingsConfig_gasLimit = Nothing
            , _deploymentSettingsConfig_caps = Nothing
            , _deploymentSettingsConfig_extraSigners = []
            , _deploymentSettingsConfig_includePreviewTab = includePreviewTab
            }

          capabilities = mGasPayer <&>
            maybe mempty (\a -> fst a =: [_dappCap_cap defaultGASCapability])

      pure
        ( cfg & networkCfg_setSender .~ fmapMaybe (fmap unAccountName) (updated mSender)
        , buildDeploymentSettingsResult ideL mSender (fmap fst <$> mGasPayer) (pure mempty) cChainId capabilities ttl gasLimit (pure code) deployConfig
        , cChainId
        , mGasPayer
        )

    let preventProgress = (\r gp -> isLeft r || isNothing gp)
          <$> result
          <*> gasPayer

    command <- performEvent $ tagMaybe (current $ fmap hush result) eNewAccount
    controls <- modalFooter $ buildDeployTabFooterControls
      customConfigTab
      includePreviewTab
      curSelection
      progressButtonLabalFn
      preventProgress

  pure
    ( ("Add New Account", (conf, never))
    , attachWith
        (\ns res -> createAccountSubmit ideL (_deploymentSettingsResult_chainId res) res ns)
        (current $ ideL ^. network_selectedNodes)
        command
    )
  where
    progressButtonLabalFn DeploymentSettingsView_Keys = "Create Account"
    progressButtonLabalFn _ = "Next"

createAccountSubmit
  :: ( Monoid mConf
     , CanSubmitTransaction t m
     , HasNetwork model t
     , HasWalletCfg mConf key t
     , HasLogger model t
     )
  => model
  -> ChainId
  -> DeploymentSettingsResult key
  -> [Either a NodeInfo]
  -> Workflow t m (Text, (mConf, Event t ()))
createAccountSubmit model chainId result nodeInfos = Workflow $ do
  let cmd = _deploymentSettingsResult_command result

  pb <- getPostBuild

  txnSubFeedback <- elClass "div" "modal__main transaction_details" $
    submitTransactionWithFeedback model cmd chainId nodeInfos

  let txnListenStatus = _transactionSubmitFeedback_listenStatus txnSubFeedback

  -- Fire off a /local request and add the account if that is successful. This
  -- is optimistic but reduces the likelyhood we create the account _without_
  -- saving it, which is what would happen before if the user closed the dialog
  -- without using the "Done" button. We don't check that the sender has enough
  -- gas here, so it is possible to add non-existant accounts to the wallet.
  -- That seems better than the alternative (spending gas to create an account
  -- and not having access to it).
  let req = NetworkRequest
        { _networkRequest_cmd = cmd
        , _networkRequest_chainRef = ChainRef Nothing chainId
        , _networkRequest_endpoint = Endpoint_Local
        }

  resp <- performLocalRead (model ^. logger) (model ^. network) $ [req] <$ pb

  let localOk = fforMaybe resp $ \case
        [(_, That (_, PLiteral (LString "Write succeeded")))] -> Just ()
        [(_, These _ (_, PLiteral (LString "Write succeeded")))] -> Just ()
        _ -> Nothing

      onTxnFailed = ffilter (== Status_Failed) $ updated txnListenStatus

  done <- modalFooter $ confirmButton def "Done"

  pure
    ( ("Creating Account", (mempty, done))
    , never
    )
