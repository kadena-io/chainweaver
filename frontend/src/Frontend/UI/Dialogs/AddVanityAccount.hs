{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
module Frontend.UI.Dialogs.AddVanityAccount
  ( uiAddAccountButton
  , uiCreateAccountButton
  ) where

import Control.Applicative (liftA2)
import Data.Aeson ((.=), object, toJSON)
import Control.Lens                           ((^.),(<>~), (^?), to, _Left)
import Control.Error                          (hush)
import Control.Monad.Trans.Class              (lift)
import Control.Monad.Trans.Maybe              (MaybeT (..), runMaybeT)
import Data.Maybe                             (isNothing)
import Data.Either                            (isLeft)
import qualified Data.Map as Map
import Data.Text                              (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import qualified Data.Set as Set

import Kadena.SigningApi

import Reflex
import Reflex.Dom.Core

import Reflex.Network.Extended

import Frontend.UI.DeploymentSettings
import Frontend.UI.Dialogs.DeployConfirmation (CanSubmitTransaction, submitTransactionWithFeedback, _Status_Done, transactionSubmitFeedback_listenStatus)
import Frontend.UI.Dialogs.AddVanityAccount.DefineKeyset (HasDefinedKeyset (..), HasKeysetInputs (..), DefinedKeyset, uiDefineKeyset, emptyKeysetPresets)
import Frontend.UI.Dialogs.Receive.Legacy

import Frontend.UI.Modal.Impl
import Frontend.UI.Widgets
import Frontend.UI.Widgets.Helpers (dialogSectionHeading)

import Frontend.Crypto.Class
import Frontend.JsonData
import Frontend.Network
import Frontend.Wallet
import Frontend.Foundation
import Frontend.Log
import Frontend.TxBuilder

import Pact.Types.ChainMeta (PublicMeta (..), TTLSeconds)

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
       , HasModalCfg mConf (ModalImpl m key t) t
       )
  => ModalIde m key t
  -> m mConf
uiAddAccountButton m = do
  eOpenAddAccount <- uiButton headerBtnCfg  (text "+ Add Account")
  pure $ mempty & modalCfg_setModal .~ (Just (uiAddAccountDialog m) <$ eOpenAddAccount)

uiAddAccountDialog
  :: ( MonadWidget t m, Monoid mConf
     , HasWalletCfg mConf key t
     )
  => ModalIde m key t
  -> Event t ()
  -> m (mConf, Event t ())
uiAddAccountDialog model _onCloseExternal = mdo
  onClose <- modalHeader $ text "Add Account"
  name <- modalMain $ do
    dialogSectionHeading mempty "Notice"
    divClass "group" $ text "Add an Account here to display its status. You can add accounts that you do not own as well as accounts that do not exist yet."
    dialogSectionHeading mempty "Add Account"
    divClass "group" $ fmap snd $ uiAccountNameInputNoDropdown "Account Name" True Nothing never $ checkAccountNameAvailability
      <$> (model ^. network_selectedNetwork)
      <*> (model ^. wallet_accounts)
  modalFooter $ do
    onCancel <- cancelButton def "Cancel"
    onAdd <- confirmButton (def & uiButtonCfg_disabled .~ (isNothing <$> name)) "Add"
    let val = runMaybeT $ do
          net <- lift $ current $ model ^. network_selectedNetwork
          n <- MaybeT $ current name
          pure (net, n)
        conf = mempty & walletCfg_importAccount .~ tagMaybe val onAdd
    -- Since this always succeeds, we're okay to close on the add button event
    return ( conf
           , onCancel <> onClose <> onAdd
           )

uiCreateAccountButton :: DomBuilder t m => UiButtonCfg -> m (Event t ())
uiCreateAccountButton cfg =
  uiButton (cfg & uiButtonCfg_class <>~ "button_type_secondary" <> "button_type_secondary") $ do
    elClass "span" "button__text" $ text "+ Create Account"

createAccountSplashBaseText, createAccountSplashKeysetInfoText :: Text
createAccountSplashBaseText =
  "In order to receive funds to an Account, the unique Account must be recorded on the blockchain."
createAccountSplashKeysetInfoText =
  " First configure the Account by defining which keys are required to sign transactions. Then create the Account by recording it on the blockchain."

data CreateAccountMethod
  = CreateAccountMethod_SelfGasPayer
  | CreateAccountMethod_ExternalAccount
  | CreateAccountMethod_ShareTxBuilder
  deriving Eq

createAccountNotGasPayer
  :: ( Monoid mConf
     , MonadWidget t m
     )
  => Workflow t m (Text, (mConf, Event t ()))
  -> AccountName
  -> ChainId
  -> AccountGuard
  -> Workflow t m (Text, (mConf, Event t ()))
createAccountNotGasPayer splashWF name chain keyset = Workflow $ do
  modalMain $ do
    dialogSectionHeading mempty "Notice"
    divClass "group" $ text "The text below contains all of the Account info you have just configured. Share this Tx Builder with someone else to pay the gas for the transaction to create the Account."
    dialogSectionHeading mempty "Tx Builder"
    divClass "group" $ do
      _ <- uiDisplayTxBuilderWithCopy False $ TxBuilder
        { _txBuilder_accountName = name
        , _txBuilder_chainId = chain
        , _txBuilder_keyset = keyset ^? _AccountGuard_KeySetLike . to toPactKeyset
        }
      pure ()
  modalFooter $ do
    back <- cancelButton def "Back"
    done <- confirmButton def "Done"

    pure ( ("Create Account", (mempty, done))
        , splashWF <$ back
        )

createAccountConfig
  :: forall key t m mConf model
  . ( MonadWidget t m
    , HasUISigningModelCfg mConf key t
    , HasCrypto key (Performable m)
    , HasJsonData model t
    , HasLogger model t
    , HasNetwork model t
    , HasWallet model key t
    , HasTransactionLogger m
    )
  => model
  -> (Dynamic t (Maybe DeploymentSettingsResultError) -> m ())
  -> Workflow t m (Text, (mConf, Event t ()))
  -> AccountName
  -> ChainId
  -> AccountGuard
  -> Workflow t m (Text, (mConf, Event t ()))
createAccountConfig ideL renderErrors splashWF name chainId keyset = Workflow $ mdo
  (cfg, cChainId, mGasPayer, ttl, gasLimit, _,  _) <- divClass "modal__main transaction_details" $ do
    renderErrors $ (^? _Left) <$> result
    uiCfg
      Nothing
      ideL
      (predefinedChainIdDisplayed chainId)
      Nothing
      (Just defaultTransactionGasLimit)
      []
      TxnSenderTitle_GasPayer
      Nothing
      $ uiAccountDropdown def (pure $ \_ a -> fromMaybe False (accountHasFunds a)) (pure id)

  let payload = HM.singleton tempkeyset $ toJSON' keyset
      toJSON' (AccountGuard_KeySetLike (KeySetHeritage ksKeys ksPred _ksRef)) =
        object
          [
            "keys" .= ksKeys
          , "pred" .= ksPred
          ]
      toJSON' (AccountGuard_Other pactGuard) = toJSON pactGuard
      code = mkPactCode name
      deployConfig = DeploymentSettingsConfig
        { _deploymentSettingsConfig_chainId = fmap value . userChainIdSelect . getChainsFromHomogenousNetwork
        , _deploymentSettingsConfig_userTab = Nothing :: Maybe (Text, m ())
        , _deploymentSettingsConfig_code = pure code
        , _deploymentSettingsConfig_sender = uiAccountDropdown def (pure $ \_ _ -> True) (pure id)
        , _deploymentSettingsConfig_data = Just payload
        , _deploymentSettingsConfig_nonce = Nothing
        , _deploymentSettingsConfig_ttl = Nothing
        , _deploymentSettingsConfig_gasLimit = Nothing
        , _deploymentSettingsConfig_caps = Nothing
        , _deploymentSettingsConfig_extraSigners = []
        , _deploymentSettingsConfig_includePreviewTab = False
        }

      networkAccounts = ffor2 (ideL ^. wallet_accounts) (ideL ^. network_selectedNetwork) $ \ad n ->
        fromMaybe mempty $ Map.lookup n (unAccountData ad)

      senderKeyPairs = ffor3 networkAccounts (ideL ^. wallet_keys) mGasPayer $
        \accs keys -> foldMap $ getSigningPairs chainId keys accs . Set.singleton

      capabilities = ffor senderKeyPairs $ \kps -> Map.unionsWith (<>) $ ffor kps (=: [_dappCap_cap defaultGASCapability])

      conf = cfg & networkCfg_setSender .~ fmapMaybe (fmap unAccountName) (updated mGasPayer)
      result = buildDeploymentSettingsResult
        ideL
        mGasPayer
        (pure mempty)
        cChainId
        capabilities
        ttl
        gasLimit
        (pure code)
        deployConfig

  let preventProgress = (\r gp -> isLeft r || isNothing gp) <$> result <*> mGasPayer

  modalFooter $ do
    onBack <- cancelButton def "Back"
    onNewAccount <- confirmButton (def & uiButtonCfg_disabled .~ preventProgress) "Create Account"

    command <- performEvent $ tagMaybe (current $ fmap hush result) onNewAccount

    pure ( ("Add New Account", (conf, never))
         , leftmost
           [ attachWith
               (\ns res -> createAccountSubmit ideL (_deploymentSettingsResult_chainId res) res ns)
               (current $ ideL ^. network_selectedNodes)
               command

           , splashWF <$ onBack
           ]
         )

createAccountSubmit
  :: forall mConf model key t m a
  .  ( Monoid mConf
     , HasWalletCfg mConf key t
     , CanSubmitTransaction t m
     , HasLogger model t
     , HasTransactionLogger m
     )
  => model
  -> ChainId
  -> DeploymentSettingsResult key
  -> [Either a NodeInfo]
  -> Workflow t m (Text, (mConf, Event t ()))
createAccountSubmit model chainId result nodeInfos = Workflow $ do
  let cmd = _deploymentSettingsResult_command result
      sender = _deploymentSettingsResult_sender result

  submitFeedback <- elClass "div" "modal__main transaction_details" $
    submitTransactionWithFeedback model cmd sender chainId nodeInfos

  let succeeded = fmapMaybe (^? _Status_Done) (submitFeedback ^. transactionSubmitFeedback_listenStatus . to updated)

  done <- modalFooter $ confirmButton def "Done"

  pure
    ( ("Creating Account",
      ( mempty & walletCfg_refreshBalances <>~ succeeded
      , done
      ))
    , never
    )
