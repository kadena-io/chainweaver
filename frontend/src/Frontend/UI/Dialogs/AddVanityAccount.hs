{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.UI.Dialogs.AddVanityAccount
  ( uiAddVanityAccount
  ) where

import Control.Error (hush)
import Control.Monad (join)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Lens ((^.))
import Control.Applicative (liftA2)
import Data.Maybe (isNothing,maybe)
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T

import Data.Aeson (Object, Value (String,Array))
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Reflex
import Reflex.Dom.Core
import Reflex.Dom.Contrib.CssClass (renderClass)

import Reflex.Extended (tagOnPostBuild)
import Reflex.Network.Extended (Flattenable, flatten)

import Frontend.UI.Modal.Impl (ModalIde,modalFooter, modalHeader)
import Frontend.UI.Widgets
import Frontend.UI.DeploymentSettings
import Frontend.UI.Dialogs.DeployConfirmation (CanSubmitTransaction, TransactionSubmitFeedback (..), submitTransactionWithFeedback)

import Frontend.AppCfg
import Frontend.Crypto.Class (HasCrypto, cryptoGenKey)
import Frontend.Crypto.Ed25519 (keyToText)
import Frontend.JsonData
import Frontend.Ide (_ide_wallet)
import Frontend.Network (HasNetworkCfg, ChainId, NodeInfo, defaultTransactionGasLimit, networkCfg_setSender, network_selectedNodes)
import Frontend.Wallet (AccountName, Account (..), KeyPair (..), HasWalletCfg (..),unAccountName, checkAccountNameValidity, findNextKey)

-- Allow the user to create a 'vanity' account, which is an account with a custom name
-- that lives on the chain. Requires GAS to create.

type HasUISigningModelCfg mConf key t =
  ( Monoid mConf, Flattenable mConf t, HasWalletCfg mConf key t
  , HasJsonDataCfg mConf t, HasNetworkCfg mConf t
  )

uiAddVanityAccount
  :: forall key t m mConf
  . ( MonadWidget t m
    , HasUISigningModelCfg mConf key t
    , HasCrypto key (Performable m)
    )
  => AppCfg key t m
  -> ModalIde m key t
  -> Event t ()
  -> m (mConf, Event t ())
uiAddVanityAccount _appCfg ideL _onCloseExternal = do
  rec
    onClose <- modalHeader $ dynText title
    result <- workflow $ uiAddVanityAccountSettings ideL
    let (title, (eNewAccount0, conf0)) = fmap splitDynPure $ splitDynPure result

  conf <- flatten =<< tagOnPostBuild conf0
  let eNewAccount = switch $ current eNewAccount0
  pure ( conf & walletCfg_importAccount .~ eNewAccount
       , leftmost [() <$ eNewAccount, onClose]
       )

mkPubkeyPactData :: KeyPair key -> Object
mkPubkeyPactData = HM.singleton "pubkey" . Array . V.singleton . String . keyToText . _keyPair_publicKey

mkPactCode :: Maybe AccountName -> Text
mkPactCode (Just acc) = "(coin.create-account \"" <> unAccountName acc <> "\" (read-keyset \"pubkey\"))"
mkPactCode _ = ""

uiAddVanityAccountSettings
  :: forall key t m mConf
  . ( MonadWidget t m
    , HasUISigningModelCfg mConf key t
    , HasCrypto key (Performable m)
    )
  => ModalIde m key t
  -> Workflow t m (Text, (Event t (Account key), mConf))
uiAddVanityAccountSettings ideL = Workflow $ do
  pb <- getPostBuild

  let inputElem lbl wrapperCls = divClass wrapperCls $ flip mkLabeledClsInput lbl
        $ \cls -> uiInputElement $ def & initialAttributes .~ "class" =: (renderClass cls)

      w = _ide_wallet ideL
      dNextKey = findNextKey w

      validateAccountName = checkAccountNameValidity w

      uiAccountNameInput = divClass "vanity-account-create__account-name" $ do
        dEitherAccName <- (validateAccountName <*>) . value <$>
          inputElem "Account Name" "vanity-account-create__account-name-input"

        dAccNameDirty <- holdUniqDyn =<< holdDyn False (True <$ updated dEitherAccName)

        divClass "vanity-account-create__account-name-error" $
          dyn_ $ ffor2 dAccNameDirty dEitherAccName $ \d e -> if d then either text (const blank) e else blank

        pure $ hush <$> dEitherAccName

      uiAcc = Just $ ("Reference Data",) $ liftA2 (,) uiAccountNameInput
        (value <$> inputElem "Notes" "vanity-account-create__notes")

  eKeyPair <- performEvent $ cryptoGenKey <$> current dNextKey <@ pb
  dKeyPair <- holdDyn Nothing (Just . (\(pr,pu) -> KeyPair pu (Just pr)) <$> eKeyPair)

  rec
    (curSelection, eNewAccount, _) <- buildDeployTabs Nothing controls

    (conf, result, dAccount) <- elClass "div" "modal__main transaction_details" $ do
      (cfg, cChainId, ttl, gasLimit, mAccountDetails) <- tabPane mempty curSelection DeploymentSettingsView_Cfg $
        -- Is passing around 'Maybe x' everywhere really a good way of doing this ?
        uiCfg Nothing ideL (userChainIdSelect ideL) Nothing (Just defaultTransactionGasLimit) uiAcc

      (mSender, capabilities) <- tabPane mempty curSelection DeploymentSettingsView_Keys $
        uiSenderCapabilities ideL cChainId Nothing $ uiSenderDropdown def ideL cChainId

      let dAccountName = join <$> sequence (fst <$> mAccountDetails)
          dNotes = sequence (snd <$> mAccountDetails)
          dPayload = fmap mkPubkeyPactData <$> dKeyPair
          code = mkPactCode <$> dAccountName

          account = runMaybeT $ Account
            <$> MaybeT dAccountName
            <*> MaybeT dKeyPair
            <*> MaybeT cChainId
            <*> MaybeT dNotes

      let mkSettings payload = DeploymentSettingsConfig
            { _deploymentSettingsConfig_chainId = userChainIdSelect
            , _deploymentSettingsConfig_userTab = Nothing
            , _deploymentSettingsConfig_userSection = uiAcc
            , _deploymentSettingsConfig_code = code
            , _deploymentSettingsConfig_sender = uiSenderDropdown def
            , _deploymentSettingsConfig_data = payload
            , _deploymentSettingsConfig_nonce = Nothing
            , _deploymentSettingsConfig_ttl = Nothing
            , _deploymentSettingsConfig_gasLimit = Nothing
            , _deploymentSettingsConfig_caps = Nothing
            , _deploymentSettingsConfig_extraSigners = []
            }

      pure
        ( cfg & networkCfg_setSender .~ fmapMaybe (fmap unAccountName) (updated mSender)
        , fmap mkSettings dPayload >>= buildDeploymentSettingsResult ideL mSender cChainId capabilities ttl gasLimit code
        , account
        )

    let preventProgress = (\a r -> isNothing a || isNothing r) <$> dAccount <*> result

    command <- performEvent $ tagMaybe (current result) eNewAccount
    controls <- modalFooter $ buildDeployTabFooterControls Nothing curSelection progressButtonLabalFn preventProgress

  pure
    ( ("Add New Vanity Account", (never, conf))
    , attachWith
        (\ns res -> vanityAccountCreateSubmit dAccount (_deploymentSettingsResult_chainId res) res ns)
        (current $ ideL ^. network_selectedNodes)
        command
    )
  where
    progressButtonLabalFn DeploymentSettingsView_Keys = "Create Vanity Account"
    progressButtonLabalFn _ = "Next"

vanityAccountCreateSubmit
  :: ( Monoid mConf
     , CanSubmitTransaction t m
     )
  => Dynamic t (Maybe (Account key))
  -> ChainId
  -> DeploymentSettingsResult key
  -> [Either a NodeInfo]
  -> Workflow t m (Text, (Event t (Account key), mConf))
vanityAccountCreateSubmit dAccount chainId result nodeInfos = Workflow $ do
  let cmd = _deploymentSettingsResult_command result

  txnSubFeedback <- elClass "div" "modal__main transaction_details" $
    submitTransactionWithFeedback cmd chainId nodeInfos

  -- If the message has no value yet, or it is an error then disable the 'done' button to
  -- avoid incorrectly triggering the import of the new account.
  let isDisabled = maybe True isLeft <$> _transactionSubmitFeedback_message txnSubFeedback

  eNewAccount <- modalFooter $ uiButtonDyn
    (def & uiButtonCfg_class .~ "button_type_confirm" & uiButtonCfg_disabled .~ isDisabled)
    (text "Done")

  pure
    ( ("Creating Vanity Account", (tagMaybe (current dAccount) eNewAccount, mempty))
    , never
    )
