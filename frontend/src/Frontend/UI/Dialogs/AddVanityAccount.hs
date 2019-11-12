{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.UI.Dialogs.AddVanityAccount
  ( uiAddVanityAccountSettings
  ) where

import           Control.Applicative                    (liftA2)
import           Control.Error                          (hush)
import           Control.Lens                           ((^.))
import           Control.Monad                          (join)
import           Control.Monad.Trans.Class              (lift)
import           Control.Monad.Trans.Maybe              (MaybeT (..), runMaybeT)
import           Data.Either                            (isLeft)
import           Data.Maybe                             (isNothing, maybe)
import           Data.Text                              (Text)

import           Data.Aeson                             (Object,
                                                         Value (Array, String))
import qualified Data.HashMap.Strict                    as HM
import qualified Data.Vector                            as V

import           Reflex
import           Reflex.Dom.Contrib.CssClass            (renderClass)
import           Reflex.Dom.Core

import           Reflex.Network.Extended                (Flattenable)

import           Frontend.UI.DeploymentSettings
import           Frontend.UI.Dialogs.DeployConfirmation (CanSubmitTransaction, TransactionSubmitFeedback (..),
                                                         submitTransactionWithFeedback)
import           Frontend.UI.Modal.Impl                 (ModalIde, modalFooter)
import           Frontend.UI.Widgets

import           Frontend.Crypto.Class                  (HasCrypto,
                                                         cryptoGenKey)
import           Frontend.Crypto.Ed25519                (keyToText)
import           Frontend.Ide                           (_ide_wallet)
import           Frontend.JsonData
import           Frontend.Network                       (ChainId, HasNetworkCfg,
                                                         NodeInfo,
                                                         defaultTransactionGasLimit,
                                                         networkCfg_setSender,
                                                         network_selectedNetwork,
                                                         network_selectedNodes)
import           Frontend.Wallet                        (Account (..),
                                                         AccountName,
                                                         HasWalletCfg (..),
                                                         KeyPair (..),
                                                         checkAccountNameValidity,
                                                         findNextKey,
                                                         unAccountName)

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

mkPubkeyPactData :: KeyPair key -> Object
mkPubkeyPactData  = HM.singleton tempkeyset . Array . V.singleton . String . keyToText . _keyPair_publicKey

mkPactCode :: Maybe AccountName -> Text
mkPactCode (Just acc) = "(coin.create-account \"" <> unAccountName acc <> "\" (read-keyset \"" <> tempkeyset <> "\"))"
mkPactCode _ = ""

uiAddVanityAccountSettings
  :: forall key t m mConf
  . ( MonadWidget t m
    , HasUISigningModelCfg mConf key t
    , HasCrypto key (Performable m)
    )
  => ModalIde m key t
  -> Dynamic t (Maybe ChainId)
  -> Dynamic t Text
  -> Workflow t m (Text, (mConf, Event t ()))
uiAddVanityAccountSettings ideL mChainId initialNotes = Workflow $ do
  pb <- getPostBuild

  let inputElem lbl wrapperCls = divClass wrapperCls $ flip mkLabeledClsInput lbl
        $ \cls -> uiInputElement $ def & initialAttributes .~ "class" =: (renderClass cls)

      notesInput = divClass "vanity-account-create__notes" $ flip mkLabeledClsInput "Notes"
        $ \cls -> uiInputElement $ def
                  & initialAttributes .~ "class" =: (renderClass cls)
                  & inputElementConfig_setValue .~ (current initialNotes <@ pb)

      w = _ide_wallet ideL
      dNextKey = findNextKey w

      validateAccountName = checkAccountNameValidity w

      uiAccountNameInput = divClass "vanity-account-create__account-name" $ do
        dEitherAccName <- (validateAccountName <*>) . value <$>
          inputElem "Account Name" "vanity-account-create__account-name-input"

        dAccNameDirty <- holdUniqDyn =<< holdDyn False (True <$ updated dEitherAccName)

        divClass "vanity-account-create__account-name-error" $
          dyn_ $ ffor2 dAccNameDirty dEitherAccName $ curry $ \case
            (True, Left e) -> text e
            _ -> blank

        pure $ hush <$> dEitherAccName

      uiAcc = Just $ ("Reference Data",) $ liftA2 (,) uiAccountNameInput (value <$> notesInput)

  eKeyPair <- performEvent $ cryptoGenKey <$> current dNextKey <@ pb
  dKeyPair <- holdDyn Nothing $ ffor eKeyPair $ \(pr,pu) -> Just $ KeyPair pu $ Just pr

  rec
    (curSelection, eNewAccount, _) <- buildDeployTabs Nothing controls

    (conf, result, dAccount) <- elClass "div" "modal__main transaction_details" $ do
      (cfg, cChainId, ttl, gasLimit, mAccountDetails) <- tabPane mempty curSelection DeploymentSettingsView_Cfg $
        -- Is passing around 'Maybe x' everywhere really a good way of doing this ?
        uiCfg Nothing ideL (userChainIdSelectWithPreselect ideL mChainId) Nothing (Just defaultTransactionGasLimit) uiAcc

      (mSender, capabilities) <- tabPane mempty curSelection DeploymentSettingsView_Keys $
        uiSenderCapabilities ideL cChainId Nothing $ uiSenderDropdown def ideL cChainId

      let dAccountName = join <$> traverse fst mAccountDetails
          dNotes = traverse snd mAccountDetails
          dPayload = fmap mkPubkeyPactData <$> dKeyPair
          code = mkPactCode <$> dAccountName

          account = runMaybeT $ Account
            <$> MaybeT dAccountName
            <*> MaybeT dKeyPair
            <*> MaybeT cChainId
            <*> lift (ideL ^. network_selectedNetwork)
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
    ( ("Add New Vanity Account", (conf, never))
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
     , HasWalletCfg mConf key t
     )
  => Dynamic t (Maybe (Account key))
  -> ChainId
  -> DeploymentSettingsResult key
  -> [Either a NodeInfo]
  -> Workflow t m (Text, (mConf, Event t ()))
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

  let cfg  = mempty & walletCfg_importAccount .~ (tagMaybe (current dAccount) eNewAccount)

  pure
    ( ("Creating Vanity Account", (cfg, eNewAccount))
    , never
    )
