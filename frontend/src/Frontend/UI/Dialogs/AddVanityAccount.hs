{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.UI.Dialogs.AddVanityAccount
  ( --uiAddVanityAccountSettings
    uiAddVanityAccountButton
  ) where

import           Control.Lens                           ((^.),(<>~))
import           Control.Monad.Trans.Class              (lift)
import           Control.Monad.Trans.Maybe              (MaybeT (..), runMaybeT)
import           Data.Functor.Identity                  (Identity(..))
import           Data.Maybe                             (isNothing,fromMaybe)
import           Data.Text                              (Text)
import           Data.Aeson                             (Object, Value (Array, String))
import qualified Data.HashMap.Strict                    as HM
import qualified Data.Vector                            as V

import Pact.Types.PactValue
import Pact.Types.Exp
import Data.Some (Some(Some))
import Data.These (These (..))

import           Reflex
import           Reflex.Dom.Contrib.CssClass            (renderClass)
import           Reflex.Dom.Core

import           Reflex.Network.Extended

import           Frontend.UI.DeploymentSettings
import           Frontend.UI.Dialogs.DeployConfirmation (Status (..), TransactionSubmitFeedback (..), CanSubmitTransaction, submitTransactionWithFeedback)
import           Frontend.UI.Modal.Impl
import           Frontend.UI.Widgets
import           Frontend.UI.Wallet
import           Frontend.UI.Widgets.AccountName (uiAccountNameInput)

import           Frontend.Crypto.Class
import           Frontend.Crypto.Ed25519                (keyToText)
import           Frontend.JsonData
import           Frontend.Network
import           Frontend.Wallet
import Reflex.Extended

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

mkPubkeyPactData :: PublicKey -> Object
mkPubkeyPactData = HM.singleton tempkeyset . Array . V.singleton . String . keyToText

mkPactCode :: Maybe AccountName -> Text
mkPactCode (Just acc) = "(coin.create-account \"" <> unAccountName acc <> "\" (read-keyset \"" <> tempkeyset <> "\"))"
mkPactCode _ = ""

uiAddVanityAccountButton
  :: forall t m key mConf
     . ( MonadWidget t m, Monoid mConf
       , HasCrypto key (Performable m)
       , HasModalCfg mConf (ModalImpl m key t) t
       )
  => ModalIde m key t
  -> m mConf
uiAddVanityAccountButton m = do
  eOpenAddAccount <- uiButton (def & uiButtonCfg_class <>~ " main-header__add-account-button")  (text "+ Add Account")
  pure $ mempty & modalCfg_setModal .~ (Just (uiAddVanityAccountDialog m) <$ eOpenAddAccount)

uiAddVanityAccountDialog
  :: ( MonadWidget t m, Monoid mConf, Flattenable mConf t
     , HasWalletCfg mConf key t, HasJsonDataCfg mConf t, HasNetworkCfg mConf t
     , HasCrypto key (Performable m)
     )
  => ModalIde m key t
  -> Event t ()
  -> m (mConf, Event t ())
uiAddVanityAccountDialog model _onCloseExternal = mdo
  onClose <- modalHeader $ dynText title
  dwf <- workflow (uiAddVanityAccountSettings model never Nothing Nothing "")
  let (title, (conf, dEvent)) = fmap splitDynPure $ splitDynPure dwf
  mConf <- flatten =<< tagOnPostBuild conf
  return (mConf, leftmost [switch $ current dEvent, onClose])

uiAddVanityAccountSettings
  :: forall key t m mConf
  . ( MonadWidget t m
    , HasUISigningModelCfg mConf key t
    , HasCrypto key (Performable m)
    )
  => ModalIde m key t
  -> Event t ()
  -> Maybe (AccountName, VanityAccount)
  -> Maybe ChainId
  -> Text
  -> Workflow t m (Text, (mConf, Event t ()))
uiAddVanityAccountSettings ideL onInflightChange mInflightAcc mChainId initialNotes = Workflow $ do

  let
    getNotes = unAccountNotes . _vanityAccount_notes

    notesInput initCfg = divClass "vanity-account-create__notes" $ mkLabeledClsInput True "Notes"
      $ \cls -> uiInputElement $ initCfg
          & initialAttributes <>~ "class" =: (renderClass cls)
          & inputElementConfig_initialValue .~ fromMaybe initialNotes (fmap (getNotes . snd) mInflightAcc)

  let includePreviewTab = False
      customConfigTab = Nothing

  rec
    let
      uiAcc = do
        name <- uiAccountNameInput ideL selChain $ fmap fst mInflightAcc
        pk <- mkLabeledClsInput True "Public Key" (uiPublicKeyDropdown ideL)
        notes <- notesInput def
        pure (name, pk, mkAccountNotes <$> value notes)
      uiAccSection = ("Reference Data", uiAcc)

    (curSelection, eNewAccount, _) <- buildDeployTabs customConfigTab includePreviewTab controls

    (conf, result, dAccount, selChain) <- elClass "div" "modal__main transaction_details" $ do
      _ <- widgetHold blank $ ffor onInflightChange $ \_ -> divClass "group" $
        text "The incomplete vanity account has been verified on the chain and added to your wallet. You may continue to create a new vanity account or close this dialog and start using the new account."

      (cfg, cChainId, ttl, gasLimit, Identity (dAccountName, dPublicKey, dNotes)) <- tabPane mempty curSelection DeploymentSettingsView_Cfg $
        -- Is passing around 'Maybe x' everywhere really a good way of doing this ?
        uiCfg Nothing ideL (userChainIdSelectWithPreselect ideL (constDyn mChainId)) Nothing (Just defaultTransactionGasLimit) (Identity uiAccSection) Nothing

      (mSender, signers, capabilities) <- tabPane mempty curSelection DeploymentSettingsView_Keys $
        uiSenderCapabilities ideL cChainId Nothing $ uiSenderDropdown def never ideL cChainId

      let dPayload = fmap mkPubkeyPactData <$> dPublicKey
          code = mkPactCode <$> dAccountName

          account = runMaybeT $ (,,,)
            <$> lift (ideL ^. network_selectedNetwork)
            <*> MaybeT dAccountName
            <*> MaybeT cChainId
            <*> vanity
          vanity = VanityAccount
            <$> MaybeT dPublicKey
            <*> lift dNotes
            <*> pure (AccountInfo Nothing Nothing False)
            <*> pure True

      let mkSettings payload = DeploymentSettingsConfig
            { _deploymentSettingsConfig_chainId = userChainIdSelect
            , _deploymentSettingsConfig_userTab = Nothing :: Maybe (Text, m ())
            , _deploymentSettingsConfig_code = code
            , _deploymentSettingsConfig_sender = uiSenderDropdown def never
            , _deploymentSettingsConfig_data = payload
            , _deploymentSettingsConfig_nonce = Nothing
            , _deploymentSettingsConfig_ttl = Nothing
            , _deploymentSettingsConfig_gasLimit = Nothing
            , _deploymentSettingsConfig_caps = Nothing
            , _deploymentSettingsConfig_extraSigners = []
            , _deploymentSettingsConfig_includePreviewTab = includePreviewTab
            }

      pure
        ( cfg & networkCfg_setSender .~ fmapMaybe (fmap (\(Some x) -> accountRefToName x)) (updated mSender)
        , fmap mkSettings dPayload >>= buildDeploymentSettingsResult ideL mSender signers cChainId capabilities ttl gasLimit code
        , account
        , cChainId
        )

    let preventProgress = (\a r -> isNothing a || isNothing r) <$> dAccount <*> result

    command <- performEvent $ tagMaybe (current result) eNewAccount
    controls <- modalFooter $ buildDeployTabFooterControls
      customConfigTab
      includePreviewTab
      curSelection
      progressButtonLabalFn
      preventProgress

  let conf0 = conf & walletCfg_importAccount .~ tagMaybe (current dAccount) command

  pure
    ( ("Add New Vanity Account", (conf0, never))
    , attachWith
        (\ns res -> vanityAccountCreateSubmit ideL dAccount (_deploymentSettingsResult_chainId res) res ns)
        (current $ ideL ^. network_selectedNodes)
        command
    )
  where
    progressButtonLabalFn DeploymentSettingsView_Keys = "Create Vanity Account"
    progressButtonLabalFn _ = "Next"

vanityAccountCreateSubmit
  :: ( Monoid mConf
     , CanSubmitTransaction t m
     , HasNetwork model t
     , HasWalletCfg mConf key t
     )
  => model
  -> Dynamic t (Maybe (NetworkName, AccountName, ChainId, VanityAccount))
  -> ChainId
  -> DeploymentSettingsResult key
  -> [Either a NodeInfo]
  -> Workflow t m (Text, (mConf, Event t ()))
vanityAccountCreateSubmit model dAccount chainId result nodeInfos = Workflow $ do
  let cmd = _deploymentSettingsResult_command result

  pb <- getPostBuild

  txnSubFeedback <- elClass "div" "modal__main transaction_details" $
    submitTransactionWithFeedback cmd chainId nodeInfos

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

  resp <- performLocalRead (model ^. network) $ [req] <$ pb

  let localOk = fforMaybe resp $ \case
        [(_, That (_, PLiteral (LString "Write succeeded")))] -> Just ()
        [(_, These _ (_, PLiteral (LString "Write succeeded")))] -> Just ()
        _ -> Nothing

      onTxnFailed = ffilter (== Status_Failed) $ updated txnListenStatus

      conf = mempty
        & walletCfg_importAccount .~ tagMaybe (current dAccount) localOk
        & walletCfg_delAccount .~ attachWithMaybe
          (\m _ -> fmap (\(n,a,c,_) -> (n, Some $ AccountRef_Vanity a c)) m)
          (current dAccount) onTxnFailed

  done <- modalFooter $ confirmButton def "Done"

  pure
    ( ("Creating Vanity Account", (conf, done))
    , never
    )
