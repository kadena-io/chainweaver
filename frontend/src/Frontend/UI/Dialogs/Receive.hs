{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
-- | Dialog for displaying account information required for receiving transfers
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.Receive
  ( uiReceiveModal
  ) where

import Control.Applicative (liftA2)
import Control.Lens ((^.), (<>~), (^?), at, to, _Just)
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Either (rights)
import qualified Data.IntMap as IntMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Reflex
import Reflex.Dom

import Frontend.Crypto.Class (HasCrypto)

import Frontend.Foundation
import Frontend.TxBuilder
import Frontend.Network
import Frontend.Log

import Frontend.UI.Dialogs.Receive.Legacy
import Frontend.UI.DeploymentSettings (transactionDisplayNetwork)
import Frontend.UI.FormWidget
import Frontend.UI.KeysetWidget

import Frontend.UI.Modal
import Frontend.UI.Transfer
import Frontend.UI.Widgets
import Frontend.UI.Widgets.Helpers (dialogSectionHeading)
import Frontend.Wallet

import qualified Pact.Types.Term as Pact

uiReceiveModal
  :: ( MonadWidget t m
     , Monoid mConf
     , HasNetwork model t
     , HasNetworkCfg mConf t
     , Flattenable mConf t
     , HasCrypto key m
     , HasCrypto key (Performable m)
     , HasWallet model key t
     , HasLogger model t
     , HasTransactionLogger m
     )
  => NetworkName
  -> [Either Text NodeInfo]
  -> Text
  -> model
  -> AccountName
  -> ChainId
  -> Maybe AccountDetails
  -> Event t ()
  -> m (mConf, Event t ())
uiReceiveModal network nodes modalTitle model account chain mdetails _onClose = do
  onClose <- modalHeader $ text modalTitle

  divClass "modal__main receive" $ do
    dmks <- receiveToNonexistentAccount model network nodes account chain mdetails

    dyn_ $ ffor dmks $ \mks -> do
      case mks of
        (Nothing, Nothing) -> text "Something should be here"
        (Just ks, ref) -> do
          let txb = TxBuilder account chain (Just $ userToPactKeyset ks) ref --TODO bruh
          dialogSectionHeading mempty "Account Information"
          divClass "group" $ uiDisplayTxBuilderWithCopy True txb

  let doneCfg = def & uiButtonCfg_class <>~ "button_type_confirm"
  done <- modalFooter $ uiButtonDyn doneCfg $ text "Done"

  pure (mempty, onClose <> done)

receiveToNonexistentAccount
  :: ( MonadWidget t m
     , HasNetwork model t
     , HasCrypto key (Performable m)
     , HasCrypto key m
     , HasWallet model key t
     , HasLogger model t
     , HasTransactionLogger m
     , TriggerEvent t m
     )
  => model
  -> NetworkName
  -> [Either Text NodeInfo]
  -> AccountName
  -> ChainId
  -> Maybe AccountDetails
  -> m (Dynamic t (Maybe UserKeyset, Maybe Pact.KeySetName))
receiveToNonexistentAccount model net nodes account chain mdetails = do
    case _accountDetails_guard <$> mdetails of
      Just (AccountGuard_Other g) -> case g of
        Pact.GKeySetRef ref -> do
          let chain' = case chain of -- in case the chain id is 0, make sure 00 becomes 0
                ChainId c -> case c of
                  "00" -> ChainId "0"
                  _ -> ChainId c

          eAccountMap <- lookupKeySets (model ^. logger) net (rights nodes) chain' [account]
          accDyn <- holdDyn Nothing eAccountMap
          pure $ ffor accDyn $ \case
            Nothing -> (Nothing, Nothing)
            Just m ->
              let ks = m ^?
                    at account
                    . _Just
                    . _AccountStatus_Exists
                    . accountDetails_guard
                    . _AccountGuard_KeySetLike
                    . to toPactKeyset
                    . to userFromPactKeyset

              in (ks, Just ref)
        _ -> pure $ constDyn (Nothing, Nothing)

      Just (AccountGuard_KeySetLike d) -> pure $ constDyn (Just $ userFromPactKeyset $ toPactKeyset d, Nothing)

      Nothing -> do
        let dynWalletKeys = Set.fromList . fmap (_keyPair_publicKey . _key_pair) . IntMap.elems <$>
              model ^. wallet_keys
        res <- dyn $ ffor dynWalletKeys $ \walletKeys -> do
          dialogSectionHeading mempty "Description"
          mks1 <- divClass "group" $ do
            mks1 <- case parsePublicKey $ unAccountName account of
              Left _ -> do
                para1
                pure Nothing
              Right pk -> do
                let ks = UserKeyset (Set.singleton pk) KeysAll
                if Set.member pk walletKeys
                  then pure $ Just ks
                  else do
                    el "p" $ text "NOTICE: This account name looks like a public key, but it is not one of Chainweaver's keys.  Make sure it is the key you want before continuing!"
                    pure $ Just ks

            para2
            return mks1

          case mks1 of
            Nothing -> do
              dialogSectionHeading mempty "Define Keyset"
              divClass "group" $ keysetFormWidget (mkCfg Nothing)
            Just ks -> pure (constDyn $ Just ks)
        fmap join $ holdDyn (constDyn (Nothing, Nothing)) (fmap (fmap (\a -> (a, Nothing))) res)
  where
    para1 = el "p" $ text $ T.unwords
      [ "Before you can receive coins, you must decide who owns this account."
      , "You do this by specifying a keyset."
      , "A keyset contains one or more public keys for everyone who owns the account"
      , "as well as a predicate that decides how many keys have to sign to send coins."
      , "After you add keys, a Tx Builder will appear."
      ]
    para2 = el "p" $ text $ T.unwords
      [ "To receive funds, copy the the Tx Builder."
      , "It contains the information necessary to send Kadena coins to this account."
      , "Paste it into the To field in Chainweaver's Transfer section"
      , "or send it someone else so they can use it to transfer."
      ]

uiReceiveModal0
  :: ( MonadWidget t m
     , Monoid mConf
     , HasNetwork model t
     , HasNetworkCfg mConf t
     , HasCrypto key (Performable m)
     , HasCrypto key m
     , HasLogger model t
     , HasTransactionLogger m
     )
  => model
  -> AccountName
  -> ChainId
  -> AccountDetails
  -> Event t ()
  -> Workflow t m (mConf, Event t ())
uiReceiveModal0 model account chain details onClose = Workflow $ do
  let
    netInfo = getNetworkInfoTriple $ model ^. network

  (showingAddr, chain, (conf, ttl, gaslimit, transferInfo)) <- divClass "modal__main receive" $ do
    rec
      showingTxBuilder <- toggle True $ onAddrClick <> onReceiClick

      dialogSectionHeading mempty "Destination"
      divClass "group" $ do
        transactionDisplayNetwork model
        displayText "Chain ID" (_chainId chain) mempty

      (onAddrClick, ((), ())) <- controlledAccordionItem showingTxBuilder mempty
        (accordionHeaderBtn "Option 1: Copy and share Tx Builder") $ do
          uiDisplayTxBuilderWithCopy True
            $ TxBuilder account chain
              (details ^? accountDetails_guard . _AccountGuard_KeySetLike . to toPactKeyset)
              (Pact.KeySetName <$> details ^? accountDetails_guard . _AccountGuard_KeySetLike . ksh_ref . _Just)

      (onReceiClick, results) <- controlledAccordionItem (not <$> showingTxBuilder) mempty
        (accordionHeaderBtn "Option 2: Transfer from non-Chainweaver Account") $ do
        legacyReceive <- uiReceiveFromLegacy model
        pure ( _receiveFromLegacy_conf legacyReceive
             , _receiveFromLegacy_ttl legacyReceive
             , _receiveFromLegacy_gasLimit legacyReceive
             , _receiveFromLegacy_transferInfo legacyReceive
             )

    pure (showingTxBuilder, chain, snd results)

  let needsSender = liftA2 (&&) (isNothing <$> transferInfo) (not <$> showingAddr)

  doneNext <- modalFooter $ uiButtonDyn
    (def
     & uiButtonCfg_class <>~ "button_type_confirm"
     & uiButtonCfg_disabled .~ needsSender
    )
    $ dynText (bool "Submit Transfer" "Close" <$> showingAddr)

  let
    done = gate (current showingAddr) doneNext
    deploy = gate (not <$> current showingAddr) doneNext

    submit = flip push deploy $ \() -> runMaybeT $ do
      t <- lift $ sample $ current ttl
      g <- lift $ sample $ current gaslimit
      ni <- MaybeT $ sample $ current netInfo
      ti <- MaybeT $ sample $ current transferInfo
      pure $ receiveFromLegacySubmitTransfer model onClose account chain t g ni ti

  pure ( (conf, onClose <> done)
       , submit
       )

displayText
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Text
  -> Text
  -> Text
  -> m (Event t Text)
displayText lbl v cls = mkLabeledInputView True lbl attrFn $ pure v
  where
    attrFn cfg = uiInputElement $ cfg
      & initialAttributes <>~ ("disabled" =: "true" <> "class" =: (" " <> cls))
