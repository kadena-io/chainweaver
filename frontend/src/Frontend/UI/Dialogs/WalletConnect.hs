{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Frontend.UI.Dialogs.WalletConnect where

import Control.Monad (join, void, forM_, forM, unless)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime.Compat
import Data.Time.Format.ISO8601.Compat
import Reflex.Dom hiding (Request)
import Language.Javascript.JSaddle (valToJSON, liftJSM, JSM, fun, jsg, js0)
import Data.ByteString.Lazy (toStrict, fromStrict)
import qualified Data.ByteString.Lazy as LB (ByteString)
import Data.Aeson (encode, decode)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import WalletConnect.Wallet hiding (PublicKey)
import Frontend.AppCfg
import Frontend.Foundation hiding (Request)
import Frontend.UI.Form.Common
import Frontend.UI.Modal
import Frontend.UI.Widgets
import Frontend.UI.Widgets.Helpers (dialogSectionHeading)
import Common.Network
import Common.Wallet

uiWalletConnect
  :: ( MonadWidget t m
     , Monoid mConf
     )
  => WalletConnect t
  -> Event t ()
  -> m (mConf, Event t ())
uiWalletConnect wc@(WalletConnect pairings sessions _ _ _) _ = do
  onClose <- modalHeader $ text "Wallet Connect"

  modalMain $ uiSegment mempty $ do
    uiGroupHeader mempty $
      dialogSectionHeading mempty "New Pairing"

    uiGroupHeader mempty $ do
      divClass "group" $ do
        ie <- mkLabeledClsInput False "Please paste the URI (beginning with 'wc:') and click Pair" $ \cls -> uiTextAreaElement $ def
          & initialAttributes .~ "class" =: renderClass cls
        ev <- uiButton btnCfgPrimary $ text "Pair"
        doNewPairing wc (tag (current $ value ie) ev)

    uiGroupHeader mempty $
      dialogSectionHeading mempty "Active Sessions"

    dyn $ ffor sessions $ \ss -> do
      forM_ ss $ \session -> uiGroup "segment" $ do
        let m = snd $ _session_peer session
        showMetaData m
        ev <- uiButton btnCfgTertiary $ text "Disconnect"
        performEvent $ ffor ev $ \_ -> liftJSM $ _session_disconnect session
    pure ()

  pure (mempty, onClose)
  where

uiWalletConnectSessionProposal
  :: ( MonadWidget t m
     , Monoid mConf
     )
  => ([PublicKey], (NetworkName, Proposal))
  -> Event t ()
  -> m (mConf, Event t ())
uiWalletConnectSessionProposal (keys, (networkName, Proposal _ ttl (_, meta) _ respond)) _ = do
  onClose <- modalHeader $ text "Wallet Connect Session"

  doneEv <- modalMain $ uiSegment mempty $ do
    uiGroupHeader mempty $
      dialogSectionHeading mempty "Please select keys for this session"

    dSelAccs <- uiGroup "segment" $ do
      sel <- forM keys $ \k -> do
        cb <- uiCheckbox mempty False def (text $ keyToText k)
        pure $ ffor (value cb) $ \case
          True -> Just (keyToText k)
          _ -> Nothing
      pure $ catMaybes <$> sequence sel


    -- Dapp info
    uiGroup "segment" $ do
      dialogSectionHeading mempty "dApp metadata"
      showMetaData meta

      el "p" $ do
        text "Duration: "
        text $ T.pack $ formatShow (alternativeDurationTimeFormat ExtendedFormat) (calendarTimeTime $ fromIntegral ttl)

      let
        cfg = btnCfgPrimary
          & uiButtonCfg_disabled .~ fmap null dSelAccs
      approve <- uiButtonDyn cfg $ text "Approve"
      reject <- uiButton btnCfgTertiary $ text "Reject"
      let
        toAccount a = walletConnectChainId networkName <> ":" <> a
        approveEv = fforMaybe (tag (current dSelAccs) approve) $ \case
          [] -> Nothing
          vs -> Just $ Right $ map toAccount vs
        rEv = leftmost [ Left () <$ reject, approveEv ]

      performEvent $ ffor rEv $ \v -> liftJSM $ respond v

  pure (mempty, leftmost [onClose, doneEv])

uiWalletConnectSessionPermissionError
  :: ( MonadWidget t m
     , Monoid mConf
     )
  => ([Method], [Chain], NetworkName, Proposal)
  -> Event t ()
  -> m (mConf, Event t ())
uiWalletConnectSessionPermissionError (methods, chains, networkName, Proposal _ _ (_, meta) _ respond) _ = do
  onClose <- modalHeader $ text "Wallet Connect Session"

  doneEv <- modalMain $ uiSegment mempty $ do
    uiGroupHeader mempty $
      dialogSectionHeading mempty "There is an error in the incoming session proposal request"

    uiGroup "segment" $ do
      dialogSectionHeading mempty "dApp metadata"
      showMetaData meta

    unless (null methods) $ uiGroup "segment" $ do
      text $ "These methods are not supported : " <>
        (T.intercalate ", " methods)

    unless (null chains) $ uiGroup "segment" $ do
      text $ "These chains are not supported : " <>
        (T.intercalate ", " chains)

    uiButton btnCfgTertiary $ text "Close"
  pure (mempty, leftmost [onClose, doneEv])

uiWalletConnectSigReqError
  :: ( MonadWidget t m
     , Monoid mConf
     )
  => (Maybe Metadata, String, Request)
  -> Event t ()
  -> m (mConf, Event t ())
uiWalletConnectSigReqError (mMeta, err, Request _ method params) _ = do
  onClose <- modalHeader $ text "Wallet Connect Session"

  doneEv <- modalMain $ uiSegment mempty $ do
    uiGroupHeader mempty $
      dialogSectionHeading mempty "There was an error in the incoming signing request request"

    forM mMeta $ \meta -> uiGroup "segment" $ do
      dialogSectionHeading mempty "dApp metadata"
      showMetaData meta

    uiGroup "segment" $ do
      text $ "Error : " <> T.pack err

    uiGroup "segment" $ do
      el "p" $ text $ "Method : " <> method
      el "p" $ text $ "Params : "
      el "p" $ text (decodeUtf8 . toStrict $ encode params)
    uiButton btnCfgTertiary $ text "Close"
  pure (mempty, leftmost [onClose, doneEv])

showMetaData m = do
  uiGroupHeader mempty $ text $ _metadata_name m
  el "p" $ text $ _metadata_url m
  el "p" $ text $ _metadata_description m

walletConnectChainId :: NetworkName -> Text
walletConnectChainId network
  | n == "mainnet" = "kadena:mainnet01"
  | n == "testnet" = "kadena:testnet04"
  | otherwise = "kadena:testnet04" -- TODO: FIXME: only for testing
  where n = textNetworkName network

