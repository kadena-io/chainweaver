{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Frontend.UI.Dialogs.SigBuilder where

import           Control.Error
import           Control.Lens
import           Control.Monad (join)
import qualified Data.Aeson as A
import           Data.Aeson.Parser.Internal (jsonEOF')
import           Data.Attoparsec.ByteString
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LB
import           Data.Functor (void)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.IntMap as IMap
import           Data.YAML
import qualified Data.YAML.Aeson as Y
import           Pact.Types.ChainMeta (PublicMeta)
import           Pact.Types.Command
import           Pact.Types.Hash (hash, toUntypedHash, unHash)
import           Pact.Types.SigData
import           Pact.Types.Util             (decodeBase64UrlUnpadded)
------------------------------------------------------------------------------
import           Reflex
import           Reflex.Dom hiding (Key)
------------------------------------------------------------------------------
import           Common.Wallet
import           Frontend.Crypto.Class
import           Frontend.Foundation
import           Frontend.Network
import           Frontend.UI.Modal
import           Frontend.UI.Transfer
import           Frontend.UI.Widgets
import           Frontend.UI.Widgets.Helpers (dialogSectionHeading)
import           Frontend.Wallet
------------------------------------------------------------------------------
import Frontend.UI.Modal.Impl
------------------------------------------------------------------------------

sigBuilderCfg
  :: forall t m key mConf
   . ( MonadWidget t m
     , HasCrypto key m
     )
  => ModalIde m key t
  -> Event t ()
  -> m (ModalIdeCfg m key t)
sigBuilderCfg m evt = do
  pure $ mempty & modalCfg_setModal .~ (Just (uiSigBuilderDialog m) <$ evt)

uiSigBuilderDialog
  ::
  ( MonadWidget t m
  , Monoid mConf
  , HasNetwork model t
  , HasWallet model key t
  , HasCrypto key m
  , ModalIde m key t ~ model
  )
  => ModalIde m key t
  -> Event t ()
  -> m (mConf, Event t ())
uiSigBuilderDialog model _onCloseExternal = do
  dCloses <- workflow $ txnInputDialog model Nothing
  pure (mempty, switch $ current dCloses)

type SigBuilderWorkflow t m model key = 
  ( MonadWidget t m
  , HasNetwork model t
  , HasWallet model key t
  , HasCrypto key m
  , ModalIde m key t ~ model
  )
txnInputDialog
  ::
  ( MonadWidget t m
  , HasNetwork model t
  , HasWallet model key t
  , HasCrypto key m
  , ModalIde m key t ~ model
  )
  => ModalIde m key t
  -> Maybe Text
  -> Workflow t m (Event t ())
txnInputDialog model mInitVal = Workflow $ mdo
  onClose <- modalHeader $ text "Signature Builder"
  let cwKeys = IMap.elems <$> (model^.wallet_keys)
      selNodes = model ^. network_selectedNodes
      networkId = (fmap (mkNetworkName . nodeVersion) . headMay . rights) <$> selNodes
      keysAndNet = current $ (,) <$> cwKeys <*> networkId

  dmSigData <- modalMain $
    divClass "group" $ do
      parseInputToSigDataWidget mInitVal

  (onCancel, approve) <- modalFooter $ (,)
    <$> cancelButton def "Cancel"
    <*> confirmButton (def & uiButtonCfg_disabled .~ ( isNothing <$> dmSigData )) "Approve"

  let approveE = fmapMaybe id $ tag (current dmSigData) approve
      sbr = attachWith (\(keys, net) (sd, pl) -> SigBuilderRequest sd pl net keys) keysAndNet approveE
  return (onCancel <> onClose, approveSigDialog model <$> sbr)

approveSigDialog
  :: SigBuilderWorkflow t m model key
  => ModalIde m key t
  -> SigBuilderRequest key
  -> Workflow t m (Event t ())
approveSigDialog model sbr = Workflow $ do
  let sigDataText = T.decodeUtf8 $ LB.toStrict $ A.encode $ _sbr_sigData sbr
  onClose <- modalHeader $ text "Approve Transaction"
  modalMain $
    divClass "group" $ displaySBR sbr
  back <- modalFooter $ confirmButton def "Back"
  let workflowEvent = leftmost
        [ txnInputDialog model (Just sigDataText) <$ back
        ]
  return (onClose, workflowEvent)

data SigBuilderRequest key = SigBuilderRequest
  { _sbr_sigData        :: SigData Text
  , _sbr_payload        :: Payload PublicMeta Text
  , _sbr_currentNetwork :: Maybe NetworkName
  , _sbr_cwKeys         :: [ Key key ]
  }

-- TEMP
displaySBR :: (MonadWidget t m) => SigBuilderRequest key -> m ()
displaySBR (SigBuilderRequest sd p net keys) = do
  text $ tshow net
  text $ tshow $ ffor keys $ \k -> _keyPair_publicKey $ _key_pair k
  text $ tshow sd
  text $ tshow p

data DataToBeSigned
  = DTB_SigData (SigData T.Text, Payload PublicMeta Text)
  | DTB_ErrorString String
  | DTB_EmptyString

parseInputToSigDataWidget :: MonadWidget t m
  => Maybe Text
  -> m (Dynamic t (Maybe (SigData Text, Payload PublicMeta Text)))
parseInputToSigDataWidget mInitVal =
  divClass "group" $ do
    txt <- fmap value $ mkLabeledClsInput False "Paste Transaction" $ \cls -> uiTextAreaElement $ def
      & initialAttributes .~ "class" =: renderClass cls
      & textAreaElementConfig_initialValue .~ (fromMaybe "" mInitVal)
    let dmSigningData = validInput . parseBytes . T.encodeUtf8 <$> txt
    pure dmSigningData
  where
    parsePayload :: Text -> Either String (Payload PublicMeta Text)
    parsePayload cmdText =
      first (const "Invalid cmd field inside SigData.") $
        A.eitherDecodeStrict (T.encodeUtf8 cmdText)

    parseAndAttachPayload sd =
      case parsePayload =<< (justErr "Payload missing" $ _sigDataCmd sd) of
        Left e -> DTB_ErrorString e
        Right p -> DTB_SigData (sd, p)

    parseBytes "" = DTB_EmptyString
    parseBytes bytes =
      -- Parse the JSON, and consume all the input
      case parseOnly jsonEOF' bytes of
        Right val -> case A.fromJSON val of
          A.Success sigData -> parseAndAttachPayload sigData
          A.Error errStr -> DTB_ErrorString errStr
             --TODO: Add payload case later
            -- case A.fromJSON val of
            -- A.Success payload -> Just $ Right payload
            -- A.Error errorStr -> ErrorString errorStr
        -- We did not receive JSON, try parsing it as YAML
        Left _ -> case Y.decode1Strict bytes of
          Right sigData -> parseAndAttachPayload sigData
          Left (pos, errorStr) -> DTB_ErrorString $ prettyPosWithSource pos (LB.fromStrict bytes) errorStr


    validInput DTB_EmptyString = Nothing
    validInput (DTB_ErrorString _) = Nothing
    validInput (DTB_SigData info) = Just info


    -- dyn_ $ ffor eitherHashDyn $ \case
    --   Left _ -> blank
    --   Right ev -> uiDetailsCopyButton $ current ev
