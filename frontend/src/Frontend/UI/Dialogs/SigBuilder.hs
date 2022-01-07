{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Frontend.UI.Dialogs.SigBuilder where

import Reflex
import Reflex.Dom.Core


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
import qualified Data.IntMap as IntMap
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
import           Frontend.Crypto.Class
import           Frontend.Foundation
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
     )
  => ModalIde m key t
  -> Event t ()
  -> m (ModalIdeCfg m key t)
sigBuilderCfg m evt = do
  pure $ mempty & modalCfg_setModal .~ (Just (uiSigBuilderDialog m) <$ evt)

uiSigBuilderDialog
  :: ( MonadWidget t m, Monoid mConf
     )
  => ModalIde m key t
  -> Event t ()
  -> m (mConf, Event t ())
uiSigBuilderDialog model _onCloseExternal = do
  dCloses <- workflow $ txnInputDialog model
  pure (mempty, switch $ current dCloses)

txnInputDialog
  :: 
  ( MonadWidget t m
  )
  => ModalIde m key t
  -> Workflow t m (Event t ())
txnInputDialog model = Workflow $ mdo
  onClose <- modalHeader $ text "Signature Builder"
  sigDataE <- modalMain $
    divClass "group" $ do
      parseInputToSigDataWidget

  mSigData <- holdDyn Nothing $ Just <$> sigDataE
  (onCancel, approve) <- modalFooter $ (,)
    <$> cancelButton def "Cancel"
    -- <*> confirmButton def "Approve"
    <*> confirmButton (def & uiButtonCfg_disabled .~ ( isNothing <$> mSigData )) "Approve"
  let approveE = fmapMaybe id $ tag (current mSigData) approve
  return (onCancel <> onClose, approveSigDialog <$> approveE)

approveSigDialog 
  :: 
  ( MonadWidget t m
  )
  => (SigData Text, Payload PublicMeta Text)
  -> Workflow t m (Event t ())
approveSigDialog (sigData, payload) = Workflow $ do
  onClose <- modalHeader $ text "Approve Transaction"
  modalMain $
    divClass "group" $ text "hi"
  _ <- modalFooter $ cancelButton def "Back"
  return (onClose, never) -- leftmost [ _ <$ back
  


parseInputToSigDataWidget :: MonadWidget t m => m (Event t (SigData Text, Payload PublicMeta Text))
parseInputToSigDataWidget =
  divClass "group" $ do
    txt <- fmap value $ mkLabeledClsInput False "Paste UnSigned Transaction" $ \cls -> uiTextAreaElement $ def
      & initialAttributes .~ "class" =: renderClass cls
    let
      parseBytes "" = Nothing
      parseBytes bytes =
        -- Parse the JSON, and consume all the input
        case parseOnly jsonEOF' bytes of
            -- If we do receive JSON, it can be of two types: either a SigData Text,
            -- or a Payload PublicMeta Text
          Right val -> case A.fromJSON @(SigData Text) val of
            A.Success sigData -> Just $ Right sigData
            A.Error errStr -> Just $ Left errStr
               --TODO: Add payload case later
              -- case A.fromJSON val of
              -- A.Success payload -> Just $ Right payload
              -- A.Error errorStr -> ErrorString errorStr
          -- We did not receive JSON, try parsing it as YAML
          Left _ -> case Y.decode1Strict bytes of
            Right sigData -> Just $ Right sigData
            Left (pos, errorStr) -> Just $ Left $ prettyPosWithSource pos (LB.fromStrict bytes) errorStr

      signingDataEv = fmap attachPayload . parseBytes . T.encodeUtf8 <$> updated txt

      -- Convert a functor value into a pair with a given header
      withHeader h t = fmap ((,) h) t

      -- Parse payload from given text
      parsePayload :: Text -> Either String (Payload PublicMeta Text)
      parsePayload cmdText =
        first (const "Invalid cmd field inside SigData.") $
          A.eitherDecodeStrict (T.encodeUtf8 cmdText)

      attachPayload sdOrErr = do
        sd <- sdOrErr
        t <- justErr "Payload missing" $ _sigDataCmd sd
        p <- parsePayload t
        pure (sd, p)

    pure $ snd $ fanEither $ fmapMaybe id signingDataEv

    -- dyn_ $ ffor eitherHashDyn $ \case
    --   Left _ -> blank
    --   Right ev -> uiDetailsCopyButton $ current ev
