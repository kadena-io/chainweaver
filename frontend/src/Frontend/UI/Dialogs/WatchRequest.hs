{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Dialog for watching request keys
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.WatchRequest
  ( uiWatchRequestDialog
  ) where

import Control.Applicative (liftA2)
import Control.Lens hiding (failover)
import Control.Monad (void)
import Data.Either (rights)
import Reflex.Dom
import Safe (headMay)
import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Util as Pact

import Frontend.Foundation hiding (Arg)
import Frontend.Network
import Frontend.UI.Dialogs.DeployConfirmation (Status(..), listenToRequestKey, transactionStatusSection, transactionResultSection)
import Frontend.UI.Modal
import Frontend.UI.Widgets
import Frontend.UI.Widgets.Helpers (dialogSectionHeading)

-- | A modal for watching request keys
uiWatchRequestDialog
  :: ( Flattenable mConf t
     , Monoid mConf
     , HasNetwork model t
     , MonadWidget t m
     )
  => model -> Event t () -> m (mConf, Event t ())
uiWatchRequestDialog model onCloseExternal = do
  (conf, closes) <- fmap splitDynPure $ workflow $ inputRequestKey model onCloseExternal
  mConf <- flatten =<< tagOnPostBuild conf
  let close = switch $ current closes
  pure (mConf, close)

-- | Allow the user to input a request key and chain
inputRequestKey
  :: ( Monoid mConf
     , MonadWidget t m
     , HasNetwork model t
     )
  => model
  -> Event t () -- ^ Modal was externally closed
  -> Workflow t m (mConf, Event t ())
inputRequestKey model onCloseExternal = Workflow $ mdo
  let nodes = fmap rights $ model ^. network_selectedNodes
  close <- modalHeader $ text "Watch Request Key"
  choice <- modalMain $ do
    dialogSectionHeading mempty "Notice"
    divClass "group" $ do
      text "If you have a request key from a previously submitted transaction, you can use this dialog to wait for and display the results."
    dialogSectionHeading mempty "Required Information"
    info <- divClass "group" $ do
      mChain <- fmap value $ mkLabeledClsInput False "Chain ID" $
        uiChainSelection (headMay <$> nodes) (pure Nothing)
      eKey <- mkLabeledInput False "Request Key" uiInputElement def
      let parseRequestKey t
            | Right v <- Pact.fromText' t = Right v
            | otherwise = Left "Invalid hash"
      pure $ liftA2 (,) (current mChain) (fmap parseRequestKey . current $ value eKey)
    void $ runWithReplace blank $ ffor err $ \e -> do
      dialogSectionHeading mempty "Error"
      divClass "group" $ text e
    pure info
  (cancel, next) <- modalFooter $ do
    cancel' <- cancelButton def "Cancel"
    next' <- confirmButton def "Next"
    pure (cancel', next')
  let f (Just c, Right k) = Right (c, k)
      f (_, Left e) = Left e
      f (Nothing, _) = Left "You must select a chain"
      (err, ok) = fanEither $ attachWith (const . f) choice next
      nextScreen = attachWith (watchRequestKey onCloseExternal) (current nodes) ok
  pure ((mempty, close <> cancel), nextScreen)

watchRequestKey
  :: (Monoid mConf, MonadWidget t m)
  => Event t () -- ^ Modal was externally closed
  -> [NodeInfo] -- ^ Nodes to try
  -> (ChainId, Pact.RequestKey) -- ^ User selected chain and request key
  -> Workflow t m (mConf, Event t ())
watchRequestKey onCloseExternal nodes (chain, reqKey) = Workflow $ mdo -- can't use 'rec' due to GHC bug
  close <- modalHeader $ text "Transaction Status"
  pb <- getPostBuild
  let clientEnvs = mkClientEnvs nodes chain
  (listenStatus, message, _) <- listenToRequestKey clientEnvs $ leftmost
    [ Just reqKey <$ (pb <> reloadKey)
    , Nothing <$ (closeModal <> onCloseExternal) -- Cancel any inflight request when the modal is closed
    ]
  reloadKey <- elClass "div" "modal__main transaction_details" $ do
    mkLabeledInput True "Transaction Hash" uiInputElement $ def
      & initialAttributes .~ "disabled" =: "disabled"
      & inputElementConfig_initialValue .~ Pact.requestKeyToB16Text reqKey
    reload <- transactionStatusSection (pure Status_Done) listenStatus
    transactionResultSection message
    pure reload
  done <- modalFooter $ do
    confirmButton def "Done"
  let closeModal = close <> done
  pure ((mempty, close <> done), never)
