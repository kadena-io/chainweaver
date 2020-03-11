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

import Debug.Trace
import Control.Lens hiding (failover)
import Control.Error (hush)
import Control.Monad (forM)

import Data.List.NonEmpty (NonEmpty ((:|)))

import Data.Either (rights)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM

import Reflex.Dom

import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Util as Pact
import qualified Pact.Types.API as Pact
import qualified Pact.Types.Pretty as Pact

import qualified Text.URI as URI
import qualified Servant.Client.JSaddle as S
import qualified Pact.Server.ApiClient as Api

import Frontend.Foundation hiding (Arg)
import Frontend.Network
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

uiRequestKeyInput
  :: ( MonadWidget t m
     )
  => Bool
  -> m ( Event t (Maybe Pact.RequestKey)
       , Dynamic t (Maybe Pact.RequestKey)
       )
uiRequestKeyInput inlineLabel = do
  let
    parseRequestKey :: Text -> Either Text Pact.RequestKey
    parseRequestKey t | Text.null t = Left "Please enter a Request Key"
                      | Right v <- Pact.fromText' t = Right v
                      | otherwise = Left "Invalid hash"

    mkMsg True (Left e) = PopoverState_Error e
    mkMsg _    _ = PopoverState_Disabled

    showPopover (ie, _) = pure $ _inputElement_input ie <&> \t ->
      mkMsg (not $ Text.null t) (parseRequestKey $ Text.strip t)

    uiKeyInput cfg = do
      inp <- uiInputElement cfg
      pure (inp, _inputElement_raw inp)

  (inputE, _) <- mkLabeledInput inlineLabel "Request Key" (uiInputWithPopover uiKeyInput snd showPopover) def

  pure ( hush . parseRequestKey <$> _inputElement_input inputE
       , hush . parseRequestKey <$> value inputE
       )


-- | Allow the user to input a request key and chain
inputRequestKey
  :: ( Monoid mConf
     , MonadWidget t m
     , HasNetwork model t
     )
  => model
  -> Event t () -- ^ Modal was externally closed
  -> Workflow t m (mConf, Event t ())
inputRequestKey model _ = Workflow $ do
  let
    nodes = fmap rights $ model ^. network_selectedNodes

  close <- modalHeader $ text "Watch Request Key"

  modalMain $ do
    dialogSectionHeading mempty "Notice"
    divClass "group" $ do
      text "If you have a request key from a previously submitted transaction, you can use this dialog to wait for and display the results."

    dialogSectionHeading mempty "Required Information"
    dRqKey <- divClass "group" $ fmap snd $ uiRequestKeyInput False

    rec
      let lockRefresh = (||)
            <$> fmap isNothing dRqKey
            <*> blockSpam

      onRequest <- divClass "group" $
        confirmButton (def & uiButtonCfg_disabled .~ lockRefresh) "Refresh Status"

      onTimeoutFinished <- delay 5 onRequest

      blockSpam <- toggle False $ leftmost
        [ onRequest
        , onTimeoutFinished
        ]

    let
      showPollResponse (Pact.PollResponses pollMap) = case HM.elems pollMap ^? _head of
        Nothing -> "Request Key not found"
        Just commandResult -> case commandResult ^. Pact.crResult of
          Pact.PactResult (Left err) -> "Error: " <> Pact.tShow err
          Pact.PactResult (Right a) -> Pact.renderCompactText a

      onPollRequestKey = tagMaybe (current dRqKey) onRequest

      collectResults nodes0 rKey cb = do
        nodeChainMap <- buildNodeChainClientEnvs nodes0
        let allenvs = foldMap (fmap snd) $ Map.elems nodeChainMap
        pollRequestKey allenvs (traceShowId rKey) cb

    onPollResponse <- performEventAsync $ collectResults
      <$> current nodes
      <@> onPollRequestKey

    dResp <- holdDyn Nothing $ fmap Just onPollResponse

    dialogSectionHeading mempty "Transaction Result"
    divClass "group" $ do
      maybeDyn dResp >>= \md -> dyn_ $ ffor md $ \case
        Nothing -> text "Please enter a valid Request Key and click 'Refresh Status'"
        Just a -> dynText $ either (Text.unlines . fmap prettyPrintNetworkError) (showPollResponse) <$> a

  done <- modalFooter $ do
    confirmButton def "Done"

  pure ( (mempty, done <> close)
       , never
       )

buildNodeChainClientEnvs :: MonadJSM m => [NodeInfo] -> m (Map NodeInfo [(ChainId, S.ClientEnv)])
buildNodeChainClientEnvs nodes = fmap Map.fromList $ forM nodes $ \node -> let chains = getChains node in
  fmap (node,) $ flip foldMapM chains $ \chain ->
    getChainRefBaseUrl (ChainRef (Just $ nodeInfoRef node) chain) (Just node) <&> \case
      Left _ -> []
      Right chainUrl -> S.parseBaseUrl (URI.renderStr chainUrl) & \case
        Nothing -> []
        Just baseUrl -> [(chain, S.mkClientEnv baseUrl)]

pollRequestKey
  :: MonadJSM m
  => [S.ClientEnv]
  -> Pact.RequestKey
  -> (Either [NetworkError] Pact.PollResponses -> IO ())
  -> m ()
pollRequestKey clientEnvs rKey cb = do
  doReqFailover clientEnvs (Api.poll Api.apiV1Client $ Pact.Poll (rKey :| []))
  >>= liftIO . cb . over _Left (fmap packHttpErr)
