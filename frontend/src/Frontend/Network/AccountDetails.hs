{-|

Copyright   :  (C) 2020 Kadena
License     :  BSD-style (see the file LICENSE)

-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend.Network.AccountDetails where

import           Control.Lens hiding ((.=))
import           Control.Monad.State.Strict
import           Data.Map (Map)
import qualified Data.Map as Map

import           Kadena.SigningApi (AccountName(..))
import qualified Pact.Server.ApiClient as Api
import           Pact.Types.ChainId
import qualified Pact.Types.Command as Pact
import           Pact.Types.Pretty
import           Reflex
import           Reflex.Dom.Core

import           Common.Foundation
import           Common.Wallet
import           Frontend.Crypto.Class
import           Frontend.Foundation
import           Frontend.Log
import           Frontend.Network
import           Frontend.PactQueries
import           Frontend.UI.Dialogs.Send

-- Might need to move this into a different Types module later
data ChainAccount = ChainAccount
  { _ca_chain :: ChainId
  , _ca_account :: AccountName
  } deriving (Eq,Ord,Show)

getAccountDetails
  :: ( Reflex t, TriggerEvent t m, MonadJSM m
     , HasNetwork model t
     , MonadHold t m
     , Adjustable t m
     , HasLogger model t
     , HasCrypto key m
     )
  => model
  -> Event t ChainAccount
  -> m (Event t (Maybe (AccountStatus AccountDetails)))
getAccountDetails model eca = do
    let netAndCa = flip push eca $ \ca -> do
          ni <- sampleNetInfo model
          return $ (ca,) <$> ni
    dd <- networkHold (pure never) (go <$> netAndCa)
    pure $ switch $ current dd
  where
    go (ca, netInfo) = do
      let nodes = _sharedNetInfo_nodes netInfo
          chain = _ca_chain ca
          acct = _ca_account ca
          extractDetails Nothing = Nothing
          extractDetails (Just m) = Map.lookup acct m
      ks <- lookupKeySets (model ^. logger) (_sharedNetInfo_network netInfo)
                   nodes chain [acct]
      pure $ extractDetails <$> ks

-- | Lookup the keyset of some accounts
lookupKeySets
  :: ( TriggerEvent t m, MonadJSM m
     , HasCrypto key m
     )
  => Logger t
  -> NetworkName
  -- ^ Which network we are on
  -> [NodeInfo]
  -- ^ Envs which point to the appropriate chain
  -> ChainId
  -> [AccountName]
  -- ^ Account on said chain to find
  -> m (Event t (Maybe (Map AccountName (AccountStatus AccountDetails))))
lookupKeySets logL networkName nodes chain accounts = do
  let code = renderCompactText $ accountDetailsObject (map unAccountName accounts)
  pm <- mkPublicMeta chain
  cmd <- buildCmd Nothing networkName pm [] [] code mempty mempty
  (result, trigger) <- newTriggerEvent
  let envs = mkClientEnvs nodes chain
  liftJSM $ forkJSM $ do
    r <- doReqFailover envs (Api.local Api.apiV1Client cmd) >>= \case
      Left _ -> pure Nothing
      Right cr -> case Pact._crResult cr of
        Pact.PactResult (Right pv) -> case parseAccountDetails pv of
          Left _ -> pure Nothing
          Right balances -> liftIO $ do
            putLog logL LevelInfo $ "lookupKeysets: success:"
            putLog logL LevelInfo $ tshow balances
            pure $ Just balances
        Pact.PactResult (Left e) -> do
          putLog logL LevelInfo $ "lookupKeysets failed:" <> tshow e
          pure Nothing

    liftIO $ trigger r
  pure result

