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

module Frontend.PactQueries where

import           Data.Text (Text)
import           Kadena.SigningTypes
import           Pact.Types.ChainMeta
import qualified Data.Text as T
import           Pact.Types.Command

import           Frontend.Crypto.Class (HasCrypto)
import           Frontend.Foundation hiding (Arg)
import           Frontend.Network

mkPublicMeta :: MonadIO m => ChainId -> m PublicMeta
mkPublicMeta cid = do
  now <- getCreationTime
  return $ PublicMeta
    { _pmChainId = cid
    , _pmSender = "chainweaver"
    , _pmTTL = 600
    , _pmCreationTime = now
    , _pmGasLimit = defaultTransactionGasLimit
    , _pmGasPrice = defaultTransactionGasPrice
    }

mkCoinDetailsCmd
  :: ( MonadJSM m
     , HasCrypto key m
     )
  => NetworkName
  -- ^ Which network we are on
  -> ChainId
  -> AccountName
  -- ^ Account on said chain to find
  -> m (Command Text)
mkCoinDetailsCmd networkName chainId accountName = do
  let code = T.unwords
        [ "(coin.details"
        , tshow $ unAccountName accountName
        , ")"
        ]
  pm <- mkPublicMeta chainId
  buildCmd Nothing networkName pm [] [] code mempty mempty
