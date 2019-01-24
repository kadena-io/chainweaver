{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Frontend.Backend.Client where

import           Control.Monad.Catch               (MonadThrow(..))
import           Pact.Server.API                   (pactServerAPI)
import           Pact.Types.API
import           Reflex.Dom.Core                   (MonadWidget)
import           Servant
import qualified Servant.Client                    as SC
import           Servant.Reflex

getBaseUrl :: (MonadThrow m) => BackendUri -> m BaseUrl
getBaseUrl backendUri = SC.parseBaseUrl $ T.unpack backendUri

makeSend :: forall t m. (MonadWidget t m)
  => BaseUrl
  -> Dynamic t (Either Text SubmitBatch)
  -> Event t ()
  -> m (Event t (ReqResult () (ApiResponse RequestKeys)))
makeSend baseUrl = let
  ((send :<|> _ :<|> _ :<|> _) :<|> _ :<|> _) =
        client pactServerAPI (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn baseUrl)
  in send

makePoll baseUrl = let
  ((_ :<|> poll :<|> _ :<|> _) :<|> _ :<|> _) =
        client pactServerAPI (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn baseUrl)
  in poll

makeListen baseUrl = let
  ((_ :<|> _ :<|> listen :<|> _) :<|> _ :<|> _) =
        client pactServerAPI (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn baseUrl)
  in listen

makeLocal baseUrl = let
  ((_ :<|> _ :<|> _ :<|> local) :<|> _ :<|> _) =
        client pactServerAPI (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn baseUrl)
  in local

makeVerify baseUrl = let
  ((_ :<|> _ :<|> _ :<|> _) :<|> verify :<|> _) =
        client pactServerAPI (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn baseUrl)
  in verify

makeVersion baseUrl = let
  ((_ :<|> _ :<|> _ :<|> _) :<|> _ :<|> version) =
        client pactServerAPI (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn baseUrl)
  in version
