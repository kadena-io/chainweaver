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

import           Data.Aeson.Types                  (Value)
import           Data.Proxy
import           Control.Monad.Catch               (MonadThrow(..))
import qualified Data.Text                         as T
import qualified Pact.Analyze.Remote.Types as Analyze
import           Pact.Server.API                   (pactServerAPI)
import           Pact.Types.API
import           Pact.Types.Command
import           Reflex.Dom.Core hiding            (Value)
import           Servant.API
import qualified Servant.Client                    as SC
import           Servant.Reflex

getBaseUrl :: (MonadThrow m) => T.Text -> m BaseUrl
getBaseUrl backendUri = SC.parseBaseUrl $ T.unpack backendUri

makeSend :: forall t m. (MonadWidget t m)
  => BaseUrl
  -> Dynamic t (Either T.Text SubmitBatch)
  -> Event t ()
  -> m (Event t (ReqResult () (ApiResponse RequestKeys)))
makeSend baseUrl = let
  ((send :<|> _ :<|> _ :<|> _) :<|> _ :<|> _) =
        client pactServerAPI (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn baseUrl)
  in send

makePoll :: forall t m. (MonadWidget t m)
  => BaseUrl
  -> Dynamic t (Either T.Text Poll)
  -> Event t ()
  -> m (Event t (ReqResult () (ApiResponse PollResponses)))
makePoll baseUrl = let
  ((_ :<|> poll :<|> _ :<|> _) :<|> _ :<|> _) =
        client pactServerAPI (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn baseUrl)
  in poll

makeListen :: forall t m. (MonadWidget t m)
  => BaseUrl
  -> Dynamic t (Either T.Text ListenerRequest)
  -> Event t ()
  -> m (Event t (ReqResult () (ApiResponse ApiResult)))
makeListen baseUrl = let
  ((_ :<|> _ :<|> listen :<|> _) :<|> _ :<|> _) =
        client pactServerAPI (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn baseUrl)
  in listen

makeLocal :: forall t m. (MonadWidget t m)
  => BaseUrl
  -> Dynamic t (Either T.Text (Command T.Text))
  -> Event t ()
  -> m (Event t (ReqResult () (ApiResponse (CommandSuccess Value))))
makeLocal baseUrl = let
  ((_ :<|> _ :<|> _ :<|> local) :<|> _ :<|> _) =
        client pactServerAPI (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn baseUrl)
  in local

makeVerify :: forall t m. (MonadWidget t m)
  => BaseUrl
  -> Dynamic t (Either T.Text Analyze.Request)
  -> Event t ()
  -> m (Event t (ReqResult () Analyze.Response))
makeVerify baseUrl = let
  ((_ :<|> _ :<|> _ :<|> _) :<|> verify :<|> _) =
        client pactServerAPI (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn baseUrl)
  in verify

makeVersion :: forall t m. (MonadWidget t m)
  => BaseUrl
  -> Event t ()
  -> m (Event t (ReqResult () T.Text))
makeVersion baseUrl = let
  ((_ :<|> _ :<|> _ :<|> _) :<|> _ :<|> version) =
        client pactServerAPI (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn baseUrl)
  in version
