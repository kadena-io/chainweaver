{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE ExtendedDefaultRules   #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Pact example files.
--
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.ModuleExplorer.Example
  ( -- * Example References
    ExampleRef
  , _ExampleRef_HelloWorld
  , _ExampleRef_SimplePayments
  , _ExampleRef_InternationalPayments
    -- * Available examples
  , examples
    -- * Get some information about examples
  , exampleName
  , exampleFileName
  , exampleDataName
   -- * Retrieve example code
  , fetchExample
  ) where

------------------------------------------------------------------------------
import qualified Bound
import Control.Monad.Except (throwError)
import Control.Monad (void, (<=<))
import           Control.Arrow               ((***), left)
import           Data.Bifunctor (second)
import           Control.Lens
import           Data.Aeson                  as Aeson (Result (..), fromJSON, FromJSON, Value)
import           Data.Default
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Reflex
import           Reflex.Dom.Core             (HasJSContext, MonadHold,
                                              PostBuild, XhrResponse (..),
                                              newXMLHttpRequest, xhrRequest)
------------------------------------------------------------------------------
import qualified Pact.Compile                as Pact
import qualified Pact.Parse                  as Pact
import           Pact.Types.Lang
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Editor
import           Frontend.Foundation
import           Frontend.JsonData
import           Frontend.Messages
import           Frontend.ModuleExplorer     as API
import           Frontend.Repl
import           Frontend.Wallet



-- | A reference to one of the predefined example files.
data ExampleRef
  = ExampleRef_HelloWorld
  | ExampleRef_SimplePayments
  | ExampleRef_InternationalPayments
  deriving (Eq, Ord, Show, Enum, Bounded)

makePactPrisms ''ExampleRef

-- | List of all available examples.
examples :: [ExampleRef]
examples = [minBound .. maxBound]


-- | Names of examples as shown to the user.
exampleName :: ExampleRef -> Text
exampleName = \case
  ExampleRef_HelloWorld
    -> "Hello World"
  ExampleRef_SimplePayments
    -> "Simple Payment"
ExampleRef_InternationalPayments
  -> "International Payment"

-- | File name of Pact code for the given example.
exampleFileName :: ExampleRef -> Text
exampleFileName = \case
  ExampleRef_HelloWorld
    -> static @ "examples/helloWorld-1.0.repl"
  ExampleRef_SimplePayments
    -> static @ "examples/simplePayments-1.0.repl"
  ExampleRef_InternationalPayments
    -> static @ "examples/internationalPayments-1.0.repl"

-- | File name of JSON data for example.
exampleDataName :: ExampleRef -> Text
exampleDataName = \case
  ExampleRef_HelloWorld
    -> static @ "examples/helloWorld-1.0.data.json"
  ExampleRef_SimplePayments
    -> static @ "examples/simplePayments-1.0.data.json"
  ExampleRef_InternationalPayments
    -> static @ "examples/internationalPayments-1.0.data.json"

-- | Actually fetch Example code and data.
--
--   First value in returned pair is the Pact code, the second is the retrieved
--   JSON data.
fetchExample
  :: ( PerformEvent t m, TriggerEvent t m, MonadJSM (Performable m)
     , HasJSContext (Performable m)
     )
  => Event t ExampleRef -> m (Event t (ExampleRef, (Text, Text)))
fetchExample onExampleModule =
  performEventAsync $ ffor onExampleModule $ \example cb -> void . forkJSM $ do
    let
      callback = liftIO . cb . (example,) . (codeFromResponse *** codeFromResponse)

    let codeReq = xhrRequest "GET" (exampleFileName example) def
    void $ newXMLHttpRequest codeReq $ \codeRes -> do
      let jsonReq = xhrRequest "GET" (exampleDataName example) def
      void $ newXMLHttpRequest jsonReq $ \jsonRes ->
        callback (codeRes, jsonRes)
