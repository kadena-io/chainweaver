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
    ExampleRef (..)
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
import           Control.Monad            (void)
import qualified Data.Aeson                        as A
import           Control.Arrow            ((***))
import           Data.Default
import           Data.Text                (Text)
import           Reflex
import           Reflex.Dom.Core          (HasJSContext,
                                           XhrResponse (..), newXMLHttpRequest,
                                           xhrRequest)
------------------------------------------------------------------------------
import           Obelisk.Generated.Static
------------------------------------------------------------------------------
import           Frontend.Foundation



-- | A reference to one of the predefined example files.
data ExampleRef
  = ExampleRef_HelloWorld
  | ExampleRef_SimplePayments
  | ExampleRef_InternationalPayments
  | ExampleRef_SimpleVerify
  | ExampleRef_Accounts
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

makePactPrisms ''ExampleRef


instance A.ToJSON ExampleRef
instance A.FromJSON ExampleRef

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
  ExampleRef_SimpleVerify
    -> "Verification"
  ExampleRef_Accounts
    -> "Accounts"

-- | File name of Pact code for the given example.
exampleFileName :: ExampleRef -> Text
exampleFileName = \case
  ExampleRef_HelloWorld
    -> static @ "examples/helloWorld-1.0.repl"
  ExampleRef_SimplePayments
    -> static @ "examples/simplePayments-1.0.repl"
  ExampleRef_InternationalPayments
    -> static @ "examples/internationalPayments-1.0.repl"
  ExampleRef_SimpleVerify
    -> static @ "examples/verification-1.0.repl"
  ExampleRef_Accounts
    -> static @ "examples/accounts-1.0.repl"

-- | File name of JSON data for example.
exampleDataName :: ExampleRef -> Text
exampleDataName = \case
  ExampleRef_HelloWorld
    -> static @ "examples/helloWorld-1.0.data.json"
  ExampleRef_SimplePayments
    -> static @ "examples/simplePayments-1.0.data.json"
  ExampleRef_InternationalPayments
    -> static @ "examples/internationalPayments-1.0.data.json"
  ExampleRef_SimpleVerify
    -> static @ "examples/verification-1.0.data.json"
  ExampleRef_Accounts
    -> static @ "examples/accounts-1.0.data.json"

-- | Actually fetch Example code and data.
--
--   First value in returned pair is the Pact code, the second is the retrieved
--   JSON data.
--
--   TODO: We don't actually use the json data right now, we maybe never will
--   as I am not yet sure whether we actually want to store/restore JSON data
--   and whether it would be useful/secure.
fetchExample
  :: ( PerformEvent t m, TriggerEvent t m, MonadJSM (Performable m)
     , HasJSContext JSM
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


codeFromResponse :: XhrResponse -> Text
codeFromResponse =
    fromMaybe "error: could not connect to server" . _xhrResponse_responseText

