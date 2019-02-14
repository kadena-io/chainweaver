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
  , _ExampleRef_SimplePayment
  , _ExampleRef_InternationalPayment
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
import           Control.Arrow                   ((***))
import           Control.Error.Safe              (justZ)
import           Control.Monad                   (void)
import qualified Data.Aeson                      as A
import           Data.Default
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Reflex
import           Reflex.Dom.Core                 (HasJSContext,
                                                  XhrResponse (..),
                                                  newXMLHttpRequest, xhrRequest)
import           Safe                            (readMay)
------------------------------------------------------------------------------
import           Obelisk.Generated.Static
------------------------------------------------------------------------------
import           Frontend.Foundation
import           Frontend.ModuleExplorer.RefPath as MP



-- | A reference to one of the predefined example files.
data ExampleRef
  = ExampleRef_HelloWorld
  | ExampleRef_SimplePayment
  | ExampleRef_InternationalPayment
  | ExampleRef_Verification
  | ExampleRef_Accounts
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Read)

makePactPrisms ''ExampleRef


instance A.ToJSON ExampleRef where
  toJSON = A.genericToJSON compactEncoding
  toEncoding = A.genericToEncoding compactEncoding

instance A.FromJSON ExampleRef where
  parseJSON = A.genericParseJSON compactEncoding

-- | List of all available examples.
examples :: [ExampleRef]
examples = [minBound .. maxBound]


instance IsRefPath ExampleRef where
  renderRef = mkRefPath . shorten . T.pack . show
    where
      -- Only works for "ExampleRef_Blah" kind of names:
      shorten =  head . reverse . filter (/= "") . T.splitOn "_"

  parseRef = do
    n <- ("ExampleRef_" <>) <$> MP.anySingle
    justZ $ readMay . T.unpack $ n


-- | Names of examples as shown to the user.
exampleName :: ExampleRef -> Text
exampleName = \case
  ExampleRef_HelloWorld
    -> "Hello World"
  ExampleRef_SimplePayment
    -> "Simple Payment"
  ExampleRef_InternationalPayment
    -> "International Payment"
  ExampleRef_Verification
    -> "Verification"
  ExampleRef_Accounts
    -> "Accounts"

-- | File name of Pact code for the given example.
exampleFileName :: ExampleRef -> Text
exampleFileName = \case
  ExampleRef_HelloWorld
    -> static @ "examples/helloWorld-1.0.repl"
  ExampleRef_SimplePayment
    -> static @ "examples/simplePayments-1.0.repl"
  ExampleRef_InternationalPayment
    -> static @ "examples/internationalPayments-1.0.repl"
  ExampleRef_Verification
    -> static @ "examples/verification-1.0.repl"
  ExampleRef_Accounts
    -> static @ "examples/accounts-1.0.repl"


-- | File name of JSON data for example.
exampleDataName :: ExampleRef -> Text
exampleDataName = \case
  ExampleRef_HelloWorld
    -> static @ "examples/helloWorld-1.0.data.json"
  ExampleRef_SimplePayment
    -> static @ "examples/simplePayments-1.0.data.json"
  ExampleRef_InternationalPayment
    -> static @ "examples/internationalPayments-1.0.data.json"
  ExampleRef_Verification
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

