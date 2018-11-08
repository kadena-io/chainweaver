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

-- | Ide, IdeCfg: Model and ModelConfig for the pact-web application.
--
--   Ide contains all the state that is relevant to the application, IdeCfg
--   provides the means to manipulate it, e.g. through user interactions.
--
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.Ide
  ( -- * Types and Classes
    -- ** The basic Model and ModelConfig types
    IdeCfg (..)
  , HasIdeCfg (..)
  , Ide (..)
  , HasIde (..)
  -- * Constants
  , demos
  -- * Creation
  , makeIde
  -- ** Auxiliary Types
  , LogMsg
  , ExampleContract (..)
  , DeployedContract (..)
  , TransactionInfo (..)
  , EnvSelection (..)
  , PactFunction (..)
  , Modal (..)
  )where

------------------------------------------------------------------------------
import qualified Bound
import           Control.Lens
import           Data.Aeson               as Aeson (Object, Result (..), encode,
                                                    fromJSON)
import qualified Data.Set as Set
import           Data.Default
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Set                 (Set)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Generics.Deriving.Monoid (mappenddefault, memptydefault)
import           GHC.Generics             (Generic)
import           Reflex
import           Reflex.Adjustable.Class
import           Reflex.Dom.Core          (DomBuilder, HasJSContext, MonadHold,
                                           PostBuild, XhrResponse (..),
                                           performRequestAsync, xhrRequest)
import           Reflex.NotReady.Class
------------------------------------------------------------------------------
import           Obelisk.Generated.Static
import qualified Pact.Compile             as Pact
import qualified Pact.Parse               as Pact
import           Pact.Repl
import           Pact.Repl.Types
import           Pact.Types.Lang
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Foundation
import           Frontend.JsonData
import           Frontend.Wallet

type LogMsg = Text

data ExampleContract = ExampleContract
  { _exampleContract_name :: Text
  , _exampleContract_code :: Text
  , _exampleContract_data :: Text
  } deriving Show

makePactLenses ''ExampleContract

data DeployedContract = DeployedContract
  { _deployedContract_name        :: Text
  , _deployedContract_backendName :: BackendName
  , _deployedContract_backendUri  :: BackendUri
  } deriving (Eq, Ord, Show)

makePactLenses ''DeployedContract

-- | Data needed to send transactions to the server.
data TransactionInfo = TransactionInfo
  { _transactionInfo_keys :: Set KeyName
  , _transactionInfo_backend :: BackendName
  } deriving (Eq, Ord, Show)


-- | The available panels in the `envPanel`
data EnvSelection
  = EnvSelection_Repl -- ^ REPL for interacting with loaded contract
  | EnvSelection_Env -- ^ Widgets for editing (meta-)data.
  | EnvSelection_Msgs -- ^ Compiler errors and other messages to be shown.
  | EnvSelection_Functions -- ^ Functions available for deployed contracts
  | EnvSelection_ModuleExplorer -- ^ The module explorer
  deriving (Eq, Ord, Show)

-- | Useful data about a pact function
data PactFunction = PactFunction
  { _pactFunction_module        :: ModuleName
  , _pactFunction_name          :: Text
  , _pactFunction_defType       :: DefType
  , _pactFunction_documentation :: Maybe Text
  , _pactFunction_type          :: FunType (Term Name)
  }

-- | Request a modal dialog of some kind.
data Modal
  = Modal_DeployConfirmation  -- ^ Display a deploy confirmation dialog.
  | Modal_NoModal             -- ^ Don't display a Modal.

-- | Configuration for sub-modules.
--
--   State is controlled via this configuration.
data IdeCfg t = IdeCfg
  { _ideCfg_wallet      :: WalletCfg t
  , _ideCfg_jsonData    :: JsonDataCfg t
  , _ideCfg_backend     :: BackendCfg t
  , _ideCfg_selContract :: Event t (Either ExampleContract DeployedContract)
    -- ^ Select a contract to load into the editor. Also the currently selected
    -- deploy backend will be set to the contract's backend, if applicable.
  , _ideCfg_setCode     :: Event t Text
    -- ^ Set the code manually, e.g. on user edit.
  , _ideCfg_load        :: Event t ()
    -- ^ Load code into the repl.
  , _ideCfg_setMsgs     :: Event t [LogMsg]
    -- ^ Set errors that should be shown to the user.
  , _ideCfg_setDeployed :: Event t (Maybe (BackendUri, [PactFunction]))
    -- ^ Update the last loaded deployed function list
  , _ideCfg_selEnv      :: Event t EnvSelection
    -- ^ Switch tab of the right pane.
  , _ideCfg_clearRepl   :: Event t ()
    -- ^ Make the REPL fresh again, ready for new contracts.
  , _ideCfg_deploy      :: Event t TransactionInfo
    -- ^ Deploy the currently edited code/contract.
  , _ideCfg_reqModal    :: Event t Modal
   -- ^ Request a modal dialog of the given type.
  }
  deriving Generic

makePactLenses ''IdeCfg

-- | Current IDE state.
data Ide t = Ide
  { _ide_code             :: Dynamic t Text
  -- ^ Currently loaded/edited PACT code.
  , _ide_deployed         :: Dynamic t (Maybe (BackendUri, [PactFunction]))
  -- ^ Last loaded deployed contract
  , _ide_selectedContract :: Dynamic t (Either ExampleContract DeployedContract)
  -- ^ The currently selected contract name.
  , _ide_wallet           :: Wallet t
  , _ide_jsonData         :: JsonData t
  , _ide_backend          :: Backend t
  , _ide_msgs             :: Dynamic t [LogMsg]
  , _ide_load             :: Event t ()
  -- ^ Forwarded _ideCfg_load. TODO: Modularize Repl properly and get rid of this.
  , _ide_clearRepl        :: Event t ()
  -- ^ Forwarded _ideCfg_clearRepl. TODO: Modularize Repl properly and get rid of this.
  -- ^ The backend the user wants to deploy to.
  , _ide_envSelection     :: Dynamic t EnvSelection
  -- ^ Currently selected tab in the right pane.
  , _ide_modal            :: Dynamic t Modal
  -- ^ The modal dialog that currently gets displayed.
  }
  deriving Generic

makePactLenses ''Ide


makeIde
  :: forall t m
  . ( MonadHold t m, PerformEvent t m, MonadFix m
    , MonadJSM (Performable m), MonadJSM m
    , NotReady t m, Adjustable t m, HasJSContext (Performable m)
    , TriggerEvent t m, MonadSample t (Performable m), PostBuild t m
    )
  => IdeCfg t -> m (Ide t)
makeIde userCfg = build $ \ ~(cfg, ideL) -> do
    walletL <- makeWallet $ _ideCfg_wallet cfg
    json <- makeJsonData walletL $ _ideCfg_jsonData cfg
    backendL <- makeBackend walletL $ cfg ^. ideCfg_backend
    errors <- holdDyn [] $ cfg ^. ideCfg_setMsgs
    deployed <- holdDyn Nothing $ cfg ^. ideCfg_setDeployed

    (onNewCode, contractReceivedCfg) <- loadContract ideL
    code <- holdDyn "" $ leftmost
      [ onNewCode
      , cfg ^. ideCfg_setCode
      , "Loading ..." <$ cfg ^. ideCfg_selContract
      ]
    selContract <- holdDyn (Left initialDemoContract) $ cfg ^. ideCfg_selContract

    let
      mkReq = do
        c       <- ideL ^. ide_code
        ed      <- ideL ^. ide_jsonData . jsonData_data
        mbs     <- ideL ^. ide_backend . backend_backends
        pure $ \bName -> do
          bs <- mbs
          b <- Map.lookup bName bs
          d <- ed ^? _Right
          pure $ BackendRequest c d b
      addSigning f a = (\mkReq -> mkReq (_transactionInfo_keys a)) <$> f (_transactionInfo_backend a)
    onResp <- backendRequest (ideL ^. ide_wallet)
      (attachWithMaybe addSigning (current mkReq) (_ideCfg_deploy cfg))

    let
      jsonErrorString = either (Just . showJsonError) (const Nothing) <$>
          ideL ^. ide_jsonData . jsonData_data
      jsonErrorsOnLoad =
        fmap maybeToList . tag (current jsonErrorString) $ cfg ^. ideCfg_load
      msgs = leftmost
        [ (:[]) . prettyPrintBackendErrorResult . snd <$> onResp
        , jsonErrorsOnLoad
        , [] <$ cfg ^. ideCfg_selContract
        ]
      refresh = fmapMaybe (either (const Nothing) (const $ Just ()) . snd) onResp
      ourCfg = mempty
        & ideCfg_setMsgs .~ msgs
        & ideCfg_backend . backendCfg_refreshModule .~ refresh
        & ideCfg_clearRepl .~ (() <$ cfg ^. ideCfg_selContract)
        & ideCfg_wallet . walletCfg_clearAll .~ (() <$ cfg ^. ideCfg_selContract)

    envSelection <- makeEnvSelection cfg

    modal <- holdDyn Modal_NoModal $ _ideCfg_reqModal cfg

    pure
      ( mconcat [ourCfg, userCfg, contractReceivedCfg]
      , Ide
          { _ide_code = code
          , _ide_deployed = deployed
          , _ide_selectedContract = selContract
          , _ide_wallet = walletL
          , _ide_jsonData = json
          , _ide_msgs = errors
          , _ide_backend = backendL
          , _ide_envSelection = envSelection
          , _ide_load = _ideCfg_load cfg
          , _ide_clearRepl = _ideCfg_clearRepl cfg
          , _ide_modal = modal
          }
      )
  where
    build :: ((IdeCfg t, Ide t) -> m (IdeCfg t, Ide t)) -> m (Ide t)
    build = fmap snd . mfix

    loadContract :: Ide t -> m (Event t Text, IdeCfg t)
    loadContract ideL = do
      onNewContractName <- tagOnPostBuild $ ideL ^. ide_selectedContract
      let (onExampleContract, onDeployedContract) = fanEither onNewContractName
      -- Loading of example contracts
      code <- loadContractData $ fmap _exampleContract_code onExampleContract
      json <- loadContractData $ fmap _exampleContract_data onExampleContract
      onCodeJson <- waitForEvents (,) onExampleContract code json

      -- Loading of deployed contracts
      deployedResult <- backendRequest (ideL ^. ide_wallet) $
        ffor onDeployedContract $ \c -> BackendRequest
          { _backendRequest_code = mconcat
            [ "(describe-module '"
            , _deployedContract_name c
            , ")"
            ]
          , _backendRequest_data = mempty
          , _backendRequest_backend = _deployedContract_backendUri c
          , _backendRequest_signing = Set.empty
          }
      let (deployedResultError, deployedValue) = fanEither $ sequence <$> deployedResult
          (deployedDecodeError, deployedModule) = fanEither $ ffor deployedValue $ \(uri, v) -> case fromJSON v of
            Aeson.Error e   -> Left e
            Aeson.Success a -> Right (uri, a)

      pure
        ( leftmost [ fmap fst onCodeJson
                   , ffor deployedModule $ \(_, m) -> _unCode (_mCode m)
                   ]
        , mempty
            & ideCfg_setDeployed .~ ffor deployedModule
              (\(uri, m) -> (,) uri <$> listPactFunctions (_unCode $ _mCode m))
            & ideCfg_jsonData . jsonDataCfg_setRawInput .~ fmap snd onCodeJson
            -- TODO: Something better than this for reporting errors
            & ideCfg_setMsgs .~ leftmost
              [ pure . T.pack <$> deployedDecodeError
              , pure . T.pack . show <$> deployedResultError
              ]
        )

    loadContractData onNewContractName =
      fmap (fmap codeFromResponse)
      . performRequestAsync $ ffor onNewContractName
      $ \example -> xhrRequest "GET" example def

makeEnvSelection
  :: forall t m. (MonadHold t m, Reflex t)
  => IdeCfg t
  -> m (Dynamic t EnvSelection)
makeEnvSelection cfg = do
  let
    onError =
      fmap (const EnvSelection_Msgs) . fmapMaybe listToMaybe
        $ _ideCfg_setMsgs cfg
    onLoad = EnvSelection_Repl <$ (cfg ^. ideCfg_load)

    -- Disabled Functions tab for now:
    {- onDeployedLoad = EnvSelection_Functions <$ (cfg ^. ideCfg_setDeployed) -}
    onDeployedLoad = never

  holdDyn EnvSelection_Env $ leftmost
    [ cfg ^. ideCfg_selEnv
    , onDeployedLoad
    , onError -- Order important - we want to see errors.
    , onLoad
    ]

-- | Get the top level functions from a 'Term'
getFunctions :: Term Name -> [PactFunction]
getFunctions (TModule _ body _) = getFunctions $ Bound.instantiate undefined body
getFunctions (TDef name moduleName defType funType _ docs _) = [PactFunction moduleName name defType (_mDocs docs) funType]
getFunctions (TList list1 _ _) = getFunctions =<< list1
getFunctions _ = []

-- | Parse and compile the code to list the top level function data
listPactFunctions :: Text -> Maybe [PactFunction]
listPactFunctions code = case Pact.compileExps Pact.mkEmptyInfo <$> Pact.parseExprs code of
  Right (Right terms) -> Just $ concatMap getFunctions terms
  _                   -> Nothing

codeFromResponse :: XhrResponse -> Text
codeFromResponse =
    fromMaybe "error: could not connect to server" . _xhrResponse_responseText

exampleData :: [ExampleContract]
exampleData =
  [ ExampleContract "Hello World"
    (static @ "examples/helloWorld-1.0.repl")
    (static @ "examples/helloWorld-1.0.data.json")
  , ExampleContract "Simple Payment"
    (static @ "examples/simplePayments-1.0.repl")
    (static @ "examples/simplePayments-1.0.data.json")
  , ExampleContract "International Payment"
    (static @ "examples/internationalPayments-1.0.repl")
    (static @ "examples/internationalPayments-1.0.data.json")

  {- , ExampleContract "Commercial Paper" "examples/commercialPaper-1.0" -}
  ]

demos :: Map Int ExampleContract
demos = Map.fromList $ zip [0..] exampleData

-- | What demo do we load on startup:
initialDemo :: Int
initialDemo = 0

initialDemoContract :: ExampleContract
initialDemoContract = fromJust $ Map.lookup initialDemo demos


-- Instances:

instance Semigroup Modal where
  a <> b = a

instance Reflex t => Semigroup (IdeCfg t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (IdeCfg t) where
  mempty = memptydefault
  mappend = (<>)

instance Semigroup EnvSelection where
  sel1 <> _ = sel1

instance Semigroup DeployedContract where
  sel1 <> _ = sel1

instance Semigroup TransactionInfo where
  sel1 <> _ = sel1

instance HasWalletCfg (IdeCfg t) t where
  walletCfg = ideCfg_wallet

instance HasJsonDataCfg (IdeCfg t) t where
  jsonDataCfg = ideCfg_jsonData

instance Flattenable (IdeCfg t) t where
  flattenWith doSwitch ev =
    IdeCfg
      <$> flattenWith doSwitch (_ideCfg_wallet <$> ev)
      <*> flattenWith doSwitch (_ideCfg_jsonData <$> ev)
      <*> flattenWith doSwitch (_ideCfg_backend <$> ev)
      <*> doSwitch never (_ideCfg_selContract <$> ev)
      <*> doSwitch never (_ideCfg_setCode <$> ev)
      <*> doSwitch never (_ideCfg_load <$> ev)
      <*> doSwitch never (_ideCfg_setMsgs <$> ev)
      <*> doSwitch never (_ideCfg_setDeployed <$> ev)
      <*> doSwitch never (_ideCfg_selEnv <$> ev)
      <*> doSwitch never (_ideCfg_clearRepl <$> ev)
      <*> doSwitch never (_ideCfg_deploy <$> ev)
      <*> doSwitch never (_ideCfg_reqModal <$> ev)
