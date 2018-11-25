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
  -- * Creation
  , makeIde
  -- ** Auxiliary Types
  , LogMsg
  , TransactionInfo (..)
  , EnvSelection (..)
  , Modal (..)
  )where

------------------------------------------------------------------------------
import           Control.Lens
import qualified Data.Map                 as Map
import           Data.Set                 (Set)
import           Generics.Deriving.Monoid (mappenddefault, memptydefault)
import           GHC.Generics             (Generic)
import           Reflex
import           Reflex.NotReady.Class
import           Reflex.Dom.Core          (HasJSContext)
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Foundation
import           Frontend.JsonData
import           Frontend.Wallet
import           Frontend.Editor
import           Frontend.ModuleExplorer.Impl
import           Frontend.Messages
import           Frontend.Repl

-- | Data needed to send transactions to the server.
data TransactionInfo = TransactionInfo
  { _transactionInfo_keys    :: Set KeyName
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

-- | Request a modal dialog of some kind.
data Modal
  = Modal_DeployConfirmation  -- ^ Display a deploy confirmation dialog.
  | Modal_NoModal             -- ^ Don't display a Modal.

-- | Configuration for sub-modules.
--
--   State is controlled via this configuration.
data IdeCfg t = IdeCfg
  { _ideCfg_wallet         :: WalletCfg t
  , _ideCfg_jsonData       :: JsonDataCfg t
  , _ideCfg_backend        :: BackendCfg t
  , _ideCfg_moduleExplorer :: ModuleExplorerCfg t
  , _ideCfg_editor         :: EditorCfg t
  , _ideCfg_repl           :: ReplCfg t
  , _ideCfg_messages       :: MessagesCfg t
  , _ideCfg_selEnv         :: Event t EnvSelection
    -- ^ Switch tab of the right pane.
  , _ideCfg_deploy         :: Event t TransactionInfo
    -- ^ Deploy the currently edited code/contract.
  , _ideCfg_reqModal       :: Event t Modal
   -- ^ Request a modal dialog of the given type.
  }
  deriving Generic

makePactLenses ''IdeCfg

-- | Current IDE state.
data Ide t = Ide
  { _ide_moduleExplorer :: ModuleExplorer t
  , _ide_editor         :: Editor t
  , _ide_messages       :: Messages t
  , _ide_wallet         :: Wallet t
  , _ide_jsonData       :: JsonData t
  , _ide_backend        :: Backend t
  , _ide_repl           :: WebRepl t
  , _ide_envSelection   :: Dynamic t EnvSelection
  -- ^ Currently selected tab in the right pane.
  , _ide_modal          :: Dynamic t Modal
  -- ^ The modal dialog that currently gets displayed.
  }
  deriving Generic

makePactLenses ''Ide


makeIde
  :: forall t m
  . ( MonadHold t m, PerformEvent t m, MonadFix m
    , MonadJSM (Performable m), MonadJSM m
    , NotReady t m, Adjustable t m, HasJSContext (Performable m)
    , MonadSample t (Performable m)
    , TriggerEvent t m, PostBuild t m
    )
  => IdeCfg t -> m (Ide t)
makeIde userCfg = build $ \ ~(cfg, ideL) -> do
    walletL <- makeWallet $ _ideCfg_wallet cfg
    json <- makeJsonData walletL $ _ideCfg_jsonData cfg
    backendL <- makeBackend walletL $ cfg ^. ideCfg_backend
    (explrCfg, moduleExplr) <- makeModuleExplorer cfg
    editorL <- makeEditor ideL cfg
    messagesL <- makeMessages cfg
    (replCfgL, replL) <- makeRepl ideL cfg

    let
      mkReq = do
        c       <- ideL ^. editor_code
        ed      <- ideL ^. jsonData_data
        mbs     <- ideL ^. backend_backends
        pure $ \bName -> do
          bs <- mbs
          b <- Map.lookup bName bs
          d <- ed ^? _Right
          pure $ BackendRequest c d b
      addSigning f a = (\cMkReq -> cMkReq (_transactionInfo_keys a)) <$> f (_transactionInfo_backend a)
    onResp <- backendRequest (ideL ^. ide_wallet)
      (attachWithMaybe addSigning (current mkReq) (_ideCfg_deploy cfg))

    let
      msgs =  prettyPrintBackendErrorResult <$> onResp
      refresh = fmapMaybe (either (const Nothing) (const $ Just ())) onResp
      ourCfg = mempty
        & messagesCfg_send .~ msgs
        & backendCfg_refreshModule .~ refresh

    envSelection <- makeEnvSelection ideL $ cfg ^. ideCfg_selEnv

    modal <- holdDyn Modal_NoModal $ _ideCfg_reqModal cfg

    pure
      ( mconcat [ourCfg, userCfg, explrCfg, replCfgL]
      , Ide
        { _ide_editor = editorL
        , _ide_wallet = walletL
        , _ide_jsonData = json
        , _ide_messages = messagesL
        , _ide_backend = backendL
        , _ide_repl = replL
        , _ide_moduleExplorer = moduleExplr
        , _ide_envSelection = envSelection
        , _ide_modal = modal
        }
      )
  where
    build :: ((IdeCfg t, Ide t) -> m (IdeCfg t, Ide t)) -> m (Ide t)
    build = fmap snd . mfix

makeEnvSelection
  :: forall t m. (MonadHold t m, Reflex t)
  => Ide t
  -> Event t EnvSelection
  -> m (Dynamic t EnvSelection)
makeEnvSelection ideL onSelect = do
  let
    onMessages = EnvSelection_Msgs <$ ideL ^. messages_gotNew
    onLoad = EnvSelection_Repl <$ ideL ^. repl_newOutput

  holdDyn EnvSelection_Env $ leftmost
    [ onSelect
    , onMessages
    , onLoad
    ]

-- Instances:

instance Semigroup Modal where
  a <> _ = a

instance Reflex t => Semigroup (IdeCfg t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (IdeCfg t) where
  mempty = memptydefault
  mappend = (<>)

instance Semigroup EnvSelection where
  sel1 <> _ = sel1

instance Semigroup TransactionInfo where
  sel1 <> _ = sel1

instance HasWalletCfg (IdeCfg t) t where
  walletCfg = ideCfg_wallet

instance HasJsonDataCfg (IdeCfg t) t where
  jsonDataCfg = ideCfg_jsonData

instance HasBackendCfg (IdeCfg t) t where
  backendCfg = ideCfg_backend

instance HasModuleExplorerCfg (IdeCfg t) t where
  moduleExplorerCfg = ideCfg_moduleExplorer

instance HasEditorCfg (IdeCfg t) t where
  editorCfg = ideCfg_editor

instance HasMessagesCfg (IdeCfg t) t where
  messagesCfg = ideCfg_messages

instance HasReplCfg (IdeCfg t) t where
  replCfg = ideCfg_repl

instance HasWallet (Ide t) t where
  wallet = ide_wallet

instance HasJsonData (Ide t) t where
  jsonData = ide_jsonData

instance HasBackend (Ide t) t where
  backend = ide_backend

instance HasModuleExplorer (Ide t) t where
  moduleExplorer = ide_moduleExplorer

instance HasEditor (Ide t) t where
  editor = ide_editor

instance HasWebRepl (Ide t) t where
  webRepl = ide_repl

instance HasMessages (Ide t) t where
  messages = ide_messages

instance Flattenable (IdeCfg t) t where
  flattenWith doSwitch ev =
    IdeCfg
      <$> flattenWith doSwitch (_ideCfg_wallet <$> ev)
      <*> flattenWith doSwitch (_ideCfg_jsonData <$> ev)
      <*> flattenWith doSwitch (_ideCfg_backend <$> ev)
      <*> flattenWith doSwitch (_ideCfg_moduleExplorer <$> ev)
      <*> flattenWith doSwitch (_ideCfg_editor <$> ev)
      <*> flattenWith doSwitch (_ideCfg_repl <$> ev)
      <*> flattenWith doSwitch (_ideCfg_messages <$> ev)
      <*> doSwitch never (_ideCfg_selEnv <$> ev)
      <*> doSwitch never (_ideCfg_deploy <$> ev)
      <*> doSwitch never (_ideCfg_reqModal <$> ev)
