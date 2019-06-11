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
  , EnvSelection (..)
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Void                    (Void)
import           Generics.Deriving.Monoid     (mappenddefault, memptydefault)
import           GHC.Generics                 (Generic)
import           Reflex
import           Reflex.Dom.Core              (HasJSContext)
------------------------------------------------------------------------------
import           Obelisk.Route.Frontend       (R, RouteToUrl (..), Routed (..),
                                               SetRoute (..))
------------------------------------------------------------------------------
import           Common.Route                 (FrontendRoute)
import           Frontend.Network
import           Frontend.Editor
import           Frontend.Foundation
import           Frontend.GistStore
import           Frontend.JsonData
import           Frontend.Messages
import           Frontend.ModuleExplorer.Impl
import           Frontend.OAuth
import           Frontend.Repl
import           Frontend.Routes
import           Frontend.Wallet

-- We don't really depend on UI here, I just don't bother to move `HasModalCfg`
-- into its own module for now. If we do it should probably me
-- "Frontend.Modal".
import           Frontend.UI.Modal

-- | The available panels in the `envPanel`
data EnvSelection
  = EnvSelection_Env -- ^ Widgets for editing (meta-)data.
  | EnvSelection_Repl -- ^ REPL for interacting with loaded contract
  | EnvSelection_Msgs -- ^ Compiler errors and other messages to be shown.
  | EnvSelection_ModuleExplorer -- ^ The module explorer
  deriving (Eq, Ord, Show, Enum, Bounded)


-- | Configuration for sub-modules.
--
--   State is controlled via this configuration.
data IdeCfg modal t = IdeCfg
  { _ideCfg_wallet         :: WalletCfg t
  , _ideCfg_jsonData       :: JsonDataCfg t
  , _ideCfg_network        :: NetworkCfg t
  , _ideCfg_moduleExplorer :: ModuleExplorerCfg t
  , _ideCfg_editor         :: EditorCfg t
  , _ideCfg_repl           :: ReplCfg t
  , _ideCfg_messages       :: MessagesCfg t
  , _ideCfg_oAuth          :: OAuthCfg t
  , _ideCfg_gistStore      :: GistStoreCfg t
  , _ideCfg_selEnv         :: Event t EnvSelection
    -- ^ Switch tab of the right pane.
  , _ideCfg_setModal       :: LeftmostEv t (Maybe modal)
   -- ^ Request a modal dialog. Use `Nothing` to close an existing modal
   --   dialog.
  }
  deriving Generic

makePactLenses ''IdeCfg

-- | Current IDE state.
data Ide modal t = Ide
  { _ide_moduleExplorer :: ModuleExplorer t
  , _ide_editor         :: Editor t
  , _ide_messages       :: Messages t
  , _ide_wallet         :: Wallet t
  , _ide_jsonData       :: JsonData t
  , _ide_network        :: Network t
  , _ide_repl           :: WebRepl t
  , _ide_oAuth          :: OAuth t
  , _ide_gistStore      :: GistStore t
  , _ide_envSelection   :: Dynamic t EnvSelection
  -- ^ Currently selected tab in the right pane.
  , _ide_modal          :: Dynamic t (Maybe modal)
  -- ^ The modal dialog that currently gets displayed.
  }
  deriving Generic

makePactLenses ''Ide

makeIde
  :: forall t m modal
  . ( MonadHold t m, PerformEvent t m, MonadFix m
    , MonadJSM (Performable m), MonadJSM m
    , HasJSContext (Performable m)
    , MonadSample t (Performable m)
    , TriggerEvent t m, PostBuild t m
    , RouteToUrl (R FrontendRoute) m, Routed t (R FrontendRoute) m
    , SetRoute t (R FrontendRoute) m
    , HasCommonConfigs m
    )
  => IdeCfg modal t -> m (Ide modal t)
makeIde userCfg = build $ \ ~(cfg, ideL) -> do

    walletL <- makeWallet $ _ideCfg_wallet cfg
    json <- makeJsonData walletL $ _ideCfg_jsonData cfg
    (networkCfgL, networkL) <- makeNetwork walletL $ cfg ^. ideCfg_network
    (explrCfg, moduleExplr) <- makeModuleExplorer ideL cfg
    (editorCfgL, editorL) <- makeEditor ideL cfg
    (oAuthCfgL, oAuthL) <- makeOAuth cfg
    (gistStoreCfgL, gistStoreL) <- makeGistStore ideL cfg
    messagesL <- makeMessages cfg
    (replCfgL, replL) <- makeRepl ideL cfg
    routesCfg <- handleRoutes ideL

    envSelection <- makeEnvSelection ideL $ cfg ^. ideCfg_selEnv

    modal <- holdDyn Nothing $ unLeftmostEv (_ideCfg_setModal cfg)

    pure
      ( mconcat
          [ userCfg
          , explrCfg
          , replCfgL
          , networkCfgL
          , editorCfgL
          , oAuthCfgL
          , gistStoreCfgL
          , routesCfg
          ]
      , Ide
        { _ide_editor = editorL
        , _ide_wallet = walletL
        , _ide_jsonData = json
        , _ide_messages = messagesL
        , _ide_network = networkL
        , _ide_repl = replL
        , _ide_moduleExplorer = moduleExplr
        , _ide_envSelection = envSelection
        , _ide_modal = modal
        , _ide_oAuth = oAuthL
        , _ide_gistStore = gistStoreL
        }
      )
  where
    build :: ((IdeCfg modal t, Ide modal t) -> m (IdeCfg modal t, Ide modal t)) -> m (Ide modal t)
    build = fmap snd . mfix

makeEnvSelection
  :: forall t m modal. (MonadHold t m, Reflex t)
  => Ide modal t
  -> Event t EnvSelection
  -> m (Dynamic t EnvSelection)
makeEnvSelection ideL onSelect = do
  let
    onMessages = EnvSelection_Msgs <$ ideL ^. messages_gotNew
    onLoad = EnvSelection_Repl <$ ideL ^. repl_newOutput
    onJsonData = EnvSelection_Env <$ updated (ideL ^. jsonData_data)

  holdDyn EnvSelection_Env $ leftmost
    [ onSelect
    , onMessages
    , onLoad
    , onJsonData
    ]

-- Instances:

instance Reflex t => Semigroup (IdeCfg modal t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (IdeCfg modal t) where
  mempty = memptydefault
  mappend = (<>)

instance Semigroup EnvSelection where
  sel1 <> _ = sel1

instance HasWalletCfg (IdeCfg modal t) t where
  walletCfg = ideCfg_wallet

instance HasJsonDataCfg (IdeCfg modal t) t where
  jsonDataCfg = ideCfg_jsonData

instance HasNetworkCfg (IdeCfg modal t) t where
  networkCfg = ideCfg_network

instance HasModuleExplorerCfg (IdeCfg modal t) t where
  moduleExplorerCfg = ideCfg_moduleExplorer

instance HasEditorCfg (IdeCfg modal t) t where
  editorCfg = ideCfg_editor

instance HasMessagesCfg (IdeCfg modal t) t where
  messagesCfg = ideCfg_messages

instance HasOAuthCfg (IdeCfg modal t) t where
  oAuthCfg = ideCfg_oAuth

instance HasGistStoreCfg (IdeCfg modal t) t where
  gistStoreCfg = ideCfg_gistStore

instance HasModalCfg (IdeCfg modal t) modal t where
  -- type ModalType (IdeCfg (Modal IdeCfg m t) t) m t = Modal IdeCfg m t
  type ModalCfg (IdeCfg modal t) t = IdeCfg Void t

  modalCfg_setModal f configL =
    (\setModal -> configL { _ideCfg_setModal = LeftmostEv setModal })
      <$> f (unLeftmostEv . _ideCfg_setModal $ configL)

instance HasReplCfg (IdeCfg modal t) t where
  replCfg = ideCfg_repl

instance HasWallet (Ide modal t) t where
  wallet = ide_wallet

instance HasJsonData (Ide modal t) t where
  jsonData = ide_jsonData

instance HasNetwork (Ide modal t) t where
  network = ide_network

instance HasModuleExplorer (Ide modal t) t where
  moduleExplorer = ide_moduleExplorer

instance HasEditor (Ide modal t) t where
  editor = ide_editor

instance HasWebRepl (Ide modal t) t where
  webRepl = ide_repl

instance HasMessages (Ide modal t) t where
  messages = ide_messages

instance HasOAuth (Ide modal t) t where
  oAuth = ide_oAuth

instance HasGistStore (Ide modal t) t where
  gistStore = ide_gistStore

instance Flattenable (IdeCfg modal t) t where
  flattenWith doSwitch ev =
    IdeCfg
      <$> flattenWith doSwitch (_ideCfg_wallet <$> ev)
      <*> flattenWith doSwitch (_ideCfg_jsonData <$> ev)
      <*> flattenWith doSwitch (_ideCfg_network <$> ev)
      <*> flattenWith doSwitch (_ideCfg_moduleExplorer <$> ev)
      <*> flattenWith doSwitch (_ideCfg_editor <$> ev)
      <*> flattenWith doSwitch (_ideCfg_repl <$> ev)
      <*> flattenWith doSwitch (_ideCfg_messages <$> ev)
      <*> flattenWith doSwitch (_ideCfg_oAuth <$> ev)
      <*> flattenWith doSwitch (_ideCfg_gistStore <$> ev)
      <*> doSwitch never (_ideCfg_selEnv <$> ev)
      <*> fmap LeftmostEv (doSwitch never (unLeftmostEv . _ideCfg_setModal <$> ev))
