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
{-# LANGUAGE ConstraintKinds           #-}

-- | Messages: Report messages that are relevant to the user.
--
--   E.g. The response of a deployment, errors when loading code into the REPL, ..
--
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.Messages
  ( -- * Types and Classes
    -- ** The basic Model and ModelConfig types
    MessagesCfg (..)
  , HasMessagesCfg (..)
  , Messages (..)
  , HasMessages (..)
    -- ** Other types
  , LogMsg
  -- * Creation
  , makeMessages
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Text                (Text)
import           Generics.Deriving.Monoid (mappenddefault, memptydefault)
import           GHC.Generics             (Generic)
import           Reflex
------------------------------------------------------------------------------
import           Frontend.Foundation


-- | Type of messages that can be logged.
type LogMsg = Text

-- | Type of messages that have been logged.
--
--   Currently just `Text`, we might add a timestamp later on.
type LoggedMsg = Text

-- | Configuration for Messages
--
--   State is controlled via this configuration.
data MessagesCfg t = MessagesCfg
  { _messagesCfg_send :: Event t [LogMsg]
    -- ^ Send a message that should be displayed to the user.
  , _messagesCfg_clear :: Event t ()
    -- ^ Empty messages log.
  }
  deriving Generic

makePactLenses ''MessagesCfg

-- | Current ModuleExploer state.
data Messages t = Messages
  { _messages_messages :: Dynamic t [LoggedMsg]
  -- ^ Currently logged messages. Newest ones are on top.
  , _messages_gotNew :: Event t ()
  -- ^ Gets triggered whenever new messages arrived.
  }
  deriving Generic

makePactLenses ''Messages


makeMessages
  :: forall t m cfg
  . (Reflex t, MonadHold t m, MonadFix m
    , HasMessagesCfg cfg t
    )
  => cfg -> m (Messages t)
makeMessages cfg = do
  msgs <- foldDyn id [] $ mergeWith (.) [ (<>) <$> cfg ^. messagesCfg_send
                                        , const [] <$ cfg ^. messagesCfg_clear
                                        ]
  pure $ Messages
    { _messages_messages = msgs
    , _messages_gotNew = () <$ cfg ^. messagesCfg_send
    }


-- Instances

instance Reflex t => Semigroup (MessagesCfg t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (MessagesCfg t) where
  mempty = memptydefault
  mappend = (<>)

-- TODO: Those instances should really be derived via Generic.
instance Flattenable (MessagesCfg t) t where
  flattenWith doSwitch ev =
    MessagesCfg
      <$> doSwitch never (_messagesCfg_send <$> ev)
      <*> doSwitch never (_messagesCfg_clear <$> ev)
