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

-- |
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.UI.Modal
  ( Modal
  , HasModalCfg (..)
  , modalHeader
  , modalMain
  , modalBody
  , modalFooter
  ) where

------------------------------------------------------------------------------
import           Control.Lens hiding (element)
import qualified GHCJS.DOM.GlobalEventHandlers as Events
import           Reflex
import           Reflex.Dom
import           Data.Void
------------------------------------------------------------------------------
import           Frontend.Foundation
import           Frontend.UI.Button
------------------------------------------------------------------------------

-- | Type of modal dialog.
--
--   It is some arbitrary widget, preferably built with `modalHeader`,
--   `modalBody` and `modalFooter`.  It provides some config and an `Event`
--   that will trigger close on the dialog.
type Modal baseCfg m t = m (ModalCfg baseCfg t, Event t ())

{- type ModalCfg cfg m t = cfg (Modal (cfg Void t) t) t -}

-- IdeCfg t modal
-- IdeCfg t (m (Mu IdeCfg t))
class HasModalCfg cfg modal t | cfg -> modal where
  -- | A config should provide a variant of itself that does not depend on
  -- modal for use in the modal.
  --
  --  E.g. SomeCfg Void
  type ModalCfg cfg t
  modalCfg_setModal :: Lens' cfg (Event t (Maybe modal))

{- newtype Mu a = Mu {unMu :: a (Mu a)} -}

{- newtype Modal cfg m modal = Modal { unModal :: m (cfg modal) } -}

{- type MuModal cfg m = Mu (Modal cfg m) -}



-- | Create a modal dialog header.
modalHeader
  :: forall t m a. MonadWidget t m
  => m ()
  -- ^ Content of the h2 in the header.
  -> m ( Event t ())
  -- ^ Close event
modalHeader header = divClass "modal__header" $ do
  elClass "h2" "modal__heading" $ do
    header
    uiButton (def & uiButtonCfg_class .~ "modal__button-close") $
      text "x"

-- | Wrap body and footer in this.
modalMain :: MonadWidget t m => m a -> m a
modalMain  = divClass "modal__main"

-- | Create a modal dialog body.
modalBody
  :: forall t m a. MonadWidget t m
  => m a
  -- ^ The actual body of the dialog.
  -> m a
  -- ^ Wrapped up body
modalBody = divClass "modal__body"

-- | Create a modal dialog footer.
modalFooter
  :: forall t m a. MonadWidget t m
  => m a
  -- ^ The actual footer of the dialog.
  -> m a
  -- ^ Wrapped up footer
modalFooter = divClass "modal__footer"

