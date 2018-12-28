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

module Frontend.UI.Modal.Impl
  ( -- * Types
    ModalIdeCfg
  , ModalIde
  , ModalImpl
    -- * Show it
  , showModal
    -- * Build it
  , module Frontend.UI.Modal
  ) where

------------------------------------------------------------------------------
import           Control.Lens hiding (element)
import qualified Data.Map as Map
import           Data.Proxy
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as Events
import           Reflex
import           Reflex.Dom
import           Data.Void (Void)
------------------------------------------------------------------------------
import           Frontend.Foundation
import           Frontend.Ide
import           Frontend.UI.Modal
------------------------------------------------------------------------------

type ModalImpl m t = m (IdeCfg Void t, Event t ())

type ModalIdeCfg m t = IdeCfg (ModalImpl m t) t

type ModalIde m t = Ide (ModalImpl m t) t

-- | Show the current modal dialog as given in the model.
showModal :: forall t m. MonadWidget t m => ModalIde m t -> m (ModalIdeCfg m t)
showModal ideL = do
    document <- DOM.currentDocumentUnchecked

    onEsc <- wrapDomEventMaybe document (`EventM.on` Events.keyDown) $ do
      key <- getKeyEvent
      pure $ if keyCodeLookup (fromIntegral key) == Escape then Just () else Nothing

    let staticAttrs = ("id" =: "modal-root")
    let mkAttrs vis = staticAttrs <>
          if vis then ("class" =: "open") else mempty

    elDynAttr "div" (mkAttrs <$> isVisible) $ do
      (backdropEl, ev) <- elAttr' "div" ("class" =: "screen") $
        networkView (mayMkModal <$> _ide_modal ideL)
      onFinish <- switchHold never $ snd <$> ev
      mCfgVoid <- flatten $ fst <$> ev
      let
        mCfg :: ModalIdeCfg m t
        mCfg = mCfgVoid { _ideCfg_setModal = LeftmostEv never }

      let
        onClose = leftmost [ onFinish
                           , onEsc
                           , domEvent Click backdropEl
                           ]
        lConf = mempty & ideCfg_setModal .~ (LeftmostEv $ Nothing <$ onClose)
      pure $ lConf <> mCfg
  where
    isVisible = isJust <$> _ide_modal ideL

    mayMkModal = maybe (pure (mempty, never)) mkModal


-- | Puts content in a .modal class container and stops event propagation
mkModal
  :: forall t m a. MonadWidget t m
  => m (a, Event t ())
  -- ^ The dialog
  -> m (a, Event t ())
  -- ^ Wrapped up dialog.
mkModal = fmap snd . element "div" elCfg
  where
    elCfg =
       (def :: ElementConfig EventResult t (DomBuilderSpace m))
       & initialAttributes .~ Map.mapKeys (AttributeName Nothing)
         ("class" =: "modal")
       & elementConfig_eventSpec %~ addEventSpecFlags
         (Proxy :: Proxy (DomBuilderSpace m)) Click (const stopPropagation)
