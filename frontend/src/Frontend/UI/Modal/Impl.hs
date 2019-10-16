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
import           Control.Lens                  hiding (element)
import qualified Data.Map                      as Map
import           Data.Proxy
import           Data.Void                     (Void)
import qualified GHCJS.DOM                     as DOM
import qualified GHCJS.DOM.EventM              as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as Events
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.Foundation
import           Frontend.Ide
import           Frontend.UI.Modal
------------------------------------------------------------------------------

type ModalImpl m key t = Event t () -> m (IdeCfg Void key t, Event t ())

type ModalIdeCfg m key t = IdeCfg (ModalImpl m key t) key t

type ModalIde m key t = Ide (ModalImpl m key t) key t

-- | Show the current modal dialog as given in the model.
showModal :: forall key t m. MonadWidget t m => ModalIde m key t -> m (ModalIdeCfg m key t)
showModal ideL = do
    document <- DOM.currentDocumentUnchecked

    onEsc <- wrapDomEventMaybe document (`EventM.on` Events.keyDown) $ do
      key <- getKeyEvent
      pure $ if keyCodeLookup (fromIntegral key) == Escape then Just () else Nothing

    let mkCls vis = "modal" <>
          if vis then "modal_open" else mempty

    elDynKlass "div" (mkCls <$> isVisible) $ mdo
      (backdropEl, ev) <- elClass' "div" "modal__screen" $
        networkView (mayMkModal onClose <$> _ide_modal ideL)
      onFinish <- switchHold never $ snd . snd <$> ev
      mCfgVoid <- flatten $ fst . snd <$> ev

      -- Ignore clicks that started in the dialog. This is necessary because
      -- when one selects text in a line edit with the mouse, he would easily
      -- cause an unintended closing of the dialog:
      onModalMousePressed <- switchHold never $ fst <$> ev
      isDown <- hold False $ leftmost
        [ onModalMousePressed
          -- So we are not losing future clicks:
        , False <$ domEvent Click backdropEl
        ]
      let onBackdropClick = gate (fmap not isDown) $ domEvent Click backdropEl
      let
        mCfg :: ModalIdeCfg m key t
        mCfg = mCfgVoid { _ideCfg_setModal = LeftmostEv never }

      let
        onClose = leftmost [ onFinish
                           , onEsc
                           , onBackdropClick
                           ]
        lConf = mempty & ideCfg_setModal .~ (LeftmostEv $ Nothing <$ onClose)
      pure $ lConf <> mCfg
  where
    isVisible = isJust <$> _ide_modal ideL

    mayMkModal
      :: Event t ()
      -> Maybe (Event t () -> m (IdeCfg Void key t, Event t ()))
      -> m (Event t Bool, (IdeCfg Void key t, Event t ()))
    mayMkModal e = maybe (pure (never, (mempty, never))) (\f -> mkModal f e)


-- | Puts content in a .modal class container and stops event propagation
mkModal
  :: forall t m a. MonadWidget t m
  => (Event t () -> m (a, Event t ()))
  -- ^ The dialog
  -> Event t () -> m (Event t Bool, (a, Event t ()))
  -- ^ Wrapped up dialog.
mkModal e e' = do
    (modalL, r) <- element "div" elCfg $ e e'
    let
      onDown = domEvent Mousedown modalL
      onUp = domEvent Mouseup modalL
    let
      isDown = leftmost
        [ True <$ onDown
        , False <$ onUp
        ]
    pure (isDown, r)
  where
    elCfg =
       (def :: ElementConfig EventResult t (DomBuilderSpace m))
       & initialAttributes .~ Map.mapKeys (AttributeName Nothing)
         ("class" =: "modal__dialog")
       & elementConfig_eventSpec %~ addEventSpecFlags
         (Proxy :: Proxy (DomBuilderSpace m)) Click (const stopPropagation)
