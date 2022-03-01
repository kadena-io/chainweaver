{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- This allows us to more easily use JSaddle without the noise of annotating everything to Text.
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- |
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.UI.Modal.Impl
  ( -- * Types
    ModalIdeCfg
  , ModalIde
  , ModalImpl
    -- * Show it
  , showModal
  , showModalBrutal
    -- * Build it
  , module Frontend.UI.Modal
  ) where

import Prelude hiding ((!!))
import Control.Lens hiding (element,(#))
import Data.Functor (($>))
import Control.Monad (void, (>=>))
import Data.Text (Text)
import Data.Void (Void)
import Reflex
import Reflex.Dom

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as Events
import qualified GHCJS.DOM.Types as DOM
import Language.Javascript.JSaddle (JSVal, (#), (!), (!!), (<#))
import qualified Language.Javascript.JSaddle as JSaddle

import Frontend.Foundation
import Frontend.Ide
import Frontend.UI.Modal

type ModalImpl m key t = Event t () -> m (IdeCfg Void key t, Event t ())

type ModalIdeCfg m key t = IdeCfg (ModalImpl m key t) key t

type ModalIde m key t = Ide (ModalImpl m key t) key t

modalBodyAttr :: Text
modalBodyAttr = "data-modal-open"

withBody :: MonadJSM m => (JSVal -> JSM a) -> m a
withBody f = liftJSM $ JSaddle.jsg "document" ! "body" >>= f

setBodyStyle :: MonadJSM m => Text -> JSVal -> m ()
setBodyStyle s v = withBody $ (! "style") >=> \o -> (o <# s $ v)

mkOnEsc
  :: ( TriggerEvent t m
     , MonadJSM m
     , DOM.IsEventTarget doc
     , DOM.IsGlobalEventHandlers doc
     )
  => doc
  -> m (Event t ())
mkOnEsc document = wrapDomEventMaybe document (`EventM.on` Events.keyDown) $ do
  key <- getKeyEvent
  pure $ if keyCodeLookup (fromIntegral key) == Escape then Just () else Nothing

mkModalWrapperClass :: Bool -> CssClass
mkModalWrapperClass vis = "modal" <> if vis then "modal_open" else mempty

-- This function is equivalent to the following JS for discovering the padding
-- required for the vertical scrollbar.
--
-- (window.innerWidth - document.getElementsByTagName('html')[0].clientWidth)
getScrollBarWidth :: MonadJSM m => m Double
getScrollBarWidth = liftJSM $ do
  innerW <- JSaddle.jsg "window" ! "innerWidth" >>= JSaddle.valToNumber
  clientWidth <- (JSaddle.jsg "document" # "getElementsByTagName") ["html"]
    >>= (!! 0)
    >>= (! "clientWidth")
    >>= JSaddle.valToNumber
  pure $ innerW - clientWidth

addPreventScrollClass :: MonadJSM m => m ()
addPreventScrollClass = withBody $ \b -> do
  w <- getScrollBarWidth
  _ <- b # "setAttribute" $ [modalBodyAttr, "true"]
  -- Account for the differing widths of scroll bars.
  JSaddle.toJSVal (tshow w <> "px") >>= setBodyStyle "paddingRight"

removePreventScrollClass :: MonadJSM m => m ()
removePreventScrollClass = void . withBody $ \b -> do
  -- Reset the padding back now that the scrollbar might be back.
  JSaddle.toJSVal 0 >>= setBodyStyle "paddingRight"
  b # "removeAttribute" $ [modalBodyAttr]

-- We can't use jsaddle to set the stopPropagation handler: with
-- jsaddle-warp, there is a race condition that can cause the handler for
-- the outer div to run before the stopPropagation is processed, closing
-- the modal unexpectedly. This is particularly noticable under heavy
-- network load.
-- This hack just ensures this handler runs immediately and clicks can't
-- slip through the modal__dialog container.
-- We add the listener upon the first launch of the modal to ensure that
-- modal__dialog actually exists.
jsHackForStopPropagation
  :: ( PerformEvent t m
     , MonadJSM (Performable m)
     )
  => Event t ()
  -> m ()
jsHackForStopPropagation onTrigger = performEvent_ $ ffor onTrigger $ \_ -> void $ DOM.liftJSM $ do
  JSaddle.eval "document.querySelector('.modal__dialog').addEventListener('click', function(e) { e.stopPropagation(); });"

-- | Show the current modal dialog as given in the model.
showModal :: forall key t m. MonadWidget t m => ModalIde m key t -> m (ModalIdeCfg m key t)
showModal ideL = do
    document <- DOM.currentDocumentUnchecked

    onEsc <- mkOnEsc document

    elDynKlass "div" (mkModalWrapperClass <$> isVisible) $ mdo
      ev <- elClass "div" "modal__screen" $
        divClass "modal__dialog" $ networkView $ ffor (_ide_modal ideL) $ \case
          Nothing -> removePreventScrollClass $> (mempty, never) -- The modal is closed
          Just f -> addPreventScrollClass >> f onClose -- The modal is open
      onFinish <- switchHold never $ snd <$> ev
      mCfgVoid <- flatten $ fst <$> ev

      let
        mCfg :: ModalIdeCfg m key t
        mCfg = mCfgVoid { _ideCfg_setModal = LeftmostEv never }
        onClose = leftmost
          [ onFinish
          , onEsc
          ]
        lConf = mempty & ideCfg_setModal .~ (LeftmostEv $ Nothing <$ onClose)

      usedModal <- headE $ updated $ _ide_modal ideL
      jsHackForStopPropagation $ () <$ usedModal

      pure $ lConf <> mCfg
  where
    isVisible = isJust <$> _ide_modal ideL

showModalBrutal
  :: MonadWidget t m
  => CssClass
  -> (Event t () -> m (Event t a, Event t ()))
  -> m (Event t a)
showModalBrutal modalCls theModal = do
  document <- DOM.currentDocumentUnchecked
  onEsc <- mkOnEsc document
  modalDisplay onEsc
  where
    modalDisplay onEsc = mdo
      dModalState <- holdDyn (Just theModal) $ Nothing <$ onFin
      (modalResult, onFin) <- elDynKlass "div" (mkModalWrapperClass . isJust <$> dModalState) $ mdo
        (backdropEl, ev) <- elClass' "div" "modal__screen" $
          divClass ("modal__dialog" <> " " <> renderClass modalCls) $ networkView $ ffor dModalState $ \case
            Nothing -> removePreventScrollClass $> (never, never) -- The modal is closed
            Just f -> addPreventScrollClass >> f onClose -- The modal is open
        onFinish <- switchHold never $ snd <$> ev
        modalOutput <- switchHold never $ fst <$> ev
        let
          onClose = leftmost
            [ onFinish
            , onEsc
            , domEvent Click backdropEl
            ]

        getPostBuild >>= jsHackForStopPropagation
        pure (modalOutput, onClose)
      pure modalResult
