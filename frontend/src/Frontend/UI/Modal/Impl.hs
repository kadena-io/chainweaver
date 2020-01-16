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

-- | Show the current modal dialog as given in the model.
showModal :: forall key t m. MonadWidget t m => ModalIde m key t -> m (ModalIdeCfg m key t)
showModal ideL = do
    document <- DOM.currentDocumentUnchecked

    onEsc <- wrapDomEventMaybe document (`EventM.on` Events.keyDown) $ do
      key <- getKeyEvent
      pure $ if keyCodeLookup (fromIntegral key) == Escape then Just () else Nothing

    let mkCls vis = "modal" <>
          if vis then "modal_open" else mempty

        -- This function is equivalent to the following JS for discovering the padding
        -- required for the vertical scrollbar.
        --
        -- (window.innerWidth - document.getElementsByTagName('html')[0].clientWidth)
        getScrollBarWidth = liftJSM $ do
          innerW <- JSaddle.jsg "window" ! "innerWidth" >>= JSaddle.valToNumber
          clientWidth <- (JSaddle.jsg "document" # "getElementsByTagName") ["html"]
            >>= (!! 0)
            >>= (! "clientWidth")
            >>= JSaddle.valToNumber
          pure $ innerW - clientWidth

        addPreventScrollClass = withBody $ \b -> do
          w <- getScrollBarWidth
          _ <- b # "setAttribute" $ [modalBodyAttr, "true"]
          -- Account for the differing widths of scroll bars.
          JSaddle.toJSVal (tshow w <> "px") >>= setBodyStyle "paddingRight"

        removePreventScrollClass = withBody $ \b -> do
          -- Reset the padding back now that the scrollbar might be back.
          JSaddle.toJSVal 0 >>= setBodyStyle "paddingRight"
          b # "removeAttribute" $ [modalBodyAttr]

    elDynKlass "div" (mkCls <$> isVisible) $ mdo
      (backdropEl, ev) <- elClass' "div" "modal__screen" $
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
          , domEvent Click backdropEl
          ]
        lConf = mempty & ideCfg_setModal .~ (LeftmostEv $ Nothing <$ onClose)

      -- We can't use jsaddle to set the stopPropagation handler: with
      -- jsaddle-warp, there is a race condition that can cause the handler for
      -- the outer div to run before the stopPropagation is processed, closing
      -- the modal unexpectedly. This is particularly noticable under heavy
      -- network load.
      -- This hack just ensures this handler runs immediately and clicks can't
      -- slip through the modal__dialog container.
      -- We add the listener upon the first launch of the modal to ensure that
      -- modal__dialog actually exists.
      usedModal <- headE $ updated $ _ide_modal ideL
      performEvent_ $ ffor usedModal $ \_ -> void $ DOM.liftJSM $ do
        JSaddle.eval "document.querySelector('.modal__dialog').addEventListener('click', function(e) { e.stopPropagation(); });"

      pure $ lConf <> mCfg
  where
    isVisible = isJust <$> _ide_modal ideL
