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

module Frontend.UI.Modal where

------------------------------------------------------------------------------
import           Control.Lens hiding (element)
import qualified Data.Dependent.Map as DMap
import           Data.Map                    (Map)
import           Data.Text                   (Text)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as Events
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Foundation
import           Frontend.Ide
import           Frontend.JsonData
import           Frontend.RightPanel
import           Frontend.UI.Button
import           Frontend.UI.Dialogs.DeployConfirmation
import           Frontend.UI.JsonData
import           Frontend.UI.Repl
import           Frontend.UI.Wallet
import           Frontend.Wallet
import           Frontend.Widgets
------------------------------------------------------------------------------

showModal :: forall t m. MonadWidget t m => Ide t -> m (IdeCfg t)
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
        networkView (specificModal <$> _ide_modal ideL)
      onFinish <- switchHold never $ snd <$> ev
      mCfg <- flatten $ fst <$> ev

      let
        onClose = leftmost [ onFinish
                           , onEsc
                           , domEvent Click backdropEl
                           ]
        lConf = mempty & ideCfg_reqModal .~ (Modal_NoModal <$ onClose)
      pure $ lConf <> mCfg
  where
    isVisible = getIsVisible <$> _ide_modal ideL
    getIsVisible = \case
      Modal_NoModal -> False
      _             -> True

    specificModal :: Modal -> m (IdeCfg t, Event t ())
    specificModal = \case
      Modal_NoModal -> pure (mempty, never)
      Modal_DeployConfirmation -> do
        (aCfg, (onCancel, onAccept), onClose) <- genericModalBody
          (text "Deployment Settings")
          (uiDeployConfirmation ideL)
          (genericModalFooter "Cancel" "Deploy")
        let bCfg = mempty & ideCfg_deploy .~ onAccept
        pure (aCfg <> bCfg, leftmost [onClose, onCancel, onAccept])

genericModalBody
  :: MonadWidget t m
  => m ()
  -- ^ The modal header
  -> m a
  -- ^ The modal body
  -> m b
  -- ^ Tho modal footer
  -> m (a,b,Event t ())
  -- ^ Returns the body value, footer value, and a close event
genericModalBody header body footer = do
    elAttrStopPropagationNS Click "div" ("class" =: "modal") $ do
      onClose <- divClass "modal-header" $
        el "h2" $ do
          header
          (e,_) <- elAttr' "button" ("class" =: "modal-close") $ text "x"
          return $ domEvent Click e
      bres <- divClass "modal-body" body
      fres <- divClass "modal-footer" footer
      return (bres, fres, onClose)

-- TODO Might need to generalize this to m () instead of Text later
genericModalFooter
  :: MonadWidget t m
  => Text
  -- ^ Text for action A
  -> Text
  -- ^ Text for action B
  -> m (Event t (), Event t ())
  -- ^ Events for actions A and B respectively
genericModalFooter actionA actionB = do
  (a,_) <- el' "button" $ text actionA
  text " "
  (b,_) <- el' "button" $ text actionB
  return (domEvent Click a, domEvent Click b)

-- Copied from Reflex.Dom.Old and modified to allow specifying attrs
elAttrStopPropagationNS
  :: forall t m en a. (MonadWidget t m)
  => EventName en
  -> Text
  -> Map AttributeName Text
  -> m a
  -> m a
elAttrStopPropagationNS en elementTag attrs child = do
  let f = GhcjsEventFilter $ \_ -> do
        return (stopPropagation, return Nothing)
      cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec . ghcjsEventSpec_filters %~ DMap.insert en f
        & elementConfig_initialAttributes .~ attrs
  snd <$> element elementTag cfg child
