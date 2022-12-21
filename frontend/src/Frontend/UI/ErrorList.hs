{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | List showing live errors and warnings of `Editor` with quick fix buttons
--   if available.
-- Copyright   :  (C) 2020-2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.UI.ErrorList
  ( uiErrorList
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad           ((<=<))
import           Data.List.NonEmpty      (nonEmpty)
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.Editor
import           Frontend.Foundation
import           Frontend.UI.Widgets
------------------------------------------------------------------------------


-- | UI for live showing of errors and offering of quick fixes.
uiErrorList
  :: ( MonadWidget t m, HasEditorCfg mConf t, Monoid mConf
     , HasEditor model t
     )
  => model
  -> m mConf
uiErrorList m = do
  annotations <- holdUniqDyn $ m ^. editor_annotations
  dmd <- maybeDyn $ nonEmpty <$> annotations
  onQuickFix <- switchHold never <=< dyn $ ffor dmd $ \case
    Nothing -> pure never
    Just d -> accordionItem True "segment" "Errors" $ do
      elClass "div" "group" $ do
        quickFixes <- simpleList (toList <$> d) errorItem
        pure $ switchDyn $ fmap leftmost quickFixes
  pure $ mempty & editorCfg_applyQuickFix .~ onQuickFix

errorItem
  :: forall t m
  .  (MonadWidget t m)
  => Dynamic t Annotation -> m (Event t QuickFix)
errorItem da =
  switchHold never <=< dyn $ ffor da $ \a -> do
    elClass "div"  "error-list segment segment_type_small-primary" $ do
      elClass "div" "error-list__msg" $ do
          renderIcon $ _annotation_type a
          for_ (_annotation_pos a) $ \(r,c) ->
            elClass "div" "error-list__line-number" $ text $ tshow r <> ":" <> tshow c
          case _annotation_source a of
            AnnotationSource_Json -> text "[JSON] "
            AnnotationSource_Pact -> blank
          text $ _annotation_msg a
      renderQuickFix $ makeQuickFix a
  where
    renderQuickFix = maybe (pure never) $ \qf -> do
      let btnCls = ""
      onClick <- uiButton ( btnCfgTertiary
                 & uiButtonCfg_class %~ (<> btnCls)
                 & uiButtonCfg_title .~ Just (renderQuickFixTitle qf)
               ) $
        text "Fix"
      pure $ qf <$ onClick

    renderQuickFixTitle = \case
      QuickFix_UnreadableKeyset ks -> "Adds keyset '" <> ks <> "' to Env."
      QuickFix_UndefinedKeyset ks -> "Adds (define-keyset '" <> ks <> " ...) and also adds an empty keyset to Env."

    renderIcon t = elClass "div" ("icon error-list__icon " <> annoCls t) blank

    annoCls = \case
      AnnoType_Warning -> "icon_type_warning"
      AnnoType_Error -> "icon_type_error"
