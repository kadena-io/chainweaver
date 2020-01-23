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
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.UI.ErrorList
  ( uiErrorList
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Traversable        (for)
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
     , Flattenable mConf t , HasEditor model t
     )
  => model
  -> m mConf
uiErrorList m = do
    networkViewFlatten $ ffor (m ^. editor_annotations) $ handleEmpty $ \anns -> do
      accordionItem True "segment" "Errors" $ do
        onQuickFix <- errorList anns
        pure $ mempty & editorCfg_applyQuickFix .~ onQuickFix
  where
    handleEmpty x = \case
      [] -> pure mempty
      anns -> x anns


errorList
  :: forall t m
  .  (MonadWidget t m)
  => [Annotation] -> m (Event t QuickFix)
errorList annotations =
    elClass "div" "group" $ do
      fmap leftmost . for annotations $ \a ->
        elClass "div"  "error-list segment segment_type_small-primary" $ do
          elClass "div" "error-list__msg" $ do
              renderIcon $ _annotation_type a
              elClass "div" "error-list__line-number" $
                text $ tshow (_annotation_line a) <> ":" <> tshow (_annotation_column a) <> " "
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
