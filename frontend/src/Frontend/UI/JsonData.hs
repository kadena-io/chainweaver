{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

-- | UI for handling the data JSON object that gets attached to transactions.
--
--   The user gets an editor for entering arbitrary data, in additon a widget
--   for adding keysets based on the available keys is offered.
--
--   Copyright   :  (C) 2018 Kadena
--   License     :  BSD-style (see the file LICENSE)
--

module Frontend.UI.JsonData
  ( -- * Key management widget
    uiJsonData
  ) where

------------------------------------------------------------------------------
import           Control.Arrow         ((&&&))
import           Control.Lens
import qualified Data.Map              as Map
import           Data.Maybe
import           Data.Text             (Text)
import           Reflex.Dom.SemanticUI hiding (mainWidget)
import           Reflex.Dom.ACE.Extended
import           Reflex.Dom.Core             (keypress, _textInput_element)
import Language.Javascript.JSaddle (liftJSM, pToJSVal, js0)
import Control.Monad
import qualified Data.Text as T
import Reflex.Class.Extended

import           Frontend.JsonData
import           Frontend.Foundation



-- | UI for managing JSON data.
uiJsonData :: MonadWidget t m => JsonData t -> m (JsonDataCfg t)
uiJsonData d = do
  onNewData <- tagOnPostBuild $ d ^. jsonData_rawInput
  onSetRawInput <- elClass "div" "editor" $ dataEditor "" onNewData

  elClass "div" "keyset-editor ui segment" $ do
    ksCfg <- networkViewFlatten $ uiKeysets <$> d ^. jsonData_keysets
    onCreateKeyset <- uiCreateKeyset $ d ^. jsonData_keysets

    let
      baseCfg = JsonDataCfg
        { _jsonDataCfg_setRawInput = onSetRawInput
        , _jsonDataCfg_createKeyset = onCreateKeyset
        , _jsonDataCfg_addKey = never
        , _jsonDataCfg_delKey = never
        , _jsonDataCfg_delKeyset = never
        , _jsonDataCfg_setPred = never
        }
    pure $ baseCfg <> ksCfg

uiKeysets
  :: MonadWidget t m => DynKeysets t -> m (JsonDataCfg t)
uiKeysets ksM =
  case Map.toList ksM of
    []   -> do
      text "No keysets yet ..."
      pure mempty
    kss ->
      elClass "div" "ui relaxed middle aligned divided list" $ do
        rs <- traverse uiKeyset kss
        pure $ mconcat rs

-- | Display a single keyset on the screen.
uiKeyset
  :: MonadWidget t m => (KeysetName, DynKeyset t) -> m (JsonDataCfg t)
uiKeyset (n, ks) = elClass "div" "item" $ do
    (predIn, clicked) <- elClass "div" "right floated content" $ do
      onNewPred <- tagOnPostBuild $ ks ^. keyset_pred
      predIn <- textInput $ def
        & textInputConfig_value .~ SetValue "" (Just $ fromMaybe "" <$> onNewPred)
        & textInputConfig_placeholder .~ pure "keys-all"

      let
        buttonIcon = elClass "i" "large trash middle aligned icon" blank
      clicked <- flip button buttonIcon $ def
        & buttonConfig_emphasis .~ Static (Just Tertiary)
      pure (predIn, clicked)

    elClass "i" "large folder middle aligned icon" blank
    elClass "div" "content" $ do
      elClass "h4" "ui header" $ text n
      elClass "div" "description" $ text "Keyset"
    let
      notEmpty s = if T.null s then Nothing else Just s
      setPred = (n, ) . notEmpty <$> predIn ^. textInput_input
    pure $ mempty
      & jsonDataCfg_setPred .~ setPred
      & jsonDataCfg_delKeyset .~ fmap (const n) clicked


-- | Input widget with confirm button for creating a new keyset.
--
-- TODO: Unify this with key creation widget in "Frontend.UI.Wallet".
uiCreateKeyset
  :: MonadWidget t m => Dynamic t (DynKeysets t) -> m (Event t Text)
uiCreateKeyset ks = do
    elClass "div" "ui fluid action input" $ mdo
      name <- textInput $ def
          & textInputConfig_value .~ SetValue "" (Just $ "" <$ confirmed)
          & textInputConfig_placeholder .~ pure "Enter keyset name"

      let
        onEnter = keypress Enter name
        nameEmpty = (== "") <$> value name
        duplicate = Map.member <$> value name <*> ks

      clicked <- flip button (text "Create") $ def
        & buttonConfig_emphasis .~ Static (Just Secondary)
        & buttonConfig_disabled .~ Dyn ((||) <$> nameEmpty <*> duplicate)

      let
        confirmed = leftmost [ onEnter, clicked ]
        setFocus =
          liftJSM $ pToJSVal (_textInput_element name) ^. js0 ("focus" :: Text)

      void $ performEvent (setFocus <$ confirmed)
      pure $ tag (current $ _textInput_value name) confirmed


dataEditor
  :: MonadWidget t m
  => Text -> Event t Text
  -> m (Event t Text)
dataEditor iv sv = do
    let ac = def { _aceConfigMode = Just "ace/mode/json"
                 , _aceConfigElemAttrs = "class" =: "ace-data ace-widget"
                 }
    ace <- resizableAceWidget mempty ac (AceDynConfig $ Just AceTheme_SolarizedDark) iv sv
    return $ _extendedACE_onUserChange ace

