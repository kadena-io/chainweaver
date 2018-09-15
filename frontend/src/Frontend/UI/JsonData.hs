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

import           Frontend.JsonData
import           Frontend.Foundation



-- | UI for managing JSON data.
uiJsonData :: MonadWidget t m => JsonData t -> m (JsonDataCfg t)
uiJsonData d = do
  onNewData <- tagOnPostBuild $ d ^. jsonData_rawInput
  onSetRawInput <- elClass "div" "editor" $ dataEditor "" onNewData

  elClass "div" "keyset-editor ui segment" $ do
    onCreateKeyset <- uiCreateKeyset $ d ^. jsonData_keysets

    pure $ JsonDataCfg
      { _jsonDataCfg_setRawInput = onSetRawInput
      , _jsonDataCfg_createKeyset = onCreateKeyset
      , _jsonDataCfg_addKey = never
      , _jsonDataCfg_delKey = never
      , _jsonDataCfg_delKeyset = never
      , _jsonDataCfg_setPred = never
      }

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

