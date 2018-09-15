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

import           Frontend.JsonData
import           Frontend.Foundation



-- | UI for managing JSON data.
uiJsonData :: MonadWidget t m => JsonData t -> m (JsonDataCfg t)
uiJsonData d = do
  onNewData <- tagOnPostBuild $ d ^. jsonData_rawInput
  -- let onNewData = updated $ d ^. jsonData_rawInput
  setRawInput <- dataEditor "" onNewData

  pure $ JsonDataCfg
    { _jsonDataCfg_setRawInput = setRawInput
    , _jsonDataCfg_createKeyset = never
    , _jsonDataCfg_addKey = never
    , _jsonDataCfg_delKey = never
    , _jsonDataCfg_delKeyset = never
    , _jsonDataCfg_setPred = never
    }

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

