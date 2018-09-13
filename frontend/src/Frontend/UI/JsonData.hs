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
  -- onNewData <- tagOnPostBuild $ d ^. jsonData_rawInput
  let onNewData = updated $ d ^. jsonData_rawInput
  editorText <- dataEditor "" onNewData

  let setRawInput = updated editorText
  pure $ JsonDataCfg
    { _jsonDataCfg_setRawInput = setRawInput
    }

dataEditor
  :: MonadWidget t m
  => Text -> Event t Text
  -> m (Dynamic t Text)
dataEditor iv sv = do
    let ac = def { _aceConfigMode = Just "ace/mode/json"
                 , _aceConfigElemAttrs = "class" =: "ace-data ace-widget"
                 }
    ace <- resizableAceWidget mempty ac (AceDynConfig $ Just AceTheme_SolarizedDark) iv
    _ <- withAceInstance ace (setValueACE <$> sv)
    return $ aceValue ace

