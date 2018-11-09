{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}

-- | Widgets collection
-- Was based on semui, but now transitioning to custom widgets
module Frontend.UI.Widgets
  ( validatedInputWithButton
  , module Old
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Language.Javascript.JSaddle (js0, liftJSM, pToJSVal)
import           Reflex.Dom.Core
------------------------------------------------------------------------------
import           Frontend.Foundation
import           Frontend.UI.Button
import           Frontend.Widgets            as Old
------------------------------------------------------------------------------


-- | Validated input with button
validatedInputWithButton
  :: MonadWidget t m
  => (Text -> PushM t (Maybe Text))
  -- ^ Validation function returning `Just error message` on error.
  -> Text -- ^ Placeholder
  -> Text -- ^ Button text
  -> m (Event t Text)
validatedInputWithButton check placeholder buttonText = do
    (update, checked) :: (Event t Text, Dynamic t (Maybe Text)) <- elClass "div" "fieldset" $ mdo
      name <- inputElement $ def
          & inputElementConfig_setValue .~ (T.empty <$ confirmed)
          & initialAttributes .~ ("placeholder" =: placeholder <> "type" =: "text")
      let
        nameVal = T.strip <$> _inputElement_value name
        onEnter = keypress Enter name
        nameEmpty = (== "") <$> nameVal

      checkedL <- holdDyn Nothing $ pushAlways check $ updated nameVal
      let
        checkFailed = isJust <$> checkedL
        btnCfg = def & uiButtonCfg_disabled .~ liftA2 (||) nameEmpty checkFailed


      (clicked, _) <- uiButton btnCfg $ text buttonText

      let confirmed = leftmost [ onEnter, clicked ]
      void $ performEvent (liftJSM (pToJSVal (_inputElement_raw name) ^.  js0 ("focus" :: String)) <$ confirmed)
      pure $ (tag (current nameVal) confirmed, checkedL)

    elClass "span" "error" $ dynText $ fromMaybe "" <$> checked

    pure update
