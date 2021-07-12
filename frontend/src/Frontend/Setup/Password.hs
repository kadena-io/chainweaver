{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend.Setup.Password where

import Reflex.Dom.Core
import Reflex.Dom.Core
import Control.Monad.Fix (MonadFix)
import Data.Text (Text)
import Language.Javascript.JSaddle (MonadJSM, liftJSM)
import qualified Data.Text as T
import Frontend.Setup.Widgets
import Frontend.UI.Dialogs.ChangePassword (minPasswordLength)

import Control.Applicative (liftA2)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Frontend.Crypto.Class
import Frontend.UI.Widgets
import Frontend.Foundation


setPassword
  :: (DomBuilder t m, MonadHold t m, MonadFix m, PerformEvent t m, PostBuild t m, 
      MonadSample t (Performable m), MonadJSM (Performable m), TriggerEvent t m,
      BIP39Root key -- (Performable m)
      )
  => Dynamic t (Sentence key)
  -> m (Event t (Maybe (key, Password)))
setPassword dSentence = do
  let uiPassword' = uiPassword (setupClass "password-wrapper") (setupClass "password")

  p1elem <- uiPassword' $ "Enter password (" <> tshow minPasswordLength <> " character min.)"
  p2elem <- uiPassword' "Confirm password"

  p1Dirty <- holdUniqDyn =<< holdDyn False (True <$ _inputElement_input p1elem)
  p2Dirty <- holdUniqDyn =<< holdDyn False (True <$ _inputElement_input p2elem)

  let inputsDirty = current $ liftA2 (||) p1Dirty p2Dirty

  eCheckPassword <- fmap (gate inputsDirty) $ delay 0.2 $ leftmost
    [ _inputElement_input p1elem
    , _inputElement_input p2elem
    ]

  let (err, pass) = fanEither $
        checkPassword <$> current (fmap Password $ value p1elem) <*> current (fmap Password $ value p2elem) <@ eCheckPassword

  lastError <- holdDyn Nothing $ leftmost
    [ Just <$> err
    , Nothing <$ pass
    ]

  let dMsgClass = lastError <&> \m -> setupClass "password-message " <> case m of
        Nothing -> setupClass "hide-pw-error"
        Just _ -> setupClass "show-pw-error"

  elDynClass "div" dMsgClass $
    dynText $ fromMaybe T.empty <$> lastError

  --TODO: Make ed25519 func take Password type and unwrap there
  encryptedKeyAndPass <- performEvent $ ffor pass $ \p -> do
    sentence <- sample $ current dSentence 
    root <- deriveRoot p sentence
    return $ ffor root $ \r -> (r, p)

  pure $ leftmost
    [ Nothing <$ err
    , encryptedKeyAndPass
    ]

checkPassword :: Password -> Password -> Either Text Password
checkPassword p1'@(Password p1) (Password p2)
  | T.length p1 < minPasswordLength =
      Left $ "Passwords must be at least " <> tshow minPasswordLength <> " characters long"
  | p1 /= p2 =
      Left "Passwords must match"
  | otherwise =
      Right p1'
