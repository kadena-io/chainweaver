{-# LANGUAGE LambdaCase #-}
module Frontend.Storage
  ( getLocalStorage
  , getItemStorage
  , setItemStorage
  , GHCJS.Storage
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified GHCJS.DOM              as DOM
import qualified GHCJS.DOM.Storage      as GHCJS
import           GHCJS.DOM.Types        (JSString, MonadJSM, fromJSString,
                                         toJSString)
import qualified GHCJS.DOM.Window       as Window

-- | Get access to browser's local storage.
getLocalStorage :: MonadJSM m => m GHCJS.Storage
getLocalStorage = Window.getLocalStorage =<< DOM.currentWindowUnchecked

getItemStorage
  ::  (MonadIO m, Show (key result), FromJSON result, MonadJSM m)
  => GHCJS.Storage
  -> key result
  -> m (Maybe result)
getItemStorage storage key = (fromJsonString =<< ) <$> GHCJS.getItem storage (keyToString key)

setItemStorage
  :: (Show (key data'), ToJSON data', MonadJSM m)
  => GHCJS.Storage
  -> key data'
  -> data'
  -> m ()
setItemStorage storage key data' = GHCJS.setItem storage (keyToString key) (toJsonString data')


toJsonString :: ToJSON a => a -> JSString
toJsonString = toJSString . T.decodeUtf8 . BL.toStrict . Aeson.encode

fromJsonString :: FromJSON a => JSString -> Maybe a
fromJsonString = Aeson.decodeStrict . T.encodeUtf8 . fromJSString

keyToString :: Show a => a -> JSString
keyToString = toJSString . T.pack . show
