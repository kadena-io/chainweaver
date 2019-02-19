{-# LANGUAGE LambdaCase #-}
module Frontend.Storage
  ( localStorage
  , sessionStorage
  , getItemStorage
  , setItemStorage
  , removeItemStorage
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
localStorage :: MonadJSM m => m GHCJS.Storage
localStorage = Window.getLocalStorage =<< DOM.currentWindowUnchecked

-- | Get access to browser's session storage.
sessionStorage :: MonadJSM m => m GHCJS.Storage
sessionStorage = Window.getSessionStorage =<< DOM.currentWindowUnchecked

getItemStorage
  ::  (MonadIO m, Show (key result), FromJSON result, MonadJSM m)
  => m GHCJS.Storage
  -> key result
  -> m (Maybe result)
getItemStorage getStorage key = do
  storage <- getStorage
  (fromJsonString =<< ) <$> GHCJS.getItem storage (keyToString key)

setItemStorage
  :: (Show (key data'), ToJSON data', MonadJSM m)
  => m GHCJS.Storage
  -> key data'
  -> data'
  -> m ()
setItemStorage getStorage key data' = do
  storage <- getStorage
  GHCJS.setItem storage (keyToString key) (toJsonString data')

removeItemStorage
  ::  (MonadIO m, Show (key result), MonadJSM m)
  => m GHCJS.Storage
  -> key result
  -> m ()
removeItemStorage getStorage key = do
  storage <- getStorage
  GHCJS.removeItem storage (keyToString key)


toJsonString :: ToJSON a => a -> JSString
toJsonString = toJSString . T.decodeUtf8 . BL.toStrict . Aeson.encode

fromJsonString :: FromJSON a => JSString -> Maybe a
fromJsonString = Aeson.decodeStrict . T.encodeUtf8 . fromJSString

keyToString :: Show a => a -> JSString
keyToString = toJSString . T.pack . show
