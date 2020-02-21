module Pact.Server.ApiClient.Helpers
  ( decodeForVersion
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import Text.Printf (printf)

decodeForVersion :: String -> Int -> (Aeson.Object -> Aeson.Parser a) -> Aeson.Value -> Aeson.Parser a
decodeForVersion lbl n parse = Aeson.withObject lbl $ \o -> do
  version <- o Aeson..: "version"
  if version == n
    then parse o
    else fail $ printf "Wrong %s version. Found: %i, Expected: %i" lbl version n
