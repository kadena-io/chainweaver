module Frontend.Crypto.Password where

import Data.Text (Text)

newtype Password = Password { unPassword :: Text } deriving (Eq)
