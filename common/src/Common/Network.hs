{-# LANGUAGE OverloadedStrings #-}

module Common.Network where

import Data.Text (Text)
import Safe (readMay)
import Control.Arrow ((***), first)
import qualified Data.Text as T
import Obelisk.ExecutableConfig (get)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import           Data.Coerce                       (coerce)
import Control.Monad
import Control.Monad.Trans
import GHC.Generics (Generic)
import           Control.Error.Safe          (headErr, maximumErr)
import           Control.Monad.Except        (ExceptT (..), MonadError,
                                              liftEither, runExceptT,
                                              throwError)
import           Text.URI                    (URI (URI))
import qualified Text.URI                    as URI hiding (uriPath)
import qualified Text.Megaparsec             as MP
import qualified Text.Megaparsec.Char             as MP
import           Data.Void                   (Void)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import Control.Arrow (left)
import Control.Lens
import           Text.URI.Lens               as URI
import Data.Map (Map)
import qualified Data.Map as Map

import Common.RefPath as MP
import Common.Foundation


newtype ChainId = ChainId { unChainId :: Word }
  deriving (Eq, Ord, Bounded, Enum, ToJSON, FromJSON)

instance Show ChainId where
  show (ChainId c) = show c

instance Read ChainId where
  readsPrec n = map (first ChainId) . readsPrec n

-- | Conversion to chain id type, used in `PublicMeta`
--
--   of Pact.
toPmChainId :: ChainId -> Text
toPmChainId (ChainId chainId) = tshow chainId


-- | Name that uniquely describes a valid network.
newtype NetworkName = NetworkName
  { unNetworkName :: Text
  }
  deriving (Generic, Eq, Ord, Show, Semigroup, Monoid)


instance ToJSON NetworkName where
  toJSON = genericToJSON compactEncoding
  toEncoding = genericToEncoding compactEncoding

instance FromJSON NetworkName where
  parseJSON = genericParseJSON compactEncoding


-- | Render a network name as `Text`.
textNetworkName :: NetworkName -> Text
textNetworkName = coerce


instance IsRefPath NetworkName where
  renderRef = mkRefPath . unNetworkName

  parseRef = NetworkName <$> MP.anySingle


instance IsRefPath ChainId where
  renderRef = mkRefPath . tshow . unChainId

  parseRef = ChainId . read . T.unpack <$> MP.satisfy isWord
    where
      isWord :: Text -> Bool
      isWord = isJust . readWordMay . T.unpack

      readWordMay :: String -> Maybe Word
      readWordMay = readMay


-- | How many Pact instances to launch in development mode.
numPactInstances :: Int
numPactInstances = 1


-- | Get the port number a given pact instance is launched on.
--
--   Numeration starts at 1. So for `numPactInstances` == 2 we would have
--   instances with numbers 1 and 2.
getPactInstancePort :: Int -> Text
getPactInstancePort num = T.pack . show $ 7020 + num


-- | Where to find the network list configuration.
networksPath :: Text
networksPath = "config/common/networks"


-- | Retrieve the list of networks as specified in the static config.
--
--   Find and parse the networks configuration. If this file is not present,
--   pact-web will run in development mode.
getNetworksConfig :: IO (Either Text (NetworkName, Map NetworkName [URI.Authority]))
getNetworksConfig = runExceptT $ do
  raw <- note "Networks configuration could not be found." <=< lift . get $ networksPath
  parseNetworks raw


-- | Parse server list.
--
--
--   Format:
--
--   ```
--   networkName1: hostName1 hostName2
--   networkName2: hostName3:8080 hostName4 hostName 5
--   ...
--   ```
--
--   The first entry in the list will be considered the default, that gets initially chosen.
parseNetworks :: MonadError Text m => Text -> m (NetworkName, Map NetworkName [URI.Authority])
parseNetworks raw = do
  let
    rawNamesHosts :: [(Text, Text)]
    rawNamesHosts = map (fmap (T.dropWhile (== ':')) . T.breakOn ":") . T.lines $ raw

    strippedSplitted :: [(NetworkName, [Text])]
    strippedSplitted = map (NetworkName . T.strip *** T.words) rawNamesHosts

  --        outer list snd        host list
  parsed <- traverse (traverse (traverse parseAuthority)) strippedSplitted
  defEntry <- liftEither $ headErr "Network list was empty."  parsed
  pure (fst defEntry, Map.fromList parsed)


-- | Parse an authority from Text, failing with an error message if that is not possible.
parseAuthority :: MonadError Text m => Text -> m URI.Authority
parseAuthority t = do
    let
      -- This is just to make the URI parser perform as we need it:
      normalizedT = ("https://" <>) . fromMaybe t $ T.stripPrefix "http://" t <|> T.stripPrefix "https://" t

    uri <- parseLifted URI.parser normalizedT
    liftEither . left (const "Parsing hostname failed.") $ uri ^. uriAuthority

  where

    parseLifted :: forall a mp. MonadError Text mp => MP.Parsec Void Text a -> Text -> mp a
    parseLifted p s = liftEither . left (T.pack . show) $ MP.runParser p "uri" s
