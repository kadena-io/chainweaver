{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Common.Network where

import           Control.Applicative      ((<|>))
import           Control.Arrow            (first, (***))
import           Control.Arrow            (left)
import           Control.Error.Safe       (headErr)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except     (MonadError, liftEither, runExceptT)
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.Aeson               as A
import           Data.Coerce              (coerce)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe               (fromMaybe)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Void                (Void)
import           GHC.Generics             (Generic)
import           Obelisk.ExecutableConfig (get)
import           Safe                     (readMay)
import qualified Text.Megaparsec          as MP
import qualified Text.Megaparsec.Char     as MP
import qualified Text.URI                 as URI hiding (uriPath)
import           Text.URI.Lens

import           Common.Foundation
import           Common.RefPath           as MP
import qualified Pact.Types.ChainMeta     as Pact


newtype ChainId = ChainId { unChainId :: Word }
  deriving (Eq, Ord, Bounded, Enum, ToJSON, FromJSON)

instance Show ChainId where
  show (ChainId c) = show c

instance Read ChainId where
  readsPrec n = map (first ChainId) . readsPrec n

-- | Conversion to chain id type, used in `PublicMeta`
--
--   of Pact.
toPmChainId :: ChainId -> Pact.ChainId
toPmChainId (ChainId chainId) = Pact.ChainId $ tshow chainId


-- | Name that uniquely describes a valid network.
newtype NetworkName = NetworkName
  { unNetworkName :: Text
  }
  deriving (Generic, Eq, Ord, Show, Semigroup, Monoid, FromJSON, ToJSON, FromJSONKey, ToJSONKey)


-- | Render a network name as `Text`.
textNetworkName :: NetworkName -> Text
textNetworkName = coerce

-- | Reference for a node in a network.
newtype NodeRef = NodeRef
  { unNodeRef :: URI.Authority
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON NodeRef where
  parseJSON v = do
    t <- parseJSON v
    errVal <- runExceptT $ parseNodeRef t
    case errVal of
      Left err  -> fail $ T.unpack err
      Right val -> pure val

instance ToJSON NodeRef where
  toJSON = toJSON . renderNodeRef

instance IsRefPath ChainId where
  renderRef = mkRefPath . tshow . unChainId

  parseRef = ChainId . read . T.unpack <$> MP.satisfy isWord
    where
      isWord :: Text -> Bool
      isWord = isJust . readWordMay . T.unpack

      readWordMay :: String -> Maybe Word
      readWordMay = readMay


instance IsRefPath NodeRef where
  renderRef = mkRefPath . renderNodeRef

  parseRef = do
    v <- MP.anySingle
    errVal <- runExceptT $ parseNodeRef v
    case errVal of
      Left err  -> fail $ T.unpack err
      Right val -> pure val

-- | Reference a chain on a given node.
data ChainRef = ChainRef
  { _chainRef_node  :: Maybe NodeRef
    -- ^ The node the chain lives on. If `Nothing` we assume it lives on the
    -- currently selected network `_network_selectedNetwork`.
  , _chainRef_chain :: ChainId
    -- ^ The chain id.
  }
  deriving (Eq, Ord, Show, Generic)

instance IsRefPath ChainRef where
  renderRef (ChainRef n c) = maybe mempty renderRef n <> renderRef c
  parseRef =
    MP.try (ChainRef . Just <$> parseRef <*> parseRef)
    <|> (ChainRef Nothing <$> parseRef)

instance A.ToJSON ChainRef where
  toJSON = A.genericToJSON compactEncoding
  toEncoding = A.genericToEncoding compactEncoding

instance A.FromJSON ChainRef where
  parseJSON = A.genericParseJSON compactEncoding

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
getNetworksConfig :: IO (Either Text (NetworkName, Map NetworkName [NodeRef]))
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
parseNetworks :: MonadError Text m => Text -> m (NetworkName, Map NetworkName [NodeRef])
parseNetworks raw = do
  let
    rawNamesHosts :: [(Text, Text)]
    rawNamesHosts = map (fmap (T.dropWhile (== ':')) . T.breakOn ":") . T.lines $ raw

    strippedSplitted :: [(NetworkName, [Text])]
    strippedSplitted = map (NetworkName . T.strip *** T.words) rawNamesHosts

  --        outer list snd        host list
  parsed <- traverse (traverse (traverse parseNodeRef)) strippedSplitted
  defEntry <- liftEither $ headErr "Network list was empty."  parsed
  pure (fst defEntry, Map.fromList parsed)


-- | Parse an authority from Text, failing with an error message if that is not possible.
parseNodeRef :: forall m. MonadError Text m => Text -> m NodeRef
parseNodeRef t = do
    let
      -- This is just to make the URI parser perform as we need it:
      normalizedT = ("https://" <>) . fromMaybe t $ T.stripPrefix "http://" t <|> T.stripPrefix "https://" t

    uri <- parseLifted URI.parser normalizedT
    liftEither . left (const "Parsing hostname failed.") $ fmap NodeRef (uri ^. uriAuthority)

  where

    parseLifted :: forall a mp. MonadError Text mp => MP.Parsec Void Text a -> Text -> mp a
    parseLifted p s = liftEither . left (T.pack . show) $ MP.runParser p "uri" s


-- | Render an authority useful for serialization to disk.
renderNodeRef :: NodeRef -> Text
renderNodeRef (NodeRef (URI.Authority mUser h mp)) =
    maybe "" ((<> "@") . renderUser) mUser <> URI.unRText h <> maybe "" ((":" <>) . tshow) mp
  where
    renderUser (URI.UserInfo name mPw) = URI.unRText name <> maybe "" ((":" <>) . URI.unRText) mPw
