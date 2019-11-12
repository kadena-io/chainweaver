{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

module Common.Network
  ( NetworkName
  , uncheckedNetworkName
  , textNetworkName
  , mkNetworkName
  , networksPath
  , ChainRef(..)
  , NodeRef(..)
  , renderNodeRef
  , parseNodeRef
  , numPactInstances
  , getNetworksConfig
  , getPactInstancePort
  , parseNetworks
  , Pact.ChainId (..)
  , wrapWithBalanceChecks
  , parseWrappedBalanceChecks
  , AccountBalance(..)
  ) where

import Control.Applicative ((<|>))
import Control.Arrow (left)
import Control.Error.Safe (headErr)
import Control.Lens
import Control.Monad
import Control.Monad.Except (MonadError, liftEither, runExceptT, throwError, withExceptT)
import Control.Monad.Trans
import Data.Aeson
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Decimal (Decimal)
import Data.Default
import Data.Either (rights)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Traversable (for)
import Data.Void (Void)
import GHC.Generics (Generic)
import Kadena.SigningApi (AccountName(..), mkAccountName)
import Obelisk.Configs
import Pact.Compile (compileExps, mkEmptyInfo)
import Pact.Parse
import Pact.Types.Exp
import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Types.Term hiding (PublicKey)
import Pact.Types.Type
import Text.URI.Lens

import qualified Data.Aeson as A
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Pact.Types.ChainId as Pact
import qualified Text.Megaparsec as MP
import qualified Text.URI as URI hiding (uriPath)

import Common.Api
import Common.Orphans ()
import Common.Foundation
import Common.RefPath as MP

-- | Account balance wrapper
newtype AccountBalance = AccountBalance { unAccountBalance :: Decimal } deriving (Eq, Ord, Num)

-- | Name that uniquely describes a valid network.
newtype NetworkName = NetworkName
  { unNetworkName :: Text
  } deriving (Eq, Ord, Show)

instance FromJSON NetworkName where
  parseJSON = either (fail . T.unpack) pure . mkNetworkName <=< parseJSON
instance FromJSONKey NetworkName where
instance ToJSON NetworkName where
  toJSON = toJSON . unNetworkName
instance ToJSONKey NetworkName where

-- | Construct a 'NetworkName', and banish mainnet - for now.
mkNetworkName :: Text -> Either Text NetworkName
mkNetworkName (T.strip -> t)
  | "mainnet" `T.isInfixOf` t = Left "This wallet does not support mainnet"
  | otherwise = Right $ NetworkName t

-- | Construct a 'NetworkName' but don't perform any checks
uncheckedNetworkName :: Text -> NetworkName
uncheckedNetworkName = NetworkName . T.strip

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
  , _chainRef_chain :: Pact.ChainId
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


-- | Where to find the network list configuration
networksPath :: Text
networksPath = "common/networks"


-- | Retrieve the list of networks as specified in the static config.
--
--   Find and parse the networks configuration. If this file is not present,
--   chainweaver will run in development mode.
getNetworksConfig :: HasConfigs m => m (Either (Either Text Text) (NetworkName, Map NetworkName [NodeRef]))
getNetworksConfig = runExceptT $ do
  raw <- note (Left "Networks configuration could not be found.") <=< lift $ getTextCfg networksPath
  case T.stripPrefix "remote-source:" raw of
    Just r -> throwError $ Right r
    Nothing -> withExceptT Left $ parseNetworks raw

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
    strippedSplitted = rights $ map (\(n, refs) -> (, T.words refs) <$> mkNetworkName n) rawNamesHosts

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

-- | Helper function for compiling pact code to a list of terms
compileCode :: Text -> Either String [Term Name]
compileCode = first show . compileExps mkEmptyInfo <=< parseExprs

-- | Parse the balance checking object into a map of account balance changes and
-- the result from the inner code
parseWrappedBalanceChecks :: PactValue -> Either Text (Map AccountName AccountBalance, PactValue)
parseWrappedBalanceChecks = first ("parseWrappedBalanceChecks: " <>) . \case
  (PObject (ObjectMap obj)) -> do
    let lookupErr k = case Map.lookup (FieldKey k) obj of
          Nothing -> Left $ "Missing key '" <> k <> "' in map: " <> renderCompactText (ObjectMap obj)
          Just v -> pure v
    before <- parseAccountBalances =<< lookupErr "before"
    result <- parseResults =<< lookupErr "results"
    after <- parseAccountBalances =<< lookupErr "after"
    pure (Map.unionWith subtract before after, result)
  v -> Left $ "Unexpected PactValue (expected object): " <> renderCompactText v

-- | Turn the object of account->balance into a map
parseAccountBalances :: PactValue -> Either Text (Map AccountName AccountBalance)
parseAccountBalances = first ("parseAccountBalances: " <>) . \case
  (PObject (ObjectMap obj)) -> do
    m <- for (Map.toAscList obj) $ \(FieldKey accountText, pv) -> do
      bal <- case pv of
        PLiteral (LDecimal d) -> pure d
        t -> Left $ "Unexpected PactValue (expected decimal): " <> renderCompactText t
      acc <- mkAccountName accountText
      pure (acc, AccountBalance bal)
    pure $ Map.fromList m
  v -> Left $ "Unexpected PactValue (expected object): " <> renderCompactText v

-- | Get the last result, as we would under unwrapped deployment.
parseResults :: PactValue -> Either Text PactValue
parseResults = first ("parseResults: " <>) . \case
  PList vec -> maybe (Left "No value returned") Right $ vec ^? _last
  v -> Left $ "Unexpected PactValue (expected list): " <> renderCompactText v

-- | Wrap the code with a let binding to get the balances of the given accounts
-- before and after executing the code.
wrapWithBalanceChecks :: Set AccountName -> Text -> Either String Text
wrapWithBalanceChecks accounts code = wrapped <$ compileCode code
  where
    getBalance :: AccountName -> (FieldKey, Term Name)
    getBalance acc = (FieldKey (unAccountName acc), TApp
      { _tApp = App
        { _appFun = TVar (QName $ QualifiedName "coin" "get-balance" def) def
        , _appArgs = [TLiteral (LString $ unAccountName acc) def]
        , _appInfo = def
        }
      , _tInfo = def
      })
    -- Produce an object from account names to account balances
    accountBalances = TObject
      { _tObject = Pact.Types.Term.Object
        { _oObject = ObjectMap $ Map.fromList $ map getBalance $ Set.toAscList accounts
        , _oObjectType = TyPrim TyDecimal
        , _oKeyOrder = Nothing
        , _oInfo = def
        }
      , _tInfo = def
      }
    -- It would be nice to parse and compile the code and shove it into a
    -- giant 'Term' so we can serialise it, but 'pretty' is not guaranteed to
    -- produce valid pact code. It at least produces bad type sigs and let
    -- bindings.
    -- Thus we build this from a string and use 'pretty' where appropriate.
    -- We do need to make sure the code compiles before splicing it in.
    -- The order of execution is the same as the order of the bound variables.
    wrapped = T.unlines
      [ "(let"
      , "  ((before " <> renderCompactText accountBalances <> ")"
      , "   (results ["
      , code
      , "   ])"
      , "   (after " <> renderCompactText accountBalances <> "))"
      , "   {\"after\": after, \"results\": results, \"before\": before})"
      ]

