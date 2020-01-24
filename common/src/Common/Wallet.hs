{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Wallet
  ( PublicKey(..)
  , unsafePublicKey
  , fromPactPublicKey

  , textToKey
  , keyToText
  , parsePublicKey
  , toPactPublicKey
  , KeyPair(..)
  , AccountName(..)
  , AccountBalance(..)
  , _AccountBalance
  , AddressKeyset(..)
  , mkAddressKeyset
  , addressKeysetObject
  , addressKeyset_keys
  , addressKeyset_pred
  , toPactKeyset
  , fromPactKeyset
  , AccountNotes (unAccountNotes)
  , mkAccountNotes
  , AccountGuard(..)
  , UnfinishedCrossChainTransfer(..)
  , KeyStorage
  , AccountStatus (..)
  , _AccountStatus_Exists
  , _AccountStatus_Unknown
  , _AccountStatus_DoesNotExist
  , AccountDetails (..)
  , accountDetails_balance
  , accountDetails_keyset
    -- * Prisms for working directly with Account
  , AccountStorage(..)
  , _AccountStorage
  , Accounts(..)
  , accounts_vanity
  , AccountInfo(..)
  , accountInfo_chains
  , accountInfo_notes
  , Key(..)
  , filterKeyPairs
  , VanityAccount(..)
  , vanityAccount_notes
  , vanityAccount_unfinishedCrossChainTransfer
  , blankVanityAccount
  , pactGuardTypeText
  , fromPactGuard
  -- * Util
  , throwDecodingErr
  , decodeBase16M
  , lenientLookup
  -- * Balance checks
  , wrapWithBalanceChecks
  , parseWrappedBalanceChecks
  , getDetailsCode
  , accountDetailsObject
  , parseAccountDetails
  ) where

import Control.Applicative (liftA2)
import Control.Monad.Fail (MonadFail)
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Except (MonadError, throwError)
import Control.Newtype.Generics    (Newtype (..))
import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Decimal (Decimal)
import Data.Default
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Traversable (for)
import GHC.Generics (Generic)
import Kadena.SigningApi (AccountName(..), mkAccountName)
import Pact.Compile (compileExps, mkEmptyInfo)
import Pact.Parse
import Pact.Types.Command (RequestKey)
import Pact.Types.ChainId
import Pact.Types.Exp
import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Types.Term hiding (PublicKey)
import Pact.Types.Type
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.HashMap.Lazy as HM
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Pact.Types.Term as Pact
import qualified Pact.Types.Type as Pact

import Common.Foundation
import Common.Network (NetworkName)
import Common.Orphans ()

-- | PublicKey with a Pact compatible JSON representation.
newtype PublicKey = PublicKey ByteString
  deriving (Generic, Eq, Ord, Show)

-- | Input must be base16
unsafePublicKey :: ByteString -> PublicKey
unsafePublicKey = PublicKey

fromPactPublicKey :: Pact.PublicKey -> PublicKey
fromPactPublicKey = PublicKey . fst . Base16.decode . Pact._pubKey

toPactPublicKey :: PublicKey -> Pact.PublicKey
toPactPublicKey (PublicKey pk) = Pact.PublicKey $ Base16.encode pk

instance Newtype PublicKey

instance FromJSONKey PublicKey where
  fromJSONKey = PublicKey . fst . Base16.decode . T.encodeUtf8 <$> fromJSONKey

instance ToJSONKey PublicKey where
  toJSONKey = toJSONKeyText keyToText

instance ToJSON PublicKey where
  toEncoding = toEncoding . keyToText
  toJSON = toJSON . keyToText

instance FromJSON PublicKey where
  parseJSON = textToKey <=< parseJSON

-- | Display key in Base16 format, as expected by older Pact versions.
--
--   Despite the name, this function is also used for serializing signatures.
keyToText :: (Newtype key, O key ~ ByteString) => key -> Text
keyToText = T.decodeUtf8 . Base16.encode . unpack

-- | Read a key in Base16 format, as expected by older Pact versions.
--
--   Despite the name, this function is also used for reading signatures.
textToKey
  :: (Newtype key, O key ~ ByteString, Monad m, MonadFail m)
  => Text
  -> m key
textToKey = fmap pack . decodeBase16M . T.encodeUtf8

-- | Decode a Base16 value in a MonadFail monad and fail if there is input that
-- cannot be parsed.
decodeBase16M :: (Monad m, MonadFail m) => ByteString -> m ByteString
decodeBase16M i =
  let
    (r, rest) = Base16.decode i
  in
    if BS.null rest
       then pure r
       else fail "Input was not valid Base16 encoding."

-- | Parse just a public key with some sanity checks applied.
parsePublicKey :: MonadError Text m => Text -> m PublicKey
parsePublicKey = throwDecodingErr . textToKey <=< checkPub . T.strip

throwDecodingErr
  :: MonadError Text m
  => Maybe v
  -> m v
throwDecodingErr = throwNothing $ T.pack "Invalid base16 encoding"
  where
    throwNothing err = maybe (throwError err) pure

checkPub :: MonadError Text m => Text -> m Text
checkPub t = void (throwEmpty t) >> throwWrongLength 64 t
  where
    throwEmpty k =
      if T.null k
         then throwError $ T.pack "Key must not be empty"
         else pure k

-- | Check length of string key representation.
throwWrongLength :: MonadError Text m => Int -> Text -> m Text
throwWrongLength should k =
  if T.length k /= should
     then throwError $ T.pack "Key has unexpected length"
     else pure k

-- | Account balance wrapper
newtype AccountBalance = AccountBalance { unAccountBalance :: Decimal } deriving (Eq, Ord, Num, Show)

-- Via ParsedDecimal
instance ToJSON AccountBalance where
  toJSON = toJSON . ParsedDecimal . unAccountBalance
instance FromJSON AccountBalance where
  parseJSON x = (\(ParsedDecimal d) -> AccountBalance d) <$> parseJSON x

data AddressKeyset = AddressKeyset
  { _addressKeyset_keys :: Set PublicKey
  , _addressKeyset_pred :: Text
  } deriving (Eq,Ord,Show)

mkAddressKeyset :: Set PublicKey -> Text -> Maybe AddressKeyset
mkAddressKeyset keys predicate
  | Set.null keys = Nothing
  | otherwise = Just $ AddressKeyset
    { _addressKeyset_keys = keys
    , _addressKeyset_pred = predicate
    }

addressKeysetObject :: AddressKeyset -> Aeson.Object
addressKeysetObject o = HM.fromList
  [ "keys" .= _addressKeyset_keys o
  , "pred" .= _addressKeyset_pred o
  ]

toPactKeyset :: AddressKeyset -> Pact.KeySet
toPactKeyset ak = Pact.KeySet
  { Pact._ksKeys = Set.map toPactPublicKey $ _addressKeyset_keys ak
  , Pact._ksPredFun = Pact.Name $ Pact.BareName (_addressKeyset_pred ak) def
  }

fromPactKeyset :: Pact.KeySet -> AddressKeyset
fromPactKeyset ak = AddressKeyset
  { _addressKeyset_keys = Set.map fromPactPublicKey $ Pact._ksKeys ak
  , _addressKeyset_pred = renderCompactText $ Pact._ksPredFun ak
  }

instance ToJSON AddressKeyset where
  toJSON = Aeson.Object . addressKeysetObject

instance FromJSON AddressKeyset where
  parseJSON = withObject "AddressKeyset" $ \o -> AddressKeyset
    <$> o .: "keys"
    <*> o .: "pred"

data AccountStatus a
  = AccountStatus_Unknown
  | AccountStatus_DoesNotExist
  | AccountStatus_Exists a
  deriving (Eq, Show, Functor)

data AccountDetails = AccountDetails
  { _accountDetails_balance :: AccountBalance
  , _accountDetails_keyset :: AddressKeyset
  } deriving (Eq, Show)

makePactLenses ''AccountDetails
makePactLenses ''AddressKeyset
makePactPrisms ''AccountStatus
makePactPrisms ''AccountBalance


-- | A key consists of a public key and an optional private key.
--
data KeyPair key = KeyPair
  { _keyPair_publicKey  :: PublicKey
  , _keyPair_privateKey :: Maybe key
  } deriving (Generic, Show, Eq)

instance ToJSON key => ToJSON (KeyPair key) where
  toJSON p = object
    [ "public" .= _keyPair_publicKey p
    , "private" .= _keyPair_privateKey p
    ]

instance FromJSON key => FromJSON (KeyPair key) where
  parseJSON = withObject "KeyPair" $ \o -> do
    public <- o .: "public"
    private <- o .: "private"
    pure $ KeyPair
      { _keyPair_publicKey = public
      , _keyPair_privateKey = private
      }

makePactLenses ''KeyPair

-- | Account guards. We split this out here because we are only really
-- interested in keyset guards right now. Someday we might end up replacing this
-- with pact's representation for guards directly.
data AccountGuard
  = AccountGuard_KeySet Pact.KeySet
  -- ^ Keyset guards
  | AccountGuard_Other Pact.GuardType
  -- ^ Other types of guard
  deriving (Show, Generic)

fromPactGuard :: Pact.Guard a -> AccountGuard
fromPactGuard = \case
  Pact.GKeySet ks -> AccountGuard_KeySet ks
  g -> AccountGuard_Other $ Pact.guardTypeOf g

pactGuardTypeText :: Pact.GuardType -> Text
pactGuardTypeText = \case
  Pact.GTyKeySet -> "Keyset"
  Pact.GTyKeySetName -> "Keyset Name"
  Pact.GTyPact -> "Pact"
  Pact.GTyUser -> "User"
  Pact.GTyModule -> "Module"

instance FromJSON AccountGuard
instance ToJSON AccountGuard

-- | Account notes wrapper
newtype AccountNotes = AccountNotes { unAccountNotes :: Text } deriving (Eq, Show)

-- | We currently don't have any validation for the notes on an account, but should we
-- decide to add it then this will come in handy.
mkAccountNotes :: Text -> Maybe AccountNotes
mkAccountNotes "" = Nothing
mkAccountNotes t = Just $ AccountNotes t

instance ToJSON AccountNotes where
  toJSON = toJSON . unAccountNotes
instance FromJSON AccountNotes where
  parseJSON = maybe (fail "AccountNotes empty") pure . mkAccountNotes <=< parseJSON

data UnfinishedCrossChainTransfer = UnfinishedCrossChainTransfer
  { _unfinishedCrossChainTransfer_requestKey :: RequestKey
  , _unfinishedCrossChainTransfer_recipientChain :: ChainId
  , _unfinishedCrossChainTransfer_recipientAccount :: AccountName
  -- ^ Informational only
  , _unfinishedCrossChainTransfer_amount :: Decimal
  -- ^ Informational only
  } deriving (Eq, Show)

instance ToJSON UnfinishedCrossChainTransfer where
  toJSON ucct = object
    [ "requestKey" .= _unfinishedCrossChainTransfer_requestKey ucct
    , "recipientChain" .= _unfinishedCrossChainTransfer_recipientChain ucct
    , "recipientAccount" .= _unfinishedCrossChainTransfer_recipientAccount ucct
    , "amount" .= ParsedDecimal (_unfinishedCrossChainTransfer_amount ucct)
    ]

instance FromJSON UnfinishedCrossChainTransfer where
  parseJSON = withObject "UnfinishedCrossChainTransfer" $ \o -> do
    requestKey <- o .: "requestKey"
    recipientChain <- o .: "recipientChain"
    recipientAccount <- o .: "recipientAccount"
    ParsedDecimal amount <- o .: "amount"
    pure $ UnfinishedCrossChainTransfer
      { _unfinishedCrossChainTransfer_requestKey = requestKey
      , _unfinishedCrossChainTransfer_recipientChain = recipientChain
      , _unfinishedCrossChainTransfer_recipientAccount = recipientAccount
      , _unfinishedCrossChainTransfer_amount = amount
      }

data Key key = Key
  { _key_pair :: KeyPair key
  -- TODO consider removing the 'Maybe' in keypair and removing this type
  } deriving (Eq, Show)

instance ToJSON key => ToJSON (Key key) where
  toJSON k = object
    [ "pair" .= toJSON (_key_pair k)
    ]

instance FromJSON key => FromJSON (Key key) where
  parseJSON = withObject "Key" $ \o -> do
    pair <- o .: "pair"
    pure $ Key
      { _key_pair = pair
      }


type KeyStorage key = IntMap (Key key)

filterKeyPairs :: Set PublicKey -> IntMap (Key key) -> [KeyPair key]
filterKeyPairs s m = Map.elems $ Map.restrictKeys (toMap m) s
  where toMap = Map.fromList . fmap (\k -> (_keyPair_publicKey $ _key_pair k, _key_pair k)) . IntMap.elems

newtype Accounts = Accounts
  { _accounts_vanity :: Map AccountName (AccountInfo VanityAccount)
  } deriving (Eq, Show)

instance Semigroup Accounts where
  a1 <> a2 = Accounts
    { _accounts_vanity = Map.unionWith (<>) (_accounts_vanity a1) (_accounts_vanity a2)
    }

instance Monoid Accounts where
  mempty = Accounts mempty

instance ToJSON Accounts where
  toJSON as = object
    [ "vanity" .= toJSON (_accounts_vanity as)
    ]

instance FromJSON Accounts where
  parseJSON = withObject "Accounts" $ \o -> do
    vanity <- o .: "vanity"
    pure $ Accounts
      { _accounts_vanity = vanity
      }

data AccountInfo a = AccountInfo
  { _accountInfo_notes :: Maybe AccountNotes
  , _accountInfo_chains :: Map ChainId a
  } deriving (Functor, Eq, Show)

instance Semigroup (AccountInfo a) where
  AccountInfo notes chains <> AccountInfo _notes chains' = AccountInfo
    { _accountInfo_notes = notes
    , _accountInfo_chains = Map.union chains chains'
    }

instance Monoid (AccountInfo a) where
  mempty = AccountInfo Nothing mempty

instance ToJSON a => ToJSON (AccountInfo a) where
  toJSON as = object $ catMaybes
    [ (("notes" .=) . toJSON) <$> _accountInfo_notes as
    , Just $ "chains" .= toJSON (_accountInfo_chains as)
    ]

instance FromJSON a => FromJSON (AccountInfo a) where
  parseJSON = withObject "AccountInfo" $ \o -> do
    notes <- lenientLookup o "notes"
    chains <- o .: "chains"
    pure $ AccountInfo
      { _accountInfo_notes = notes
      , _accountInfo_chains = chains
      }


data VanityAccount = VanityAccount
  { _vanityAccount_notes :: Maybe AccountNotes
  , _vanityAccount_unfinishedCrossChainTransfer :: Maybe UnfinishedCrossChainTransfer
  } deriving (Show, Eq)

blankVanityAccount :: VanityAccount
blankVanityAccount = VanityAccount Nothing Nothing

instance ToJSON VanityAccount where
  toJSON as = object $ catMaybes
    [ (("notes" .=) . toJSON) <$> _vanityAccount_notes as
    , (("unfinishedCrossChainTransfer" .=) . toJSON) <$> _vanityAccount_unfinishedCrossChainTransfer as
    ]

instance FromJSON VanityAccount where
  parseJSON = withObject "VanityAccount" $ \o -> do
    notes <- lenientLookup o "notes"
    unfinishedCrossChainTransfer <- lenientLookup o "unfinishedCrossChainTransfer"
    pure $ VanityAccount
      { _vanityAccount_notes = notes
      , _vanityAccount_unfinishedCrossChainTransfer = unfinishedCrossChainTransfer
      }

newtype AccountStorage = AccountStorage
  { unAccountStorage :: Map NetworkName (Map AccountName (AccountInfo VanityAccount)) }
  deriving (Eq, Show)

instance Semigroup AccountStorage where
  AccountStorage a1 <> AccountStorage a2 = AccountStorage $ Map.unionWith (Map.unionWith (<>)) a1 a2

instance Monoid AccountStorage where
  mempty = AccountStorage mempty

instance ToJSON AccountStorage where
  toJSON = toJSON . unAccountStorage

instance FromJSON AccountStorage where
  parseJSON = fmap AccountStorage . parseJSON

-- | Like '.:?' but ignores values which don't parse
lenientLookup :: FromJSON a => Aeson.Object -> Text -> Aeson.Parser (Maybe a)
lenientLookup o t = pure $ case HM.lookup t o of
  Nothing -> Nothing
  Just v -> case fromJSON v of
    Error _ -> Nothing
    Success a -> Just a


-- | Helper function for compiling pact code to a list of terms
compileCode :: Text -> Either String [Term Name]
compileCode = first show . compileExps mkEmptyInfo <=< parseExprs

-- | Parse the balance checking object into a map of account balance changes and
-- the result from the inner code
parseWrappedBalanceChecks :: PactValue -> Either Text (Map AccountName (Maybe AccountBalance), PactValue)
parseWrappedBalanceChecks = first ("parseWrappedBalanceChecks: " <>) . \case
  (PObject (ObjectMap obj)) -> do
    let lookupErr k = case Map.lookup (FieldKey k) obj of
          Nothing -> Left $ "Missing key '" <> k <> "' in map: " <> renderCompactText (ObjectMap obj)
          Just v -> pure v
        f = (^? _AccountStatus_Exists . accountDetails_balance)
    before <- (fmap . fmap) f . parseAccountDetails =<< lookupErr "before"
    result <- parseResults =<< lookupErr "results"
    after <- (fmap . fmap) f . parseAccountDetails =<< lookupErr "after"
    pure (Map.unionWith (liftA2 subtract) before after, result)
  v -> Left $ "Unexpected PactValue (expected object): " <> renderCompactText v

-- | Turn the object of account->balance into a map
parseAccountDetails :: PactValue -> Either Text (Map AccountName (AccountStatus AccountDetails))
parseAccountDetails = first ("parseAccountDetails: " <>) . \case
  (PObject (ObjectMap obj)) -> do
    m <- for (Map.toAscList obj) $ \(FieldKey accountText, pv) -> do
      bal <- case pv of
        PObject (ObjectMap details) -> maybe (Left "Missing key") Right $ do
          PLiteral (LDecimal balance) <- Map.lookup "balance" details
          PGuard (GKeySet keyset) <- Map.lookup "guard" details
          pure $ AccountStatus_Exists $ AccountDetails
            { _accountDetails_balance = AccountBalance balance
            , _accountDetails_keyset = fromPactKeyset keyset
            }
        PLiteral (LBool False) -> pure AccountStatus_DoesNotExist
        t -> Left $ "Unexpected PactValue (expected decimal): " <> renderCompactText t
      acc <- mkAccountName accountText
      pure (acc, bal)
    pure $ Map.fromList m
  v -> Left $ "Unexpected PactValue (expected object): " <> renderCompactText v

-- | Get the last result, as we would under unwrapped deployment.
parseResults :: PactValue -> Either Text PactValue
parseResults = first ("parseResults: " <>) . \case
  PList vec -> maybe (Left "No value returned") Right $ vec ^? _last
  v -> Left $ "Unexpected PactValue (expected list): " <> renderCompactText v

-- | Code to get the details of the given account.
-- Returns a key suitable for indexing on if you add this to an object.
getDetailsCode :: Text -> (FieldKey, Term Name)
getDetailsCode accountName = (FieldKey accountName, TApp
  { _tApp = App
    { _appFun = TVar (QName $ QualifiedName "coin" "details" def) def
    , _appArgs = [TLiteral (LString accountName) def]
    , _appInfo = def
    }
  , _tInfo = def
  })

tryTerm :: Term Name -> Term Name -> Term Name
tryTerm defaultTo expr = TApp
  { _tApp = App
    { _appFun = TVar (Name $ BareName "try" def) def
    , _appArgs = [defaultTo, expr]
    , _appInfo = def
    }
  , _tInfo = def
  }

-- | Produce an object from account names to account details function calls
accountDetailsObject :: [Text] -> Term Name
accountDetailsObject accounts = TObject
  { _tObject = Pact.Types.Term.Object
    { _oObject = ObjectMap $ Map.fromList $ map (fmap (tryTerm (TLiteral (LBool False) def)) . getDetailsCode) accounts
    , _oObjectType = TyPrim TyDecimal
    , _oKeyOrder = Nothing
    , _oInfo = def
    }
  , _tInfo = def
  }

-- | Wrap the code with a let binding to get the balances of the given accounts
-- before and after executing the code.
wrapWithBalanceChecks :: Set AccountName -> Text -> Either String Text
wrapWithBalanceChecks accounts code = wrapped <$ compileCode code
  where
    accountBalances = accountDetailsObject $ fmap unAccountName $ Set.toAscList accounts
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

makePactLenses ''VanityAccount
makePactLenses ''AccountInfo
makePactLenses ''Accounts
makePactPrisms ''AccountStorage
