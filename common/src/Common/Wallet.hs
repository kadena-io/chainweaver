{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
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
  , AccountRef(..)
  , accountRefToName
  , lookupAccountRef
  , AccountName(..)
  , AccountBalance(..)
  , AccountNotes (unAccountNotes)
  , mkAccountNotes
  , AccountGuard(..)
  , UnfinishedCrossChainTransfer(..)
  , KeyStorage
  , Account
    -- * Prisms for working directly with Account
  , _VanityAccount
  , _NonVanityAccount
  , accountUnfinishedCrossChainTransfer
  , accountToName
  , accountNotes
  , accountChain
  , accountBalance
  , accountKey
  , AccountStorage(..)
  , _AccountStorage
  , storageAccountInfo
  , Accounts(..)
  , accounts_vanity
  , accounts_nonVanity
  , foldAccounts
  , Key(..)
  , filterKeyPairs
  , VanityAccount(..)
  , vanityAccount_notes
  , vanityAccount_info
  , vanityAccount_key
  , NonVanityAccount(..)
  , nonVanityAccount_info
  , AccountInfo(..)
  , HasAccountInfo(..)
  , blankAccountInfo
  , pactGuardTypeText
  , fromPactGuard
  -- * Util
  , throwDecodingErr
  , decodeBase16M
  -- * Balance checks
  , wrapWithBalanceChecks
  , parseWrappedBalanceChecks
  , getBalanceCode
  , accountBalanceObject
  , parseAccountBalances
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
import Data.Dependent.Sum (DSum(..), (==>))
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Set (Set)
import Data.Some (Some(..))
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
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


-- | A key consists of a public key and an optional private key.
--
data KeyPair key = KeyPair
  { _keyPair_publicKey  :: PublicKey
  , _keyPair_privateKey :: Maybe key
  } deriving Generic

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

-- | Account balance wrapper
newtype AccountBalance = AccountBalance { unAccountBalance :: Decimal } deriving (Eq, Ord, Num, Show)

-- Via ParsedDecimal
instance ToJSON AccountBalance where
  toJSON = toJSON . ParsedDecimal . unAccountBalance
instance FromJSON AccountBalance where
  parseJSON x = (\(ParsedDecimal d) -> AccountBalance d) <$> parseJSON x

-- | Account notes wrapper
newtype AccountNotes = AccountNotes { unAccountNotes :: Text } deriving (Eq, Show)

-- | We currently don't have any validation for the notes on an account, but should we
-- decide to add it then this will come in handy.
mkAccountNotes :: Text -> AccountNotes
mkAccountNotes = AccountNotes

instance ToJSON AccountNotes where
  toJSON = toJSON . unAccountNotes
instance FromJSON AccountNotes where
  parseJSON = fmap AccountNotes . parseJSON

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

--data Account key = Account
--  { _account_name :: AccountName
--  , _account_key :: KeyPair key
--  , _account_chainId :: ChainId
--  , _account_network :: NetworkName
--  , _account_notes :: AccountNotes
--  , _account_balance :: Maybe AccountBalance
--  -- ^ We also treat this as proof of the account's existence.
--  , _account_unfinishedCrossChainTransfer :: Maybe UnfinishedCrossChainTransfer
--  }
--
--instance ToJSON key => ToJSON (Account key) where
--  toJSON a = object $ catMaybes
--    [ Just $ "name" .= _account_name a
--    , Just $ "key" .= _account_key a
--    , Just $ "chain" .= _account_chainId a
--    , Just $ "network" .= _account_network a
--    , Just $ "notes" .= _account_notes a
--    , ("balance" .=) <$> _account_balance a
--    , ("unfinishedCrossChainTransfer" .=) <$> _account_unfinishedCrossChainTransfer a
--    ]
--
--instance FromJSON key => FromJSON (Account key) where
--  parseJSON = withObject "Account" $ \o -> do
--    name <- o .: "name"
--    key <- o .: "key"
--    chain <- o .: "chain"
--    network <- o .: "network"
--    notes <- o .: "notes"
--    balance <- o .:? "balance"
--    unfinishedCrossChainTransfer <- lenientLookup o "unfinishedCrossChainTransfer"
--    pure $ Account
--      { _account_name = name
--      , _account_key = key
--      , _account_chainId = chain
--      , _account_network = network
--      , _account_notes = notes
--      , _account_balance = balance
--      , _account_unfinishedCrossChainTransfer = unfinishedCrossChainTransfer
--      }

data Key key = Key
  { _key_pair :: KeyPair key
  , _key_hidden :: Bool
  , _key_notes :: AccountNotes
  }

instance ToJSON key => ToJSON (Key key) where
  toJSON k = object
    [ "pair" .= toJSON (_key_pair k)
    , "hidden" .= toJSON (_key_hidden k)
    , "notes" .= toJSON (_key_notes k)
    ]

instance FromJSON key => FromJSON (Key key) where
  parseJSON = withObject "Key" $ \o -> do
    pair <- o .: "pair"
    hidden <- fromMaybe False <$> lenientLookup o "hidden"
    notes <- fromMaybe (AccountNotes "") <$> lenientLookup o "notes"
    pure $ Key
      { _key_pair = pair
      , _key_hidden = hidden
      , _key_notes = notes
      }


type KeyStorage key = IntMap (Key key)

filterKeyPairs :: Set PublicKey -> IntMap (Key key) -> [KeyPair key]
filterKeyPairs s m = Map.elems $ Map.restrictKeys (toMap m) s
  where toMap = Map.fromList . fmap (\k -> (_keyPair_publicKey $ _key_pair k, _key_pair k)) . IntMap.elems

data AccountRef a where
  AccountRef_Vanity :: AccountName -> ChainId -> AccountRef VanityAccount
  AccountRef_NonVanity :: PublicKey -> ChainId -> AccountRef NonVanityAccount

accountRefToName :: AccountRef a -> Text
accountRefToName = \case
  AccountRef_Vanity an _ -> unAccountName an
  AccountRef_NonVanity pk _ -> keyToText pk

type Account = DSum AccountRef Identity

accountNotes :: Account -> Maybe AccountNotes
accountNotes = \case
  AccountRef_NonVanity _ _ :=> _ -> Nothing
  AccountRef_Vanity _ _ :=> Identity v -> Just $ _vanityAccount_notes v

lookupAccountRef :: Some AccountRef -> Accounts -> Maybe Account
lookupAccountRef (Some ref) accounts = case ref of
  AccountRef_Vanity an c -> do
    cs <- Map.lookup an $ _accounts_vanity accounts
    v <- Map.lookup c cs
    pure $ ref ==> v
  AccountRef_NonVanity pk c -> do
    cs <- Map.lookup pk $ _accounts_nonVanity accounts
    nv <- Map.lookup c cs
    pure $ ref ==> nv

accountToName :: Account -> AccountName
accountToName (r :=> _) = case r of
  AccountRef_Vanity n _ -> n
  AccountRef_NonVanity pk _ -> AccountName $ keyToText pk

accountChain :: Account -> ChainId
accountChain (r :=> _) = case r of
  AccountRef_Vanity _ c -> c
  AccountRef_NonVanity _ c -> c

accountKey :: Account -> PublicKey
accountKey (r :=> Identity a) = case r of
  AccountRef_Vanity _ _ -> _vanityAccount_key a
  AccountRef_NonVanity pk _ -> pk

data Accounts = Accounts
  { _accounts_vanity :: Map AccountName (Map ChainId VanityAccount)
  , _accounts_nonVanity :: Map PublicKey (Map ChainId NonVanityAccount)
  }

instance Semigroup Accounts where
  a1 <> a2 = Accounts
    { _accounts_vanity = Map.unionWith (<>) (_accounts_vanity a1) (_accounts_vanity a2)
    , _accounts_nonVanity = Map.unionWith (<>) (_accounts_nonVanity a1) (_accounts_nonVanity a2)
    }

instance Monoid Accounts where
  mempty = Accounts mempty mempty

instance ToJSON Accounts where
  toJSON as = object
    [ "vanity" .= toJSON (_accounts_vanity as)
    , "nonVanity" .= toJSON (_accounts_nonVanity as)
    ]

instance FromJSON Accounts where
  parseJSON = withObject "Accounts" $ \o -> do
    vanity <- o .: "vanity"
    nonVanity <- o .: "nonVanity"
    pure $ Accounts
      { _accounts_vanity = vanity
      , _accounts_nonVanity = nonVanity
      }

foldAccounts :: Monoid m => (Account -> m) -> Accounts -> m
foldAccounts f accounts = mconcat
  [ flip Map.foldMapWithKey (_accounts_vanity accounts) $ \n -> Map.foldMapWithKey $ \c a -> f (AccountRef_Vanity n c ==> a)
  , flip Map.foldMapWithKey (_accounts_nonVanity accounts) $ \pk -> Map.foldMapWithKey $ \c a -> f (AccountRef_NonVanity pk c ==> a)
  ]

data AccountInfo = AccountInfo
  { _accountInfo_balance :: Maybe AccountBalance
  , _accountInfo_unfinishedCrossChainTransfer :: Maybe UnfinishedCrossChainTransfer
  , _accountInfo_hidden :: Bool
  } deriving Show

blankAccountInfo :: AccountInfo
blankAccountInfo = AccountInfo
  { _accountInfo_balance = Nothing
  , _accountInfo_unfinishedCrossChainTransfer = Nothing
  , _accountInfo_hidden = False
  }

instance ToJSON AccountInfo where
  toJSON a = object $ catMaybes
    [ Just $ "balance" .= toJSON (_accountInfo_balance a)
    , (("unfinishedCrossChainTransfer" .=) . toJSON) <$> _accountInfo_unfinishedCrossChainTransfer a
    , Just $ "hidden" .= toJSON (_accountInfo_hidden a)
    ]

instance FromJSON AccountInfo where
  parseJSON = withObject "AccountInfo" $ \o -> do
    balance <- o .: "balance"
    unfinishedCrossChainTransfer <- lenientLookup o "unfinishedCrossChainTransfer"
    hidden <- o .: "hidden"
    pure $ AccountInfo
      { _accountInfo_balance = balance
      , _accountInfo_unfinishedCrossChainTransfer = unfinishedCrossChainTransfer
      , _accountInfo_hidden = hidden
      }

data VanityAccount = VanityAccount
  { _vanityAccount_key :: PublicKey -- TODO should be KeySet, but that's a huge change.
  , _vanityAccount_notes :: AccountNotes
  , _vanityAccount_info :: AccountInfo
  , _vanityAccount_inflight :: Bool
  } deriving Show

instance ToJSON VanityAccount where
  toJSON as = object $ catMaybes
    [ Just $ "key" .= toJSON (_vanityAccount_key as)
    , Just $ "notes" .= toJSON (_vanityAccount_notes as)
    , Just $ "info" .= toJSON (_vanityAccount_info as)
    , if _vanityAccount_inflight as
      then Just $ "inflight" .= toJSON True
      else Nothing
    ]

instance FromJSON VanityAccount where
  parseJSON = withObject "VanityAccount" $ \o -> do
    key <- o .: "key"
    notes <- o .: "notes"
    info <- o .: "info"
    inflight <- lenientLookup o "inflight"
    pure $ VanityAccount
      { _vanityAccount_key = key
      , _vanityAccount_notes = notes
      , _vanityAccount_info = info
      , _vanityAccount_inflight = fromMaybe False inflight
      }

data NonVanityAccount = NonVanityAccount
  { _nonVanityAccount_info :: AccountInfo
  }

instance ToJSON NonVanityAccount where
  toJSON as = object
    [ "info" .= toJSON (_nonVanityAccount_info as)
    ]

instance FromJSON NonVanityAccount where
  parseJSON = withObject "NonVanityAccount" $ \o -> do
    info <- o .: "info"
    pure $ NonVanityAccount
      { _nonVanityAccount_info = info
      }

newtype AccountStorage = AccountStorage { unAccountStorage :: Map NetworkName Accounts }

instance Semigroup AccountStorage where
  AccountStorage a1 <> AccountStorage a2 = AccountStorage $ Map.unionWith (<>) a1 a2

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
    before <- parseAccountBalances =<< lookupErr "before"
    result <- parseResults =<< lookupErr "results"
    after <- parseAccountBalances =<< lookupErr "after"
    pure (Map.unionWith (liftA2 subtract) before after, result)
  v -> Left $ "Unexpected PactValue (expected object): " <> renderCompactText v

-- | Turn the object of account->balance into a map
parseAccountBalances :: PactValue -> Either Text (Map AccountName (Maybe AccountBalance))
parseAccountBalances = first ("parseAccountBalances: " <>) . \case
  (PObject (ObjectMap obj)) -> do
    m <- for (Map.toAscList obj) $ \(FieldKey accountText, pv) -> do
      bal <- case pv of
        PLiteral (LDecimal d) -> pure $ Just $ AccountBalance d
        PLiteral (LBool False) -> pure Nothing
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

-- | Code to get the balance of the given account.
-- Returns a key suitable for indexing on if you add this to an object.
getBalanceCode :: Text -> (FieldKey, Term Name)
getBalanceCode accountName = (FieldKey accountName, TApp
  { _tApp = App
    { _appFun = TVar (QName $ QualifiedName "coin" "get-balance" def) def
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

-- | Produce an object from account names to account balance function calls
accountBalanceObject :: [Text] -> Term Name
accountBalanceObject accounts = TObject
  { _tObject = Pact.Types.Term.Object
    { _oObject = ObjectMap $ Map.fromList $ map (fmap (tryTerm (TLiteral (LBool False) def)) . getBalanceCode) accounts
    , _oObjectType = TyPrim TyDecimal
    , _oKeyOrder = Nothing
    , _oInfo = def
    }
  , _tInfo = def
  }

-- | Wrap the code with a let binding to get the balances of the given accounts
-- before and after executing the code.
wrapWithBalanceChecks :: Set (Some AccountRef) -> Text -> Either String Text
wrapWithBalanceChecks accounts code = wrapped <$ compileCode code
  where
    accountBalances = accountBalanceObject $ fmap (\(Some x) -> accountRefToName x) $ Set.toAscList accounts
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

deriveGEq ''AccountRef
deriveGCompare ''AccountRef
deriveGShow ''AccountRef
makePactLenses ''AccountInfo
makePactLenses ''VanityAccount
makePactLenses ''NonVanityAccount
makePactLenses ''Accounts
makePactPrisms ''AccountStorage

instance HasAccountInfo NonVanityAccount where accountInfo = nonVanityAccount_info
instance HasAccountInfo VanityAccount where accountInfo = vanityAccount_info

instance HasAccountInfo (DSum AccountRef Identity) where
  accountInfo = lens
    (\(r :=> Identity a) -> case r of
      AccountRef_NonVanity _ _ -> _nonVanityAccount_info a
      AccountRef_Vanity _ _ -> _vanityAccount_info a
    )
    (\(r :=> Identity a) newInfo -> case r of
      AccountRef_NonVanity pk c -> AccountRef_NonVanity pk c :=> Identity (NonVanityAccount newInfo)
      AccountRef_Vanity an c -> AccountRef_Vanity an c :=> Identity (a & vanityAccount_info .~ newInfo)
    )

accountUnfinishedCrossChainTransfer :: Account -> Maybe UnfinishedCrossChainTransfer
accountUnfinishedCrossChainTransfer = view (accountInfo . accountInfo_unfinishedCrossChainTransfer)

accountBalance :: Account -> Maybe AccountBalance
accountBalance = view (accountInfo . accountInfo_balance)

storageAccountInfo :: NetworkName -> Some AccountRef -> Traversal' AccountStorage AccountInfo
storageAccountInfo net (Some ref) = _AccountStorage . at net . _Just . case ref of
  AccountRef_NonVanity pk chain ->
    accounts_nonVanity . at pk . _Just . at chain . _Just . nonVanityAccount_info
  AccountRef_Vanity name chain ->
    accounts_vanity . at name . _Just . at chain . _Just . vanityAccount_info

_VanityAccount :: Prism' Account (AccountName, ChainId, VanityAccount)
_VanityAccount = prism' (\(a,b,c) -> AccountRef_Vanity a b :=> Identity c)
  (\case
    AccountRef_Vanity a b :=> Identity c -> Just (a,b,c)
    _ -> Nothing
  )

_NonVanityAccount :: Prism' Account (PublicKey, ChainId, NonVanityAccount)
_NonVanityAccount = prism' (\(a,b,c) -> AccountRef_NonVanity a b :=> Identity c)
  (\case
    AccountRef_NonVanity a b :=> Identity c -> Just (a,b,c)
    _ -> Nothing
  )

