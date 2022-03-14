{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.FungibleRequests where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Decimal
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Default
import Control.Monad
import Control.Lens hiding ((.=))
import Control.Applicative (liftA2, (<|>))
import Data.Traversable (for)
import Kadena.SigningTypes (AccountName(..), mkAccountName)
import Data.Bifunctor (first, second)

import Pact.Compile (compileExps, mkEmptyInfo)
import Pact.Parse
import Pact.Types.Command (RequestKey)
import Pact.Types.ChainId
import Pact.Types.Exp
import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Types.Term hiding (PublicKey)
import Pact.Types.Type
import qualified Pact.Types.Term as Pact
import qualified Pact.Types.Type as Pact

import Common.Foundation
import Common.Modules
import Common.Wallet

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
fungibleAccountDetails :: [ModuleName] -> [Text] -> Term Name
fungibleAccountDetails fungibleName accounts =
  mkPactObj $ flip fmap fungibleName $ \fung ->
    (FieldKey $ renderCompactText fung, mkPactObj $ fungibleDetails fung)
  where
    mkPactObj x = TObject
      { _tObject = Pact.Types.Term.Object
        { _oObject = ObjectMap $ Map.fromList x
        , _oObjectType = TyAny
        , _oKeyOrder = Nothing
        , _oInfo = def
        }
      , _tInfo = def
      }
    fungibleDetails fungibleName =
      let tryFalse = tryTerm $ TLiteral (LBool False) def
       in fmap tryFalse . getDetailsCode fungibleName <$> accounts

-- | Turn the object of account->balance into a map
parseAccountDetailsWithHash :: PactValue -> Either Text (Text, (Map AccountName (AccountStatus AccountDetails)))
parseAccountDetailsWithHash = first ("parseAccountDetails: " <>) . \case
  (PObject (ObjectMap obj)) -> do
    let mhKey = FieldKey "modHash"
    case Map.lookup mhKey obj of
      Just (PLiteral (LString modHash)) ->
        second (\a -> (modHash, a)) $ parseAccountBalances $ Map.delete mhKey obj
      otherwise -> Left "TODO"
  v -> Left $ "Unexpected PactValue (expected object): " <> renderCompactText v

parseAccountBalances :: Map FieldKey PactValue -> Either Text (Map AccountName (AccountStatus AccountDetails))
parseAccountBalances obj = do
  m <- for (Map.toAscList obj) $ \(FieldKey accountText, pv) -> do
    bal <- case pv of
      PObject (ObjectMap details) -> maybe (Left "Missing key") Right $ do
        PLiteral (LDecimal balance) <- Map.lookup "balance" details
        PGuard pactGuard <- Map.lookup "guard" details
        pure $ AccountStatus_Exists $ AccountDetails
          { _accountDetails_balance = AccountBalance $ forceDecimalPoint balance
          , _accountDetails_guard = fromPactGuard Nothing pactGuard
          }
      PLiteral (LBool False) -> pure AccountStatus_DoesNotExist
      t -> Left $ "Unexpected PactValue (expected decimal): " <> renderCompactText t
    acc <- mkAccountName accountText
    pure (acc, bal)
  pure $ Map.fromList m

-- Returns:
--   { 'modHash: <hash>
--   , <account-1> : <details-object>
--   , <account-2>: <details-object>
--   ...}
accountDetailsObjectWithHash :: ModuleName -> [Text] -> Term Name
accountDetailsObjectWithHash fungibleName accounts = TObject
  { _tObject = Pact.Types.Term.Object
    { _oObject = ObjectMap $ Map.fromList $ [ ("modHash", modHashExpr)]  <>
       map (fmap (tryTerm (TLiteral (LBool False) def)) . getDetailsCode fungibleName ) accounts
    , _oObjectType = TyPrim TyDecimal
    , _oKeyOrder = Nothing
    , _oInfo = def
    }
  , _tInfo = def
  }
  where
    qModName = "\"" <> renderCompactText fungibleName <> "\""
    modHashExpr =
      let (Right [res]) = compileCode $ "(at 'hash (describe-module " <> qModName <> "))"
      in res

-- accountDetailsObjectCoin :: [Text] -> Term Name
-- accountDetailsObjectCoin = accountDetailsObject ["coin"]

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

-- Should Pact even have amounts that don't have a decimal place?  It's possible to
-- receive amounts that are 'LDecimal 10' that will cause a transaction to fail if used in
-- conjunction with 'Max' etc.
forceDecimalPoint :: Decimal -> Decimal
forceDecimalPoint d = if d == roundTo 0 d then roundTo 1 d else d

parseAccountDetails :: PactValue -> Either Text (Map AccountName (AccountStatus AccountDetails))
parseAccountDetails = first ("parseAccountDetails: " <>) . \case
  (PObject (ObjectMap obj)) -> parseAccountBalances obj
  v -> Left $ "Unexpected PactValue (expected object): " <> renderCompactText v

-- | Get the last result, as we would under unwrapped deployment.
parseResults :: PactValue -> Either Text PactValue
parseResults = first ("parseResults: " <>) . \case
  PList vec -> maybe (Left "No value returned") Right $ vec ^? _last
  v -> Left $ "Unexpected PactValue (expected list): " <> renderCompactText v

-- | Code to get the details of the given account.
-- Returns a key suitable for indexing on if you add this to an object.
getDetailsCode :: ModuleName -> Text -> (FieldKey, Term Name)
getDetailsCode moduleName accountName = (FieldKey accountName, tokenDetails)
   where
     tokenDetails = TApp
       { _tApp = App
         { _appFun = TVar (QName $ QualifiedName moduleName "details" def) def
         , _appArgs = [TLiteral (LString accountName) def]
         , _appInfo = def
         }
       , _tInfo = def
       }

-- | Wrap the code with a let binding to get the balances of the given accounts
-- before and after executing the code.
wrapWithBalanceChecks :: Set AccountName -> Text -> Either String Text
wrapWithBalanceChecks accounts code = wrapped <$ compileCode code
  where
    accountBalances = accountDetailsObjectCoin $ fmap unAccountName $ Set.toAscList accounts
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

