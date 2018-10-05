{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}

-- | Editing features for the data part of Pact transactions:
--
--   See: https://pact-language.readthedocs.io/en/latest/pact-reference.html#cmd-field-and-payloads
--
--   This module deals mostly with keysets, but also offers entry of raw json
--   data. This module should likely be renamed to `Keysets` or something and
--   maybe even factor out the raw JSON part into a separate Contract module, which also holds the contract code.
module Frontend.JsonData
  ( -- * Types & Classes
    KeysetName
  , KeysetPredicate
  , DynKeysets
  , Keysets
  , KeysetKeys
  , JsonError (..)
  , KeysetV (..)
  , Keyset
  , DynKeyset
  , HasKeysetV (..)
  , JsonDataCfg (..)
  , HasJsonDataCfg (..)
  , JsonData (..)
  , HasJsonData (..)
  -- * Creation
  , makeJsonData
  -- * Utility function
  , showJsonError
  ) where


import           Control.Lens        hiding ((.=))
import           Control.Monad.Fix
import           Data.Aeson          (Object)
import           Data.Aeson
import           Data.Aeson.Types    (typeMismatch)
import qualified Data.HashMap.Strict as H
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Semigroup
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import           GHC.Generics        (Generic)
import           Reflex
-- Needed for fanOut, module coming from reflex:
import           Data.Functor.Misc   (Const2 (..))

import           Frontend.Foundation
import           Frontend.Wallet

-- | Type of a `Keyset` name.
--
--   Keysets can be referred to by name.
type KeysetName = Text

-- | Predicate for keysets.
--
--   See: https://pact-language.readthedocs.io/en/latest/pact-reference.html#keysets
--
--   This is a simple Text, because users can define custom predicates, so we
--   can't enumerate them in a sum type.
type KeysetPredicate = Text

-- | Mapping of `KeysetName` to actual `DynKeyset`s.
type DynKeysets t = Map KeysetName (DynKeyset t)
--
-- | Mapping of `KeysetName` to `Keyset`s.
type Keysets = Map KeysetName Keyset

-- | Keys in a `KeySet`.
type KeysetKeys = Map KeyName PublicKey

-- | User entered `_jsonData_rawInput` was invalid.
data JsonError =
  JsonError_NoObject -- ^ Input was no valid JSON object.
  deriving Show

-- | A Keyset as defined [here](https://pact-language.readthedocs.io/en/latest/pact-reference.html#keysets).
--
--   A keyset is basically data as anything else in the data section of a /cmd
--   REST API call, in that a `Keyset` can be accessed from Pact code in the
--   same way as any other data in the `data` section. We still add special
--   support to keysets to enable the user to easily add keys to keysets in a
--   meaningful way, by simply selecting from existing keys.
data KeysetV f = Keyset
  { _keyset_keys :: ReflexValue f KeysetKeys
  -- ^ The keys that will be included in the `Keyset`.
  , _keyset_pred :: ReflexValue f (Maybe KeysetPredicate)
  -- ^ The given `Keyset` predicate, if any.
  }
  deriving (Generic)

-- | Variant of `Keyset` with fixed values.
type Keyset = KeysetV Identity

-- | Variant of `Keyset` with `Dynamic` values.
type DynKeyset t = KeysetV (Dynamic t)

makePactLenses ''KeysetV


-- | Configuration for the `JsonData`.
--
--   Add keysets, set user data, add keys to keysets ...
data JsonDataCfg t = JsonDataCfg
  { _jsonDataCfg_setRawInput  :: Event t Text
    -- ^ Data entered by the user directly. Must be a valid JSON object or the
    -- empty string, if no raw data is needed.
    -- If not valid `_jsonData_data` will become a `Left` value describing the error.
  , _jsonDataCfg_createKeyset :: Event t Text
    -- ^ Create a new empty key set with the given name.
    -- The event will be ignored if a keyset with the given name already
    -- exists or is the empty string.
  , _jsonDataCfg_addKey       :: Event t (KeysetName, KeyName)
    -- ^ Add a new key to a given keyset, both need to exist,
    -- otherwise, the event is ignored.
  , _jsonDataCfg_delKey       :: Event t (KeysetName, KeyName)
    -- ^ Delete a given key from a `Keyset`. If either of those don't exist,
    -- the event is simply ignored.
  , _jsonDataCfg_delKeyset    :: Event t KeysetName
    -- ^ Remove a `Keyset` specified by the given name. If it does not exist,
    -- the event gets ignored.
  , _jsonDataCfg_setPred      :: Event t (KeysetName, Maybe KeysetPredicate)
    -- ^ Set the `Keyset` predicate. If predicate is `Nothing` the default
    -- predicate `keys-all` is used. The event will be ignored if the given
    -- `Keyset` does not exist.
  }
  deriving Generic

makePactLenses ''JsonDataCfg

data JsonData t = JsonData
  { _jsonData_keysets          :: Dynamic t (DynKeysets t)
    -- ^ Currently available keysets that will be added to the JSON data object
    -- to be sent in the next transaction.
  , _jsonData_rawInput         :: Dynamic t Text
    -- ^ Raw input entered by the user, which should be a valid JSON object.
  , _jsonData_data             :: Dynamic t (Either JsonError Object)
   -- ^ The result of combining keysets with raw input from the user. Will be
   -- a `Left` value if `_jsonData_rawInput` does not hold a valid JSON object.
  , _jsonData_overlappingProps :: Dynamic t (Set Text)
   -- ^ json object properties of the `_jsonData_rawInput` that overlap with keyset names.
  }
  deriving Generic

makePactLenses ''JsonData

-- | Build `JsonData` by means of the given `Wallet` and `JsonDataCfg`.
makeJsonData
  :: forall t m. (MonadHold t m, PerformEvent t m, MonadFix m)
  => Wallet t
  -> JsonDataCfg t
  -> m (JsonData t)
makeJsonData walletL rawCfg = mfix $ \json -> do
    let
      cfg = sanitizeCfg json rawCfg
    keysets <- makeKeysets walletL cfg

    rawInput <- holdDyn "" $ cfg ^. jsonDataCfg_setRawInput

    let
      keySetsObj = fmap keysetsToObject . joinKeysets $ keysets
      rawInputObj = parseObjectOrEmpty <$> rawInput

      combinedObj = do
        fromSets <- keySetsObj
        fromRaw <- rawInputObj
        pure $ mappend fromSets <$> fromRaw

    duplicates <- holdUniqDyn $ do
      let getKeys = Set.fromList . H.keys
      fromSets <- getKeys <$> keySetsObj
      fromRaw <- fmap getKeys <$> rawInputObj
      pure $ either (const Set.empty) (Set.intersection fromSets) fromRaw

    pure $ JsonData
      { _jsonData_keysets = keysets
      , _jsonData_rawInput = rawInput
      , _jsonData_data = combinedObj
      , _jsonData_overlappingProps = duplicates
      }
  where
    parseObjectOrEmpty :: Text -> Either JsonError Object
    parseObjectOrEmpty t = case T.strip t of
      "" -> Right mempty
      v  -> parseObject v

    parseObject :: Text -> Either JsonError Object
    parseObject =
      maybe (Left JsonError_NoObject) Right . decodeStrict . T.encodeUtf8


-- | Filter out invalid events.
sanitizeCfg :: Reflex t => JsonData t -> JsonDataCfg t -> JsonDataCfg t
sanitizeCfg json cfg =
    cfg & jsonDataCfg_createKeyset %~ push filterInvalid
  where
    filterInvalid nRaw = do
      ks <- sample . current $ json ^. jsonData_keysets
      let
        n = T.strip nRaw
      if n == "" || Map.member n ks
         then pure Nothing
         else pure $ Just n

-- | Make the keysets for `_jsonData_keysets`.
makeKeysets
  :: forall t m. (MonadHold t m, PerformEvent t m, MonadFix m)
  => Wallet t
  -> JsonDataCfg t
  -> m (Dynamic t (DynKeysets t))
makeKeysets walletL cfg =
    foldDyn id Map.empty
      $ leftmost [ onNewKeyset
                 , Map.delete <$> cfg ^. jsonDataCfg_delKeyset
                 ]

  where
    onNewKeyset = uncurry Map.insert <$> pushAlways makeKeyset (cfg ^. jsonDataCfg_createKeyset)

    onNewPreds = uncurry Map.singleton <$> cfg ^. jsonDataCfg_setPred

    onAddKeysetKey =
      uncurry Map.singleton <$> filterValid snd (cfg ^. jsonDataCfg_addKey)

    onDelKeysetKey =
      uncurry Map.singleton <$> filterValid snd (cfg ^. jsonDataCfg_delKey)

    predSelectors :: EventSelector t (Const2 KeysetName (Maybe KeysetPredicate))
    predSelectors = fanMap onNewPreds

    addSelectors :: EventSelector t (Const2 KeysetName KeyName)
    addSelectors = fanMap onAddKeysetKey

    delSelectors :: EventSelector t (Const2 KeysetName KeyName)
    delSelectors = fanMap onDelKeysetKey

    -- | Filter events for valid key names:
    filterValid :: (a -> KeyName) -> Event t a -> Event t a
    filterValid getName = push checkName
      where
        checkName v = do
          cKeys <- sample . current $ walletL ^. wallet_keys
          if Map.member (getName v) cKeys
             then pure $ Just v
             else pure Nothing

    makeKeyset
      :: forall mh. (MonadHold t mh, MonadFix mh)
      => KeysetName
      -> mh (KeysetName, DynKeyset t)
    makeKeyset n = do
      let
        onSetPred = select predSelectors (Const2 n)
        onNewKey = select addSelectors (Const2 n)
        onDelKey = select delSelectors (Const2 n)

        onRemovedKeys = push (\new -> do
          old <- sample . current $ walletL ^. wallet_keys
          let removed = Map.difference old new
          if Map.null removed
             then pure Nothing
             else pure $ Just . Set.fromList . Map.keys $ removed

          )
          (updated $ walletL ^. wallet_keys)

      pPred <- holdUniqDyn =<< holdDyn Nothing onSetPred

      keynames <- foldDyn id Set.empty
        $ leftmost [ Set.insert <$> onNewKey
                   , Set.delete <$> onDelKey
                   , flip Set.difference <$> onRemovedKeys
                   ]

      let
        keys :: Dynamic t (Map KeyName PublicKey)
        keys = do
          names <- Set.toList <$> keynames
          let namesMap = Map.fromList . map (,()) $ names
          wKeys <- walletL ^. wallet_keys
          let keyPairs = Map.intersection wKeys namesMap
          pure $ _keyPair_publicKey <$> keyPairs

      pure (n, Keyset keys pPred)

-- Utilities:

-- | Show a descriptive error message.
showJsonError :: JsonError -> Text
showJsonError = \case
  JsonError_NoObject -> "ERROR: Data must be a valid JSON object!"

-- | Translate `Keysets` to a JSON `Object`.
keysetsToObject :: Keysets -> Object
keysetsToObject = H.fromList . map (fmap toJSON) . Map.toList

-- | Merge the dynamic parts together to a single Dynamic `Keyset`.
joinKeyset :: Reflex t => DynKeyset t -> Dynamic t Keyset
joinKeyset (Keyset ks p) = Keyset <$> ks <*> p

-- | Translate `DynKeySets` to a `Dynamic` of `Keysets`.
joinKeysets :: Reflex t => Dynamic t (DynKeysets t) -> Dynamic t Keysets
joinKeysets = joinDynThroughMap . fmap (fmap joinKeyset)

-- Boring instances:
--
-- Create an Aeson `Object` from a given keyset.
--
-- Format of course according to the Pact reference:
--   https://pact-language.readthedocs.io/en/latest/pact-reference.html#keysets
instance ToJSON Keyset where
  toJSON (Keyset keys mPred) =
    let
      kVals = Map.elems keys
    in
      case mPred of
        Nothing -> toJSON kVals
        Just p  -> object [ ("keys", toJSON kVals), ("pred", toJSON p)]

instance FromJSON Keyset where
  parseJSON = \case
    Object v -> Keyset <$> v .: "keys" <*> v .: "pred"
    Array v  -> Keyset <$> parseJSON (Array v) <*> pure Nothing
    invalid  -> typeMismatch "Keyset" invalid

instance Reflex t => Semigroup (JsonDataCfg t) where
  c1 <> c2 =
    JsonDataCfg
      { _jsonDataCfg_setRawInput
          = leftmost [ _jsonDataCfg_setRawInput c1
                     , _jsonDataCfg_setRawInput c2
                     ]
      , _jsonDataCfg_createKeyset
          = leftmost [ _jsonDataCfg_createKeyset c1
                     , _jsonDataCfg_createKeyset c2
                     ]
      , _jsonDataCfg_addKey
          = leftmost [ _jsonDataCfg_addKey c1
                     , _jsonDataCfg_addKey c2
                     ]
      , _jsonDataCfg_delKey
          = leftmost [ _jsonDataCfg_delKey c1
                     , _jsonDataCfg_delKey c2
                     ]
      , _jsonDataCfg_delKeyset
          = leftmost [ _jsonDataCfg_delKeyset c1
                     , _jsonDataCfg_delKeyset c2
                     ]
      , _jsonDataCfg_setPred
          = leftmost [ _jsonDataCfg_setPred c1
                     , _jsonDataCfg_setPred c2
                     ]
      }

instance Reflex t => Monoid (JsonDataCfg t) where
  mempty = JsonDataCfg never never never never never never
  mappend = (<>)

instance Flattenable JsonDataCfg where
  flattenWith doSwitch ev =
    JsonDataCfg
      <$> doSwitch never (_jsonDataCfg_setRawInput <$> ev)
      <*> doSwitch never (_jsonDataCfg_createKeyset <$> ev)
      <*> doSwitch never (_jsonDataCfg_addKey <$> ev)
      <*> doSwitch never (_jsonDataCfg_delKey <$> ev)
      <*> doSwitch never (_jsonDataCfg_delKeyset <$> ev)
      <*> doSwitch never (_jsonDataCfg_setPred <$> ev)
