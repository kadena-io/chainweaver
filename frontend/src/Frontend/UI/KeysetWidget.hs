module Frontend.UI.KeysetWidget where

import           Control.Arrow ((&&&))
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Trans
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import           Data.Foldable
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import           Data.Maybe
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Pact.Types.Term (KeySet (..), mkKeySet)
import           Reflex
import           Reflex.Dom.Core

import           Common.Wallet
import           Frontend.UI.Dialogs.WatchRequest
import           Frontend.UI.Widgets

newtype PublicKeyText = PublicKeyText { unPublicKeyText :: Text }
  deriving (Eq,Ord,Show,Read)

data KeysetPred
  = KeysAll
  | KeysAny
  | Keys2
  | OtherPred { unKeysetPred :: Text }
  deriving (Eq,Ord,Show,Read)

renderKeysetPred :: KeysetPred -> Text
renderKeysetPred = \case
  KeysAny -> "keys-any"
  KeysAll -> "keys-all"
  Keys2 -> "keys-2"
  OtherPred t -> t

parseKeysetPred :: Text -> KeysetPred
parseKeysetPred = \case
  "keys-any" -> KeysAny
  "keys-all" -> KeysAll
  "keys-2" -> Keys2
  t -> OtherPred t

data UserKeyset = UserKeyset
  { _userKeyset_keys :: Set PublicKey
  , _userKeyset_pred :: KeysetPred
  } deriving (Eq,Ord,Show)

userToPactKeyset :: UserKeyset -> KeySet
userToPactKeyset (UserKeyset ks p) = mkKeySet (toPactPublicKey <$> Set.toList ks) (renderKeysetPred p)

-- | Uses thin wrapper around Text so callers can distinguish between empty
-- string and an invalid public key.
pubKeyInputWidget
  :: forall t m. MonadWidget t m
  => PublicKeyText
  -> m (Dynamic t PublicKeyText)
pubKeyInputWidget iv = do
  let
    inp cfg = do
      ie <- mkLabeledInput False mempty (uiComboBoxGlobalDatalist keyListId (unPublicKeyText iv)) cfg
      pure ( ie
           , ( value ie
             , _inputElement_input ie
             )
           )

    inputCfg = def
      & initialAttributes .~ ("placeholder" =: "Enter public key (optional)")
      & inputElementConfig_initialValue .~ (unPublicKeyText iv)

    popoverMsg t
      | T.length t /= 0 && T.length t /= 64 = PopoverState_Error "Key has unexpected length"
      | not (BS.null $ snd $ Base16.decode $ T.encodeUtf8 t) = PopoverState_Error "Input was not valid Base16 encoding"
      | otherwise = PopoverState_Disabled
    showPopover (_, (_, onInput)) = pure $ popoverMsg <$> onInput

  (_, (res, _)) <- uiInputWithPopover
    inp
    (_inputElement_raw . fst)
    showPopover
    inputCfg

  return $ PublicKeyText <$> res

prettyPred :: Text -> Text
prettyPred "keys-all" = "All keys"
prettyPred "keys-any" = "Any single key"
prettyPred "keys-2" = "Any two keys"
prettyPred p = p

keysetInputWidget
  :: (MonadWidget t m)
  => Maybe UserKeyset
  -> m (Dynamic t (Maybe UserKeyset))
keysetInputWidget iv = do
  let
    selectMsgKey = PublicKeyText ""
    selectMsgMap = Map.singleton selectMsgKey "Select"

    doAddDel yesno = fmap (yesno selectMsgKey) . updated

    allPredSelectMap nkeys = ffor nkeys $ \nks -> Map.fromList
      $ fmap (id &&& prettyPred) (dropkeys2 nks predefinedPreds)
      where
        dropkeys2 n xs | n >= 3 = xs
                       | otherwise = filter (/= keys2Predicate) xs

  (ddKeys, patchEvents) <- mkLabeledClsInput False "Public Keys" $ const $ uiAdditiveInput
    (const pubKeyInputWidget)
    (AllowAddNewRow $ doAddDel (/=))
    (AllowDeleteRow $ doAddDel (==))
    selectMsgKey
    never

  let keys = join $ distributeMapOverDynPure . Map.fromList . IntMap.toList <$> ddKeys

  predicateE <- mkLabeledClsInput False "Keys Required to Sign for Account (Predicate)" $ const
    $ uiDropdown defaultPredicate (allPredSelectMap $ fmap Map.size keys) $ def
    & dropdownConfig_attributes .~ constDyn ("class" =: "labeled-input__input")
    -- & dropdownConfig_setValue .~ _definedKeyset_predicateChange presets

  return $ do
    ks <- Set.fromList . catMaybes . map textToKey . filter (not . T.null) . map unPublicKeyText . Map.elems <$> keys
    pred <- parseKeysetPred <$> value predicateE
    if Set.null ks
      then pure Nothing
      else pure $ Just $ UserKeyset ks pred

keysetWidget
  :: (MonadWidget t m)
  => UserKeyset
  -> m ()
keysetWidget (UserKeyset keys pred) = do
    mkLabeledView False "Keys" $ forM_ keys $ \k -> do
      uiInputElement $ def
        & initialAttributes .~ "disabled" =: "disabled"
        & inputElementConfig_initialValue .~ keyToText k

    mkLabeledInput False "Predicate" uiInputElement $ def
      & initialAttributes .~ "disabled" =: "disabled"
      & inputElementConfig_initialValue .~ renderKeysetPred pred
    return ()
