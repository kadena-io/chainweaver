{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

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
import           Pact.Types.Pretty
import           Pact.Types.Term (KeySet (..), mkKeySet)
import           Reflex
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Dom.Core
import           Reflex.Network

import           Common.Foundation
import           Common.Wallet
import           Frontend.UI.Dialogs.WatchRequest
import           Frontend.UI.Form.Common
import           Frontend.UI.FormWidget
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

userFromPactKeyset :: KeySet -> UserKeyset
userFromPactKeyset (KeySet ks p) = UserKeyset (Set.map fromPactPublicKey ks) (parseKeysetPred $ renderCompactText p)

-- | Uses thin wrapper around Text so callers can distinguish between empty
-- string and an invalid public key.
pubKeyFormWidget
  :: forall t m. MonadWidget t m
  => FormWidgetConfig t PublicKeyText
  -> m (FormWidget t PublicKeyText)
pubKeyFormWidget cfg = do
  let
    cfg2 = unPublicKeyText <$> cfg
    iv = _initialValue cfg2
    inp c = do
      ie <- mkLabeledInput False mempty (uiComboBoxGlobalDatalist keyListId iv) c
      pure (ie,  _inputElement_input ie)

    inputCfg = fwc2iec id cfg2
      & initialAttributes %~ mappend ("placeholder" =: "Enter public key (optional)")

    popoverMsg t
      | T.length t /= 0 && T.length t /= 64 = PopoverState_Error "Key has unexpected length"
      | not (BS.null $ snd $ Base16.decode $ T.encodeUtf8 t) = PopoverState_Error "Input was not valid Base16 encoding"
      | otherwise = PopoverState_Disabled
    showPopover (_, onInput) = pure $ popoverMsg <$> onInput

  (ie,_) <- uiInputWithPopover
    inp
    (_inputElement_raw . fst)
    showPopover
    inputCfg

  return $ FormWidget (PublicKeyText <$> value ie) (() <$ _inputElement_input ie) (_inputElement_hasFocus ie)

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

keysetFormWidget
  :: (MonadWidget t m)
  => FormWidgetConfig t (Maybe UserKeyset)
  -> m (Dynamic t (Maybe UserKeyset))
keysetFormWidget cfg = do
  let
    selectMsgKey = PublicKeyText ""
    selectMsgMap = Map.singleton selectMsgKey "Select"

    doAddDel yesno = fmap (yesno selectMsgKey) . updated

    dropkeys2 n xs | n >= 3 = xs
                   | otherwise = filter (/= keys2Predicate) xs

  let setKeys = maybe never (fmap (fromMaybe mempty)) $ _formWidgetConfig_setValue (fmap _userKeyset_keys <$> cfg)
      toPatchIntMap f cur keys = PatchIntMap (IntMap.union resets ps)
        where
          resets = Nothing <$ cur
          ps = ifoldMap (\n a -> IntMap.singleton n $ Just $ f a) $ Set.toList keys

      setToIntMap = ifoldMap (\n a -> IntMap.singleton n $ PublicKeyText $ keyToText a) . Set.toList

      listCfg = maybe mempty (setToIntMap . _userKeyset_keys) <$> cfg
  ddKeys <- mkLabeledClsInput False "Public Keys" $ const $ growingList
    (const pubKeyFormWidget)
    (AllowAddNewRow $ doAddDel (/=) . value)
    (AllowDeleteRow $ doAddDel (==) . value)
    selectMsgKey
    never
    listCfg

  let keys = IntMap.filter (/= selectMsgKey) <$> joinDynThroughIntMap (value <$$> ddKeys)
  let newPred oldKeys newKeys = if IntMap.size oldKeys == 2 && IntMap.size newKeys == 1
                            then Just KeysAll else Nothing
      ditchKeys2 = fmapMaybe id $ attachWith newPred (current keys) (updated keys)
      predCfg = maybe KeysAll _userKeyset_pred <$> cfg
  let mkPredCfg klass = (mkPfwc predCfg)
        & initialAttributes .~ addToClassAttr (klass <> "select") mempty
        & setValue %~ (\a -> Just $ leftmost (ditchKeys2 : maybeToList a))
  predicateE <- mkLabeledClsInput False "Keys Required to Sign for Account (Predicate)" $ \klass -> mdo
    let allPreds = do
          ks <- keys
          let ps = if IntMap.size ks >= 2 then [KeysAll, KeysAny, Keys2] else [KeysAll, KeysAny]
          return $ map (\p -> (p, prettyPred $ renderKeysetPred p)) ps
    predfw <- unsafeDropdownFormWidget allPreds (mkPredCfg klass)
    -- TODO Need to put in a different dropdown implementation that correctly
    -- handles when an option goes away
    return predfw

  return $ do
    ks <- Set.fromList . catMaybes . map textToKey . filter (not . T.null) . map unPublicKeyText . IntMap.elems <$> keys
    pred <- value predicateE
    if Set.null ks
      then pure Nothing
      else pure $ Just $ UserKeyset ks pred

keyDisplayWidget
  :: MonadWidget t m
  => PublicKey
  -> m ()
keyDisplayWidget k = void $ do
    uiInputElement $ def
      & initialAttributes .~ ("class" =: "key-display" <> "disabled" =: "disabled")
      & inputElementConfig_initialValue .~ keyToText k

keysetWidget
  :: (MonadWidget t m)
  => UserKeyset
  -> m ()
keysetWidget (UserKeyset keys pred) = do
    mkLabeledView False "Keys" $ forM_ keys keyDisplayWidget

    mkLabeledInput False "Predicate" uiInputElement $ def
      & initialAttributes .~ "disabled" =: "disabled"
      & inputElementConfig_initialValue .~ renderKeysetPred pred
    return ()
