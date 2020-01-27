{-# LANGUAGE RecursiveDo #-}
module Frontend.UI.Dialogs.AddVanityAccount.DefineKeyset
  ( DefinedKeyset
  , uiDefineKeyset
  , emptyKeysetPresets
  ) where

import           Control.Lens                           ((^.))
import           Control.Error                          (hush)
import           Data.Witherable                        (wither)
import           Data.Text                              (Text)
import qualified Data.Text as T
import qualified Data.IntSet as IntSet
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import           Data.Set                               (Set)
import qualified Data.Set as Set

import           Reflex
import           Reflex.Dom.Core

import           Frontend.UI.Widgets

import           Frontend.JsonData
import           Frontend.Wallet
import           Frontend.Foundation

data KeysetInputs t i a = KeysetInputs
  { _keysetInputs_value :: Dynamic t (IntMap i)
  , _keysetInputs_set :: Dynamic t (Set a)
  }

data DefinedKeyset t = DefinedKeyset
  { _definedKeyset_internalKeys :: KeysetInputs t (Dropdown t (Maybe Int)) PublicKey
  , _definedKeyset_externalKeys :: KeysetInputs t (ExternalKeyInput t) PublicKey
  , _definedKeyset_predicate :: Dynamic t Text
  }

emptyKeysetPresets :: forall t. Reflex t => DefinedKeyset t
emptyKeysetPresets = DefinedKeyset
  { _definedKeyset_internalKeys = KeysetInputs mempty mempty
  , _definedKeyset_externalKeys = KeysetInputs mempty mempty
  , _definedKeyset_predicate = mempty
  }

data ExternalKeyInput t = ExternalKeyInput
  { _externalKeyInput_input :: Event t Text
  , _externalKeyInput_value :: Dynamic t (Maybe PublicKey)
  }

uiExternalKeyInput
  :: forall t m. MonadWidget t m
  => Dynamic t (IntMap (ExternalKeyInput t))
  -> m (KeysetInputs t (ExternalKeyInput t) PublicKey)
uiExternalKeyInput onPreselect = do
  let
    uiPubkeyInput iv = do
      (inp, dE) <- uiInputWithInlineFeedback
        (fmap parsePublicKey . value)
        (fmap (not . T.null) . value)
        id
        Nothing
        uiInputElement
        $ def
        & initialAttributes .~ (
          "placeholder" =: "External public key" <>
          "class" =: "labeled-input__input"
          )
        & inputElementConfig_initialValue .~ iv

      pure $ ExternalKeyInput
        { _externalKeyInput_input = _inputElement_input inp
        , _externalKeyInput_value = hush <$> dE
        }

    toSet :: IntMap.IntMap (ExternalKeyInput t) -> Dynamic t (Set PublicKey)
    toSet = fmap (Set.fromList . IntMap.elems) . wither _externalKeyInput_value

  pb <- getPostBuild

  let preselections = onPreselect >>= IntMap.foldMapWithKey
        (\k t -> IntMap.singleton k . fmap keyToText <$> _externalKeyInput_value t)

  v <- uiAdditiveInput
    (const uiPubkeyInput)
    _externalKeyInput_input
    (not . T.null)
    T.null
    T.empty
    (current preselections <@ pb)

  pure $ KeysetInputs v (v >>= toSet)

defineKeyset
  :: forall t m key model
     . ( MonadWidget t m
       , HasWallet model key t
       )
  => model
  -> Dynamic t (IntMap (Dropdown t (Maybe Int)))
  -> m (KeysetInputs t (Dropdown t (Maybe Int)) PublicKey)
defineKeyset model preselections0 = do
  let
    selectMsgKey = Nothing
    selectMsgMap = Map.singleton selectMsgKey "Select"

    keyElem k v = Map.singleton (Just k) $ keyToText $ _keyPair_publicKey $ _key_pair v

    dAllKeys = mappend selectMsgMap . IntMap.foldMapWithKey keyElem <$> model ^. wallet_keys

    uiSelectKey k = mkLabeledClsInput False (constDyn T.empty) $ const
      $ uiDropdown k dAllKeys $ def
      & dropdownConfig_attributes .~ constDyn ("class" =: "labeled-input__input")

    toIntSet :: IntMap.IntMap (Dropdown t (Maybe Int)) -> Dynamic t IntSet.IntSet
    toIntSet = fmap (IntSet.fromList . IntMap.elems) . wither value

    preselections = preselections0 >>= IntMap.foldMapWithKey
      (\k dd -> IntMap.singleton k . Just <$> _dropdown_value dd)

  pb <- getPostBuild

  dSelectedKeys <- uiAdditiveInput
    (const uiSelectKey)
    _dropdown_change
    (/= selectMsgKey)
    (== selectMsgKey)
    selectMsgKey
    (current preselections <@ pb)

  pure $ KeysetInputs dSelectedKeys $ ffor2 (model ^. wallet_keys) (dSelectedKeys >>= toIntSet) $ \wKeys ->
    Set.fromDistinctAscList . IntMap.elems . fmap (_keyPair_publicKey . _key_pair) . IntMap.restrictKeys wKeys

uiDefineKeyset
  :: ( MonadWidget t m
     , HasWallet model key t
     , HasJsonData model t
     )
  => model
  -> DefinedKeyset t
  -> m (Dynamic t (Maybe AddressKeyset), DefinedKeyset t)
uiDefineKeyset model presets = do
  pb <- getPostBuild
  let
    allPreds = fmap (catMaybes . Map.elems)
      $ joinDynThroughMap
      $ fmap _keyset_pred
      <$> model ^. jsonData_keysets

    allPredSelectMap = ffor allPreds $ \ps ->
      Map.fromList . fmap (\x -> (x,x)) $ ps <> predefinedPreds

  rec
    selectedKeys <- mkLabeledClsInput False "Chainweaver Keys" $ const
      $ defineKeyset model $ _keysetInputs_value $ _definedKeyset_internalKeys presets

    externalKeys <- mkLabeledClsInput False "External Keys" $ const
      $ uiExternalKeyInput $ _keysetInputs_value $ _definedKeyset_externalKeys presets

    predicate <- mkLabeledClsInput False "Predicate (Keys Required to Sign for Account)" $ const
      $ fmap value $ uiDropdown mempty allPredSelectMap $ def
      & dropdownConfig_attributes .~ constDyn ("class" =: "labeled-input__input")
      & dropdownConfig_setValue .~ (current (_definedKeyset_predicate presets) <@ pb)

  -- TODO validate this (?? validation ??)
  let
    ipks = _keysetInputs_set selectedKeys
    epks = _keysetInputs_set externalKeys

  pure ( mkAddressKeyset <$> (ipks <> epks) <*> predicate
       , DefinedKeyset selectedKeys externalKeys predicate
       )
