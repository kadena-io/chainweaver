{-# LANGUAGE RecursiveDo #-}
module Frontend.UI.Dialogs.AddVanityAccount.DefineKeyset
  ( DefinedKeyset
  , uiDefineKeyset
  , emptyKeysetPresets
  ) where

import           Control.Lens                           ((^.),ifoldMap)
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
  { _definedKeyset_internalKeys :: KeysetInputs t (Maybe (Maybe Int)) PublicKey
  , _definedKeyset_externalKeys :: KeysetInputs t (Maybe (Maybe PublicKey)) PublicKey
  , _definedKeyset_predicate :: Dynamic t Text
  }

emptyKeysetPresets :: forall t. Reflex t => DefinedKeyset t
emptyKeysetPresets = DefinedKeyset
  { _definedKeyset_internalKeys = KeysetInputs mempty mempty
  , _definedKeyset_externalKeys = KeysetInputs mempty mempty
  , _definedKeyset_predicate = mempty
  }

data ExternalKeyInput t = ExternalKeyInput
  { _externalKeyInput_input :: Event t (Maybe PublicKey)
  , _externalKeyInput_value :: Dynamic t (Maybe PublicKey)
  }

uiExternalKeyInput
  :: forall t m. MonadWidget t m
  => Event t (IntMap (Maybe (Maybe PublicKey)))
  -> m (KeysetInputs t (Maybe (Maybe PublicKey)) PublicKey)
uiExternalKeyInput onPreselection = do
  let
    uiPubkeyInput :: Maybe PublicKey -> m (ExternalKeyInput t)
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
        & inputElementConfig_initialValue .~ maybe T.empty keyToText iv

      pure $ ExternalKeyInput
        { _externalKeyInput_input = (hush . parsePublicKey) <$> _inputElement_input inp
        , _externalKeyInput_value = hush <$> dE
        }

    toSet :: IntMap.IntMap (ExternalKeyInput t) -> Dynamic t (Set PublicKey)
    toSet = fmap (Set.fromList . IntMap.elems) . wither _externalKeyInput_value

  dExternalKeyInput <- uiAdditiveInput
    (const uiPubkeyInput)
    _externalKeyInput_input
    isJust
    isNothing
    Nothing
    onPreselection

  let dFormState :: Dynamic t (IntMap.IntMap (Maybe (Maybe PublicKey)))
      dFormState = dExternalKeyInput >>= IntMap.foldMapWithKey
        (\k t -> IntMap.singleton k . Just <$> _externalKeyInput_value t)

  pure $ KeysetInputs dFormState (dExternalKeyInput >>= toSet)

defineKeyset
  :: forall t m key model
     . ( MonadWidget t m
       , HasWallet model key t
       )
  => model
  -> Event t (IntMap (Maybe (Maybe Int)))
  -> m (KeysetInputs t (Maybe (Maybe Int)) PublicKey)
defineKeyset model onPreselection = do
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

  dSelectedKeys <- uiAdditiveInput
    (const uiSelectKey)
    _dropdown_change
    (/= selectMsgKey)
    (== selectMsgKey)
    selectMsgKey
    onPreselection

  let dFormState :: Dynamic t (IntMap.IntMap (Maybe (Maybe Int)))
      dFormState = fmap (ifoldMap (\k v -> IntMap.singleton k $ Just v))
        $ joinDynThroughMap
        $ fmap (Map.fromList . IntMap.assocs . fmap value) dSelectedKeys

  pure $ KeysetInputs dFormState $ ffor2 (model ^. wallet_keys) (dSelectedKeys >>= toIntSet) $ \wKeys ->
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
      $ defineKeyset model $ current (_keysetInputs_value $ _definedKeyset_internalKeys presets) <@ pb

    externalKeys <- mkLabeledClsInput False "External Keys" $ const
      $ uiExternalKeyInput $ current (_keysetInputs_value $ _definedKeyset_externalKeys presets) <@ pb

    predicate <- mkLabeledClsInput False "Predicate (Keys Required to Sign for Account)" $ const
      $ fmap value $ uiDropdown mempty allPredSelectMap $ def
      & dropdownConfig_attributes .~ constDyn ("class" =: "labeled-input__input")
      & dropdownConfig_setValue .~ (current (_definedKeyset_predicate presets) <@ pb)

  let
    ipks = _keysetInputs_set selectedKeys
    epks = _keysetInputs_set externalKeys

  pure ( mkAddressKeyset <$> (ipks <> epks) <*> predicate
       , DefinedKeyset selectedKeys externalKeys predicate
       )
