{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontend.UI.Dialogs.AddVanityAccount.DefineKeyset
  ( DefinedKeyset (..)
  , HasDefinedKeyset (..)
  , KeysetInputs (..)
  , HasKeysetInputs (..)
  , uiDefineKeyset
  , emptyKeysetPresets
  ) where

import           Control.Arrow                          ((&&&))
import           Control.Lens hiding (element)
import           Control.Error                          (hush)
import           Data.Witherable                        (wither)
import           Data.Text                              (Text)
import qualified Data.Text as T
import qualified Data.IntSet as IntSet
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
  { _keysetInputs_value :: Dynamic t (PatchIntMap i)
  , _keysetInputs_set :: Dynamic t (Set a)
  , _keysetInputs_rowAddDelete :: Event t (PatchIntMap i)
  , _keysetInputs_rowChange :: Event t ()
  }

data DefinedKeyset t = DefinedKeyset
  { _definedKeyset_internalKeys :: KeysetInputs t (Maybe Int) PublicKey
  , _definedKeyset_externalKeys :: KeysetInputs t (Maybe Text) PublicKey
  , _definedKeyset_predicate :: Dynamic t (Maybe Text)
  , _definedKeyset_predicateChange :: Event t (Maybe Text)
  }

emptyKeysetPresets :: forall t. Reflex t => DefinedKeyset t
emptyKeysetPresets = DefinedKeyset
  { _definedKeyset_internalKeys = KeysetInputs mempty mempty never never
  , _definedKeyset_externalKeys = KeysetInputs mempty mempty never never
  , _definedKeyset_predicate = mempty
  , _definedKeyset_predicateChange = never
  }

data ExternalKeyInput t = ExternalKeyInput
  { _externalKeyInput_input :: Event t (Maybe PublicKey)
  , _externalKeyInput_value :: Dynamic t (Maybe PublicKey)
  , _externalKeyInput_raw_input :: Event t Text
  , _externalKeyInput_raw_value :: Dynamic t Text
  }

uiExternalKeyInput
  :: forall t m. MonadWidget t m
  => Event t (PatchIntMap (Maybe Text))
  -> m (KeysetInputs t (Maybe Text) PublicKey)
uiExternalKeyInput onPreselection = do
  let
    uiPubkeyInput :: Maybe Text -> m (ExternalKeyInput t)
    uiPubkeyInput iv = do
      let
        inp cfg = do
          ie <- mkLabeledInput False mempty uiInputElement cfg
          pure (ie
               , ( parsePublicKey <$> value ie
                 , parsePublicKey <$> _inputElement_input ie
                 )
               )

        inputCfg = def
          & initialAttributes .~ ("placeholder" =: "Enter public key (optional)")
          & inputElementConfig_initialValue .~ fold iv

        showPopover (_, (_, onInput)) = pure $
          either PopoverState_Error (const PopoverState_Disabled) <$> onInput

      (inputE, (dE, onE)) <- uiInputWithPopover
        inp
        (_inputElement_raw . fst)
        showPopover
        inputCfg

      pure $ ExternalKeyInput
        { _externalKeyInput_input = hush <$> onE
        , _externalKeyInput_value = hush <$> dE
        , _externalKeyInput_raw_input = _inputElement_input inputE
        , _externalKeyInput_raw_value = _inputElement_value inputE
        }

    toSet :: IntMap.IntMap (ExternalKeyInput t) -> Dynamic t (Set PublicKey)
    toSet = fmap (Set.fromList . IntMap.elems) . wither _externalKeyInput_value

  let doAddDel yesno =
        fmap (yesno . T.null) . _externalKeyInput_raw_input

  (dExternalKeyInput, onRowAddDel) <- uiAdditiveInput
    (const uiPubkeyInput)
    (AllowAddNewRow $ doAddDel not)
    (AllowDeleteRow $ doAddDel id)
    Nothing
    (traceEventWith (\(PatchIntMap m) -> "onPreselection: " <> show m) onPreselection)

  let
    dFormState :: Dynamic t (PatchIntMap (Maybe Text))
    dFormState = dExternalKeyInput >>= fmap (PatchIntMap . ensureInputLine . newIntMap . IntMap.elems)
      . wither _externalKeyInput_value
      where
        newIntMap = ifoldMap (\k -> IntMap.singleton k . Just . Just . keyToText)
        ensureInputLine im = case IntMap.lookupMax im of
          Nothing -> IntMap.singleton 0 (Just $ Just T.empty)
          Just (mkey, _) -> IntMap.insert (succ mkey) (Just $ Just T.empty) im

  pure $ KeysetInputs dFormState (dExternalKeyInput >>= toSet) onRowAddDel
    $ switchDyn $ foldMap ((() <$) . _externalKeyInput_raw_input) <$> dExternalKeyInput

defineKeyset
  :: forall t m key model
     . ( MonadWidget t m
       , HasWallet model key t
       )
  => model
  -> Event t (PatchIntMap (Maybe Int))
  -> m (KeysetInputs t (Maybe Int) PublicKey)
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

  let doAddDel yesno =
        fmap (yesno selectMsgKey) . _dropdown_change

  (dSelectedKeys, onRowAddDel) <- uiAdditiveInput
    (const uiSelectKey)
    (AllowAddNewRow $ doAddDel (/=))
    (AllowDeleteRow $ doAddDel (==))
    selectMsgKey
    onPreselection

  let dFormState :: Dynamic t (PatchIntMap (Maybe Int))
      dFormState = fmap PatchIntMap
        $ fmap (ifoldMap (\k v -> IntMap.singleton k $ Just v))
        $ joinDynThroughMap
        $ fmap (Map.fromList . IntMap.assocs . fmap value) dSelectedKeys

      x = ffor2 (model ^. wallet_keys) (dSelectedKeys >>= toIntSet) $ \wKeys -> Set.fromDistinctAscList
        . IntMap.elems
        . fmap (_keyPair_publicKey . _key_pair)
        . IntMap.restrictKeys wKeys

  pure $ KeysetInputs dFormState x onRowAddDel
    $ switchDyn $ foldMap ((() <$) . _dropdown_change) <$> dSelectedKeys

uiDefineKeyset
  :: ( MonadWidget t m
     , HasWallet model key t
     , HasJsonData model t
     )
  => model
  -> DefinedKeyset t
  -> m (Dynamic t (Maybe AccountGuard), DefinedKeyset t)
uiDefineKeyset model presets = do
  let
    allPreds = fmap (catMaybes . Map.elems)
      $ joinDynThroughMap
      $ fmap _keyset_pred
      <$> model ^. jsonData_keysets

    allPredSelectMap nkeys = ffor2 nkeys allPreds $ \nks ps -> Map.fromList
      $ fmap (Just &&& id) (dropkeys2 nks $ ps <> predefinedPreds)
      where
        dropkeys2 n xs | n >= 2 = xs
                       | otherwise = filter (/= keys2Predicate) xs

  rec
    selectedKeys <- mkLabeledClsInput False "Chainweaver Generated Keys" $ const
      $ defineKeyset model $ _keysetInputs_rowAddDelete $ _definedKeyset_internalKeys presets

    externalKeys <- mkLabeledClsInput False "Externally Generated Keys" $ const
      $ uiExternalKeyInput $ _keysetInputs_rowAddDelete $ _definedKeyset_externalKeys presets

    let allpks = _keysetInputs_set selectedKeys <> _keysetInputs_set externalKeys

    predicateE <- mkLabeledClsInput False "Predicate (Keys Required to Sign for Account)" $ const
      $ uiDropdown (Just defaultPredicate) (allPredSelectMap $ fmap Set.size allpks) $ def
      & dropdownConfig_attributes .~ constDyn ("class" =: "labeled-input__input")
      & dropdownConfig_setValue .~ _definedKeyset_predicateChange presets

  pure ( ffor2 allpks (value predicateE) $ \pks kspred -> kspred >>= \k -> mkAccountGuard pks k Nothing
       , DefinedKeyset selectedKeys externalKeys (value predicateE) (_dropdown_change predicateE)
       )

makePactLenses ''KeysetInputs
makePactLenses ''DefinedKeyset
