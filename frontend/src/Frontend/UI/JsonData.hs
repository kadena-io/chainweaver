{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | UI for handling the data JSON object that gets attached to transactions.
--
--   The user gets an editor for entering arbitrary data, in additon a widget
--   for adding keysets based on the available keys is offered.
--
--   Copyright   :  (C) 2020 Kadena
--   License     :  BSD-style (see the file LICENSE)
--
module Frontend.UI.JsonData
  ( -- * Key management widget
    uiJsonData
  , uiJsonDataSetFocus
  , uiCreateKeysets
  , uiJsonDataResult
  -- * Useful bits
  , uiKeysetKeys
  , predDropdown
  ) where

------------------------------------------------------------------------------
import           Control.Lens                       hiding ((.=))
import           Control.Monad
import           Data.Aeson.Encode.Pretty           (encodePretty)
import qualified Data.Aeson                         as Aeson
import qualified Data.ByteString.Lazy               as BSL
import qualified Data.HashMap.Strict                as H
import qualified Data.Map                           as Map
import qualified Data.IntMap                        as IntMap
import           Data.Maybe
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Char                          as Char
import           Reflex.Class.Extended
import           Reflex.Dom
import           Reflex.Dom.ACE.Extended
import           Obelisk.Generated.Static
------------------------------------------------------------------------------
import           Frontend.Foundation
import           Frontend.JsonData
import           Frontend.UI.Widgets
import           Frontend.Wallet
import           Frontend.UI.TabBar
------------------------------------------------------------------------------


-- | What to show to the user.
--
data JsonDataView
  = JsonDataView_Keysets -- ^ Keyset editor
  | JsonDataView_Raw -- ^ Raw JSON input widget
  | JsonDataView_Result -- ^ Combined result, that will be sent over the wire.
  deriving (Eq,Ord,Enum,Bounded)

showJsonTabName :: JsonDataView -> Text
showJsonTabName JsonDataView_Keysets = "Keysets"
showJsonTabName JsonDataView_Raw     = "Raw"
showJsonTabName JsonDataView_Result  = "Result"

-- | Default UI for entering new keysets, returns focus to the right panel on the main
-- input screen when new keysets are added either through the keyset field or the raw JSON
-- input.
--
uiJsonData
  :: ( MonadWidget t m, HasJsonDataCfg mConf t, Monoid mConf, Flattenable mConf t
     )
  => Wallet key t
  -> JsonData t
  -> m mConf
uiJsonData = uiJsonDataSetFocus
  (\e -> setFocusOnSelected e "input" JsonDataView_Keysets . updated)
  (\e -> setFocusOnSelected e ".ace_text-input" JsonDataView_Raw . updated)

-- | UI for creating and handling keyset information.
--
-- Takes two functions that are called when a new keyset is created or new json data is
-- entered, respectively.
--
uiJsonDataSetFocus
  :: ( MonadWidget t m, HasJsonDataCfg mConf t, Monoid mConf, Flattenable mConf t
     )
  => (Element EventResult GhcjsDomSpace t -> Dynamic t JsonDataView -> m ())
  -> (Element EventResult GhcjsDomSpace t -> Dynamic t JsonDataView -> m ())
  -> Wallet key t
  -> JsonData t
  -> m mConf
uiJsonDataSetFocus onKeysetCreate onRawInputCreate w d = divClass "tabset" $ mdo
    curSelection <- holdDyn JsonDataView_Keysets onTabClick
    (TabBar onTabClick) <- makeTabBar $ TabBarCfg
      { _tabBarCfg_tabs = [minBound .. maxBound]
      , _tabBarCfg_mkLabel = const $ text . showJsonTabName
      , _tabBarCfg_selectedTab = Just <$> curSelection
      , _tabBarCfg_classes = mempty
      , _tabBarCfg_type = TabBarType_Primary
      }

    keysetVCfg <- tabPane mempty curSelection JsonDataView_Keysets $ do
      (e, keysetCfgL) <- uiCreateKeysets w d
      onKeysetCreate e curSelection
      pure keysetCfgL

    rawVCfg <- tabPane mempty curSelection JsonDataView_Raw $ do
      onNewData <- tagOnPostBuild $ d ^. jsonData_rawInput

      let
        onDupWarning = mkDupWarning <$> (updated $ d ^. jsonData_overlappingProps)
        onObjWarning = either mkObjError (const []) <$> updated (d ^. jsonData . to getJsonDataObjectStrict)
        onUpperCaseWarning = either mkObjError anyNameUpperCase <$> updated (d ^. jsonData . to getJsonDataObjectStrict)
        onAnno = mconcat [ onObjWarning, onDupWarning, onUpperCaseWarning ]

      (e, onSetRawInput) <- elClass' "div" "wysiwyg wysiwyg_height_30" $ dataEditor onAnno "" onNewData
      onRawInputCreate e curSelection
      pure $ mempty & jsonDataCfg_setRawInput .~ onSetRawInput

    tabPane mempty curSelection JsonDataView_Result $ do
      uiJsonDataResult $ getJsonDataObjectLax d

    pure $ mconcat [ keysetVCfg, rawVCfg ]
  where
    anyNameUpperCase :: Aeson.Object -> [AceAnnotation]
    anyNameUpperCase o =
      if getAny . foldMap (Any . T.any Char.isUpper) $ H.keys o
      then mkUpperCaseError
      else []

    mkUpperCaseError = mkAnnotation "error" "Keyset names must be all lowercase"
    mkObjError = mkAnnotation "error" . showJsonError
    mkDupWarning dups =
      if Set.null dups
         then []
         else
           let
             t = "The following properties are overriden by keysets: "
             props =
               T.intercalate ", " . Set.toList $ dups
             ft = t <> props
           in
             mkAnnotation "warning" ft

    mkAnnotation aType msg =
       [ AceAnnotation
         { _aceAnnotation_row = 0 -- For simplicity, good enough for now.
         , _aceAnnotation_column = 0
         , _aceAnnotation_text = msg
         , _aceAnnotation_type = aType
         }
       ]

uiJsonDataResult :: (DomBuilder t m, PostBuild t m) => Dynamic t Aeson.Object -> m ()
uiJsonDataResult = el "pre" . dynText . fmap (safeDecodeUtf8 . BSL.toStrict . encodePretty)

uiCreateKeysets
  :: (MonadWidget t m, Monoid mConf, HasJsonDataCfg mConf t, Flattenable mConf t)
  => Wallet key t -> JsonData t -> m (Element EventResult (DomBuilderSpace m) t, mConf)
uiCreateKeysets w d = elClass' "div" "keysets group" $ do
  onCreateKeyset <- uiCreateKeyset d
  ksCfg <- elClass "div" "keyset-list" $
    networkViewFlatten $ uiKeysets w <$> d ^. jsonData_keysets
  pure $ ksCfg & jsonDataCfg_createKeyset .~ onCreateKeyset

uiKeysets
  :: (MonadWidget t m, Monoid mConf, HasJsonDataCfg mConf t)
  => Wallet key t -> DynKeysets t -> m mConf
uiKeysets w ksM = do
    case Map.toList ksM of
      []   -> do
        divClass "keyset" $ text "No keysets yet ..."
        pure mempty
      kss -> do
        rs <- traverse (divClass "keyset" . uiKeyset w) kss
        pure $ mconcat rs

-- | Display a single keyset on the screen.
uiKeyset
  :: (MonadWidget t m, Monoid mConf, HasJsonDataCfg mConf t)
  => Wallet key t
  -> (KeysetName, DynKeyset t)
  -> m mConf
uiKeyset w (n, ks) = do
    aCfg <- divClass "keyset__header" $ mdo
      elAttr "h4" ("class" =: "keyset__header-text heading_type_h4") $ do
        elClass "span" "keyset__icon" $ imgWithAlt $(static "img/keys.svg") "Keyset" blank
        text n

      onNewPred <- tagOnPostBuild . fmap (fromMaybe "") $ ks ^. keyset_pred
      onSetPred <- predDropdown onNewPred

      onDel <- divClass "keyset__delete" $
        deleteButton $ def & uiButtonCfg_title .~ Just "Delete keyset permanently"
      let
        setPred = (n, ) . notEmpty <$> onSetPred
        cfg1 = mempty
          & jsonDataCfg_setPred .~ setPred
          & jsonDataCfg_delKeyset .~ fmap (const n) onDel
      return cfg1

    onKeyClick <- uiKeysetKeys (_keyset_keys ks) (fmap (_keyPair_publicKey . _key_pair) . IntMap.elems <$> _wallet_keys w)
    let
      onAddKey = fmap fst . ffilter snd $ onKeyClick
      onDelKey = fmap fst . ffilter (not . snd) $ onKeyClick
      bCfg = mempty
        & jsonDataCfg_addKey .~ fmap (n, ) onAddKey
        & jsonDataCfg_delKey .~ fmap (n, ) onDelKey
    pure (aCfg <> bCfg)

notEmpty :: Text -> Maybe Text
notEmpty s = if T.null s then Nothing else Just s

predDropdown :: MonadWidget t m => Event t KeysetPredicate -> m (Event t KeysetPredicate)
predDropdown sv = do
    let
      itemDom v = elAttr "option" ("value" =: v) $ text v
      cfg = SelectElementConfig "" (Just sv) $
        def & initialAttributes .~ "class" =: "select_type_tertiary select_tiny keyset__pred"

    (s,_) <- uiSelectElement cfg $ do
      forM_ predefinedPreds itemDom
    return $ _selectElement_change s

-- | Input widget with confirm button for creating a new keyset.
uiCreateKeyset
  :: MonadWidget t m
  => JsonData t -> m (Event t Text)
uiCreateKeyset jsonD =
  validatedInputWithButton "group__header" check "Enter keyset name" "Create"
    where
      -- Check combined data and not only keyset names for duplicates:
      check =
        mkCheck <$> _jsonData_data jsonD

      mkCheck (keysetsObj, rawParse) ks =
        let dupe = H.member ks $ keysetsObj <> fold rawParse
        in
          if dupe then Left "This keyset name is already in use"
          else if T.any Char.isUpper ks then Left "Keyset name must be all lowercase"
          else Right ks

-- | Widget showing all avaialble keys for selecting keys
--
--   for the keyset.
uiKeysetKeys
  :: MonadWidget t m
  => Dynamic t KeysetKeys
  -> Dynamic t [PublicKey]
  -> m (Event t (PublicKey, Bool))
uiKeysetKeys ks allKeys =
  elClass "ul" "keyset__keys" $ do
    mKeys <- maybeDyn $ ffor allKeys $ \case
      [] -> Nothing
      xs -> Just $ Map.fromList $ (,()) <$> xs
    switchHold never <=< dyn $ ffor mKeys $ \case
      Nothing -> do
        el "li" $ text "No keys available ..."
        pure never
      Just keysMap -> do
        rs <- listWithKey keysMap $ \pk _ -> uiKeysetKey ks pk
        pure $ switch $ leftmost . Map.elems <$> current rs

-- | Show a single Keyset key item.
uiKeysetKey
  :: MonadWidget t m
  => Dynamic t KeysetKeys
  -> PublicKey
  -> m (Event t (PublicKey, Bool))
uiKeysetKey ks n = el "li" $ do
    onSelected <- tagOnPostBuild $ Set.member n <$> ks
    let cfg = def & checkboxConfig_setValue .~ onSelected
    cb <- uiCheckbox "keyset__key-label" False cfg $ text $ keyToText n
    pure $ (n,) <$> _checkbox_change cb

dataEditor
  :: MonadWidget t m
  => Event t [AceAnnotation]
  -> Text
  -> Event t Text
  -> m (Event t Text)
dataEditor anno iv sv = do
    let ac = def { _aceConfigMode = Just "ace/mode/json"
                 , _aceConfigElemAttrs = "class" =: "ace-data ace-widget"
                 }
    ace <- resizableAceWidget never
      mempty ac (AceDynConfig Nothing) anno iv sv
    return $ _extendedACE_onUserChange ace
