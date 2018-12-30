{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

-- | UI for handling the data JSON object that gets attached to transactions.
--
--   The user gets an editor for entering arbitrary data, in additon a widget
--   for adding keysets based on the available keys is offered.
--
--   Copyright   :  (C) 2018 Kadena
--   License     :  BSD-style (see the file LICENSE)
--
module Frontend.UI.JsonData
  ( -- * Key management widget
    uiJsonData
  ) where

------------------------------------------------------------------------------
import           Control.Lens                       hiding ((.=))
import           Control.Monad
import           Data.Aeson.Encode.Pretty           (encodePretty)
import qualified Data.ByteString.Lazy               as BSL
import qualified Data.HashMap.Strict                as H
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           Reflex.Class.Extended
import           Reflex.Dom
import           Reflex.Dom.ACE.Extended
import           Reflex.Dom.Contrib.CssClass
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

mkDataAttr :: JsonDataView -> Map Text Text
mkDataAttr t = "data-tabname" =: ("env-" <> T.toLower (showJsonTabName t))

uiJsonData
  :: MonadWidget t m
  => Wallet t
  -> JsonData t
  -> m (JsonDataCfg t)
uiJsonData w d = divClass "tabset" $ mdo
    curSelection <- holdDyn JsonDataView_Keysets onTabClick
    (TabBar onTabClick) <- makeTabBar $ TabBarCfg 
      { _tabBarCfg_tabs = [minBound .. maxBound]
      , _tabBarCfg_mkLabel = const $ text . showJsonTabName
      , _tabBarCfg_selectedTab = Just <$> curSelection
      , _tabBarCfg_classes = mempty
      , _tabBarCfg_type = TabBarType_Secondary
      }

    keysetVCfg <-tabPane ("class" =: "tab-content") curSelection JsonDataView_Keysets $ do
      (e, keysetCfgL) <- elClass' "div" "keysets group" $ do
        onCreateKeyset <- uiCreateKeyset d
        ksCfg <- elClass "div" "keyset-list" $
          networkViewFlatten $ uiKeysets w <$> d ^. jsonData_keysets

        pure $ ksCfg & jsonDataCfg_createKeyset .~ onCreateKeyset
      setFocusOnSelected e "input" JsonDataView_Keysets $ updated curSelection
      pure keysetCfgL

    rawVCfg <- tabPane ("class" =: "tab-content") curSelection JsonDataView_Raw $ do
      onNewData <- tagOnPostBuild $ d ^. jsonData_rawInput

      let
        onDupWarning = mkDupWarning <$> (updated $ d ^. jsonData_overlappingProps)
        onObjWarning = fmapMaybe (^? _Left . to mkObjError) $ updated (d ^. jsonData_data)
        onAnno = mconcat [ onObjWarning, onDupWarning ]

      (e, onSetRawInput) <- elClass' "div" "wysiwyg" $ dataEditor onAnno "" onNewData
      setFocusOnSelected e ".ace_text-input" JsonDataView_Raw $ updated curSelection
      pure $ mempty & jsonDataCfg_setRawInput .~ onSetRawInput

    tabPane ("class" =: "tab-content")
        curSelection JsonDataView_Result $ do
      let
        showData =
          either showJsonError (T.decodeUtf8 . BSL.toStrict . encodePretty)
      el "pre" $ dynText $ showData <$> d ^. jsonData_data

    pure $ mconcat [ keysetVCfg, rawVCfg ]
  where
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

uiKeysets
  :: (MonadWidget t m)
  => Wallet t -> DynKeysets t -> m (JsonDataCfg t)
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
  :: (MonadWidget t m)
  => Wallet t
  -> (KeysetName, DynKeyset t)
  -> m (JsonDataCfg t)
uiKeyset w (n, ks) = do
    aCfg <- divClass "keyset__header" $ mdo
      elAttr "h4" ("class" =: "keyset__header-text h4") $ do
        elClass "span" "keyset__icon" $ imgWithAlt (static @"img/keys.svg") "Keyset" blank
        text n

      onNewPred <- tagOnPostBuild . fmap (fromMaybe "") $ ks ^. keyset_pred
      onSetPred <- predDropdown onNewPred

      onDel <- divClass "keyset__delete" $ deleteButton
      let setPred = (n, ) . notEmpty <$> onSetPred
      let cfg1 = mempty
            { _jsonDataCfg_setPred = setPred
            , _jsonDataCfg_delKeyset = fmap (const n) onDel
            }
      return cfg1

    onKeyClick <- switchHold never <=< networkView $ uiKeysetKeys (_keyset_keys ks) . Map.keys <$> _wallet_keys w
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
        def & initialAttributes .~ "class" =: "select select_type_tertiary select_tiny keyset__pred"

    (s,_) <- selectElement cfg $ do
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
      check ks = do
        json <- sample $ current $ _jsonData_data jsonD
        keysets <- sample $ current $ _jsonData_keysets jsonD
        let dupe = case json of
              Left _  -> Map.member ks keysets
              Right j -> H.member ks j
        pure $ if dupe then Just "This keyset name is already in use." else Nothing

-- | Widget showing all avaialble keys for selecting keys
--
--   for the keyset.
uiKeysetKeys
  :: MonadWidget t m
  => Dynamic t KeysetKeys
  -> [KeyName]
  -> m (Event t (KeyName, Bool))
uiKeysetKeys ks allKeys =
  elClass "ul" "keyset_keys" $ do
    case allKeys of
      []   -> do
        el "li" $ text "No keys available ..."
        pure never
      _ -> do
        rs <- traverse (uiKeysetKey ks) allKeys
        pure $ leftmost rs

-- | Show a single Keyset key item.
uiKeysetKey
  :: MonadWidget t m
  => Dynamic t KeysetKeys
  -> KeyName
  -> m (Event t (KeyName, Bool))
uiKeysetKey ks n = el "li" $ do
    onSelected <- tagOnPostBuild $ Map.member n <$> ks
    cb <- checkbox False $ def & checkboxConfig_setValue .~ onSelected
    text n
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
    ace <- resizableAceWidget
      mempty ac (AceDynConfig Nothing) anno iv sv
    return $ _extendedACE_onUserChange ace
