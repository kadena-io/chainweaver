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
import           Control.Lens                hiding ((.=))
import           Control.Monad
import           Data.Aeson.Encode.Pretty    (encodePretty)
import           Data.Bool
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.HashMap.Strict         as H
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Reflex.Class.Extended
import           Reflex.Dom.ACE.Extended
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Dom.Contrib.Vanishing
import qualified Reflex.Dom.Contrib.Widgets.DynTabs as Tabs
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.Foundation
import           Frontend.Ide
import           Frontend.JsonData
import           Frontend.UI.Icon
import           Frontend.Wallet
import           Frontend.Widgets
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
showJsonTabName JsonDataView_Raw = "Raw"
showJsonTabName JsonDataView_Result = "Result"

mkDataAttr :: JsonDataView -> Map Text Text
mkDataAttr t = "data-tabname" =: ("env-" <> T.toLower (showJsonTabName t))

instance MonadWidget t m => Tabs.Tab t m JsonDataView where
  tabIndicator t isActive = do
    let mkAttrs True = "class" =: "active"
        mkAttrs False = mempty
    (e,_) <- elDynAttr' "button" ((mkDataAttr t <>) . mkAttrs <$> isActive) $
      text $ showJsonTabName t
    return $ domEvent Click e

tabPaneActive
  :: (MonadWidget t m)
  => Map Text Text
  -> Dynamic t JsonDataView
  -> JsonDataView
  -> m a
  -> m a
tabPaneActive staticAttrs currentTab t child = do
  let mkKlass cur = addToClassAttr
        (if cur == t then singleClass "active" else mempty) (staticAttrs <> mkDataAttr t)
  elDynAttr "div" (mkKlass <$> currentTab) child

uiJsonData
  :: MonadWidget t m
  => Wallet t
  -> JsonData t
  -> m (JsonDataCfg t)
uiJsonData w d = divClass "tabset" $ mdo
    tabs <- divClass "tab-nav" $ Tabs.tabBar def
    let curSelection = Tabs._tabBar_curTab tabs
    keysetVCfg <- tabPaneActive ("class" =: "tab-content")
        curSelection JsonDataView_Keysets $ do
      divClass "keys" $ do
        onCreateKeyset <- uiCreateKeyset d
        ksCfg <- elClass "div" "keyset-list" $
          networkViewFlatten $ uiKeysets w <$> d ^. jsonData_keysets

        pure $ ksCfg & jsonDataCfg_createKeyset .~ onCreateKeyset

    rawVCfg <- tabPaneActive ("class" =: "tab-content")
        curSelection JsonDataView_Raw $ do
      onNewData <- tagOnPostBuild $ d ^. jsonData_rawInput

      let
        onDupWarning = mkDupWarning <$> (updated $ d ^. jsonData_overlappingProps)

      onSetRawInput <- elClass "div" "wysiwyg" $ dataEditor onDupWarning "" onNewData
      pure $ mempty & jsonDataCfg_setRawInput .~ onSetRawInput

    tabPaneActive ("class" =: "tab-content")
        curSelection JsonDataView_Result $ do
      let
        showData =
          either showJsonError (T.decodeUtf8 . BSL.toStrict . encodePretty)
      el "pre" $ dynText $ showData <$> d ^. jsonData_data

    pure $ mconcat [ keysetVCfg, rawVCfg ]
  where
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
             [ AceAnnotation
               { _aceAnnotation_row = 0 -- For simplicity, good enough for now.
               , _aceAnnotation_column = 0
               , _aceAnnotation_text = ft
               , _aceAnnotation_type = "warning"
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
        pure mempty -- $ mconcat rs

-- | Display a single keyset on the screen.
uiKeyset
  :: (MonadWidget t m)
  => Wallet t
  -> (KeysetName, DynKeyset t)
  -> m (JsonDataCfg t)
uiKeyset w (n, ks) = do
    aCfg <- divClass "header" $ mdo
      elAttr' "h4" ("class" =: "keyset-chooser-toggle") $ text (" " <> n)

      onSetPred <- divClass "pred" $ do
        el "label" $ text "Pred:"
        onNewPred <- tagOnPostBuild . fmap (fromMaybe "") $ ks ^. keyset_pred
        predDropdown onNewPred

      onDel <- divClass "delete" $ do
        el "button" $ uiIcon "fa-trash" $ def
          & iconConfig_size .~ Just IconLG
      let setPred = (n, ) . notEmpty <$> onSetPred
      let cfg1 = mempty
            { _jsonDataCfg_setPred = updated setPred
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

predDropdown :: MonadWidget t m => Event t KeysetPredicate -> m (Dynamic t KeysetPredicate)
predDropdown sv = do
    let preds = ["keys-all", "keys-2", "keys-any"]
        m = Map.fromList $ map (\x -> (x,x)) preds
    d <- dropdown (head preds) (constDyn m) $ def & setValue .~ traceEvent "sv" sv

    -- Should only fail if people are manually supplying bad input
    return $ (m Map.!) <$> value d

-- | Input widget with confirm button for creating a new keyset.
uiCreateKeyset
  :: MonadWidget t m
  => JsonData t -> m (Event t Text)
uiCreateKeyset jsonD = validatedInputWithButton check "Enter keyset name" "Create"
  where
    -- Check combined data and not only keyset names for duplicates:
    check ks = do
      json <- sample $ current $ _jsonData_data jsonD
      keysets <- sample $ current $ _jsonData_keysets jsonD
      let dupe = case json of
            Left _  -> Map.member ks keysets
            Right j -> H.member ks j
      pure $ if dupe then Left "This keyset name is already in use." else Right ks

-- | Widget showing all avaialble keys for selecting keys
--
--   for the keyset.
uiKeysetKeys
  :: MonadWidget t m
  => Dynamic t KeysetKeys
  -> [KeyName]
  -> m (Event t (KeyName, Bool))
uiKeysetKeys ks allKeys =
  elClass "div" "keys-list" $ el "ul" $ do
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

viewToText :: JsonDataView -> Text
viewToText = \case
  JsonDataView_Keysets -> "Keysets"
  JsonDataView_Raw -> "JSON"
  JsonDataView_Result -> "Combined Result"
