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
import           Control.Lens
import           Control.Monad
import           Data.Aeson.Encode.Pretty    (encodePretty)
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Language.Javascript.JSaddle (js0, liftJSM, pToJSVal)
import           Reflex.Class.Extended
import           Reflex.Dom.ACE.Extended
import           Reflex.Dom.Core             (keypress, _textInput_element)
import           Reflex.Dom.SemanticUI       hiding (mainWidget)

import           Frontend.Foundation
import           Frontend.JsonData
import           Frontend.Wallet
import           Frontend.Widgets


-- | What to show to the user.
--
data JsonDataView
  = JsonDataView_Keysets -- ^ Keyset editor
  | JsonDataView_Raw -- ^ Raw JSON input widget
  | JsonDataView_Result -- ^ Combined result, that will be sent over the wire.
  deriving (Eq)



-- | UI for managing JSON data.
uiJsonData
  :: forall t m. MonadWidget t m
  => Wallet t
  -> JsonData t
  -> m (JsonDataCfg t)
uiJsonData w d = mdo
    curSelection <- holdDyn JsonDataView_Keysets onSelect
    onSelect <- menu
      ( def & classes .~ pure "dark top attached tabular menu"
      )
      $ tabs curSelection

    keysetVCfg <- tabPane
        ("class" =: "keyset-editor ui segment")
        curSelection JsonDataView_Keysets $ do
      onCreateKeyset <- uiCreateKeyset $ d ^. jsonData_keysets
      ksCfg <- elClass "div" "keyset-list" $
        networkViewFlatten $ uiKeysets w <$> d ^. jsonData_keysets

      pure $ ksCfg & jsonDataCfg_createKeyset .~ onCreateKeyset

    rawVCfg <- tabPane
        ("class" =: "light ui segment json-data-editor-tab")
        curSelection JsonDataView_Raw $ do
      onNewData <- tagOnPostBuild $ d ^. jsonData_rawInput
      onSetRawInput <- elClass "div" "editor" $ dataEditor "" onNewData
      pure $ mempty & jsonDataCfg_setRawInput .~ onSetRawInput

    tabPane
        ("class" =: "ui code-font full-size json-data-result-tab")
        curSelection JsonDataView_Result $ do
      let
        showData =
          either showJsonError (T.decodeUtf8 . BSL.toStrict . encodePretty)
      el "pre" $ dynText $ showData <$> d ^. jsonData_data


    pure $ mconcat [ keysetVCfg, rawVCfg ]

  where
    tabs :: Dynamic t JsonDataView -> m (Event t JsonDataView)
    tabs curSelection = do
      let
        selections = [ JsonDataView_Keysets, JsonDataView_Raw, JsonDataView_Result ]
      leftmost <$> traverse (tab curSelection) selections

    tab :: Dynamic t JsonDataView -> JsonDataView -> m (Event t JsonDataView)
    tab curSelection self = do
      onClick <- makeClickable $ menuItem' (def & classes .~ dynClasses [boolClass "active" . Dyn $ fmap (== self) curSelection ]) $
        text $ viewToText self
      pure $ self <$ onClick


uiKeysets
  :: MonadWidget t m => Wallet t -> DynKeysets t -> m (JsonDataCfg t)
uiKeysets w ksM =
  {- elClass "div" "ui relaxed middle aligned divided list" $ do -}
  elClass "div" "ui fluid accordion flex-accordion flex-content" $ do
    case Map.toList ksM of
      []   -> do
        text "No keysets yet ..."
        elClass "div" "ui hidden divider" blank
        pure mempty
      kss -> do
        rs <- traverse uiKeysetDivider kss
        pure $ mconcat rs
  where
    uiKeysetDivider x = do
      c <- uiKeyset w x
      elClass "div" "ui hidden divider" blank
      pure c

-- | Display a single keyset on the screen.
uiKeyset
  :: forall t m. MonadWidget t m
  => Wallet t
  -> (KeysetName, DynKeyset t)
  -> m (JsonDataCfg t)
uiKeyset w (n, ks) = mdo
    isActive <- foldDyn (const not) False onToggle

    (onToggle, titleCfg) <- uiTitle isActive
    contentCfg <- uiContent isActive

    pure $ mconcat [ titleCfg, contentCfg ]
  where
    uiTitle :: Dynamic t Bool -> m (Event t (), JsonDataCfg t)
    uiTitle isActive = elClass "div" "keyset-title" $ do
      let
        titleClass = "ui header heading title" <> fmap activeClass isActive
      (e, _) <- elDynClass' "h4" titleClass $ do
        el "div" $ elClass "i" "dropdown icon" blank
        elClass "div" "content" $ do
          text n
          elClass "div" "sub header" $
            void . networkView $ renderKeys <$> _keyset_keys ks

      cfg <- elClass "div" "keyset-title-right" $ do
        onSetPred <- predDropdown
        let
          buttonIcon = elClass "i" "large trash right aligned icon" blank
        onDel <- flip button buttonIcon $ def
          & buttonConfig_emphasis .~ Static (Just Tertiary)
          & classes .~ Static "input-aligned-btn"

        let
          notEmpty s = if T.null s then Nothing else Just s
          setPred = (n, ) . notEmpty <$> onSetPred
        pure $ mempty
          & jsonDataCfg_setPred .~ setPred
          & jsonDataCfg_delKeyset .~ fmap (const n) onDel

      pure (domEvent Click e, cfg)

    uiContent :: Dynamic t Bool -> m (JsonDataCfg t)
    uiContent isActive =
      elDynClass "div" ("ui top attached segment content " <> fmap hiddenClass isActive) $ do
        onKeyClick <-
          switchHold never <=< networkView
          $ uiKeysetKeys (_keyset_keys ks) . Map.keys <$> w ^. wallet_keys

        let
          onAddKey = fmap fst . ffilter snd $ onKeyClick
          onDelKey = fmap fst . ffilter (not . snd) $ onKeyClick
        pure $ mempty
          & jsonDataCfg_addKey .~ fmap (n, ) onAddKey
          & jsonDataCfg_delKey .~ fmap (n, ) onDelKey

    activeClass = \case
      False -> ""
      True -> " active"

    hiddenClass = \case
      False -> " json-data-hidden-content"
      True -> " active"

    renderKeys nks =
      case splitAt 3 (Map.keys nks) of
        ([], [])  -> text "Empty keyset"
        (ksN, []) -> renderKeyList ksN
        (ksN, _)  -> renderKeyList (ksN <> [".."])

    renderKeyList = traverse_ (elClass "div" "ui label" . text)

    predDropdown = do
      onNewPred <- tagOnPostBuild . fmap (fromMaybe "") $ ks ^. keyset_pred
      let
        cfg = def & dropdownConfig_placeholder .~ "keys-all"

      elDynClass "div" "ui labeled input json-data-title-dropdown" $ do
        elClass "div" "ui label" $ text "Pred:"
        mvcSearchDropdown cfg ["keys-all", "keys-2", "keys-any"] onNewPred

    -- Unlike plain reflex-dom components, semantic-reflex does provide means
    -- for setting the value of the widget from an external source, so we need
    -- to emulate that feature (for making sure the widget always displays the
    -- right value):
    mvcSearchDropdown
      :: DropdownConfig t
      -> [Text]
      -> Event t Text
      -> m (Event t Text)
    mvcSearchDropdown cfg choices onNewVal = mdo
      -- For ignoring changes made by us (we are already up2date):
      shouldVal <- holdDyn Nothing $ Just <$> onNewVal
      let
        onExternalSet :: Event t Text
        onExternalSet = fmapMaybe id . updated $ do
          sV <- shouldVal
          uV <- userValue
          if sV == uV && isJust sV
             then pure Nothing
             else pure sV

        dropdownEv :: Event t (m (Dropdown t Text))
        dropdownEv =
          flip (searchDropdown cfg) (TaggedStatic choices) <$> onExternalSet
      dynEv <-
        widgetHold (pure never)
        . fmap (fmap (updated . _dropdown_value))
        $ dropdownEv
      onUpdate <- switchHold never $ updated dynEv

      userValue <- holdDyn Nothing $ Just <$> onUpdate
      pure onUpdate



-- | Input widget with confirm button for creating a new keyset.
--
-- TODO: Unify this with key creation widget in "Frontend.UI.Wallet" and uiAddKeysetKey widget below.
uiCreateKeyset
  :: MonadWidget t m => Dynamic t (DynKeysets t) -> m (Event t Text)
uiCreateKeyset ks = do
    elClass "div" "ui fluid action input" $ mdo
      name <- textInput $ def
          & textInputConfig_value .~ SetValue "" (Just $ "" <$ confirmed)
          & textInputConfig_placeholder .~ pure "Enter keyset name"

      let
        onEnter = keypress Enter name
        nameEmpty = (== "") <$> value name
        duplicate = Map.member <$> value name <*> ks

      clicked <- flip button (text "Create") $ def
        & buttonConfig_emphasis .~ Static (Just Secondary)
        & buttonConfig_disabled .~ Dyn ((||) <$> nameEmpty <*> duplicate)

      let
        confirmed = leftmost [ onEnter, clicked ]
        setFocus =
          liftJSM $ pToJSVal (_textInput_element name) ^. js0 ("focus" :: Text)

      void $ performEvent (setFocus <$ confirmed)
      pure $ tag (current $ _textInput_value name) confirmed


-- | Widget showing all avaialble keys for selecting keys
--
--   for the keyset.
uiKeysetKeys
  :: MonadWidget t m
  => Dynamic t KeysetKeys
  -> [KeyName]
  -> m (Event t (KeyName, Bool))
uiKeysetKeys ks allKeys =
  elClass "div" "ui grid keyset-keys" $ do
    case allKeys of
      []   -> elClass "div" "sixteen wide column" $ do
        text "No keys available ..."
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
uiKeysetKey ks n = elClass "div" "four wide column" $ do
    onSelected <- tagOnPostBuild $ Map.member n <$> ks
    let
      checkboxCfg =
        def & checkboxConfig_setValue .~ (SetValue False (Just onSelected))
    b <- flip checkbox checkboxCfg $ do
      el "div" $ do
        elClass "i" "key middle aligned icon" blank
        text n
    pure $ (n,) <$> _checkbox_change b

dataEditor
  :: MonadWidget t m
  => Text -> Event t Text
  -> m (Event t Text)
dataEditor iv sv = do
    let ac = def { _aceConfigMode = Just "ace/mode/json"
                 , _aceConfigElemAttrs = "class" =: "ace-data ace-widget"
                 }
    ace <- resizableAceWidget mempty ac (AceDynConfig $ Just AceTheme_SolarizedDark) iv sv
    return $ _extendedACE_onUserChange ace

viewToText :: JsonDataView -> Text
viewToText = \case
  JsonDataView_Keysets -> "Keysets"
  JsonDataView_Raw -> "Raw Input"
  JsonDataView_Result -> "Combined Result"

