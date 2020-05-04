{-|

This module contains fairly low level input widgets using the composable
FormWidget infrastructure.

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Frontend.UI.Form.Common where

------------------------------------------------------------------------------
import           Control.Arrow
import           Control.Lens hiding (element)
import           Control.Monad
import           Control.Monad.Except
import qualified Data.Bimap as Bimap
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Text.Read as T
import           GHC.Word
import           Data.Decimal                (Decimal)
import qualified Data.Decimal                as D
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Dom.Core
import           System.Random
import           Text.Read                   (readMaybe)
------------------------------------------------------------------------------
import           Pact.Types.ChainId (ChainId (..))
------------------------------------------------------------------------------
import           Frontend.Foundation
import           Frontend.Network
import           Frontend.UI.Common
import           Frontend.UI.FormWidget
------------------------------------------------------------------------------

-- | reflex-dom `inputElement` with chainweaver default styling:
textFormWidget
  :: DomBuilder t m
  => PrimFormWidgetConfig t Text
  -> m (FormWidget t Text)
textFormWidget cfg = do
    let iecCfg = pfwc2iec id cfg
    ie <- inputElement $ iecCfg & initialAttributes %~ (addInputElementCls . addNoAutofillAttrs)
    return (ie2iw id ie)

-- | Parsing input element with chainweaver styling
parsingFormWidget
  :: DomBuilder t m
  => (Text -> Either String a)
  -> (Either String a -> Text)
  -> PrimFormWidgetConfig t (Either String a)
  -> m (FormWidget t (Either String a))
parsingFormWidget fromText toText cfg = do
    let iecCfg = pfwc2iec toText cfg
    ie <- inputElement $ iecCfg & initialAttributes %~ (addInputElementCls . addNoAutofillAttrs)
    return (ie2iw fromText ie)

-- | Decimal input element with chainweaver default styling
decimalFormWidget
  :: DomBuilder t m
  => PrimFormWidgetConfig t (Either String Decimal)
  -> m (FormWidget t (Either String Decimal))
decimalFormWidget cfg = do
  let p t = maybe (Left "Not a valid amount") Right $ readMaybe (T.unpack t)
  parsingFormWidget p (either (const "") tshow) cfg

-- | Standard form element for amount fields.
amountFormWidget
  :: DomBuilder t m
  => PrimFormWidgetConfig t (Either String Decimal)
  -> m (FormWidget t (Either String Decimal))
amountFormWidget cfg = do
    parsingFormWidget p (either (const "") tshow) cfg
  where
    p t = case readMaybe (T.unpack t) of
            Nothing -> Left "Not a valid number"
            Just x
              | x < 0 -> Left "Cannot be negative"
              | D.decimalPlaces x > maxCoinPrecision -> Left "Too many decimal places"
              | otherwise -> Right x

dropdownFormWidget
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Ord a)
  => Dynamic t (Map a Text)
  -> PrimFormWidgetConfig t a
  -> m (FormWidget t a)
dropdownFormWidget options cfg = do
  let k0 = _initialValue cfg
      setK = fromMaybe never $ view setValue cfg
      initAttrs = view initialAttributes cfg
      modifyAttrs = view modifyAttributes cfg
  optionsWithAddedKeys <- fmap (zipDynWith Map.union options) $ foldDyn Map.union (k0 =: "") $ fmap (=: "") setK
  defaultKey <- holdDyn k0 setK
  let (indexedOptions, ixKeys) = splitDynPure $ ffor optionsWithAddedKeys $ \os ->
        let xs = fmap (\(i, (k, v)) -> ((i, k), ((i, k), v))) $ zip [0::Int ..] $ Map.toList os
        in (Map.fromList $ map snd xs, Bimap.fromList $ map fst xs)
  let scfg = def
        & selectElementConfig_elementConfig . elementConfig_initialAttributes .~ initAttrs
        & selectElementConfig_elementConfig . elementConfig_modifyAttributes .~ modifyAttrs
        & selectElementConfig_setValue .~ fmap (T.pack . show) (attachPromptlyDynWithMaybe (flip Bimap.lookupR) ixKeys setK)
  (eRaw, _) <- selectElement scfg $ listWithKey indexedOptions $ \(i, k) v -> do
    let optionAttrs = fmap (\dk -> "value" =: T.pack (show i) <> if dk == k then "selected" =: "selected" else mempty) defaultKey
    elDynAttr "option" optionAttrs $ dynText v
  let lookupSelected ks v = do
        key <- T.readMaybe $ T.unpack v
        Bimap.lookup key ks
  let eChange = attachPromptlyDynWith lookupSelected ixKeys $ _selectElement_change eRaw
  let readKey keys mk = fromMaybe k0 $ do
        k <- mk
        guard $ Bimap.memberR k keys
        return k
  dValue <- fmap (zipDynWith readKey ixKeys) $ holdDyn (Just k0) $ leftmost [eChange, fmap Just setK]
  return $ FormWidget dValue (() <$ attachPromptlyDynWith readKey ixKeys eChange) (_selectElement_hasFocus eRaw)

-- | Like comboBoxGlobalDatalist but handles creation of a unique datalist ID
-- for you.
comboBox
  :: ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m
     , MonadIO m
     )
  => Dynamic t [Text]
  -> PrimFormWidgetConfig t Text
  -> m (FormWidget t Text)
comboBox options cfg = do
  listNum :: Word64 <- liftIO randomIO
  let listId = "datalist-" <> T.pack (show listNum)
  res <- comboBoxGlobalDatalist listId cfg
  comboBoxDatalist listId options
  pure res

-- | HTML5 combo box widget that presents a dropdown but also allows work like a
-- text input.
comboBoxGlobalDatalist
  :: DomBuilder t m
  => Text
  -> PrimFormWidgetConfig t Text
  -> m (FormWidget t Text)
comboBoxGlobalDatalist datalistId cfg = do
  ie <- inputElement $ pfwc2iec id cfg
    & initialAttributes %~ (<> "list" =: datalistId)
  pure $ ie2iw id ie

-- | Renders an options list suitable for use with HTML5 combo boxes.
comboBoxDatalist
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Text
  -> Dynamic t [Text]
  -> m ()
comboBoxDatalist datalistId items = elAttr "datalist" ("id" =: datalistId) $ do
  void $ simpleList items $ \v ->
    elDynAttr "option" (("value" =:) <$> v) blank
  pure ()

uiMandatoryChainSelection
  :: MonadWidget t m
  => Dynamic t [ChainId]
  -> CssClass
  -> FormWidgetConfig t ChainId
  -> m (FormWidget t ChainId)
uiMandatoryChainSelection options cls cfg = do
  let chains = map (id &&& (("Chain " <>) .  _chainId)) <$> options
      mkOptions cs = Map.fromList cs
      staticCls = cls <> "select"
      allCls = renderClass <$> pure staticCls
      dcfg = def
        & dropdownConfig_attributes .~ (("class" =:) <$> allCls)
        & dropdownConfig_setValue .~ fromMaybe never (view setValue cfg)

  ddE <- dropdown (_initialValue cfg) (mkOptions <$> chains) dcfg
  pure $ FormWidget (value ddE) (() <$ _dropdown_change ddE) (constDyn False)
