{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Confirmation dialog for deploying modules and calling functions on the
-- network.
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.UI.Dialogs.CallFunction
  ( HasUICallFunctionModelCfg
  , HasUICallFunctionModel
  , uiCallFunction
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Data.List                      (intersperse)
import qualified Data.Map                       as Map
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Reflex
import           Reflex.Dom
import           Safe                           (headMay, readMay)
------------------------------------------------------------------------------
import           Pact.Types.Lang                (Arg (..), FunType (..),
                                                 GuardType (..), Name,
                                                 PrimType (..), Term, Type (..))
import           Pact.Types.Pretty (renderCompactText)
------------------------------------------------------------------------------
import           Frontend.Crypto.Class
import           Frontend.Foundation            hiding (Arg)
import           Frontend.JsonData              (HasJsonData (..), JsonData, HasJsonDataCfg)
import           Frontend.ModuleExplorer
import           Frontend.Network
import           Frontend.Log
import           Frontend.UI.DeploymentSettings
import           Frontend.UI.Dialogs.DeployConfirmation (fullDeployFlow, deployConfirmationConfig_modalTitle)
import           Frontend.UI.Modal
import           Frontend.UI.Widgets
import           Frontend.UI.Widgets.Helpers    (preventUpAndDownArrow, preventScrollWheel)
import           Frontend.Wallet                (HasWallet (..))
------------------------------------------------------------------------------

type HasUICallFunctionModel model key t =
  (HasModuleExplorer model t, HasNetwork model t, HasWallet model key t, HasJsonData model t)

type HasUICallFunctionModelCfg mConf t =
  ( Monoid mConf, Flattenable mConf t, HasModuleExplorerCfg mConf t
  , HasNetworkCfg mConf t
  )

-- | Modal dialog for calling a function.
uiCallFunction
  :: forall key t m mConf model
  . ( MonadWidget t m, HasUICallFunctionModelCfg mConf t
    , HasUICallFunctionModel model key t, HasJsonDataCfg mConf t
    , HasCrypto key (Performable m)
    , HasLogger model t
    , HasTransactionLogger m
    )
  => model
  -> Maybe DeployedModuleRef
  -> PactFunction
  -> Event t ()
  -> m (mConf, Event t ())
uiCallFunction m mModule func _onClose
  | functionIsCallable func, Just moduleRef <- mModule = do
    let content = mdo
          (cfg, result, mPactCall) <- uiDeploymentSettings m $ DeploymentSettingsConfig
            { _deploymentSettingsConfig_userTab = parametersTab m func
            , _deploymentSettingsConfig_chainId = \_ -> pure $ pure $ Just $ _chainRef_chain . _moduleRef_source $ moduleRef
            , _deploymentSettingsConfig_code = fromMaybe (pure $ buildCall func []) mPactCall
            , _deploymentSettingsConfig_sender = uiAccountDropdown def (pure $ \_ _ -> True) (pure id)
            , _deploymentSettingsConfig_data = Nothing
            , _deploymentSettingsConfig_ttl = Nothing
            , _deploymentSettingsConfig_nonce = Nothing
            , _deploymentSettingsConfig_gasLimit = Nothing
            , _deploymentSettingsConfig_caps = Nothing
            , _deploymentSettingsConfig_extraSigners = []
            , _deploymentSettingsConfig_includePreviewTab = True
            }
          pure (cfg, result)
        deployConfirmCfg = def
          & deployConfirmationConfig_modalTitle .~ (headerTitle <> " " <> _pactFunction_name func)

    fullDeployFlow deployConfirmCfg m (functionType >> content) _onClose
  | otherwise = do
    onClose <- modalHeader $ do
      text headerTitle
      elClass "span" "heading_type_h1" $
        text $ _pactFunction_name func
    functionType
    modalMain blank -- to push the footer to the bottom
    onAccept <- modalFooter $ confirmButton def "Ok"
    pure (mempty, leftmost [onClose, onAccept])
  where
    headerTitle =
      case _pactFunction_defType func of
        Defun   -> "Function: "
        Defpact -> "Pact: "
        Defcap  -> "Capability: "

    functionType = uiSegment "padded" $ do
      elClass "div" "segment segment_type_secondary code-font code-font_type_function-desc" $ do
        renderSignature func
        el "br" blank
        el "br" blank
        renderDescription func


-- | Tab showing edits for function parameters (if any):
parametersTab
  :: (MonadWidget t m, HasJsonData model t)
  => model
  -> PactFunction
  -> Maybe (Text, m (Dynamic t Text))
parametersTab m func =
  let
    fType = _pactFunction_type func
    fArgs = _ftArgs fType
  in
    if null fArgs
    then Nothing
    else Just . ("Parameters",) $
      divClass "group" $ do
        args :: [ Dynamic t Text ] <- traverse (funArgEdit (m ^. jsonData)) fArgs
        pure $ buildCall func <$> sequence args

-- | Build a function call
buildCall
  :: PactFunction
  -> [Text] -- ^ Function arguments
  -> Text -- ^ Pact function call
buildCall func args = fold
  [ "("
  , renderCompactText (_pactFunction_module func)
  , "."
  , _pactFunction_name func
  , if null args then "" else " "
  , T.unwords args
  , ")"
  ]

-- renderQualified :: PactFunction -> Text
-- renderQualified func = (coerce . _pactFunction_module) func <> "." <> _pactFunction_name func

renderDescription :: MonadWidget t m => PactFunction -> m ()
renderDescription func = traverse_ (uiCodeFont "code-font_type_function-desc") $
  _pactFunction_documentation func

renderSignature :: MonadWidget t m => PactFunction -> m ()
renderSignature f = do
  let fType = _pactFunction_type f
      args = _ftArgs fType
  renderArgs $ _ftArgs fType
  when ((not . null) args) $ do
    argDelimiter
    uiCodeFont "code-font_type_fun-return-arrow" "->"
    argDelimiter
  uiCodeFont "code-font_type_pact-type" $ prettyTextCompact (_ftReturn fType)

renderArgs :: MonadWidget t m => [Arg (Term Name)] -> m ()
renderArgs = sequence_ . intersperse argDelimiter . map renderArg

argDelimiter :: MonadWidget t m => m ()
argDelimiter = uiCodeFont "code-font_type_arg-delimiter" mempty

renderArg :: MonadWidget t m => Arg (Term Name) -> m ()
renderArg a = do
  uiCodeFont "code-font_type_pact-arg-name" $ (_aName a)
  argDelimiter
  uiCodeFont "code-font_type_pact-type" $ prettyTextCompact (_aType a)


data InputSize
  = InputSize_Large -- Should go on another line
  | InputSize_Small -- Should stay on the same line

-- | Render `InputSize` to class name.
inputSizeClass :: InputSize -> CssClass
inputSizeClass = \case
  InputSize_Large -> "large-input"
  InputSize_Small -> "small-input"

-- | Labeled argument edit widget.
funArgEdit
  :: forall t m
  . MonadWidget t m
  => JsonData t
  -> Arg (Term Name)
  -> m (Dynamic t Text)
funArgEdit json arg = do
  let
      aName = _aName arg
      aType = _aType arg
      sizeClass = inputSizeClass . funTypeInputSize $ aType
  elKlass "div" ("labeled-input segment segment_type_tertiary" <> sizeClass) $ do
    divClass "label labeled-input__label" $ text aName
    funTypeInput json aType

-- | Get the size of a type input widget.
funTypeInputSize :: Type v -> InputSize
funTypeInputSize = \case
  TyPrim TyBool -> InputSize_Small
  _             -> InputSize_Large

funTypeInput
  :: forall t m v
  . MonadWidget t m
  => JsonData t
  -> Type v
  -> m (Dynamic t Text)
funTypeInput json = \case
    TyPrim TyInteger -> mkIntInput
    TyPrim TyDecimal -> mkDecimalInput
    -- Not working properly:
    {- TyPrim TyTime -> mkInput "datetime-local" "" -}
    TyPrim TyTime -> mkInput "text" "" def
    TyPrim TyBool -> mkCheckbox False
    TyPrim TyString -> mkTextInput
    TyPrim (TyGuard (Just GTyKeySet)) -> keysetSelector json
    _ -> do
      r <- mkTextArea ""
      text "Note: Strings must be quoted."
      pure r
  where
    mkTextInput :: m (Dynamic t Text)
    mkTextInput = fmap (surroundWith "\"" . T.dropAround (=='\"')) <$> mkInput "text" "" def
      where
        surroundWith s x = s <> x <> s

    mkDecimalInput :: m (Dynamic t Text)
    mkDecimalInput = fmap fixNum <$> mkInput "number" "0.0" def
      where
        fixNum x = if T.isInfixOf "." x then x else x <> ".0"

    mkIntInput :: m (Dynamic t Text)
    mkIntInput = mdo
      let
        onInvalid = ffilter (not . maybeValid) $ _inputElement_input i
        onValid = ffilter maybeValid $ _inputElement_input i

        isValid :: Text -> Bool
        isValid t = maybe False (const True) (readInt t) || T.null t

        maybeValid :: Text -> Bool
        maybeValid t = isValid t || t == "-"

        readInt :: Text -> Maybe Int
        readInt = readMay . T.unpack

        uiIntInput cfg0 = do
          ie <- uiInputElement cfg0
          pure (ie, (value ie, readInt <$> _inputElement_input ie))

        showIntPopover (_, (_, onInput)) = pure $ ffor onInput $ \case
          Nothing -> PopoverState_Error "Not a valid int"
          _ -> PopoverState_Disabled

      lastValid <- hold "0" onValid

      let
        onInvalidLastValid = tag lastValid onInvalid
        cfg = def
          -- Does not work well with "number":
          & initialAttributes .~ ("type" =: "text" <> "class" =: "labeled-input__input input_type_secondary")
          & inputElementConfig_initialValue .~ "0"
          & inputElementConfig_setValue .~ onInvalidLastValid
      i <- fmap fst $ uiInputWithPopover uiIntInput (_inputElement_raw . fst) showIntPopover cfg
      pure $ value i

    mkCheckbox :: Bool -> m (Dynamic t Text)
    mkCheckbox iVal =
      let
        itemDom v = elAttr "option" ("value" =: v) $ text (T.toTitle v)
        cfg = SelectElementConfig (if iVal then "true" else "false") Nothing def
          & initialAttributes .~ "class" =: "labeled-input__input select_type_secondary"
      in
        fmap (_selectElement_value . fst) . uiSelectElement cfg $
          traverse_ itemDom [ "false", "true" ]

    mkTextArea :: Text -> m (Dynamic t Text)
    mkTextArea iVal =
      let
        attrs = "class" =: "labeled-input__input input input_type_textarea input_type_secondary"
          <> "placeholder" =: "Input .."
          <> noAutofillAttrs
        cfg = def
          & textAreaElementConfig_initialValue .~ iVal
          & initialAttributes .~ attrs

      in
        _textAreaElement_value <$> textAreaElement cfg

    mkInput :: Text -> Text -> InputElementConfig EventResult t (DomBuilderSpace m) -> m (Dynamic t Text)
    mkInput iType iVal cfg = do
      i <- uiInputElement $ cfg
        & initialAttributes .~ ("type" =: iType <> "class" =: "labeled-input__input input_type_secondary")
        & inputElementConfig_initialValue .~ iVal
        & inputElementConfig_elementConfig . elementConfig_eventSpec %~ preventUpAndDownArrow @m
      preventScrollWheel $ _inputElement_raw i
      pure $ _inputElement_value i

keysetSelector :: MonadWidget t m => JsonData t -> m (Dynamic t Text)
keysetSelector json = do
    -- TODO: At some point we should really get rid of this delay hacks:
    onPostBuild <- delay 0.1 =<< getPostBuild
    let
      keysetNames = Map.keys <$> json ^. jsonData_keysets
      itemDom v = elAttr "option" ("value" =: v) $ text v
      addReadKeyset x = "(read-keyset \"" <> x <> "\")"
      initKeyset = current $ fromMaybe "" . headMay <$> keysetNames
      cfg = SelectElementConfig "" (Just $ tag initKeyset onPostBuild) def &
        initialAttributes .~ "class" =: "select_type_secondary labeled-input__input"


    (s,_) <- uiSelectElement cfg $ void $ dyn $ ffor keysetNames $ \names -> do
      traverse_ itemDom names
    pure $ addReadKeyset <$> _selectElement_value s
