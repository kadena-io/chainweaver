{-# LANGUAGE ConstraintKinds       #-}
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

-- | Confirmation dialog for deploying modules and calling functions on the
-- backend.
-- Copyright   :  (C) 2018 Kadena
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
import           Data.Coerce             (coerce)
import           Data.List               (intersperse)
import qualified Data.Map                as Map
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Traversable        (for)
import           Reflex
import           Reflex.Dom
import           Safe (readMay, headMay)
------------------------------------------------------------------------------
import           Pact.Types.Lang         (Arg (..), FunType (..),
                                          ModuleName (..), Name, PrimType (..),
                                          Term, Type (..), GuardType(..))
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Foundation     hiding (Arg)
import           Frontend.Ide
import           Frontend.JsonData       (HasJsonData (..), JsonData)
import           Frontend.ModuleExplorer
import           Frontend.UI.Modal
import           Frontend.UI.Widgets
import           Frontend.Wallet         (HasWallet (..))
import           Frontend.UI.DeploymentSettings
------------------------------------------------------------------------------

type HasUICallFunctionModel model t =
  (HasModuleExplorer model t, HasBackend model t, HasWallet model t, HasJsonData model t)

type HasUICallFunctionModelCfg mConf t =
  ( Monoid mConf, Flattenable mConf t, HasModuleExplorerCfg mConf t
  )

-- | Modal dialog for calling a function.
uiCallFunction
  :: forall t m  mConf model
  . (MonadWidget t m, HasUICallFunctionModelCfg mConf t, HasUICallFunctionModel model t)
  => model
  -> Maybe DeployedModuleRef
  -> PactFunction
  -> m (mConf, Event t ())
uiCallFunction m mModule func = do
    onClose <- modalHeader $ do
      text "Function: "
      elClass "span" "heading_type_h1" $
        text $ _pactFunction_name func
    modalMain $ do
      mCallAndKeys <- modalBody $ do
        mPactCall <- uiSegment mempty $ do
          elClass "div" "segment segment_type_secondary code-font code-font_type_function-desc" $ do
            renderSignature func
            el "br" blank
            el "br" blank
            renderDescription func
          for mModule $ \ _ -> divClass "fun-arg-editor" $ do
            let fType = _pactFunction_type func
                fModule = _pactFunction_module func
                fName = _pactFunction_name func
                fArgs = _ftArgs fType
            args :: [ Dynamic t Text ] <- traverse (funArgEdit (m ^. jsonData)) fArgs
            pure $ buildCall fModule fName <$> sequence args

        signingKeys <- case mModule of
            Nothing -> pure mempty
            Just _  -> uiSegment mempty $ signingKeysWidget (m ^. wallet)

        pure $ do
          pactCall <- mPactCall
          moduleL <- mModule
          pure (pactCall, signingKeys, moduleL)

      modalFooter $
        case mCallAndKeys of
          Nothing -> do
            onAccept <- confirmButton def "Ok"
            pure (mempty, leftmost [onClose, onAccept])
          Just (pactCall, signingKeys, moduleL) -> do
            onCancel <- cancelButton def "Cancel"
            text " "
            -- let isDisabled = maybe True (const False) <$> transInfo
            onCall <- confirmButton (def & uiButtonCfg_disabled .~ pure False) "Call"

            let
              transaction :: Dynamic t (Text, TransactionInfo)
              transaction = do
                code <- pactCall
                keys <- signingKeys
                let b = backendRefName $ _moduleRef_source moduleL
                pure $
                  ( code
                  , TransactionInfo
                    { _transactionInfo_keys = keys
                    , _transactionInfo_backend = b
                    }
                  )

              onReq = tag (current transaction) onCall
              cfg = mempty & moduleExplorerCfg_deployCode .~ onReq

            pure (cfg, leftmost [onClose, onCancel, onCall])


-- | Build a function call
--
-- TODO: Proper namespace support
buildCall
  :: ModuleName -- ^ Module name
  -> Text -- ^ Function name
  -> [Text] -- ^ Function arguments
  -> Text -- ^ Pact function call
buildCall (ModuleName m _) n args = mconcat [ "(", coerce m, ".", n , " " , T.unwords args, ")" ]

-- renderQualified :: PactFunction -> Text
-- renderQualified func = (coerce . _pactFunction_module) func <> "." <> _pactFunction_name func

renderDescription :: MonadWidget t m => PactFunction -> m ()
renderDescription func = traverse_ (uiCodeFont "code-font_type_function-desc") $
  _pactFunction_documentation func

renderSignature :: MonadWidget t m => PactFunction -> m ()
renderSignature f = do
  let fType = _pactFunction_type f
  renderArgs $ _ftArgs fType
  argDelimiter
  uiCodeFont "code-font_type_fun-return-arrow" "->"
  argDelimiter
  uiCodeFont "code-font_type_pact-type" $ tshow (_ftReturn fType)

renderArgs :: MonadWidget t m => [Arg (Term Name)] -> m ()
renderArgs = sequence_ . intersperse argDelimiter . map renderArg

argDelimiter :: MonadWidget t m => m ()
argDelimiter = uiCodeFont "code-font_type_arg-delimiter" mempty

renderArg :: MonadWidget t m => Arg (Term Name) -> m ()
renderArg a = do
  uiCodeFont "code-font_type_pact-arg-name" $ (_aName a)
  text " "
  uiCodeFont "code-font_type_pact-type" $ tshow (_aType a)


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
  elKlass "div" ("group labled-input segment segment_type_tertiary" <> sizeClass) $ do
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
    TyPrim TyTime -> mkInput "text" ""
    TyPrim TyBool -> mkCheckbox False
    TyPrim TyString -> mkTextInput
    TyPrim (TyGuard (Just GTyKeySet)) -> keysetSelector json
    _ -> do
      r <- mkTextArea ""
      text "Note: Strings must be quoted."
      pure r
  where
    mkTextInput :: m (Dynamic t Text)
    mkTextInput = fmap (surroundWith "\"" . T.dropAround (=='\"')) <$> mkInput "text" ""
      where
        surroundWith s x = s <> x <> s

    mkDecimalInput :: m (Dynamic t Text)
    mkDecimalInput = fmap fixNum <$> mkInput "number" "0.0"
      where
        fixNum x = if T.isInfixOf "." x then x else x <> ".0"

    mkIntInput :: m (Dynamic t Text)
    mkIntInput = mdo
      let
        onInvalid = ffilter (not . isValid) $ _inputElement_input i
        onValid = ffilter isValid $ _inputElement_input i

        isValid :: Text -> Bool
        isValid t = maybe False (const True) (readInt t) || T.null t

        readInt :: Text -> Maybe Int
        readInt = readMay . T.unpack

      lastValid <- hold "0" onValid

      let
        onInvalidLastValid = tag lastValid onInvalid
        cfg = def
          -- Does not work well weith "number":
          & initialAttributes .~ ("type" =: "text" <> "class" =: "labeled-input__input input_type_secondary")
          & inputElementConfig_initialValue .~ "0"
          & inputElementConfig_setValue .~ onInvalidLastValid
      i <- uiInputElement cfg
      pure $ _inputElement_value i


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
        cfg = def
          & textAreaElementConfig_initialValue .~ iVal
          & initialAttributes .~ attrs

      in
        _textAreaElement_value <$> textAreaElement cfg

    mkInput :: Text -> Text -> m (Dynamic t Text)
    mkInput iType iVal =
      let
        cfg = def
          & initialAttributes .~ ("type" =: iType <> "class" =: "labeled-input__input input_type_secondary")
          & inputElementConfig_initialValue .~ iVal
      in
        _inputElement_value <$> uiInputElement cfg

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
