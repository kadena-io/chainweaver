{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE ExtendedDefaultRules   #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

-- | The code editor, holds the currently edited code.
--
--   Also other editor features like error reporting go here.
--
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.Editor
  ( -- * Types and Classes
    -- ** The basic Model and ModelConfig types
    EditorCfg (..)
  , HasEditorCfg (..)
  , Editor (..)
  , HasEditor (..)
    -- ** Auxilary types and functions
  , module Editor
  -- * Creation
  , makeEditor
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import qualified Data.List                  as L
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Word                  (Word32)
import           Generics.Deriving.Monoid   (mappenddefault, memptydefault)
import           GHC.Generics               (Generic)
import           Reflex
import           System.Random              (newStdGen, randoms)

#ifdef  ghcjs_HOST_OS
import           Data.Map                   (Map)
#endif
------------------------------------------------------------------------------
import           Frontend.Network
import           Frontend.Editor.Annotation as Editor
import           Frontend.Editor.QuickFix   as Editor
import           Frontend.Foundation
import           Frontend.JsonData
import           Frontend.Messages
import           Frontend.Repl
import           Frontend.Wallet

-- | Configuration for the `Editor`.
data EditorCfg t = EditorCfg
  { _editorCfg_setCode       :: Event t Text
    -- ^ Set the source code/text of the editor and mark it as modified.
  , _editorCfg_loadCode      :: Event t Text
    -- ^ Same as `setCode` but instead of marking the editor state as modified,
    -- it sets it to unmodified.
  , _editorCfg_applyQuickFix :: Event t QuickFix
    -- ^ Apply a given quick fix.
  , _editorCfg_clearModified :: Event t ()
    -- ^ Clear the `modified` flag of the editor.
  }
  deriving Generic

makePactLenses ''EditorCfg

-- | Current editor state.
--
--   Currently we just hold the current Text, this will likely be extended with
--   information about whether or not the current code got modified since last
--   save, type errors and similar things.
data Editor t = Editor
  { _editor_code        :: Dynamic t Text
    -- ^ Currently loaded/edited PACT code.
  , _editor_annotations :: Event t [Annotation]
    -- ^ Annotations for the editor.
  , _editor_quickFixes  :: Dynamic t [QuickFix]
    -- ^ Available quick fixes.
  , _editor_modified    :: Dynamic t Bool
    -- ^ Editor content has not yet been saved?
  }
  deriving Generic

makePactLenses ''Editor


type HasEditorModel model t = (HasJsonData model t, HasWallet model t, HasNetwork model t)

type HasEditorModelCfg mConf t = (HasJsonDataCfg mConf t, Monoid mConf)

type ReflexConstraints t m =
  ( MonadHold t m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m)
  , MonadFix m, MonadIO m, PostBuild t m, MonadSample t (Performable m)
  )

-- | Create an `Editor` by providing a `Config`.
makeEditor
  :: forall t m cfg model mConf
  . ( ReflexConstraints t m
    , HasEditorCfg cfg t, HasEditorModel model t
    , HasEditorModelCfg mConf t
    , HasConfigs m
    )
  => model -> cfg -> m (mConf, Editor t)
makeEditor m cfg = mdo
    t <-  holdDyn "" $ leftmost
      [ cfg ^. editorCfg_setCode
      , cfg ^. editorCfg_loadCode
      , onFix
      ]

    modified <- holdDyn False $ leftmost
      [ False <$ cfg ^. editorCfg_loadCode
      , True <$  cfg ^. editorCfg_setCode
      , True <$  onFix
      , False <$ cfg ^. editorCfg_clearModified
      ]

    annotations <- typeCheckVerify m t
    quickFixes  <- holdDyn [] $ makeQuickFixes <$> annotations
    gen <- liftIO newStdGen
    (quickFixCfg, onFix) <- applyQuickFix (randoms gen) t $ cfg ^. editorCfg_applyQuickFix
    pure
      ( quickFixCfg
      , Editor
        { _editor_code = t
        , _editor_annotations = annotations
        , _editor_quickFixes = quickFixes
        , _editor_modified = modified
        }
      )

-- First parameter is a stream of random numbers to use for generating keyset names.
applyQuickFix
  :: forall t mConf m
  . ( Reflex t, MonadHold t m, MonadFix m
    , HasEditorModelCfg mConf t
    )
  => [Word32] -> Dynamic t Text -> Event t QuickFix -> m (mConf, Event t Text)
applyQuickFix rs t onQuickFix = do
  let
    onNewDefKeyset = fmapMaybe (^? _QuickFix_MissingKeyset) onQuickFix
    mkName r n = (n, n <> "-" <> tshow r)
  onNewRandKeyset <- zipListWithEvent mkName rs onNewDefKeyset

  let

    onNewKeyset = leftmost
     [ fmapMaybe (^? _QuickFix_MissingEnvKeyset) onQuickFix
     , snd <$> onNewRandKeyset
     ]

    fixCode :: Dynamic t ((Text, Text) -> Text)
    fixCode = do
      let
        isPreamble = (\x -> T.isPrefixOf ";" x || T.null x) . T.strip
        splitLeadingComments = L.break (not . isPreamble . T.strip) . T.lines
      (preamble, remCode) <- splitLeadingComments <$> t
      pure $ \(ks, ksn) -> T.unlines
        ( preamble
          <> [ "\n;; For more information about keysets checkout:"
             , ";; https://pact-language.readthedocs.io/en/latest/pact-reference.html#keysets-and-authorization"
             , "(define-keyset '" <> ks <> " (read-keyset \"" <> ksn <> "\"))\n"
             ]
          <> remCode
        )

  pure
    ( mempty & jsonDataCfg_createKeyset .~ onNewKeyset
    , attachWith id (current fixCode) onNewRandKeyset
    )

-- | Type check and verify code.
typeCheckVerify
  :: ( ReflexConstraints t m, HasEditorModel model t
     , HasConfigs m
     )
  => model -> Dynamic t Text -> m (Event t [Annotation])
typeCheckVerify m t = mdo
    let newInput = leftmost [ updated t, tag (current t) $ updated (m ^. jsonData_data)  ]
    -- Reset repl on each check to avoid memory leak.
    onReplReset <- throttle 1 $ newInput
    onTypeCheck <- delay 0 onReplReset

    let
      onTransSuccess = fmapMaybe (^? _Right) $ replL ^. repl_transactionFinished

    (replO :: MessagesCfg t, replL) <- makeRepl m $ mempty
      { _replCfg_sendTransaction = onTypeCheck
      , _replCfg_reset = () <$ onReplReset
      , _replCfg_verifyModules = Map.keysSet . _ts_modules <$> onTransSuccess
      }
    let
#ifdef  ghcjs_HOST_OS
    cModules <- holdDyn Map.empty $ _ts_modules <$> onTransSuccess
    let
      newAnnotations = mconcat
       [ attachPromptlyDynWith parseVerifyOutput cModules $ _repl_modulesVerified replL
       , concatMap annoFallbackParser <$> replO ^. messagesCfg_send
       ]
#else
      newAnnotations = mconcat
       [ parseVerifyOutput <$> _repl_modulesVerified replL
       , concatMap annoFallbackParser <$> replO ^. messagesCfg_send
       ]
#endif
    pure newAnnotations
  where
-- Line numbers are off on ghcjs:
#ifdef  ghcjs_HOST_OS
    parseVerifyOutput :: Map ModuleName Int -> VerifyResult -> [Annotation]
    parseVerifyOutput ms rs =
      let
        successRs :: [(ModuleName, Text)]
        successRs = fmapMaybe (traverse (^? _Right)) . Map.toList $ rs

        parsedRs :: Map ModuleName [Annotation]
        parsedRs = Map.fromList $ fmapMaybe (traverse annoParser) successRs

        fixLineNumber :: Int -> Annotation -> Annotation
        fixLineNumber n a = a { _annotation_line = _annotation_line a + n }

        fixLineNumbers :: Int -> [Annotation] -> [Annotation]
        fixLineNumbers n = map (fixLineNumber n)
      in
        concat . Map.elems $ Map.intersectionWith fixLineNumbers ms parsedRs
#else
    parseVerifyOutput :: VerifyResult -> [Annotation]
    parseVerifyOutput rs =
      let
        successRs :: [(ModuleName, Text)]
        successRs = fmapMaybe (traverse (^? _Right)) . Map.toList $ rs

        parsedRs :: [(ModuleName, [Annotation])]
        parsedRs = mapMaybe (traverse annoParser) successRs
      in
        concatMap snd parsedRs
#endif
-- Instances:

instance Reflex t => Semigroup (EditorCfg t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (EditorCfg t) where
  mempty = memptydefault
  mappend = (<>)

instance Flattenable (EditorCfg t) t where
  flattenWith doSwitch ev =
    EditorCfg
      <$> doSwitch never (_editorCfg_setCode <$> ev)
      <*> doSwitch never (_editorCfg_loadCode <$> ev)
      <*> doSwitch never (_editorCfg_applyQuickFix <$> ev)
      <*> doSwitch never (_editorCfg_clearModified <$> ev)

