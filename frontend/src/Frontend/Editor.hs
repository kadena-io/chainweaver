{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | The code editor, holds the currently edited code.
--
--   Also other editor features like error reporting go here.
--
-- Copyright   :  (C) 2020 Kadena
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
import           Control.Applicative        (liftA2)
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

-- #ifdef  ghcjs_HOST_OS
import           Data.Map                   (Map)
import           Data.Bitraversable         (bitraverse)
import           Control.Monad              (join)
-- #endif
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
  , _editor_annotations :: Dynamic t [Annotation]
    -- ^ Annotations for the editor.
  , _editor_quickFixes  :: Dynamic t [QuickFix]
    -- ^ Available quick fixes.
  , _editor_modified    :: Dynamic t Bool
    -- ^ Editor content has not yet been saved?
  }
  deriving Generic

makePactLenses ''Editor


type HasEditorModel model key t = (HasJsonData model t, HasWallet model key t, HasNetwork model t)

type HasEditorModelCfg mConf t = (HasJsonDataCfg mConf t, Monoid mConf)

type ReflexConstraints t m =
  ( MonadHold t m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m)
  , MonadFix m, MonadIO m, PostBuild t m, MonadSample t (Performable m)
  )

-- | Create an `Editor` by providing a `Config`.
makeEditor
  :: forall key t m cfg model mConf
  . ( ReflexConstraints t m
    , HasEditorCfg cfg t, HasEditorModel model key t
    , HasEditorModelCfg mConf t
    , HasConfigs m
    )
  => model -> cfg -> m (mConf, Editor t)
makeEditor m cfg = mdo
    t <- holdDyn "" $ leftmost
      [ cfg ^. editorCfg_setCode
      , cfg ^. editorCfg_loadCode
      , onCodeFix
      ]

    modified <- holdDyn False $ leftmost
      [ False <$ cfg ^. editorCfg_loadCode
      , True  <$ cfg ^. editorCfg_setCode
      , True  <$ onCodeFix
      , False <$ cfg ^. editorCfg_clearModified
      ]

    gen <- liftIO newStdGen
    (quickFixCfg, onCodeFix) <- applyQuickFix (randoms gen) t $ cfg ^. editorCfg_applyQuickFix
    codeAnnotations <- holdDyn [] =<< typeCheckVerify m t
    let dQuickFixes = constDyn []
    -- let codeAnnotations = constDyn []
    -- let dataAnnotations = ffor (m ^. jsonData . to getJsonDataError) $ foldMap $ (: []) . annoJsonParser . showJsonError
    pure
      ( quickFixCfg
      , Editor
        { _editor_code = t
        -- , _editor_annotations = liftA2 (<>) codeAnnotations dataAnnotations
        , _editor_annotations = codeAnnotations
        , _editor_quickFixes = dQuickFixes   -- makeQuickFixes <$> codeAnnotations
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
    onNewDefKeyset = fmapMaybe (^? _QuickFix_UndefinedKeyset) onQuickFix
    mkName r n = (n, n <> "-" <> tshow r)
  onNewRandKeyset <- zipListWithEvent mkName rs onNewDefKeyset

  let

    onNewKeyset = leftmost
     [ fmapMaybe (^? _QuickFix_UnreadableKeyset) onQuickFix
     , snd <$> onNewRandKeyset
     ]

    fixCode :: Text -> (Text, Text) -> Text
    fixCode code (ks, ksn) = do
      let
        isPreamble = (\x -> T.isPrefixOf ";" x || T.null x) . T.strip
        splitLeadingComments = L.break (not . isPreamble . T.strip) . T.lines
        (preamble, remCode) = splitLeadingComments code
      T.unlines $ L.intercalate ["\n"]
        [ preamble
        , [ ";; For more information about keysets checkout:"
          , ";; https://pact-language.readthedocs.io/en/latest/pact-reference.html#keysets-and-authorization"
          , "(define-keyset '" <> ks <> " (read-keyset \"" <> ksn <> "\"))"
          ]
        , remCode
        ]

  pure
    ( mempty & jsonDataCfg_createKeyset .~ onNewKeyset
    , attachWith fixCode (current t) onNewRandKeyset
    )

-- | Type check and verify code.
typeCheckVerify
  :: ( ReflexConstraints t m, HasEditorModel model key t
     , HasConfigs m
     -- , MonadWidget t m
     )
  => model -> Dynamic t Text -> m (Event t [Annotation])
typeCheckVerify m t = mdo
    let newInput = leftmost [ updated t, tag (current t) $ updated (m ^. jsonData_data)  ]
    -- Reset repl on each check to avoid memory leak.
    onReplReset <- throttle 1 $ newInput
    -- onTypeCheck <- delay 0 onReplReset

    let
      onTransSuccess = fmapMaybe (^? _Right) $ replL ^. repl_transactionFinished

    (replO :: MessagesCfg t, replL) <- makeRepl m $ mempty
      { _replCfg_sendTransaction = onReplReset
      , _replCfg_reset = () <$ onReplReset
      , _replCfg_verifyModules = Map.keysSet . _ts_modules <$> onTransSuccess
      }
-- #ifdef  ghcjs_HOST_OS
    cModules <- holdDyn Map.empty $ _ts_modules <$> onTransSuccess
    let
      newAnnotations = attachPromptlyDynWith parseVerifyOutput cModules $ _repl_modulesVerified replL
       -- [-- , concatMap annoFallbackParser <$> replO ^. messagesCfg_send
       -- ]
-- #else
--     let
--       newAnnotations = mconcat
--        [ parseVerifyOutput <$> _repl_modulesVerified replL
--        , concatMap annoFallbackParser <$> replO ^. messagesCfg_send
--        ]
-- #endif
    pure newAnnotations
  where
-- Line numbers are off on ghcjs: https://github.com/kadena-io/pact/issues/344
-- TODO: Fix this in pact.
-- #ifdef  ghcjs_HOST_OS
    parseVerifyOutput :: Map ModuleName Int -> VerifyResult -> [Annotation]
    parseVerifyOutput ms rs =
      let
        msgsRs :: [(ModuleName, Either Text Text)]
        msgsRs = Map.toList $ rs

        parsedRs :: Map ModuleName (Either [Annotation] [Annotation])
        parsedRs = Map.fromList $ mapMaybe (traverse $ join bitraverse annoParser) msgsRs

        fixLineNumber :: Int -> Annotation -> Annotation
        fixLineNumber n a = a & annotation_pos . _Just . _1 +~ n

        fixLineNumbers :: Int -> [Annotation] -> [Annotation]
        fixLineNumbers n = map (fixLineNumber n)

        fixLineNumbersRight :: Int -> Either [Annotation] [Annotation] -> [Annotation]
        fixLineNumbersRight n = either id (fixLineNumbers n)

      in
        normalize . concat . Map.elems $ Map.intersectionWith fixLineNumbersRight ms parsedRs
-- #else
--     parseVerifyOutput :: VerifyResult -> [Annotation]
--     parseVerifyOutput rs =
--       let
--         msgsRs :: [(ModuleName, Text)]
--         msgsRs = fmap (fmap $ either id id) . Map.toList $ rs

--         parsedRs :: [(ModuleName, [Annotation])]
--         parsedRs = mapMaybe (traverse annoParser) msgsRs
--       in
--         normalize $ concatMap snd parsedRs
-- #endif
    -- Reason, see: https://github.com/kadena-io/pact/pull/532
    normalize :: [Annotation] -> [Annotation]
    normalize = map mkWarning . L.nub

    -- Verification problems should always be displayed as warnings:
    mkWarning :: Annotation -> Annotation
    mkWarning anno = anno { _annotation_type = AnnoType_Warning }


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
