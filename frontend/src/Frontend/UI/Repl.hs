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

-- |
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.UI.Repl where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Aeson                  as Aeson (Object, encode)
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.HashMap.Strict         as H
import qualified Data.List.Zipper            as Z
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.Sequence               (Seq)
import qualified Data.Sequence               as S
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Language.Javascript.JSaddle hiding (Object)
import           Reflex
import           Reflex.Dom.Core
------------------------------------------------------------------------------
import           Pact.Repl
import           Pact.Repl.Types
import           Pact.Types.Exp
import           Pact.Types.Term
------------------------------------------------------------------------------
import           Frontend.Ide
import           Frontend.JsonData
import           Frontend.Wallet
import           Frontend.Editor
import           Frontend.Messages
------------------------------------------------------------------------------

data ClickState = DownAt (Int, Int) | Clicked | Selected
  deriving (Eq,Ord,Show,Read)

data DisplayedSnippet
  = InputSnippet Text
  | OutputSnippet Text
  | OldOutputSnippet Text
  deriving (Eq,Ord,Show,Read)

staticReplHeader :: Seq DisplayedSnippet
staticReplHeader = S.fromList
      [ OutputSnippet ";; Welcome to the Pact interactive repl"
      , OutputSnippet ";; Use LOAD button to execute editor text"
      , OutputSnippet ";; then just type at the \"pact>\" prompt to interact!"
      ]

snippetWidget' :: MonadWidget t m => DisplayedSnippet -> m (Element EventResult (DomBuilderSpace m) t)
snippetWidget' = fmap fst . \case
  InputSnippet t -> elAttr' "code" ("class" =: "replOut code-font") $ text t
  OutputSnippet t -> elAttr' "code" ("class" =: "replOut code-font") $ text t
  OldOutputSnippet t -> elAttr' "code" ("class" =: "replOut code-font old") $ text t


snippetWidget :: MonadWidget t m => DisplayedSnippet -> m ()
snippetWidget = void . snippetWidget'

replWidget
    :: MonadWidget t m
    => Ide t
    -> m (IdeCfg t)
replWidget ideL = divClass "control-block repl-output" $ mdo
  (e, r) <- elClass' "div" "repl-pane code-font" $ mdo
    mapM_ snippetWidget staticReplHeader
    clickType <- foldDyn ($) Nothing $ leftmost
      [ setDown <$> domEvent Mousedown e
      , clickClassifier <$> domEvent Mouseup e
      ]
    let
      replClick = () <$
        ffilter (== Just Clicked) (updated clickType)

      codeData = do
        code <- ideL ^. editor_code
        eJson <- ideL ^. jsonData_data
        pure $ either (const Nothing) (Just . (code,)) eJson

      -- Instead of signing REPL commands with the signing keys, we sign them
      -- with every key so we don't have to keep the signing keys around.
      keysContract =
        fmap sequence $ zipDyn (Map.elems <$> _wallet_keys (_ide_wallet ideL)) codeData

      onKeysContractLoad =
        fmapMaybe id . tag (current keysContract) $ _ide_load ideL

      onNewReplContent = leftmost
        [ onKeysContractLoad
        , ([], ("", H.empty)) <$ _ide_clearRepl ideL
        ]

    widgetHold
      (replInner replClick ([], ("", H.empty)))
      (replInner replClick <$> onNewReplContent
      )
  let
    err = snd <$> r
    onErrs = fmapMaybe id . updated $ err
    newExpr = fst <$> r

  timeToScroll <- delay 0.1 $ switch $ current newExpr
  void $ performEvent (scrollToBottom (_element_raw e) <$ timeToScroll)
  pure $ mempty & messagesCfg_send .~ onErrs

replInner
    :: MonadWidget t m
    => Event t ()
    -> ([KeyPair], (Text, Object))
    -> m (Event t Text, Maybe LogMsg)
replInner replClick (signingKeys, (code, json)) = mdo
    let pactKeys =
          T.unwords
          . map (keyToText . _keyPair_publicKey)
          $ signingKeys
        codeP = mconcat
          [ "(env-data "
          , toJsonString . T.decodeUtf8 . BSL.toStrict $ encode json
          , ")"
          , "(env-keys ["
          , pactKeys
          , "])"
          ]
    initState <- liftIO $ initReplState StringEval (Just "pact-1.staging.kadena.obsidian.systems") (Just 443)
    stateOutErr0 <- runReplStep0 (initState, mempty) codeP code
    let stateAndOut0 = (\(a,b,_) -> (a, b)) stateOutErr0
    stateAndOut <- holdDyn stateAndOut0 evalResult

    _ <- dyn (mapM_ snippetWidget . snd <$> stateAndOut)
    newInput <- replInput replClick
    evalResult <- performEvent $
      attachWith runReplStep (current stateAndOut) newInput
    return (newInput, stateOutErr0 ^. _3)
  where
      surroundWith :: Semigroup s => s -> s -> s
      surroundWith o i = o <> i <> o

      escapeJSON = T.replace "\"" "\\\""

      toJsonString = surroundWith "\"" . escapeJSON

      keyToText = T.decodeUtf8 . BSL.toStrict . encode


replInput :: MonadWidget t m => Event t () -> m (Event t Text)
replInput setFocus = do
    divClass "repl-input-controls code-font" $ mdo
      elClass "div" "prompt" $ text "pact>"
      let sv = leftmost
            [ mempty <$ enterPressed
            , fromMaybe "" . Z.safeCursor <$> tagPromptlyDyn commandHistory key
            ]
      ti <- textInput (def & textInputConfig_setValue .~ sv
                           & textInputConfig_attributes .~ pure ("class" =: "code-font")
                      )
      let key = ffilter isMovement $ domEvent Keydown ti
      let enterPressed = keypress Enter ti
      _ <- performEvent (liftJSM (pToJSVal (_textInput_element ti) ^. js0 ("focus" :: String)) <$ setFocus)
      let newCommand = tag (current $ value ti) enterPressed
      commandHistory <- foldDyn ($) Z.empty $ leftmost
        [ addToHistory <$> newCommand
        , moveHistory <$> key
        ]
      return newCommand

addToHistory :: Eq a => a -> Z.Zipper a -> Z.Zipper a
addToHistory a z =
    if Just a == Z.safeCursor (Z.left zEnd) then zEnd else Z.push a zEnd
  where
    zEnd = Z.end z

isMovement :: (Num a, Eq a) => a -> Bool
isMovement 38 = True
isMovement 40 = True
isMovement _  = False

moveHistory :: (Num a1, Eq a1) => a1 -> Z.Zipper a -> Z.Zipper a
moveHistory 38 = Z.left
moveHistory 40 = Z.right
moveHistory _  = id

runReplStep0
    :: MonadIO m
    => (ReplState, Seq DisplayedSnippet)
    -> Text
    -> Text
    -> m (ReplState, Seq DisplayedSnippet, Maybe LogMsg)
runReplStep0 (s1,snippets1) codePreamble code = do
    (r,s2) <- liftIO $ runStateT (evalRepl' $ T.unpack codePreamble) s1
    (r2,s3) <-
      -- Pact does not seem to like to be fed no input.
      -- This is hot fix for https://www.pivotaltracker.com/story/show/161933997
      -- it should be resolved better with  https://www.pivotaltracker.com/story/show/161646925 .
      if T.null code
         then pure (r, s2)
         else liftIO $ runStateT (evalPact $ T.unpack code) (unsetReplLib s2)
    let snippet = case r2 of
                    Left _ -> mempty
                    Right _ -> S.singleton . OutputSnippet . T.unlines . map showTerm $
                                 reverse $ _rTermOut s3
        err = either (Just . T.pack) (const Nothing) r2
    return (s3, snippets1 <> snippet, err)

runReplStep
    :: MonadIO m
    => (ReplState, Seq DisplayedSnippet)
    -> Text
    -> m (ReplState, Seq DisplayedSnippet)
runReplStep (s1,snippets1) e = do
    (eterm,s2) <- liftIO $ runStateT (evalRepl' $ T.unpack e) s1
    return (s2, snippets1 <> S.fromList [InputSnippet ("pact> " <> e), OutputSnippet $ showResult eterm])

showResult :: Show n => Either String (Term n) -> Text
showResult (Right v) = showTerm v
showResult (Left e)  = "Error: " <> T.pack e

showTerm :: Show n => Term n -> Text
showTerm (TLiteral (LString t) _) = t
showTerm t = T.pack $ show t

setDown :: (Int, Int) -> t -> Maybe ClickState
setDown clickLoc _ = Just $ DownAt clickLoc

clickClassifier :: (Int, Int) -> Maybe ClickState -> Maybe ClickState
clickClassifier clickLoc (Just (DownAt loc1)) =
  if clickLoc == loc1 then Just Clicked else Just Selected
clickClassifier _ _ = Nothing

scrollToBottom :: (PToJSVal t, MonadJSM m) => t -> m ()
scrollToBottom e = liftJSM $ do
    let pElem = pToJSVal e
    (pElem <# ("scrollTop" :: String)) (pElem ^. js ("scrollHeight" :: String))
