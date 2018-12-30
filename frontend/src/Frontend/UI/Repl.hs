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
import qualified Data.List.Zipper            as Z
import           Data.Maybe
import           Data.Sequence               (Seq)
import qualified Data.Sequence               as S
import           Data.Text                   (Text)
import           Language.Javascript.JSaddle hiding (Object)
import           Reflex
import           Reflex.Dom.Core
import           GHC.Exts (toList)
import           GHCJS.DOM.Element (scrollIntoView)
------------------------------------------------------------------------------
import           Frontend.Repl
import           Frontend.UI.Widgets (setFocusOn)
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
      , OutputSnippet ";; Use 'LOAD into REPL' button to execute editor text"
      , OutputSnippet ";; then just type at the \"pact>\" prompt to interact!"
      , OutputSnippet ";;"
      , OutputSnippet ";; To reset the REPL type 'reset'!"
      ]

snippetWidget' :: MonadWidget t m => DisplayedSnippet -> m (Element EventResult (DomBuilderSpace m) t)
snippetWidget' = fmap fst . \case
  InputSnippet t
    -> elAttr' "code" ("class" =: "repl__out") $ text $ "pact> " <> t
  OutputSnippet t
    -> elAttr' "code" ("class" =: "repl__out") $ text t
  OldOutputSnippet t
    -> elAttr' "code" ("class" =: "repl__out repl__old") $ text t

snippetWidget :: MonadWidget t m => DisplayedSnippet -> m ()
snippetWidget = void . snippetWidget'

displayReplOutput :: MonadWidget t m => ReplOutput -> m ()
displayReplOutput = snippetWidget . replOutToDisplayed
  where
    replOutToDisplayed = \case
      ReplOutput_Cmd t -> InputSnippet t
      ReplOutput_Res t -> OutputSnippet t

replWidget
    :: (MonadWidget t m, HasWebRepl model t, HasReplCfg mConf t, Monoid mConf)
    => model
    -> m mConf
replWidget m = do
  (e, onNewInput) <- elClass' "div" "repl" $ do
    mapM_ snippetWidget staticReplHeader
    void $ simpleList (toList <$> m ^. repl_output) (dyn . fmap displayReplOutput)
    replInput

  clickType <- foldDyn ($) Nothing $ leftmost
    [ setDown <$> domEvent Mousedown e
    , clickClassifier <$> domEvent Mouseup e
    ]
  let
    replClick = () <$
      ffilter (== Just Clicked) (updated clickType)
  setFocusOn e "input" replClick

  let
    onReset = () <$ ffilter (== "reset") onNewInput
    onCmd   = ffilter (/= "reset") onNewInput
  pure $ mempty
    & replCfg_sendCmd .~ onCmd
    & replCfg_reset   .~ onReset


replInput :: MonadWidget t m => m (Event t Text)
replInput = do
    divClass "repl__input-controls" $ mdo
      (e, _) <- elClass' "div" "repl__prompt" $ text "pact>"
      let sv = leftmost
            [ mempty <$ enterPressed
            , fromMaybe "" . Z.safeCursor <$> tagPromptlyDyn commandHistory key
            ]
      ti <- textInput (def & textInputConfig_setValue .~ sv
                           & textInputConfig_attributes .~ pure ("class" =: "code-font repl__input")
                      )
      let key = ffilter isMovement $ domEvent Keydown ti
      let enterPressed = keypress Enter ti
      let newCommand = tag (current $ value ti) enterPressed
      commandHistory <- foldDyn ($) Z.empty $ leftmost
        [ addToHistory <$> newCommand
        , moveHistory <$> key
        ]

      doScrollIntoView ti

      return newCommand
  where
    doScrollIntoView ti = do
      onPostBuild <- getPostBuild
      onReady <- delay 0.1 $ leftmost [onPostBuild, keypress Enter ti]
      performEvent_ $ ffor onReady $ \_ -> liftJSM $
        scrollIntoView (_textInput_element ti) True

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

setDown :: (Int, Int) -> t -> Maybe ClickState
setDown clickLoc _ = Just $ DownAt clickLoc

clickClassifier :: (Int, Int) -> Maybe ClickState -> Maybe ClickState
clickClassifier clickLoc (Just (DownAt loc1)) =
  if clickLoc == loc1 then Just Clicked else Just Selected
clickClassifier _ _ = Nothing
