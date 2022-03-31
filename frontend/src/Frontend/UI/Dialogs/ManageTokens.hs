{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.UI.Dialogs.ManageTokens
  ( uiManageTokens
  ) where


import Control.Lens hiding (failover)
import Control.Error (hush)
import Control.Monad (forM, void)
import Data.Bifunctor (first)
import Data.Either (rights)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Text as T
import Reflex.Dom

import Pact.Types.Pretty  (renderCompactText)
import Pact.Types.Names   (parseModuleName)

import Frontend.Foundation hiding (Arg)
import Frontend.ModuleExplorer.ModuleRef
import Frontend.Network (HasNetwork(..))
import Frontend.UI.Modal
import Frontend.UI.Widgets
import Frontend.UI.Widgets.Helpers
import Frontend.Wallet


-- | A modal for watching request keys
uiManageTokens
  :: ( Flattenable mConf t
     , Monoid mConf
     , HasNetwork model t
     , HasWalletCfg mConf key t
     , HasWallet model key t
     , MonadWidget t m
     )
  => model -> Event t () -> m (mConf, Event t ())
uiManageTokens model onCloseExternal = do
  (conf, closes) <- splitE <$> inputToken model onCloseExternal
  mConf <- flatten conf
  close <- switchHold never closes
  pure (mConf, close)

-- | Allow the user to input a new fungible
inputToken
  :: ( Monoid mConf
     , MonadWidget t m
     , HasNetwork model t
     , HasWallet model key t
     , HasWalletCfg mConf key t
     )
  => model
  -> Event t () -- ^ Modal was externally closed
  -> m (Event t (mConf, Event t ()))
inputToken model _ = do
  close <- modalHeader $ text "Manage Tokens"
  networkView $ model ^. network_modules <&> \moduleMap ->
    if Map.null moduleMap
      then do
        void $ modalMain $ text "Loading..."
        pure mempty
      else do
        (dmFung, clickEv, addEv) <- modalMain $ do
          tokenList <- sample $ current $ model ^. wallet_tokenList
          currentFung <- sample $ current $ model ^. wallet_fungible

          let
            tokenValidator textEv = do
              let
                isValidToken moduleList token = case parseModuleName token of
                  Left err -> Failure $ "Invalid Token: " <> T.pack err
                  Right mdule -> if mdule `elem` moduleList
                    then Success mdule
                    else Failure "Unsupported Token"
              pure $ isValidToken <$> current (moduleListDyn model) <@> textEv

          dialogSectionHeading mempty "Tokens"
          (dmFung, addEv) <- divClass "flex" $ do
            dmFung <- divClass "group flex-grow" $
              fmap snd $ uiTextInputAsync "Enter token" True (Nothing, PopoverState_Disabled) never tokenValidator moduleListId
            addEv <- confirmButton (def & uiButtonCfg_class .~ "margin" & uiButtonCfg_disabled .~ (isNothing <$> dmFung)) "Add"
            pure (dmFung, addEv)
          eventEv <- networkView $ model ^. wallet_tokenList <&> \ne -> do
            clicks <- forM (NE.toList ne) $ \token -> do
              (e, _) <- accordionItem' False "segment segment_type_secondary"
                (do
                  text $ _mnName token
                  deleteButtonNaked $ def & uiButtonCfg_class .~ "accordion__title-button")
                (text $ renderCompactText token)
              pure $ token <$ e
            pure $ leftmost clicks
          clickEv <- switchHold never eventEv
          pure (dmFung, clickEv, addEv)
        done <- modalFooter $ do
          confirmButton def "Done"
        let 
          fungE = tagMaybe (current dmFung) done
          conf = mempty & walletCfg_fungibleModule .~ fungE & walletCfg_delModule .~ clickEv & walletCfg_addModule .~ (fmapMaybe id $ tag (current dmFung) addEv)
        pure (conf, done <> close)
