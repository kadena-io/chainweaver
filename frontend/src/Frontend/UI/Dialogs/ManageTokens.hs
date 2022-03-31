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
import qualified Data.Text as T
import Reflex.Dom

import Pact.Types.Pretty  (renderCompactText)

import Frontend.Foundation hiding (Arg)
import Frontend.ModuleExplorer.ModuleRef
import Frontend.Network (HasNetwork(..))
import Frontend.UI.Modal
import Frontend.UI.Widgets
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
  (conf, closes) <- fmap splitDynPure $ workflow $
    inputToken model onCloseExternal
  mConf <- flatten =<< tagOnPostBuild conf
  let close = switch $ current closes
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
  -> Workflow t m (mConf, Event t ())
inputToken model _ =
  let
    next = Workflow $ do
      close <- modalHeader $ text "Manage Tokens"
      (dmFung, clickEv) <- modalMain $ do
        tokenList <- sample $ current $ model ^. wallet_tokenList
        currentFung <- sample $ current $ model ^. wallet_fungible

        let
          alwaysPasses acc = pure $ Success . flip ModuleName Nothing <$> acc
        dmFung <- divClass "group" $ fmap snd $ uiTextInputAsync "Enter token" True (Nothing, PopoverState_Disabled) never alwaysPasses moduleListId
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
        pure (dmFung, clickEv)
      done <- modalFooter $ do
        confirmButton def "Done"
      let 
        fungE = tagMaybe (current dmFung) done
        conf = mempty & walletCfg_fungibleModule .~ fungE & walletCfg_delModule .~ clickEv & walletCfg_addModule .~ (fmapMaybe id $ updated dmFung)
      pure ( (conf, done <> close)
          , never
          )
  in
    Workflow $ do
      close <- modalHeader $ text "Manage Tokens"
      void $ modalMain $ text "Loading..."
      pure ((mempty, close), next <$ updated (model ^. network_modules))
