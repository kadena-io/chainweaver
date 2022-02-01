{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.UI.Dialogs.SwitchToken
  ( uiSwitchToken
  ) where


import Control.Lens hiding (failover)
import Control.Error (hush)
import Data.Either (rights)
import Reflex.Dom

import Frontend.Foundation hiding (Arg)
import Frontend.UI.Modal
import Frontend.UI.Widgets
import Frontend.Wallet


-- | A modal for watching request keys
uiSwitchToken
  :: ( Flattenable mConf t
     , Monoid mConf
     , HasWalletCfg mConf key t
     , HasWallet model key t
     , MonadWidget t m
     )
  => model -> Event t () -> m (mConf, Event t ())
uiSwitchToken model onCloseExternal = do
  (conf, closes) <- fmap splitDynPure $ workflow $
    inputToken model onCloseExternal
  mConf <- flatten =<< tagOnPostBuild conf
  let close = switch $ current closes
  pure (mConf, close)

-- | Allow the user to input a new fungible
inputToken
  :: ( Monoid mConf
     , MonadWidget t m
     , HasWallet model key t
     , HasWalletCfg mConf key t
     )
  => model
  -> Event t () -- ^ Modal was externally closed
  -> Workflow t m (mConf, Event t ())
inputToken model _ = Workflow $ do
  close <- modalHeader $ text "Switch Token"
  dmFung <- modalMain $ do
    tokenList <- sample $ current $ model ^. wallet_tokenList
    currentFung <- sample $ current $ model ^. wallet_fungible

--     uiNodeInput cfg = do
--       ie <- uiInputElement cfg
--       pure (ie, parseNodeRefFull . T.strip <$> _inputElement_input ie)

--     showNodePopover =
--       pure . fmap (either PopoverState_Error (const PopoverState_Disabled)) . snd

--     elClass "li" "table__row table__row_type_primary" $ do
--       divClass "table__row-counter" blank
--       (nodeInput, _) <- divClass "table__cell table__cell_size_flex" $
--         uiInputWithPopover uiNodeInput (_inputElement_raw . fst) showNodePopover $ def
--           & inputElementConfig_initialValue .~ maybe "" renderNodeRef initVal
--           & initialAttributes .~ mconcat
--             [ "class" =: "input_width_full"
--             , "placeholder" =: "Add node"
--             ]
    divClass "group" $ fmap snd $ uiTokenInput False Nothing
  done <- modalFooter $ do
    confirmButton def "Done"
  let 
    fungE = tagMaybe (hush <$> current dmFung) done
    conf = mempty & walletCfg_fungibleModule .~ fungE
  pure ( (conf, done <> close)
       , never
       )
