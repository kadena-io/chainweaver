{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Frontend.UI.Dialogs.SigBuilder where

import Reflex
import Reflex.Dom.Core

import Frontend.UI.Modal.Impl
import Frontend.UI.Widgets

sigBuilderCfg
  :: forall t m key mConf
   . ( MonadWidget t m
     )
  => ModalIde m key t
  -> Event t ()
  -> m (ModalIdeCfg m key t)
sigBuilderCfg m evt = do
  pure $ mempty & modalCfg_setModal .~ (Just (uiSigBuilderDialog m) <$ evt)

uiSigBuilderDialog
  :: ( MonadWidget t m, Monoid mConf
     )
  => ModalIde m key t
  -> Event t ()
  -> m (mConf, Event t ())
uiSigBuilderDialog model _onCloseExternal = mdo
  onClose <- modalHeader $ text "Add Account"
  modalMain $ text "hello"
  -- do
  --   dialogSectionHeading mempty "Notice"
  --   divClass "group" $ text "Add an Account here to display its status. You can add accounts that you do not own as well as accounts that do not exist yet."
  --   dialogSectionHeading mempty "Add Account"
  --   divClass "group" $ fmap snd $ uiAccountNameInputNoDropdown "Account Name" True Nothing never $ checkAccountNameAvailability
  --     <$> (model ^. network_selectedNetwork)
  --     <*> (model ^. wallet_accounts)
  modalFooter $ do
    onCancel <- cancelButton def "Cancel"
    -- onAdd <- confirmButton (def & uiButtonCfg_disabled .~ (isNothing <$> name)) "Add"
    -- let val = runMaybeT $ do
    --       net <- lift $ current $ model ^. network_selectedNetwork
    --       n <- MaybeT $ current name
    --       pure (net, n)
    --     conf = mempty & walletCfg_importAccount .~ tagMaybe val onAdd
    -- Since this always succeeds, we're okay to close on the add button event
    return ( mempty
           , onCancel
           )
