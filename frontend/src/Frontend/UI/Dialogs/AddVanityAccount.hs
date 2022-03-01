{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
module Frontend.UI.Dialogs.AddVanityAccount
  ( uiWatchAccountButton
  , uiCreateAccountButton
  ) where

import Control.Lens                           ((^.),(<>~))
import Control.Monad.Trans.Class              (lift)
import Control.Monad.Trans.Maybe              (MaybeT (..), runMaybeT)
import Data.Maybe                             (isNothing)

import Reflex
import Reflex.Dom.Core

import Frontend.UI.Modal.Impl
import Frontend.UI.Widgets
import Frontend.UI.Widgets.Helpers (dialogSectionHeading)

import Frontend.Network
import Frontend.Wallet

uiWatchAccountButton
  :: forall t m key mConf
     . ( MonadWidget t m, Monoid mConf
       , HasModalCfg mConf (ModalImpl m key t) t
       )
  => ModalIde m key t
  -> m mConf
uiWatchAccountButton m = do
  eOpenAddAccount <- uiButton headerBtnCfg  (text "+ Watch Account")
  pure $ mempty & modalCfg_setModal .~ (Just (uiWatchAccountDialog m) <$ eOpenAddAccount)

uiAddAccountDialog
  :: ( MonadWidget t m, Monoid mConf
     , HasWalletCfg mConf key t
     )
  => ModalIde m key t
  -> Event t ()
  -> m (mConf, Event t ())
uiAddAccountDialog model _onCloseExternal = mdo
  onClose <- modalHeader $ text "Add Account"
  name <- modalMain $ do
    dialogSectionHeading mempty "Notice"
    divClass "group" $ text "Add an Account here to display its status. You can add accounts that you do not own as well as accounts that do not exist yet."
    dialogSectionHeading mempty "Add Account"
    divClass "group" $ fmap snd $ uiAccountNameInputNoDropdown "Account Name" True Nothing never $ checkAccountNameAvailability
      <$> (model ^. network_selectedNetwork)
      <*> (model ^. wallet_accounts)
  modalFooter $ do
    onCancel <- cancelButton def "Cancel"
    onAdd <- confirmButton (def & uiButtonCfg_disabled .~ (isNothing <$> name)) "Add"
    let val = runMaybeT $ do
          net <- lift $ current $ model ^. network_selectedNetwork
          n <- MaybeT $ current name
          pure (net, n)
        conf = mempty & walletCfg_importAccount .~ tagMaybe val onAdd
    -- Since this always succeeds, we're okay to close on the add button event
    return ( conf
           , onCancel <> onClose <> onAdd
           )

uiCreateAccountButton :: DomBuilder t m => UiButtonCfg -> m (Event t ())
uiCreateAccountButton cfg =
  uiButton (cfg & uiButtonCfg_class <>~ "button_type_secondary" <> "button_type_secondary") $ do
    elClass "span" "button__text" $ text "+ Create Account"

data CreateAccountMethod
  = CreateAccountMethod_SelfGasPayer
  | CreateAccountMethod_ExternalAccount
  | CreateAccountMethod_ShareTxBuilder
  deriving Eq
