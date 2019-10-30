module Frontend.UI.Dialogs.AddAccount
  ( uiAddAccount
  ) where

import           Control.Lens

import           Frontend.Wallet
import           Frontend.Crypto.Ed25519 (parsePublicKey)

import           Frontend.UI.Modal
import           Frontend.UI.Widgets

type HasAddAccountModel model t =
  (HasWallet model PrivateKey t)

type HasAddAccountModelCfg mConf t =
  ( Monoid mConf, Flattenable mConf t
  , HasWalletCfg mConf PrivateKey t
  )


uiAddAccount
  :: forall t m model mConf.
    ( MonadWidget t m, HasAddAccountModel model t, HasAddAccountModelCfg mConf t
    )
  => model
  -> Event t () -> m (mConf, Event t ())
uiAddAccount m _onClose = do
  onClose <- modalHeader $ text "Add Account"
  _f
