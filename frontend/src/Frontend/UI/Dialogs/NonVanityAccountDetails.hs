{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecursiveDo #-}
-- | Dialog for viewing the details of an account.
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.NonVanityAccountDetails
  ( uiNonVanityAccountDetails
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Text (Text)
import qualified Data.IntMap as IntMap
import qualified Pact.Types.ChainId as Pact
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.KadenaAddress (textKadenaAddress)
------------------------------------------------------------------------------
import           Frontend.UI.Modal
import           Frontend.Wallet
import           Frontend.Crypto.Ed25519 (keyToText)
import           Frontend.UI.Widgets
import           Frontend.UI.Widgets.Helpers (dialogSectionHeading)
import           Frontend.Foundation
------------------------------------------------------------------------------

type HasUiNonVanityAccountDetailsModelCfg mConf key t =
  ( Monoid mConf
  , Flattenable mConf t
  , HasWalletCfg mConf key t
  )

uiNonVanityAccountDetails
  :: ( HasUiNonVanityAccountDetailsModelCfg mConf key t
     , MonadWidget t m
     )
  => model
  -> Event t ()
  -> m (mConf, Event t ())
uiNonVanityAccountDetails model _onCloseExternal = mdo
  onClose <- modalHeader $ dynText title

  dwf <- workflow (uiNonVanityAccountDetailsDialog model onClose)

  let (title, (conf, dEvent)) = fmap splitDynPure $ splitDynPure dwf

  mConf <- flatten =<< tagOnPostBuild conf

  return ( mConf
         , leftmost [switch $ current dEvent, onClose]
         )

uiNonVanityAccountDetailsDialog
  :: ( HasUiNonVanityAccountDetailsModelCfg mConf key t
     , MonadWidget t m
     )
  => model
  -> Event t ()
  -> Workflow t m (Text, (mConf, Event t ()))
uiNonVanityAccountDetailsDialog model onClose = Workflow $ do
  let displayText lbl v cls =
        let
          attrFn cfg = uiInputElement $ cfg
            & initialAttributes <>~ ("disabled" =: "true" <> "class" =: (" " <> cls))
        in
          mkLabeledInputView False lbl attrFn $ pure v

  notesEdit <- divClass "modal__main account-details" $ do
    dialogSectionHeading mempty "Info"
    divClass "group" $ do
      -- Account name
      _ <- displayText "Account Name" "futz" "account-details__name"
      -- Public key
      _ <- displayText "Public Key" "keks" "account-details__pubkey"
      -- Chain id
      _ <- displayText "Chain ID" "watz" "account-details__chain-id"
      -- separator
      horizontalDashedSeparator
      -- Notes edit
      --
  modalFooter $ do
    onDone <- confirmButton def "Done"

    pure ( ("Account Details", (mempty, leftmost [onClose, onDone]))
         , never
         )

