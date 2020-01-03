{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Dialog for viewing the details of a key.
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.KeyDetails
  ( uiKeyDetails
  ) where
 
------------------------------------------------------------------------------
import           Control.Lens
import           Data.Text (Text)
------------------------------------------------------------------------------
import           Reflex
import           Reflex.Dom hiding (Key)
------------------------------------------------------------------------------
import           Frontend.UI.Modal
import           Frontend.Wallet
import           Frontend.UI.Widgets
import           Frontend.Foundation
------------------------------------------------------------------------------

type HasUiKeyDetailsModelCfg mConf key t =
  ( Monoid mConf
  , Flattenable mConf t
  , HasWalletCfg mConf key t
  )

uiKeyDetails
  :: ( HasUiKeyDetailsModelCfg mConf key t
     , MonadWidget t m
     )
  => Key key
  -> Event t ()
  -> m (mConf, Event t ())
uiKeyDetails key _onCloseExternal = mdo
  onClose <- modalHeader $ dynText title
  dwf <- workflow (uiKeyDetailsDetails key onClose)
  let (title, (conf, dEvent)) = fmap splitDynPure $ splitDynPure dwf
  mConf <- flatten =<< tagOnPostBuild conf
  return ( mConf
         , leftmost [switch $ current dEvent, onClose]
         )

uiKeyDetailsDetails
  :: ( HasUiKeyDetailsModelCfg mConf key t
     , MonadWidget t m
     )
  => Key key
  -> Event t ()
  -> Workflow t m (Text, (mConf, Event t ()))
uiKeyDetailsDetails key onClose = Workflow $ do
  let displayText lbl v cls =
        let
          attrFn cfg = uiInputElement $ cfg
            & initialAttributes <>~ ("disabled" =: "true" <> "class" =: (" " <> cls))
        in
          mkLabeledInputView False lbl attrFn $ pure v

  divClass "modal__main key-details" $ do
    divClass "group" $ do
      -- Public key
      _ <- displayText "Public Key" (keyToText $ _keyPair_publicKey $ _key_pair key) "key-details__pubkey"
      -- Notes
      _ <- displayText "Notes" (unAccountNotes $ _key_notes key) "key-details__notes"
      pure ()

  modalFooter $ do
    onDone <- confirmButton def "Done"

    pure ( ("Key Details", (mempty, leftmost [onClose, onDone]))
         , never
         )

