{-# LANGUAGE ConstraintKinds #-}
-- | Dialog for viewing the details of an account.
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
module Frontend.UI.Dialogs.AccountDetails
  ( uiAccountDetails
  ) where
------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad (void)
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.KadenaAddress (mkKadenaAddress, textKadenaAddress)
------------------------------------------------------------------------------
import           Frontend.Wallet
import           Frontend.Network
import           Frontend.Crypto.Ed25519 (keyToText)
import           Frontend.UI.Modal
import           Frontend.UI.Widgets
import           Frontend.Foundation
------------------------------------------------------------------------------
type HasUiAccountDetailsModel model key t =
  ( HasNetwork model t
  )

type HasUiAccountDetailsModelCfg mConf key t =
  ( Monoid mConf, Flattenable mConf t
  )

uiAccountDetails
  :: forall m t model mConf key.
     ( HasUiAccountDetailsModel model key t
     , HasUiAccountDetailsModelCfg mConf key t
     , MonadWidget t m
     )
  => model
  -> Account key
  -> Event t ()
  -> m (mConf, Event t ())
uiAccountDetails m a _onCloseExternal = do
  let dKAddr = (\n -> textKadenaAddress $ mkKadenaAddress n (_account_chainId a) (_account_name a)) <$> m ^. network_selectedNetwork

  let displayText lbl v cls =
        let
          attrFn cfg = uiInputElement $ cfg
            & initialAttributes <>~ ("disabled" =: "true" <> "class" =: (" " <> cls))
        in
          mkLabeledInputView attrFn lbl v

  onClose <- modalHeader $ text "Account Details"
  modalMain $ divClass "modal__main account-details" $ do
    elClass "h2" "heading heading_type_h2" $ text "Info"
    divClass "group" $ do
      -- Account name
      _ <- displayText "Account Name" (constDyn $ unAccountName (_account_name a)) "account-details__name"
      -- Public key
      _ <- displayText "Public Key" (constDyn . keyToText . _keyPair_publicKey $ _account_key a) "account-details__pubkey"
      -- Chain id
      _ <- displayText "Chain ID" (constDyn . T.pack . show $ _account_chainId a) "account-details__chain-id"
      -- separator
      el "hr" blank
      -- Kadena Address
      _ <- displayText "Kadena Address" dKAddr "account-details__kadena-address"
      -- copy
      _ <- divClass "account-details__copy-btn-wrapper" $ copyButton (def
        & uiButtonCfg_class .~ constDyn "account-details__copy-btn button_type_confirm"
        & uiButtonCfg_title .~ constDyn (Just "Copy")
        ) $ current dKAddr

      pure ()

    let
      txnHist :: Dynamic t [(Text, Double, Text)]
      txnHist = constDyn
          [ ("dateA", -10.22, "63b2eba4ed70d4612dc4" )
          , ("dateB", 5.4333, "5ffc1f7fef7a44738625")
          , ("dateC", -105.701, "c59d9840b0b6609083c7")
          , ("dateD", 1023.00, "3hfc1f7fef7a44738636")
          ]

    elClass "h2" "heading heading_type_h2" $ text "History"
    divClass "group" $ do
      let tableAttrs = "class" =: "table account-details__txn-history-table"

      void $ elAttr "table" tableAttrs $ do
        el "thead" $ el "tr" $ do
          let mkHeading = elClass "th" "table__heading account-details__txn-history-table-heading" . text
          traverse_ mkHeading $
            [ "Date"
            , "Amount"
            , "Transaction Hash"
            ]

        el "tbody" $ simpleList txnHist $ \txn -> do
          elClass "tr" "table__row account-details__txn-history-row" $ do

            let showAmount d = (if d > 0 then "+" else T.empty) <> tshow d
                td = elClass "td" "tabel__cell_size_flex"

                amountCell d = divClass "account-details__txn-history-cell-amount" $ do
                  divClass "amount-kda-label" $ text "KDA"
                  divClass "amount-amount" $ dynText (showAmount <$> d)

            td $ dynText (view _1 <$> txn)
            td $ amountCell (view _2 <$> txn)
            td $ dynText (view _3 <$> txn)


  modalFooter $ do
    _ <- cancelButton (def & uiButtonCfg_class <>~ " account-details__remove-account-btn") "Remove Account"
    onDone <- confirmButton def "Done"
    pure
      ( mempty
      , leftmost [onClose, onDone]
      )
