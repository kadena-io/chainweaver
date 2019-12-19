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
import           Data.Map (Map)

import Pact.Types.ChainId (ChainId)
import qualified Pact.Types.ChainId as Pact
------------------------------------------------------------------------------
import           Reflex
import           Reflex.Dom
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
  => model
  -> PublicKey
  -> Dynamic t (Map ChainId NonVanityAccount)
  -> Event t ()
  -> m (mConf, Event t ())
uiKeyDetails model pubKey keyChainInfo _onCloseExternal = mdo
  onClose <- modalHeader $ dynText title
  dwf <- workflow (uiKeyDetailsDetails model pubKey keyChainInfo onClose)
  let (title, (conf, dEvent)) = fmap splitDynPure $ splitDynPure dwf
  mConf <- flatten =<< tagOnPostBuild conf
  return ( mConf
         , leftmost [switch $ current dEvent, onClose]
         )

uiKeyDetailsDetails
  :: ( HasUiKeyDetailsModelCfg mConf key t
     , MonadWidget t m
     )
  => model
  -> PublicKey
  -> Dynamic t (Map ChainId NonVanityAccount)
  -> Event t ()
  -> Workflow t m (Text, (mConf, Event t ()))
uiKeyDetailsDetails model pubKey keyChainInfo onClose = Workflow $ do
  let displayText lbl v cls =
        let
          attrFn cfg = uiInputElement $ cfg
            & initialAttributes <>~ ("disabled" =: "true" <> "class" =: (" " <> cls))
        in
          mkLabeledInputView False lbl attrFn $ pure v

  conf <- divClass "modal__main key-details" $ do
    _ <- divClass "group" $ do
      -- Public key
      displayText "Public Key" (keyToText pubKey) "key-details__pubkey"

    _ <- elClass "h2" "heading heading_type_h2" $ text "Chain Specific Info"
    divClass "group" $
      uiChainTable model keyChainInfo
 
  modalFooter $ do
    onDone <- confirmButton def "Done"

    pure ( ("Key Details", (conf, leftmost [onClose, onDone]))
         , never
         )

uiChainTable
  :: ( MonadFix m
     , Monoid mConf
     , DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     )
  => model
  -> Dynamic t (Map ChainId NonVanityAccount)
  -> m mConf
uiChainTable model keyChainInfo = do
  let
    tableAttrs =
      "style" =: "table-layout: fixed; width: 98%"
      <> "class" =: "key-details table"
  _events <- elAttr "table" tableAttrs $ do
    el "colgroup" $ do
      elAttr "col" ("style" =: "width: 40%") blank
      elAttr "col" ("style" =: "width: 50%") blank
      elAttr "col" ("style" =: "width: 20%") blank
    el "thead" $ el "tr" $ do
      let mkHeading = elClass "th" "key-details__table-heading" . text
      traverse_ mkHeading $
        [ "Chain"
        , "Balance"
        , ""
        ]

    el "tbody" $ do
      listWithKey keyChainInfo (uiChainTableRow model)

  pure mempty

tableButton :: DomBuilder t m => Text -> UiButtonCfg -> m (Event t ())
tableButton lbl cfg = uiButton (cfg
  & uiButtonCfg_class <>~ " key-details__table-button"
  ) $ text lbl

uiChainTableRow
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => model
  -> ChainId
  -> Dynamic t NonVanityAccount
  -> m (Event t ())
uiChainTableRow _model chain dNonVanity = do
  let
    keyDeetCls = mappend "key-details__"

    showBal (Just b) = tshow b <> " KDA"
    showBal Nothing = "Unknown"

    burgerBtn = tableButton "TBC" $ def
      & uiButtonCfg_class .~ "key-details__table-button--hamburger"

  elClass "tr" (keyDeetCls "table-row") $ do
    let
      td :: DomBuilder t m => m a -> m a
      td = elClass "td" (keyDeetCls "table-cell")

    td $ divClass (keyDeetCls "table-id") $ text $ Pact._chainId chain
    td $ divClass (keyDeetCls "table-balance") $ dynText
      $ fmap (showBal .fmap unAccountBalance . _accountInfo_balance . _nonVanityAccount_info) dNonVanity

    -- TODO: Wire through enough info for 'uiSendModal'
    td $ divClass (keyDeetCls "table-button") burgerBtn
