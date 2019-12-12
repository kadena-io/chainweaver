{-# LANGUAGE TemplateHaskell #-}
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
import           Control.Monad (void)
import           Data.Text (Text)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Pact.Types.ChainId as Pact
import           Data.Decimal (Decimal)
------------------------------------------------------------------------------
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.UI.Modal
import           Frontend.Wallet
import           Frontend.UI.Widgets
import           Frontend.Foundation
------------------------------------------------------------------------------

data ChainInfo = ChainInfo
  { _chainInfo_notes :: Text
  , _chainInfo_balance :: Decimal
  }
makeClassy ''ChainInfo

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
  -> Event t ()
  -> m (mConf, Event t ())
uiKeyDetails model _onCloseExternal = mdo
  onClose <- modalHeader $ dynText title
  dwf <- workflow (uiKeyDetailsDetails model onClose)
  let (title, (conf, dEvent)) = fmap splitDynPure $ splitDynPure dwf
  mConf <- flatten =<< tagOnPostBuild conf
  return ( mConf
         , leftmost [switch $ current dEvent, onClose]
         )

-- TODO : Misc data for development REMOVE!!!!
keyStub :: Text
keyStub = "4150056dee2b549925cf2a70039769867e34cf7c456a238f31ccb82c60f24a44a"

keyNotes :: Text
keyNotes = "I am the notes for the top level key"

chainMap0 :: Reflex t => Dynamic t (Map Pact.ChainId ChainInfo)
chainMap0 = constDyn $ Map.fromList
  [ (Pact.ChainId "0", ChainInfo "I am on chain 0" 127.0)
  , (Pact.ChainId "1", ChainInfo "1 lolz" 0)
  , (Pact.ChainId "2", ChainInfo "I am on chain 2" 1.0)
  , (Pact.ChainId "3", ChainInfo "I am on chain 3" 2.0)
  , (Pact.ChainId "4", ChainInfo "I am on chain 4" 72.0)
  , (Pact.ChainId "5", ChainInfo "I am on chain 5" 35e-6)
  , (Pact.ChainId "6", ChainInfo "I am on chain 6" 0)
  , (Pact.ChainId "7", ChainInfo "I am on chain 7" 1.0)
  , (Pact.ChainId "8", ChainInfo "I am on chain 8" 0.1)
  , (Pact.ChainId "9", ChainInfo "I am on chain 9" 0.333333)
  ]

uiKeyDetailsDetails
  :: ( HasUiKeyDetailsModelCfg mConf key t
     , MonadWidget t m
     )
  => model
  -> Event t ()
  -> Workflow t m (Text, (mConf, Event t ()))
uiKeyDetailsDetails model onClose = Workflow $ do
  let displayText lbl v cls =
        let
          attrFn cfg = uiInputElement $ cfg
            & initialAttributes <>~ ("disabled" =: "true" <> "class" =: (" " <> cls))
        in
          mkLabeledInputView False lbl attrFn $ pure v

  conf <- divClass "modal__main key-details" $ do
    divClass "group" $ do
      -- Public key
      _ <- displayText "Public Key" keyStub "key-details__pubkey"
      -- Top level notes
      _ <- displayText "Notes" keyNotes "key-details__top-notes"
      pure ()

    elClass "h2" "heading heading_type_h2" $ text "Chain Specific Info"
    divClass "group" $ do
      uiChainTable model chainMap0
 
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
  -> Dynamic t (Map Pact.ChainId ChainInfo)
  -> m mConf
uiChainTable model chainMap = do
  let
    tableAttrs =
      "style" =: "table-layout: fixed; width: 98%"
      <> "class" =: "key-details table"
  _events <- elAttr "table" tableAttrs $ do
    el "colgroup" $ do
      elAttr "col" ("style" =: "width: 15%") blank
      elAttr "col" ("style" =: "width: 40%") blank
      elAttr "col" ("style" =: "width: 30%") blank
      elAttr "col" ("style" =: "width: 10%") blank
    el "thead" $ el "tr" $ do
      let mkHeading = elClass "th" "key-details__table-heading" . text
      traverse_ mkHeading $
        [ "Chain"
        , "Notes"
        , "Balance"
        , ""
        ]

    el "tbody" $ do
      listWithKey chainMap (uiChainTableRow model)

  -- let onChainModal = switchDyn $ leftmost . Map.elems <$> events
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
  -> Pact.ChainId
  -> Dynamic t ChainInfo
  -> m (Event t ())
uiChainTableRow _model k cInfo = do
  let
    keyDeetCls = mappend "key-details__"
    showBal b = tshow b <> " KDA"

    burgerBtn = tableButton "..." $ def
      & uiButtonCfg_class .~ "key-details__table-button--hamburger"

  elClass "tr" (keyDeetCls "table-row") $ do
    let td = elClass "td" (keyDeetCls "table-cell")

    td $ divClass (keyDeetCls "table-id") $ text $ Pact._chainId k
    td $ divClass (keyDeetCls "table-notes") $ dynText $ fmap _chainInfo_notes cInfo
    td $ divClass (keyDeetCls "table-balance") $ dynText $ fmap (showBal. _chainInfo_balance) cInfo
    td $ divClass (keyDeetCls "table-buttons") $ void $ burgerBtn

  pure never

