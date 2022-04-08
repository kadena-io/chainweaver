{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.UI.Dialogs.ManageTokens
  ( uiManageTokens
  ) where


import Control.Lens hiding (failover)
import Control.Error (hush)
import Control.Monad (foldM, forM, void)
import Data.Bifunctor (first)
import Data.List (foldl')
import Data.Either (rights)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.These
import Reflex.Dom

import Pact.Types.Exp
import Pact.Types.PactValue
import Pact.Types.Pretty  (renderCompactText)
import Pact.Types.Names   (parseModuleName)

import Frontend.Crypto.Class (HasCrypto(..))
import Frontend.Foundation hiding (Arg)
import Frontend.Log (HasLogger(..))
import Frontend.ModuleExplorer.ModuleRef
import Frontend.Network
import Frontend.UI.Modal
import Frontend.UI.Widgets
import Frontend.UI.Widgets.Helpers
import Frontend.Wallet

-- | A function for inverting a map, that goes from a key type to a list type.
--
-- [(1, ['a', 'b']), (2, ['b', 'c'])] => [('a', [1]), ('b', [1,2]), ('c', [2])]
invertMap :: (Ord a) => Map.Map k [a] -> Map.Map a [k]
invertMap m = foldl' (\mp (k, as) -> 
  foldl' (\mp1 a -> Map.insertWith (++) a [k] mp1) mp as
  ) Map.empty $ Map.toList m

-- | A modal for watching request keys
uiManageTokens
  :: ( Flattenable mConf t
     , Monoid mConf
     , HasLogger model t
     , HasCrypto key m
     , HasTransactionLogger m
     , HasNetwork model t
     , HasWalletCfg mConf key t
     , HasWallet model key t
     , MonadWidget t m
     )
  => model -> Event t () -> m (mConf, Event t ())
uiManageTokens model onCloseExternal = do
  (conf, closes) <- splitE <$> inputToken model onCloseExternal
  mConf <- flatten conf
  close <- switchHold never closes
  pure (mConf, close)

-- | Allow the user to input a new fungible
inputToken
  :: ( Monoid mConf
     , MonadWidget t m
     , HasLogger model t
     , HasCrypto key m
     , HasTransactionLogger m
     , HasNetwork model t
     , HasWallet model key t
     , HasWalletCfg mConf key t
     )
  => model
  -> Event t () -- ^ Modal was externally closed
  -> m (Event t (mConf, Event t ()))
inputToken model _ = do
  close <- modalHeader $ text "Manage Tokens"
  networkView $ model ^. network_modules <&> \chainToModuleMap ->
    if Map.null chainToModuleMap
      then do
        void $ modalMain $ text "Loading Tokens..."
        pure mempty
      else mdo
        (dmFung, deleteEv) <- modalMain $ do
          dialogSectionHeading mempty "Tokens"
          dmFung <- divClass "flex" $ mdo
            let
              tokenValidator inputEv = do
                let
                  moduleToChainMap = invertMap chainToModuleMap
                  chainList = Map.keys chainToModuleMap
                dynEv <- networkHold (pure never) $ inputEv <&> \input ->
                  case parseModuleName input of
                    -- User input is not a valid module name
                    Left err -> pure never
                    -- User input is a valid module name, check if it already exists on any chain
                    Right mdule -> do
                      let
                        -- In case the module exists on some chains, this will store the list of those chainIds.
                        -- If the module does not exist on any chains, it will store all the chainIds.
                        chainIdsToCheck = fromMaybe chainList $ Map.lookup mdule moduleToChainMap
                        net = model ^. network
                        createCode moduleName = "(contains \"fungible-v2\" (at 'interfaces (describe-module \"" <> moduleName <> "\")))"

                      reqEv <- networkView $ getNetworkNameAndMeta net <&> \(netName, netMetadata) ->
                        forM chainIdsToCheck $ \chainId ->
                          mkSimpleReadReq (createCode (renderCompactText mdule)) netName netMetadata $ ChainRef Nothing chainId
                      respEv <- performLocalRead (model ^. logger) net reqEv
                      pure $ respEv <&> \responses ->
                        -- In the following foldM, we have used Either as a Monad to short circuit the fold.
                        -- We short circuit the fold as soon as we find a `True`.
                        -- Note: In order to short circuit, we need to use `Left`, which is commonly used for errors.
                        --       Here, `Left` does NOT represent errors, only a way to short circuit.
                        -- If we encounter an unknown error, we keep the already existing error with us.
                        either id id $
                          foldM (\err (_, netErrorResult) ->
                            case netErrorResult of
                              That (_, pVal) -> case pVal of
                                PLiteral (LBool b) -> if b
                                  then Left $ Success mdule
                                  else Right $ Failure "Contract not a token"
                                x -> Right err
                              _ -> Right err
                            ) (Failure "This module does not exist on any chain") responses

                switchHold never $ updated dynEv

              triggerEv inputEl = () <$ tag (current $ value inputEl) addClickEv
            dmFung <- divClass "group flex-grow" $
              fmap snd $ uiTextInputAsync "Enter token" True (Nothing, PopoverState_Disabled) never triggerEv tokenValidator moduleListId
            addClickEv <- confirmButton (def & uiButtonCfg_class .~ "margin") "Add"
            pure dmFung
          eventEv <- networkView $ localListDyn <&> \ne -> do
            delClicks <- forM (NE.toList ne) $ \token -> do
              (e, _) <- accordionItem' False "segment segment_type_secondary"
                (do
                  text $ _mnName token
                  deleteButtonNaked $ def & uiButtonCfg_class .~ "accordion__title-button")
                (text $ renderCompactText token)
              pure $ token <$ e
            pure $ leftmost delClicks
          deleteEv <- switchHold never eventEv
          pure (dmFung, deleteEv)
        done <- modalFooter $ do
          confirmButton def "Done"
        initialTokens <- sample $ current $ model ^. wallet_tokenList
        let
          addToken (t NE.:| ts) newToken = t NE.:| (newToken : ts)
          deleteToken (t NE.:| ts) token = t NE.:| filter (/= token) ts
          processUserAction action tokens = either (deleteToken tokens) (addToken tokens) action
          addEv = fmapMaybe id $ updated dmFung
          tokenListDyn = model ^. wallet_tokenList
        localListDyn <- foldDyn processUserAction initialTokens $ leftmost
          [ Left <$> deleteEv
          , Right <$> addEv
          ]
        let 
          fungE = tagMaybe (current dmFung) done
          conf = mempty & walletCfg_moduleList .~ tag (current localListDyn) done
        pure (conf, done <> close)
