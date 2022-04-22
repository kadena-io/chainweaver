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
import Pact.Types.Runtime (ModuleName)

import Frontend.Crypto.Class (HasCrypto(..))
import Frontend.Foundation hiding (Arg)
import Frontend.Log (HasLogger(..))
import Frontend.ModuleExplorer.ModuleRef
import Frontend.Network
import Frontend.UI.Common
import Frontend.UI.Modal
import Frontend.UI.FormWidget (mkCfg)
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
     , HasCrypto key (Performable m)
     )
  => model -> Event t () -> m (mConf, Event t ())
uiManageTokens model onCloseExternal = do
  (conf, closes) <- splitE <$> inputToken model onCloseExternal
  mConf <- flatten conf
  close <- switchHold never closes
  pure (mConf, close)

tokenInput
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM (Performable m)
     , MonadFix m
     , HasLogger model t
     , HasNetwork model t
     , HasCrypto key (Performable m)
     , HasTransactionLogger m
     , MonadSample t (Performable m)
     , MonadIO m
     )
  => model
  -> Map.Map ChainId [ModuleName]
  -> Behavior t [ModuleName]
  -> m (Dynamic t (Maybe ModuleName))
tokenInput model chainToModuleMap bLocalList = do
  --TODO: Sample higher?
  netNAM <- sample $ current $ getNetworkNameAndMeta model
  let tokenValidator' = tokenValidator netNAM bLocalList
  (ti, _) <- mkLabeledInput True "Enter Token"
    (textFormWidgetAsync PopoverState_Disabled tokenValidator') $ mkCfg Nothing
  -- => PopoverState
  -- -> (Event t Text -> m (Event t (ValidationResult Text a)))
  -- -> PrimFormWidgetConfig t (Maybe Text)
  -- -> m (FormWidget t (Maybe a), Event t (Maybe Text))
  -- textFormWidgetAsync initPopState isValid cfg = mdo
  pure $ constDyn Nothing
  where
    moduleToChainMap = invertMap chainToModuleMap

    -- TODO: Is this an appropriate way to query all chains a node provides access to?
    chainList = Map.keys chainToModuleMap

    isModuleFungiblePact moduleName =
      -- Writing the query in this way allows us to restrict the functions that can produce an error
      -- In the query below, only the `describe-module` function can generate an error.
      -- If `describe-module` generated an error, it means that the user entered module is not present on any chain.
      -- If `describe-module` didn't generate any error, it means that the user entered module is a valid module,
      --      though it might not be a valid "fungible-v2" contract. For this contract, we should return a False.
      "(let ((moduleDesc (describe-module \"" <> moduleName <> "\")))" <>
        "(contains \"fungible-v2\"" <>
          "(if (contains 'interfaces moduleDesc)" <>
            "(at 'interfaces moduleDesc)" <>
            "[]" <>
            ")))"

    checkModuleIsFungible (netName, netMetadata) evModule = do
      --TODO: This is a bad hack to get the modname used in the req
      dModName <- holdDyn Nothing $ Just <$> evModule
      let net = model ^. network
      -- In case the module exists on some chains, this will store the list of those chainIds.
      -- If the module does not exist on any chains, it will store all the chainIds.
      let chainIdsToCheck = fromMaybe chainList . flip Map.lookup moduleToChainMap

      reqEv <- performEvent $ evModule <&> \mdule -> forM (chainIdsToCheck mdule) $ \chainId ->
        mkSimpleReadReq (isModuleFungiblePact (renderCompactText mdule))
          netName netMetadata $ ChainRef Nothing chainId
      respEv <- performLocalRead (model ^. logger) net reqEv
      pure $ attach (current dModName) respEv <&> \(mdule, responses) ->

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

    tokenValidator netAndMeta bLocalList inputEv = do
      let
        inputEvAndModList = attach bLocalList inputEv
        -- eNetAndMeta = tag (current $ getNetworkNameAndMeta net) inputEv
        (failureEv, validNameEv) = fanEither $ ffor inputEvAndModList $ \(localList, rawInput) -> do
          case parseModuleName rawInput of
            -- User input is not a valid module name
            Left err -> Left $ Failure "Invalid module name"
            -- User input is a valid module name, check if it already exists on any chain
            Right mdule -> case mdule `elem` localList of
              True -> Left $ Failure "Token already added"
              False -> Right mdule
      resultEv <- checkModuleIsFungible netAndMeta validNameEv
      pure $ leftmost [failureEv, resultEv]

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

     , HasCrypto key (Performable m)
     , HasTransactionLogger m
     , MonadSample t (Performable m)
     )
  => model
  -> Event t () -- ^ Modal was externally closed
  -> m (Event t (mConf, Event t ()))
inputToken model _ = do
  close <- modalHeader $ text "Manage Tokens"
  networkView $ model ^. network_modules <&> \chainToModuleMap ->
    if (Map.null chainToModuleMap) then ((modalMain $ text "Loading Tokens...") >> pure mempty) else
      mdo
        (dmFung, deleteEv) <- modalMain $ do
          dialogSectionHeading mempty "Tokens"
          dmFung <- divClass "flex" $ mdo
            --TODO: add trigger
            -- let triggerEv inputEl = () <$ tag (current $ value inputEl) addClickEv
            dmFung <- divClass "group flex-grow" $ tokenInput model chainToModuleMap $ fmap NE.toList $ current localListDyn
            addClickEv <- confirmButton (def & uiButtonCfg_class .~ "margin") "Add"
            pure dmFung

          eventEv <- networkView $ localListDyn <&> \ne -> do
            delClicks <- forM (NE.toList ne) $ \token -> do
              ev <- divClass "flex paddingLeftTopRight" $ do
                divClass "flex-grow paddingTop" $ text $ renderTokenName token
                if token == kdaToken
                  then pure never
                  else deleteButtonNaked def
              pure $ token <$ ev
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
        localListDyn <- foldDyn processUserAction initialTokens $ leftmost
          [ Left <$> deleteEv
          , Right <$> addEv
          ]
        let
          fungibleDyn = _wallet_fungible $ model ^. wallet
          fungE = fmapMaybe id $ (,) <$> current fungibleDyn <*> current localListDyn <@ done <&> \(currentFungible, localList) ->
            -- If currently selected token is not in the new local list
            -- that means it was deleted by the user. Switch to the head element ie "coin".
            if currentFungible `notElem` localList
              then Just $ NE.head localList
              else Nothing
          conf = mempty & walletCfg_moduleList .~ tag (current localListDyn) done & walletCfg_fungibleModule .~ fungE
        pure (conf, done <> close)
