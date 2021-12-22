{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Pact repl functionality as needed in chainweaver.
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.Repl
  ( -- * Interface
    ReplCfg (..)
  , HasReplCfg (..)
  , WebRepl (..)
  , HasWebRepl (..)
  , ReplOutput (..)
  , ModuleName (..)
  , VerifyResult
  , TransactionSuccess (..)
    -- * Implementation and Creation
  , HasReplModelCfg
  , HasReplModel
  , makeRepl
  ) where

------------------------------------------------------------------------------
import           Control.Arrow              (left)
import           Control.Lens               hiding ((|>))
import           Control.Monad              ((>>))
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Aeson                 as Aeson (Object, encode)
import qualified Data.ByteString.Lazy       as BSL
import           Data.Either                (isRight)
import qualified Data.HashMap.Strict        as HM
import qualified Data.IntMap                as IntMap
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Sequence              (Seq, (|>))
import qualified Data.Sequence              as S
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Generics.Deriving.Monoid   (mappenddefault, memptydefault)
import           GHC.Generics               hiding (to)
import           Reflex
import qualified Text.Trifecta              as TF
import qualified Text.Trifecta.Delta        as Delta
------------------------------------------------------------------------------
import           Pact.Parse                 (exprsOnly)
import           Pact.Repl
import           Pact.Repl.Types
import           Pact.Types.Exp
import           Pact.Types.Info
import           Pact.Types.Term            (ModuleName (..), Name, Term (..), mnNamespace)
------------------------------------------------------------------------------
import           Frontend.Network
import           Frontend.Foundation
import           Frontend.JsonData
import           Frontend.Messages
import           Frontend.ModuleExplorer.Example (exampleNamespacesFile)
import           Frontend.Wallet
import           Common.Api (getVerificationServerUrl)
import           Common.Modules
------------------------------------------------------------------------------

-- | Output of Repl to be shown to the user.
data ReplOutput
  = ReplOutput_Cmd Text -- ^ Command that got entered/executed.
  | ReplOutput_Res Text -- ^ Result of executed command.

-- | Result of a transaction in case of success.
data TransactionSuccess = TransactionSuccess
  { _ts_term    :: Term Name -- ^ The term of the last expression.
  , _ts_modules :: Map ModuleName Int -- ^ Modules that got loaded in this transaction.
  } deriving (Eq, Show)
makePactLenses 'TransactionSuccess


type VerifyResult = Map ModuleName (Either Text Text)


data ReplCfg t = ReplCfg
  { _replCfg_sendTransaction :: Event t Text
    -- ^ Send code to the REPL that won't be echoed and will be put in a transaction.
  , _replCfg_verifyModules   :: Event t (Set ModuleName)
    -- ^ Verify loaded modules.
  , _replCfg_sendCmd         :: Event t Text
    -- ^ Send command to be executed by the REPL that should show up in the output.
  , _replCfg_reset           :: Event t ()
    -- ^ Reset repl state.
  }
  deriving Generic

makePactLenses ''ReplCfg

-- | Current ModuleExploer state.
--
--   This is called WebRepl instead of Repl to avoid confusion with the `Repl`
--   type in Pact.
data WebRepl t = WebRepl
  { _repl_output              :: Dynamic t (Seq ReplOutput)
  -- ^ Repl output to be shown to the user.
  , _repl_newOutput           :: Event t Text
  -- ^ Gets triggered whenever there is some new output.
  --   Where output is really output, no echoed commands.
  , _repl_transactionFinished :: Event t (Either Text TransactionSuccess)
  -- ^ Execution of transaction finished with either an error or successfully.
  , _repl_modulesVerified     :: Event t VerifyResult
  -- ^ Result of a module verification. Either an error or output of (verify).
  }
  deriving Generic

makePactLenses ''WebRepl

-- | Result of `evalPact` and `evalRepl` function families.
type ReplResult = Either String (Term Name)


-- | Result of executing a transaction:
type TransactionResult = Either String TransactionSuccess

-- | `Repl` as used in chainweaver.
type PactRepl a = ExceptT String (StateT ReplState IO) a

-- | Data used by internal functions of this module.
data Impl t = Impl
  { _impl_state :: Dynamic t ReplState
  , _impl_repl  :: WebRepl t
  }

-- Implementation:

type HasReplModel m key t = (HasNetwork m t, HasJsonData m t, HasWallet m key t)

type HasReplModelCfg mConf t = (HasMessagesCfg mConf t, Monoid mConf)

type ReplMonad t m =
  ( Reflex t, MonadHold t m, MonadFix m, PostBuild t m, MonadIO m
  , PerformEvent t m, MonadIO (Performable m), MonadSample t (Performable m)
  )

makeRepl
  :: forall key t m cfg model mConf
  . ( ReplMonad t m, HasReplModel model key t, HasReplModelCfg mConf t
    , HasReplCfg cfg t, HasConfigs m
    )
  => model -> cfg -> m (mConf, WebRepl t)
makeRepl m cfg = build $ \ ~(_, impl) -> do
    -- Dummy state, that gets never used, so we avoid a pointless `Maybe` or
    -- sampling, which could trigger a loop:
    initState <- liftIO $ initReplState StringEval Nothing
    onPostBuild <- getPostBuild
    -- Gets ensured on deployment to never be Nothing in the production case (ghcjs build):
    verificationUri <- getVerificationServerUrl
    onResetSt <- performEvent $ initRepl verificationUri impl m <$ leftmost
      [ cfg ^. replCfg_reset
      , onPostBuild
      ]

    let envData = m ^. jsonData ^. to getJsonDataObjectLax
        keys = IntMap.elems . fmap _key_pair <$> m ^. wallet_keys

    -- Those events can happen simultaneously - so make sure we don't lose any
    -- state:
    onRNewEnvKeys  <- performStateCmd impl $ mergeWith (>>)
      [ setEnvData <$> updated envData
      , setEnvKeys <$> updated keys
      ]
    let
      onNewEnvKeys = fmap snd onRNewEnvKeys
      onNewEnvKeysErr = fmapMaybe getErr onRNewEnvKeys

      getErr = (^? _1 . _Left . to T.pack)

    onNewTransR <- runTransaction impl $ cfg ^. replCfg_sendTransaction
    onNewCmdR <- runCmd impl $ cfg ^. replCfg_sendCmd

    onVerifyR <- runVerify impl $ cfg ^. replCfg_verifyModules

    let
      onNewTransResult :: Event t TransactionResult
      onNewTransResult = fmap fst onNewTransR

      onNewTransSuccess :: Event t TransactionSuccess
      onNewTransSuccess = fmapMaybe (^? _Right) onNewTransResult

      onNewTransSt   = fmap snd onNewTransR
      onNewCmdResult = fmap fst onNewCmdR
      onNewCmdSt     = fforMaybe onNewCmdR $ \(r,s) -> if isRight r then Just s else Nothing
      onVerifySt     = fmap snd onVerifyR

    st <- holdDyn initState $ leftmost [ onResetSt
                                       , onNewEnvKeys
                                       , onNewTransSt
                                       , onNewCmdSt
                                       , onVerifySt
                                       ]
    let appendHist = flip (|>)
        onNewOutput = leftmost [ prettyTextPretty . _ts_term <$> onNewTransSuccess
                               , prettyResult <$> onNewCmdResult
                               ]

    output <- foldDyn id S.empty $ mergeWith (.)
      [ appendHist . ReplOutput_Res <$> onNewOutput
      , appendHist . ReplOutput_Cmd <$> cfg ^. replCfg_sendCmd
      , const S.empty <$ cfg ^. replCfg_reset
      ]

    let onFinishedTrans = fmap (left T.pack) onNewTransResult
        onFailedTrans = fmapMaybe (^? _Left) onFinishedTrans

    pure
      ( mempty
          & messagesCfg_send .~ (mconcat . map (fmap pure))
              [ onFailedTrans
              , onNewEnvKeysErr
              ]
      , Impl
          { _impl_state = st
          , _impl_repl = WebRepl
              { _repl_output = output
              , _repl_transactionFinished = onFinishedTrans
              , _repl_newOutput = onNewOutput
              , _repl_modulesVerified = fst <$> onVerifyR
              }
          }
      )
  where
    build = fmap (fmap _impl_repl) . mfix

    performStateCmd impl = performEvent . fmap (withRepl impl)

    -- In case we ever want to show more than the last output term:
    -- showStateTerms :: ReplState -> Text
    -- showStateTerms = T.unlines . map showTerm . reverse . _rTermOut

-- | Create a brand new Repl state and set env-data.
initRepl
  :: forall key t m model
  . (HasReplModel model key t, MonadIO m, Reflex t, MonadSample t m)
  => Maybe Text
  -- ^ Verification server URL
  -> Impl t -> model -> m (ReplState)
initRepl verificationUri oldImpl m  = do
  r <- mkState verificationUri
  let initImpl = oldImpl { _impl_state = pure r } -- Const dyn so we can use `withRepl` for initialization - gets dropped afterwards.
  env  <- sample . current $ m ^. jsonData . to getJsonDataObjectLax
  keys <- sample . current $ m ^. wallet_keys
  fmap snd . withRepl initImpl $ do
    void $ setEnvData env
    void $ setEnvKeys $ fmap _key_pair $ IntMap.elems keys
    setupNamespaces

setupNamespaces :: PactRepl ()
setupNamespaces = do
  void $ pactEvalRepl' "(begin-tx)"
  void $ pactEvalRepl' exampleNamespacesFile `catchError` error
  void $ pactEvalRepl' "(commit-tx)"

-- | Create a brand new Repl state:
mkState
  :: MonadIO m
  => Maybe Text
  -- ^ Verification server URL
  -> m ReplState
mkState verificationUri = do
    liftIO $ initReplState StringEval $ T.unpack <$> verificationUri


-- | Set env-data to the given Object
setEnvData :: Object -> PactRepl (Term Name)
setEnvData = pactEvalRepl' . ("(env-data " <>) . (<> ")") . mkCmd
  where
    mkCmd = toJsonString . safeDecodeUtf8 . BSL.toStrict . encode

    escapeJSON = T.replace "\"" "\\\""

    toJsonString = surroundWith "\"" . escapeJSON

    surroundWith :: Semigroup s => s -> s -> s
    surroundWith o i = o <> i <> o

-- | Set env-keys to the given keys
setEnvKeys :: [KeyPair key] -> PactRepl (Term Name)
setEnvKeys =
  pactEvalRepl' . ("(env-keys [" <>) . (<> "])") . renderKeys
    where
      renderKeys = T.unwords . map (keyToText' . _keyPair_publicKey)

      keyToText' = safeDecodeUtf8 . BSL.toStrict . encode


-- | Run an interactive command on the REPL.
runCmd
  :: forall t m
  . ( ReplMonad t m )
  => Impl t -> Event t Text -> m (Event t (ReplResult, ReplState))
runCmd impl onCmd =
    performEvent $ withRepl impl . pactEvalRepl' <$> onCmd

-- | Verify a module.
runVerify
  :: forall t m
  . ( ReplMonad t m )
  => Impl t -> Event t (Set ModuleName) -> m (Event t (VerifyResult, ReplState))
runVerify impl onMod =
    performEvent $ runIt . verifyModules <$> onMod
  where
    runIt cmd = do
      st <- sample . current $ _impl_state impl
      liftIO $ flip runStateT st $ cmd

    verifyModules ms = do
      rs <- traverse verifyModule . Set.toList $ ms
      pure $ Map.fromList rs

    verifyModule m = do
      r <- runExceptT . doTypeCheckAndVerify $ m
      pure (m, bimap T.pack prettyTextPretty r)

    doTypeCheckAndVerify m = do
      -- Success output of typecheck is mostly not parseable:
      void $ pactEvalRepl' $ buildTypecheck m
      pactEvalRepl' $ buildVerify m

    buildVerify m = "(verify " <> quotedFullName m <> ")"
    buildTypecheck m = "(typecheck " <> quotedFullName m <> ")"

-- | Run code in a transaction on the REPL.
runTransaction
  :: forall t m
  . ( ReplMonad t m )
  => Impl t -> Event t Text -> m (Event t (TransactionResult, ReplState))
runTransaction impl onCode =
  performEvent $ ffor onCode $ \code -> do
    withRepl impl $
      runIt code `catchError` cleanup

  where
    cleanup e = do
      void $ pactEvalRepl' "(rollback-tx)"
      throwError e

    runIt code = do
      void $ pactEvalRepl' "(begin-tx)"
      let parsed = parsePact code
      r <- ExceptT $ evalParsed code parsed
      void $ pactEvalRepl' "(commit-tx)"

      -- TODO: Will mis-behave if same module name is used in different namespaces
      rmns :: [ModuleName] <- (fmap . fmap) fst replModules
      let emns :: Map ModuleName Int = fromMaybe Map.empty $ parsed ^? TF._Success . to editorUnqualifiedModuleNames
          editorModules :: Map ModuleName Int = Map.fromList $ fforMaybe rmns $ \mn ->
            fmap (mn,) $ Map.lookup (mn & mnNamespace .~ Nothing) emns

      pure $ TransactionSuccess r editorModules

    -- TODO: can `replGetModules` actually change the state or is the type too coarse?
    replModules = do
      rs <- get
      liftIO (replGetModules rs) >>= \case
        Left err -> throwError $ show err
        Right (oldModules, rs') -> put rs' *> pure (HM.toList oldModules)

    -- TODO: Proper namespace support
    editorUnqualifiedModuleNames :: [Exp Parsed] -> Map ModuleName Int
    editorUnqualifiedModuleNames = Map.fromList . mapMaybe toModule
      where
        toModule :: Exp Parsed -> Maybe (ModuleName, Int)
        toModule = \case
          EList (ListExp (EAtom (AtomExp "module" _ _ _):EAtom (AtomExp m _ _ _):_) _ (Parsed (Delta.Lines l _ _ _) _))
            -> Just $ (ModuleName m Nothing, fromIntegral l)
          _ -> Nothing

    parsePact :: Text -> TF.Result [Exp Parsed]
    parsePact = TF.parseString exprsOnly mempty . T.unpack

    {- printExp :: Exp Parsed -> String -}
    {- printExp = \case -}
    {-   EList (ListExp (e:_) _ _) -> "List: (" <> printExp e <> ")\n" -}
    {-   EAtom a -> "Atom: (" <> show a <> ")\n" -}
    {-   ELiteral l -> "Literal: (" <> show l <> ")\n" -}
    {-   ESeparator _ -> "Separator.\n" -}

    evalParsed :: Text -> TF.Result [Exp Parsed] -> Repl (Either String (Term Name))
    evalParsed cmd = parsedCompileEval (T.unpack cmd)

-- | Drop n elements from the end of a list.
{- dropFromEnd :: Int -> [a] -> [a] -}
{- dropFromEnd n xs = zipWith const xs (drop n xs) -}

withRepl
  :: forall t m a
  . (Reflex t, MonadSample t m, MonadIO m)
  => Impl t
  -> PactRepl a
  -> m (Either String a, ReplState)
withRepl impl cmd = do
  st <- sample . current $ _impl_state impl
  liftIO $ flip runStateT st . runExceptT $ cmd

-- | Sets _and_ unsets repl lib.
pactEvalRepl' :: Text -> PactRepl (Term Name)
pactEvalRepl' t = ExceptT $ do
  id %= setReplLib
  r <- evalPact . T.unpack $ t
  e <- gets _rOut
  id %= unsetReplLib
  pure $ case e of -- This is somewhat questionable, but seems necessary.
    "" -> r
    e' -> Left e'

-- pactEvalPact :: Text -> PactRepl (Term Name)
-- pactEvalPact = ExceptT . evalPact . T.unpack

prettyResult :: Either String (Term Name) -> Text
prettyResult (Right v) = prettyTextPretty v
prettyResult (Left e)  = "Error: " <> T.pack e

-- Instances:
--
instance Reflex t => Semigroup (ReplCfg t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (ReplCfg t) where
  mempty = memptydefault
  mappend = (<>)

-- TODO: Those instances should really be derived via Generic.
instance Flattenable (ReplCfg t) t where
  flattenWith doSwitch ev =
    ReplCfg
      <$> doSwitch never (_replCfg_sendTransaction <$> ev)
      <*> doSwitch never (_replCfg_verifyModules <$> ev)
      <*> doSwitch never (_replCfg_sendCmd <$> ev)
      <*> doSwitch never (_replCfg_reset <$> ev)
