{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE ExtendedDefaultRules   #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Pact repl functionality as needed in pact-web.
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.Repl
  ( -- * Interface
    ReplCfg (..)
  , HasReplCfg (..)
  , WebRepl (..)
  , HasWebRepl (..)
  , ReplOutput (..)
    -- * Implementation and Creation
  , HasReplModelCfg
  , HasReplModel
  , makeRepl
  ) where

------------------------------------------------------------------------------
import           Control.Lens               hiding ((|>))
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Aeson                 as Aeson (Object, encode)
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.HashMap.Strict        as HM
import qualified Data.Map                   as Map
import           Data.Sequence              (Seq, (|>))
import qualified Data.Sequence              as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Generics.Deriving.Monoid   (mappenddefault, memptydefault)
import           GHC.Generics               hiding (to)
import           Reflex
------------------------------------------------------------------------------
import           Pact.Repl
import           Pact.Repl.Types
import           Pact.Types.Exp
import           Pact.Types.Term
------------------------------------------------------------------------------
import           Frontend.Backend
import           Frontend.Foundation
import           Frontend.JsonData
import           Frontend.Messages
import           Frontend.Wallet
------------------------------------------------------------------------------

-- | Output of Repl to be shown to the user.
data ReplOutput
  = ReplOutput_Cmd Text -- ^ Command that got entered/executed.
  | ReplOutput_Res Text -- ^ Result of executed command.


data ReplCfg t = ReplCfg
  { _replCfg_sendTransaction :: Event t Text
    -- ^ Send code to the REPL that won't be echoed and will be put in a transaction.
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
  { _repl_output            :: Dynamic t (Seq ReplOutput)
  -- ^ Repl output to be shown to the user.
  , _repl_newOutput         :: Event t Text
  -- ^ Gets triggered whenever there is some new output.
  --   Where output is really output, no echoed commands.
  , _repl_transactionFailed :: Event t Text
  -- ^ Execution of transaction failed with error. Will also be logged to Messages.
  }
  deriving Generic

makePactLenses ''WebRepl

-- | Result of `evalPact` and `evalRepl` function families.
type ReplResult = Either String (Term Name)

-- | `Repl` as used in pact-web.
type PactRepl = ExceptT String (StateT ReplState IO) (Term Name)

-- | Data used by internal functions of this module.
data Impl t = Impl
  { _impl_state :: Dynamic t ReplState
  , _impl_repl  :: WebRepl t
  }

-- Implementation:

type HasReplModel m t = (HasBackend m t, HasJsonData m t, HasWallet m t)

type HasReplModelCfg mConf t = (HasMessagesCfg mConf t, Monoid mConf)

type ReplMonad t m =
  ( Reflex t, MonadHold t m, MonadFix m, PostBuild t m, MonadIO m
  , PerformEvent t m, MonadIO (Performable m), MonadSample t (Performable m)
  )

makeRepl
  :: forall t m cfg model mConf
  . ( ReplMonad t m, HasReplModel model t, HasReplModelCfg mConf t
    , HasReplCfg cfg t
    )
  => model -> cfg -> m (mConf, WebRepl t)
makeRepl m cfg = build $ \ ~(_, impl) -> do
    onPostBuild <- getPostBuild
    -- Dummy state, that gets never used, so we avoid a pointless `Maybe` or
    -- sampling, which could trigger a loop:
    initState <- liftIO $ initReplState StringEval Nothing
    onResetSt <- performEvent $ initRepl impl m <$ leftmost
      [ cfg ^. replCfg_reset
      , onPostBuild
      ]

    let envData = either (const HM.empty) id <$> m ^. jsonData_data
        keys = Map.elems <$> m ^. wallet_keys

    onNewEnv  <- performStateCmd impl $ setEnvData <$> updated envData
    onNewKeys <- performStateCmd impl $ setEnvKeys <$> updated keys

    onNewTransR <- runTransaction impl $ cfg ^. replCfg_sendTransaction
    onNewCmdR <- runCmd impl $ cfg ^. replCfg_sendCmd

    let
      onLoad = leftmost [ cfg ^. replCfg_sendTransaction, cfg ^. replCfg_sendCmd ]

      mJsonError = (^? _Left . to showJsonError) <$> m ^. jsonData_data
      onEnvDataErr = fmapMaybe id . tag (current mJsonError) $ onLoad

      onNewTransResult = fmap fst onNewTransR
      onNewTransSt     = fmap snd onNewTransR
      onNewCmdResult = fmap fst onNewCmdR
      onNewCmdSt     = fmap snd onNewCmdR
    st <- holdDyn initState $ leftmost [ onResetSt
                                       , onNewTransSt
                                       , onNewCmdSt
                                       , onNewEnv
                                       , onNewKeys
                                       ]
    let appendHist = flip (|>)

    output <- foldDyn id S.empty $ mergeWith (.)
      [ appendHist . ReplOutput_Res . showTerm    <$> fmapMaybe (^? _Right) onNewTransResult
      , appendHist . ReplOutput_Res . showResult  <$> onNewCmdResult
      , appendHist . ReplOutput_Cmd               <$> cfg ^. replCfg_sendCmd
      , const S.empty <$ cfg ^. replCfg_reset
      ]

    let onFailedTrans = fmapMaybe (^? _Left . to T.pack) onNewTransResult
        onNewOutput = fmap showResult $ leftmost [ onNewTransResult, onNewCmdResult ]

    pure
      ( mempty
          & messagesCfg_send .~ leftmost [ onFailedTrans, onEnvDataErr ]
      , Impl
          { _impl_state = st
          , _impl_repl = WebRepl
              { _repl_output = output
              , _repl_transactionFailed = onFailedTrans
              , _repl_newOutput = onNewOutput
              }
          }
      )
  where
    build = fmap (fmap _impl_repl) . mfix

    performStateCmd impl = performEvent . fmap (fmap snd . withRepl impl)

    -- In case we ever want to show more than the last output term:
    -- showStateTerms :: ReplState -> Text
    -- showStateTerms = T.unlines . map showTerm . reverse . _rTermOut

    showResult :: Show n => Either String (Term n) -> Text
    showResult (Right v) = showTerm v
    showResult (Left e)  = "Error: " <> T.pack e

    showTerm :: Show n => Term n -> Text
    showTerm (TLiteral (LString t) _) = t
    showTerm t                        = T.pack $ show t

-- | Create a brand new Repl state and set env-data.
initRepl
  :: forall t m model
  . (HasReplModel model t, MonadIO m, Reflex t, MonadSample t m)
  => Impl t -> model -> m (ReplState)
initRepl oldImpl m = do
  r <- mkState m
  let initImpl = oldImpl { _impl_state = pure r }
  env  <- sample . current $ either (const HM.empty) id <$> m ^. jsonData_data
  keys <- sample . current $ Map.elems <$> m ^. wallet_keys
  fmap snd . withRepl initImpl $ do
    void $ setEnvData env
    setEnvKeys keys

-- | Create a brand new Repl state:
mkState
  :: forall t m model
  . (HasBackend model t, MonadIO m, Reflex t, MonadSample t m)
  => model -> m ReplState
mkState m = do
  let
    backends = m ^. backend_backends
    minBackend = ffor backends $ \maybeBackends -> do
      (_key, uri) <- Map.lookupMin =<< maybeBackends
      pure (T.unpack uri)
  cMinBackend <- sample . current $ minBackend
  liftIO $ initReplState StringEval cMinBackend


-- | Set env-data to the given Object
setEnvData :: Object -> PactRepl
setEnvData = pactEvalRepl' . ("(env-data " <>) . (<> ")") . mkCmd
  where
    mkCmd = toJsonString . T.decodeUtf8 . BSL.toStrict . encode

    escapeJSON = T.replace "\"" "\\\""

    toJsonString = surroundWith "\"" . escapeJSON

    surroundWith :: Semigroup s => s -> s -> s
    surroundWith o i = o <> i <> o

-- | Set env-keys to the given keys
setEnvKeys :: [KeyPair] -> PactRepl
setEnvKeys =
  pactEvalRepl' . ("(env-keys [" <>) . (<> "])") . renderKeys
    where
      renderKeys = T.unwords . map (keyToText . _keyPair_publicKey)

      keyToText = T.decodeUtf8 . BSL.toStrict . encode


-- | Run an interactive command on the REPL.
runCmd
  :: forall t m
  . ( ReplMonad t m )
  => Impl t -> Event t Text -> m (Event t (ReplResult, ReplState))
runCmd impl onCmd =
    performEvent $ withRepl impl . pactEvalPact <$> onCmd

-- | Run code in a transaction on the REPL.
runTransaction
  :: forall t m
  . ( ReplMonad t m )
  => Impl t -> Event t Text -> m (Event t (ReplResult, ReplState))
runTransaction impl onCode =
    performEvent $ ffor onCode $ \code -> withRepl impl $
      runIt code `catchError` cleanup
  where
    cleanup e = do
      void $ pactEvalRepl' "(rollback-tx)"
      throwError e

    runIt code = do
      void $ pactEvalRepl' "(begin-tx)"
      r <- pactEvalPact code
      void $ pactEvalRepl' "(commit-tx)"
      pure r


withRepl
  :: forall t m
  . (Reflex t, MonadSample t m, MonadIO m)
  => Impl t
  -> PactRepl
  -> m (ReplResult, ReplState)
withRepl impl cmd = do
  st <- sample . current $ _impl_state impl
  liftIO $ flip runStateT st . runExceptT $ cmd

pactEvalRepl' :: Text -> PactRepl
pactEvalRepl' = ExceptT . evalRepl' . T.unpack

pactEvalPact :: Text -> PactRepl
pactEvalPact = ExceptT . evalPact . T.unpack


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
      <*> doSwitch never (_replCfg_sendCmd <$> ev)
      <*> doSwitch never (_replCfg_reset <$> ev)
