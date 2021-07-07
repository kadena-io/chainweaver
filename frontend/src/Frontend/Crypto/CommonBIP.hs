{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Crypto.CommonBIP where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad (PrimState, primitive))
import Control.Monad.Reader
import Control.Monad.Ref (MonadRef, MonadAtomicRef)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Aeson.GADT.TH
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Constraint.Extras.TH
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Text (Text)
import qualified Data.Text as T
import Data.Universe.Some.TH
import Language.Javascript.JSaddle (MonadJSM)
import Obelisk.Route.Frontend
import Pact.Server.ApiClient (HasTransactionLogger)
import Pact.Types.Util (parseB16TextOnly)
import Reflex.Dom hiding (fromJSString)
import Reflex.Host.Class (MonadReflexCreateTrigger)

-- import qualified Cardano.Crypto.Wallet as Crypto
-- import qualified Control.Newtype.Generics as Newtype
-- import qualified Data.Text.Encoding as T
-- import qualified Pact.Types.Crypto as PactCrypto
-- import qualified Pact.Types.Hash as Pact

import Frontend.Crypto.Ed25519
import Frontend.Crypto.Class
import Frontend.Foundation
import Frontend.Storage

-- Right now we have a restriction where this BIPStorage
-- can never actually change.
--
-- This feels like a good thing, because our frontend schema is pretty
-- locked into the Crypto key type not being upgradeable, and we really
-- want to store all of the frontend state in frontend. This spot for
-- the root key should be a special case and stay static, I hope.
--
-- I think that we should wait till we figure out what we want from web
-- and whether we can simplify the split before we do anything too crazy here.
-- Hopefully that happens before we have to modify this storage!
data BIPStorage a where
  BIPStorage_RootKey :: BIPStorage PrivateKey
deriving instance Show (BIPStorage a)

-- bipMetaPrefix :: StoreKeyMetaPrefix
-- bipMetaPrefix = StoreKeyMetaPrefix "BIPStorage_Meta"

-- -- | Check the validity of the password by signing and verifying a message
-- passwordRoundTripTest :: Crypto.XPrv -> Text -> Bool
-- passwordRoundTripTest xprv pass = Crypto.verify (Crypto.toXPub xprv) msg $ Crypto.sign (T.encodeUtf8 pass) xprv msg
--   where
--     msg :: ByteString
--     msg = "the quick brown fox jumps over the lazy dog"

concat <$> traverse ($ ''BIPStorage)
  [ deriveGShow
  , deriveGEq
  , deriveGCompare
  , deriveUniverseSome
  , deriveArgDict
  , deriveJSONGADT
  ]

