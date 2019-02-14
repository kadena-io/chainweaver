{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Prelude hiding ((.), id)
import Control.Category (id)
import Data.Text (Text)
import Data.Map (Map)
import Data.Functor.Identity
import Data.Functor.Sum
import Data.Semigroup ((<>))
import Control.Monad.Error (MonadError)

import Obelisk.Route
import Obelisk.Route.TH

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

  -- | Route for accessing configurations that can change at runtime.
  --
  -- E.g. the list of available pact backends.
  BackendRoute_DynConfigs :: BackendRoute [Text]

  -- | Serve robots.txt at the right place.
  BackendRoute_Robots :: BackendRoute ()

-- | Path on the server where all dynamic configurations are stored.
--
--   Like `BackendRoute_ConfigPactBackends` for example. It is a directory that
--   is writable by some user (root) and provides configuration to the
--   application that can change at runtime of the server.
dynConfigsRoot :: Text
dynConfigsRoot = "dyn-configs"

-- | URL path to the pact server list.
--
--   The frontend can retrieve the current pact server list from this path.
pactServerListPath :: Text
pactServerListPath = dynConfigsRoot <> "/pact-servers"


-- | This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.
data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_Example :: FrontendRoute [Text]  -- Route for loading an example.
  FrontendRoute_Stored  :: FrontendRoute [Text]  -- Route for loading a stored file/module.
  FrontendRoute_Deployed :: FrontendRoute [Text] -- Route for loading a deployed module.
  FrontendRoute_New :: FrontendRoute ()          -- Route when editing a new file.

backendRouteEncoder
  :: Encoder (Either Text) Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName
backendRouteEncoder = handleEncoder (const (InL BackendRoute_Missing :/ ())) $
  pathComponentEncoder $ \case
    InL backendRoute -> case backendRoute of
      BackendRoute_Missing
        -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_DynConfigs
        -> PathSegment dynConfigsRoot $ pathOnlyEncoder
      BackendRoute_Robots
        -> PathSegment "robots.txt" $ unitEncoder mempty
    InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
      FrontendRoute_Main
        -> PathEnd $ unitEncoder mempty
      FrontendRoute_Example
        -> PathSegment "example" $ pathOnlyEncoderIgnoringQuery
      FrontendRoute_Stored
        -> PathSegment "stored" $ pathOnlyEncoderIgnoringQuery
      FrontendRoute_Deployed
        -> PathSegment "deployed" $ pathOnlyEncoderIgnoringQuery
      FrontendRoute_New
        -> PathSegment "new" $ unitEncoder mempty

-- | Stolen from Obelisk as it is not exported. (Probably for a reason, but it
-- seems to do what we want right now.
pathOnlyEncoderIgnoringQuery :: (Applicative check, MonadError Text parse) => Encoder check parse [Text] PageName
pathOnlyEncoderIgnoringQuery = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_decode = \(path, _query) -> pure path
  , _encoderImpl_encode = \path -> (path, mempty)
  }

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]

