module Obelisk.Generated.Static where

static :: forall n. KnownSymbol n => Text
static = "static/" <> T.pack (symbolVal (Proxy @n))
