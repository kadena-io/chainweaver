module Pact.SigningApi
  (
  ) where

import Servant.Swagger

type SigningApi = "v1" :> V1SigningApi
type V1SigningApi = "sign" :> ReqBody '[JSON] SigningRequest :> Post '[JSON] SigningResponse

swaggerDocs :: IO BSL.ByteString
swaggerDocs = do
  let bsl = encode $ toSwagger (Proxy :: Proxy SigningApi)
  BSL8.putStrLn bsl
  -- return bsl
  error bsl
