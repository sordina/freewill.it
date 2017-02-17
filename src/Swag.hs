{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Swag ( app ) where

import Lib
import Servant
import Servant.Swagger
import Data.Swagger (Swagger(..) )

type App  = Swag :<|> API
type Swag = "swagger.json" :> Get '[JSON] Swagger

app :: Application
app = serve apiWithSpec serverWithSpec

apiWithSpec :: Proxy App
apiWithSpec = Proxy

serverWithSpec :: Server App
serverWithSpec = return (toSwagger api) :<|> server
