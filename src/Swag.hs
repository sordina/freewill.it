{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Swag ( app ) where

import Lib
import API
import Servant
import Servant.Swagger
import Data.Swagger (Swagger(..) )
import Control.Concurrent.STM.TVar (TVar())

type App  = Swag :<|> API
type Swag = "swagger.json" :> Get '[JSON] Swagger

app :: (TVar AppState) -> Application
app as = serve apiWithSpec (serverWithSpec as)

apiWithSpec :: Proxy App
apiWithSpec = Proxy

serverWithSpec :: (TVar AppState) -> Server App
serverWithSpec as = return (toSwagger api) :<|> server as
