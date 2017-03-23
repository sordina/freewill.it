{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Enhancements ( app ) where

import Lib
import API
import Servant
import Servant.Swagger
import Servant.JS
import Data.Text
import Data.Swagger (Swagger(..) )
import Control.Concurrent.STM.TVar (TVar())

type App       = VanillaJS :<|> Swag :<|> API
type Swag      = "swagger.json" :> Get '[JSON] Swagger
type VanillaJS = "vanilla.js" :> Get '[PlainText] Text

app :: (TVar AppState) -> Application
app as = serve apiWithSpec (serverWithSpec as)

apiWithSpec :: Proxy App
apiWithSpec = Proxy

serverWithSpec :: (TVar AppState) -> Server App
serverWithSpec as = return (jsForAPI api vanillaJS)
               :<|> return (toSwagger api)
               :<|> server as

