{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Enhancements ( app ) where

import Lib
import API
import Util
import Servant
import Servant.Swagger
import Servant.JS
import Data.Text
import Data.Swagger (Swagger(..) )
import Control.Concurrent.STM.TVar (TVar())
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger

type VanillaJS = "vanilla.js"   :> Get '[PlainText] Text
type Swag      = "swagger.json" :> Get '[JSON]      Swagger
type App       = VanillaJS
            :<|> Swag
            :<|> API
            :<|> Redirect "users"

app :: (TVar AppState) -> Application
app as = logStdoutDev $ simpleCors $ serve apiWithSpec (serverWithSpec as)

apiWithSpec :: Proxy App
apiWithSpec = Proxy

jsOptions :: CommonGeneratorOptions
jsOptions = defCommonGeneratorOptions { urlPrefix = "http://localhost:8080" }

serverWithSpec :: (TVar AppState) -> Server App
serverWithSpec as = return (jsForAPI api (vanillaJSWith jsOptions))
               :<|> return (toSwagger api)
               :<|> server as
               :<|> redirectTo
