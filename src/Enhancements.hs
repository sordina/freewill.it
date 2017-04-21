{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Enhancements ( app ) where

import Lib
import API
import Util
import Servant
import Servant.Swagger
import Servant.JS
import Data.Text
import DB.Class
import Data.Swagger (Swagger(..) )
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Servant.Options

type VanillaJS = "vanilla.js"   :> Get '[PlainText] Text
type Swag      = "swagger.json" :> Get '[JSON]      Swagger
type App       = VanillaJS
            :<|> Swag
            :<|> API
            :<|> Raw
            :<|> Redirect "index.html"

app :: Database db M => db -> Application
app db = logStdoutDev
       $ cors (const $ Just policy) -- simpleCors
       $ provideOptions api
       $ serve apiWithEnhancements (serverWithSpec db)
  where
  policy = simpleCorsResourcePolicy
             { corsRequestHeaders = [ "content-type" ] }

apiWithEnhancements :: Proxy App
apiWithEnhancements = Proxy

jsOptions :: CommonGeneratorOptions
jsOptions = defCommonGeneratorOptions -- { urlPrefix = "http://localhost:8080" }

serverWithSpec :: Database db M => db -> Server App
serverWithSpec db = return (jsForAPI api (vanillaJSWith jsOptions))
               :<|> return (toSwagger api)
               :<|> server db
               :<|> serveDirectory ("frontend" :: String)
               :<|> redirectTo
