{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Enhancements ( app ) where

import Lib
import API
import Util
import DB.Class

import Servant
import Servant.Swagger
import Servant.Auth.Swagger ()
import Servant.Auth.Server
import Servant.JS hiding (requestBody)
import Data.Text
import Data.Swagger (Swagger(..) )
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Servant.Options
-- import qualified Network.Wai.Middleware.Debugging as L

type VanillaJS = "api-vanilla.js"   :> Get  '[PlainText] Text
type JQueryJS  = "api-jquery.js"    :> Get  '[PlainText] Text
type Swag      = "swagger.json" :> Get  '[JSON]      Swagger
type App       = VanillaJS
            :<|> JQueryJS
            :<|> Swag
            :<|> API
            :<|> Raw
            :<|> Redirect "index.html"

app :: (HasContextEntry x JWTSettings, HasContextEntry x CookieSettings, Database db M)
    => Context x -> db -> Application
app context db = logStdoutDev
               $ cors (const $ Just policy) -- simpleCors
               $ provideOptions api
               $ serveWithContext apiWithEnhancements context (serverWithSpec db js cs)
  where
  js     = getContextEntry context :: JWTSettings
  cs     = getContextEntry context :: CookieSettings
  policy = simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ] }

apiWithEnhancements :: Proxy App
apiWithEnhancements = Proxy

jsOptions :: CommonGeneratorOptions
jsOptions = defCommonGeneratorOptions -- { urlPrefix = "http://localhost:8080" }

serverWithSpec :: Database db M => db -> JWTSettings -> CookieSettings -> Server App
serverWithSpec db js cs
     = return (jsForAPI api (vanillaJSWith jsOptions))
  :<|> return (jsForAPI api (jqueryWith    jsOptions))
  :<|> return (toSwagger api)
  :<|> Lib.server db js cs
  :<|> serveDirectory ("frontend" :: String)
  :<|> redirectTo
