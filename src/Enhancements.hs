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
import Network.Wai.Middleware.Servant.Options

-- Top Level App

apiWithEnhancements :: Proxy App
apiWithEnhancements = Proxy

app :: (HasContextEntry x JWTSettings, HasContextEntry x CookieSettings, Database db M)
    => Context x -> Bool -> db -> CommonGeneratorOptions -> Application
app context minify db jsOptions
    = cors (const $ Just policy) -- simpleCors
    $ provideOptions api
    $ serveWithContext apiWithEnhancements context (serverWithSpec minify db js jsOptions cs)
  where
  js     = getContextEntry context :: JWTSettings
  cs     = getContextEntry context :: CookieSettings
  policy = simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ] }

-- Enhanced API

type VanillaJS = "api-vanilla.js" :> Get '[PlainText] Text
type JQueryJS  = "api-jquery.js"  :> Get '[PlainText] Text
type Swag      = "swagger.json"   :> Get '[JSON]      Swagger
type App       = VanillaJS
            :<|> JQueryJS
            :<|> Swag
            :<|> API
            :<|> "assets" :> Raw
            :<|> Raw
            :<|> Redirect "index.html"

-- Server

serverWithSpec :: Database db M => Bool -> db -> JWTSettings -> CommonGeneratorOptions -> CookieSettings -> Server App
serverWithSpec minify db js jsOptions cs
     = return (jsForAPI api (vanillaJSWith jsOptions))
  :<|> return (jsForAPI api (jqueryWith    jsOptions))
  :<|> return (toSwagger api)
  :<|> Lib.server db js cs
  :<|> serveDirectory (ifThenElse minify "frontend/assets" "frontend/assets_development")
  :<|> serveDirectory ("frontend" :: String)
  :<|> redirectTo
