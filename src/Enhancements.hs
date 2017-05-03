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
import Servant.JS
import Data.Text
import Data.Swagger (Swagger(..) )
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Servant.Options
import Control.Monad.IO.Class

type LoginHead = Headers '[Header "Set-Cookie" SetCookie] UserID
type LoginAPI  = "login"        :> ReqBody '[JSON] Login :> Post '[JSON] LoginHead
type VanillaJS = "vanilla.js"   :> Get  '[PlainText] Text
type Swag      = "swagger.json" :> Get  '[JSON]      Swagger
type App       = VanillaJS
            :<|> Swag
            :<|> LoginAPI
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
  :<|> return (toSwagger api)
  :<|> login js cs
  :<|> Lib.server db
  :<|> serveDirectory ("frontend" :: String)
  :<|> redirectTo

login :: JWTSettings -> CookieSettings -> Server LoginAPI
login = checkCreds

checkCreds :: JWTSettings -> CookieSettings -> Login -> Handler LoginHead
checkCreds jwtSettings cookieSettings (Login "Ali Baba" "Open Sesame") = do
   let usr = UserID (UUID "testid")
   mcookie <- liftIO $ makeCookie cookieSettings jwtSettings usr
   case mcookie of
     Nothing     -> throwError err401
     Just cookie -> return $ addHeader cookie usr
checkCreds _ _ _ = throwError err401
