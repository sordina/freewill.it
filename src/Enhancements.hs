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
import Servant.Auth
import Servant.Auth.Server
import Servant.JS
import Data.Text
import DB.Class
import Data.Swagger (Swagger(..) )
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Servant.Options
import Control.Monad.IO.Class
import Control.Monad.Error.Class

type LoginHead = Headers '[Header "Set-Cookie" SetCookie] User
type LoginAPI  = "login"        :> ReqBody '[JSON] Login :> Post '[JSON] LoginHead
type VanillaJS = "vanilla.js"   :> Get  '[PlainText] Text
type Swag      = "swagger.json" :> Get  '[JSON]      Swagger
type TestAuth  = "auth"         :> Auth '[JWT] User  :> Get '[JSON] User
type App       = VanillaJS
            :<|> Swag
            :<|> LoginAPI
            :<|> TestAuth
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
  :<|> protected -- TODO: Remove once we're happy
  :<|> server db
  :<|> serveDirectory ("frontend" :: String)
  :<|> redirectTo

login :: JWTSettings -> CookieSettings -> Server LoginAPI
login = checkCreds

checkCreds :: JWTSettings -> CookieSettings -> Login -> Handler LoginHead
checkCreds jwtSettings cookieSettings (Login "Ali Baba" "Open Sesame") = do
   let usr = User (Just (UserID (UUID "testid"))) "Ali" "Baba"
   mcookie <- liftIO $ makeCookie cookieSettings jwtSettings usr
   case mcookie of
     Nothing     -> throwError err401
     Just cookie -> return $ addHeader cookie usr
checkCreds _ _ _ = throwError err401


protected :: ( MonadError ServantErr m, MonadIO m, Show b ) => AuthResult b -> m b
protected (Authenticated user) = liftIO (putStrLn "Auth Succeeded :)") >> liftIO (print user) >> return user
protected err                  = liftIO (putStrLn "Auth Failed :("   ) >> liftIO (print err ) >> throwError err401
