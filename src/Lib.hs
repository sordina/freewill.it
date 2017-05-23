{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}

module Lib
    ( api
    , server
    , API
    ) where

import Servant hiding (Secure, NotSecure)
import Servant.Auth.Server
import Control.Monad.IO.Class
import Control.Monad.Error.Class

import API
import Data
import DB.Class

api :: Proxy API
api = Proxy

server :: Database db M => db -> JWTSettings -> CookieSettings -> Server API
server db js cs = authAPI db js cs
             :<|> userInfo db
             :<|> logout
             :<|> choiceServer db

authAPI :: Database db M => db -> JWTSettings -> CookieSettings -> Server AuthAPI
authAPI db js cs = registerAndSetCookies db js cs
              :<|> loginAndSetCookies    db js cs

choiceServer :: Database db M => db -> AuthResult UserID -> Server ChoiceAPI
choiceServer db (Authenticated u)
     = list   db u
  :<|> name   db u
  :<|> view   db u
  :<|> add    db u
  :<|> choose db u

choiceServer _db _authFail = throwAll err401

userInfo :: (ThrowAll (m User), Me db m) => db -> AuthResult UserID -> m User
userInfo db (Authenticated u) = me db u
userInfo _db _                = throwAll err401

-- TODO: Use better header technique
logout :: (ThrowAll (m a)) => AuthResult t -> m a
logout (Authenticated _) = throwAll $ err302 { errHeaders = [ ("Location", "/")
                                                            , ("Set-Cookie", "JWT-Cookie=deleted; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT" ) ] }
logout _                 = throwAll err401


registerAndSetCookies :: Database db M => db -> JWTSettings -> CookieSettings -> Server RegisterAPI
registerAndSetCookies db js cs (LoginDetails un pw) = register db un pw >>= setCookie userId js cs

loginAndSetCookies :: Database db M => db -> JWTSettings -> CookieSettings -> Server LoginAPI
loginAndSetCookies db js cs d = checkLogin db d >>= setCookie userId js cs

checkLogin :: (m ~ M, MonadError ServantErr m, Database db m) => db -> LoginDetails -> m User
checkLogin db (LoginDetails un pw) = login db un pw

setCookie :: ( MonadError ServantErr m
             , AddHeader "Set-Cookie" SetCookie withOneCookie b
             , AddHeader "Set-Cookie" SetCookie response withOneCookie
             , ToJWT session, MonadIO m
             ) => (response -> session)
               -> JWTSettings
               -> CookieSettings
               -> response
               -> m b
setCookie f js cs x = do
   mApplyCookies <- liftIO $ acceptLogin cs js (f x)
   case mApplyCookies of
     Nothing           -> throwError err401
     Just applyCookies -> return $ applyCookies x
