{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

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
import DB.Class

api :: Proxy API
api = Proxy

server :: Database db M => db -> JWTSettings -> CookieSettings -> Server API
server db js cs = authAPI db js cs
             :<|> choiceServer db

authAPI :: Database db M => db -> JWTSettings -> CookieSettings -> Server AuthAPI
authAPI _db js cs = register' js cs
               :<|> login'    js cs

choiceServer :: Database db M => db -> AuthResult UserID -> Server ChoiceAPI
choiceServer db (Authenticated u)
     = list   db u
  :<|> name   db u
  :<|> view   db u
  :<|> add    db u
  :<|> choose db u

choiceServer _db _authFail = throwAll err401

register' :: JWTSettings -> CookieSettings -> Server RegisterAPI
register' js cs d = checkLogin d >>= setCookie js cs

login' :: JWTSettings -> CookieSettings -> Server LoginAPI
login' js cs d = checkLogin d >>= setCookie js cs

checkLogin :: MonadError ServantErr m => LoginDetails -> m UserID
checkLogin (LoginDetails "Ali Baba" "Open Sesame") = return $ UserID (UUID "AliBabaXXX")
checkLogin _                                       = throwError err401

setCookie :: ( MonadError ServantErr m
             , AddHeader "Set-Cookie" SetCookie withOneCookie b
             , AddHeader "Set-Cookie" SetCookie response withOneCookie
             , ToJWT response
             , MonadIO m
             )
            => JWTSettings
            -> CookieSettings
            -> response
            -> m b

setCookie js cs x = do
   mApplyCookies <- liftIO $ acceptLogin cs js x
   case mApplyCookies of
     Nothing           -> throwError err401
     Just applyCookies -> return $ applyCookies x
