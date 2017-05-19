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
import DB.Class

api :: Proxy API
api = Proxy

server :: Database db M => db -> JWTSettings -> CookieSettings -> Server API
server db js cs = authAPI      db js cs
             :<|> userInfo
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

userInfo :: (ThrowAll (m a), Monad m) => AuthResult a -> m a
userInfo (Authenticated u) = return u
userInfo _                 = throwAll err401

registerAndSetCookies :: Database db M => db -> JWTSettings -> CookieSettings -> Server RegisterAPI
registerAndSetCookies db js cs d = checkLogin db d >>= setCookie js cs

loginAndSetCookies :: Database db M => db -> JWTSettings -> CookieSettings -> Server LoginAPI
loginAndSetCookies db js cs d = checkLogin db d >>= setCookie js cs

checkLogin :: (m ~ M, MonadError ServantErr m, Database db m) => db -> LoginDetails -> m UserID
checkLogin _db (LoginDetails "Ali Baba" "Open Sesame") = return $ UserID (UUID "AliBabaXXX") -- TODO: Only use DB
checkLogin db  (LoginDetails un         pw           ) = login db un pw

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
