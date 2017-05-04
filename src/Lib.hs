{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
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

import Servant

import API
import DB.Class
import Servant.Auth.Server
import Control.Monad.IO.Class

api :: Proxy API
api = Proxy

server :: Database db M => db -> JWTSettings -> CookieSettings -> Server API
server db js cs = login js cs :<|> choiceServer db

choiceServer :: Database db M => db -> AuthResult UserID -> Server ChoiceAPI
choiceServer db (Authenticated _user)
     = list   db
  :<|> name   db
  :<|> view   db
  :<|> add    db
  :<|> choose db

choiceServer _db _authFail = throwAll err401

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
