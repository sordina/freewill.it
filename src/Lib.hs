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

import Servant hiding (Secure, NotSecure)
import Servant.Auth.Server
import Control.Monad.IO.Class

import API
import DB.Class

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
   -- Usually you would ask a database for the user info. This is just a
   -- regular servant handler, so you can follow your normal database access
   -- patterns (including using 'enter').
   let usr = UserID (UUID "AliBabaXXX")
   mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
   case mApplyCookies of
     Nothing           -> throwError err401
     Just applyCookies -> return $ applyCookies usr
checkCreds _ _ _ = throwError err401
