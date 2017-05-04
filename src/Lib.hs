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
import Servant.Auth.Server (throwAll, AuthResult(..))

api :: Proxy API
api = Proxy

server :: Database db M => db -> Server API
server db = authServer :<|> choiceServer db

choiceServer :: Database db M => db -> AuthResult UserID -> Server ChoiceAPI
choiceServer db (Authenticated user)
     = list   db
  :<|> name   db
  :<|> view   db
  :<|> add    db
  :<|> choose db

choiceServer _db _authFail = throwAll err401

authServer :: Server AuthAPI
authServer = return []
        :<|> return []
        :<|> return []
        :<|> return []
