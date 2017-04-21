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
    , initialAppState
    , API
    ) where

import Servant

import API
import DB.Class
import DB.MemDB

api :: Proxy API
api = Proxy

server :: Database db M => db -> Server API
server db = authServer :<|> choiceServer db

choiceServer :: Database db M => db -> Server ChoiceAPI
choiceServer db = list   db
             :<|> name   db
             :<|> view   db
             :<|> add    db
             :<|> choose db

authServer :: Server AuthAPI
authServer = return mockUsers
        :<|> return mockUsers
        :<|> return mockUsers
        :<|> return mockUsers
  where
  AS _ _ _ mockUsers = initialAppState
