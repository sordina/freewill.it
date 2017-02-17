{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( api
    , help
    , server
    , API
    ) where

import Data.Text
import Servant

import API
import Util

api :: Proxy API
api = Proxy

server :: Server API
server = return users -- users
    :<|> return users -- signup
    :<|> return users -- signin
    :<|> return users -- signout
    :<|> name         -- name
    :<|> return users -- extend
    :<|> return users -- view
    :<|> return users -- make
    :<|> redirectTo "/users"

-- Ex: curl -v -L -XPOST -H "Content-Type: application/json" --data '"lol"' http://localhost:8080/name | jq .
name :: Monad m => t -> m [User]
name _ = return users

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        , User 3 "Richard" "Feynman"
        ]

help :: IO ()
help = putStrLn $ Data.Text.unpack $ layout  (Proxy :: Proxy API)
