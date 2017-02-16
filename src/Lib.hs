{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( startApp
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import API
import Util

{- TODO
   * Write tests
   * Deploy
-}

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

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
