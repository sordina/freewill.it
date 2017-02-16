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

{-

TODO:

* Clean up redirection
* Implement Site-Map
  - Signup
  - Signin
  - Signout
  - Name Choice
  - Extend Choice
  - View Choice
  - Make Choice
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
server = return users :<|> redirectTo "/users"

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        , User 3 "Richard" "Feynman"
        ]
